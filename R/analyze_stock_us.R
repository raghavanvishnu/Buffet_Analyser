#' Run the full Buffett-style analysis for a US ticker (roic.ai data)
#'
#' Parallel to analyze_stock() (the India/Screener.in version). Stages D
#' through I are pure calculation functions with no market-specific
#' dependencies, so they are reused unchanged. Stages A, B, and C originally
#' scrape a few extra values (current price, book value, payout ratio)
#' directly from a Screener.in "page" object, which doesn't exist for the
#' US/roic.ai path — so those three are recomputed here inline using
#' tidyquant (price) and roic.ai data (book value via the EPS/ROE
#' relationship, payout via dividends/EPS).
#'
#' @param ticker US ticker symbol as used by roic.ai (e.g. "AAPL")
#' @param years Projection horizon (default 10)
#' @return Same list shape as analyze_stock(): ticker, profile, metrics,
#'   checklist, stage_a, stage_b, stage_c, stage_d, stage_e, dividends,
#'   stage_f, stage_g, stage_h, stage_i
analyze_stock_us <- function(ticker, years = 10) {
  metrics <- get_buffett_metrics_us(ticker)
  checklist <- buffett_checklist(metrics)
  
  # Minimal profile - roic.ai doesn't expose a company description/pros-cons
  # the way Screener.in does, so this stays blank rather than guessing.
  profile <- get_company_profile_us(ticker)
  
  # Current price via tidyquant (no .NS suffix needed for US tickers)
  recent_prices <- tidyquant::tq_get(ticker, from = Sys.Date() - 15, to = Sys.Date())
  current_price <- tail(recent_prices$close, 1)
  
  eps_vals <- as.numeric(metrics$eps)
  roe_vals <- as.numeric(metrics$roe)
  current_eps <- eps_vals[length(eps_vals)]
  
  # Book value per share, derived from EPS and ROE for each year:
  # ROE = Net Income / Equity, EPS = Net Income / Shares
  # => Equity / Shares (book value per share) = EPS / (ROE / 100)
  book_value_vals <- eps_vals / (roe_vals / 100)
  current_book_value <- book_value_vals[length(book_value_vals)]
  
  # Payout ratio: dividends per share / EPS, per year, if roic.ai had a
  # dividends row; otherwise assume no dividends (payout = 0) rather than
  # guessing, and note it's an assumption in the report via avg_payout = 0.
  if (!is.null(metrics$dividends_per_share)) {
    dps_vals <- as.numeric(metrics$dividends_per_share)
    payout_vals <- (dps_vals / eps_vals) * 100
  } else {
    payout_vals <- rep(0, length(eps_vals))
  }
  
  # Approximate P/E: current price / current EPS (roic.ai's ratios page
  # doesn't expose a simple "current P/E" the way Screener's top-ratios does)
  current_pe <- current_price / current_eps
  
  # Historical PE: reuse get_historical_pe() with US-specific args
  year_end_dates <- as.Date(names(metrics$eps), format = "%m/%d/%Y")
  historical_pe <- get_historical_pe(ticker, metrics, yahoo_symbol = ticker, year_end_dates = year_end_dates)
  historical_pe <- historical_pe[is.finite(historical_pe) & historical_pe > 0]
  avg_historical_pe <- mean(historical_pe, na.rm = TRUE)
  pe_to_use <- if (is.finite(avg_historical_pe)) min(avg_historical_pe, current_pe, na.rm = TRUE) else current_pe
  
  # Sovereign bond comparison (US Treasury as primary, since this is a US stock)
  bond_yields <- c(
    "United States (USD, Treasury)" = 4.24,
    "India (INR, G-Sec)" = 6.70,
    "United Kingdom (GBP, Gilt)" = 4.53,
    "Germany (EUR, Bund)" = 2.85,
    "Japan (JPY)" = 2.25,
    "China (CNY)" = 1.80
  )
  initial_rate_of_return <- (current_eps / current_price) * 100
  bond_comparison <- data.frame(
    Benchmark = names(bond_yields),
    Yield = paste0(bond_yields, "%"),
    Better_Than_Stock = ifelse(initial_rate_of_return > bond_yields, "Stock Wins", "Bond Wins")
  )
  
  stage_a <- list(
    current_eps = current_eps,
    current_price = current_price,
    initial_rate_of_return = initial_rate_of_return,
    bond_comparison = bond_comparison
  )
  
  # Stage B: ROE method
  avg_roe <- mean(tail(roe_vals, years), na.rm = TRUE)
  avg_payout <- mean(tail(payout_vals, years), na.rm = TRUE)
  growth_rate <- avg_roe * (1 - avg_payout / 100)
  future_book_value <- current_book_value * (1 + growth_rate / 100)^years
  future_eps_b <- future_book_value * (avg_roe / 100)
  future_price_b <- future_eps_b * pe_to_use
  expected_return_b <- ((future_price_b / current_price)^(1/years) - 1) * 100
  
  stage_b <- list(
    avg_roe = avg_roe,
    avg_payout = avg_payout,
    growth_rate = growth_rate,
    book_value = current_book_value,
    future_book_value = future_book_value,
    future_eps = future_eps_b,
    pe_ratio = pe_to_use,
    future_price = future_price_b,
    current_price = current_price,
    expected_return_roe_method = expected_return_b
  )
  
  # Stage C: EPS growth method
  eps_start <- eps_vals[1]
  eps_end <- eps_vals[length(eps_vals)]
  n_periods <- length(eps_vals) - 1
  eps_growth_rate <- ((eps_end / eps_start)^(1/n_periods) - 1) * 100
  future_eps_c <- eps_end * (1 + eps_growth_rate / 100)^years
  future_price_c <- future_eps_c * pe_to_use
  expected_return_c <- ((future_price_c / current_price)^(1/years) - 1) * 100
  
  stage_c <- list(
    current_eps = eps_end,
    eps_growth_rate = eps_growth_rate,
    future_eps = future_eps_c,
    pe_ratio = pe_to_use,
    future_price = future_price_c,
    current_price = current_price,
    expected_return_eps_method = expected_return_c
  )
  
  # Stages D-I: pure calculation functions, reused unchanged
  stage_d <- stage_d_verdict(stage_b, stage_c)
  
  # Stage E needs a bit of adaptation since stage_e_pe_scenarios() calls
  # get_historical_pe(ticker, metrics) internally with the India default -
  # build it manually here using the historical_pe we already computed
  pe_min <- min(historical_pe, na.rm = TRUE)
  pe_mean <- avg_historical_pe
  pe_max <- max(historical_pe, na.rm = TRUE)
  compute_e_row <- function(pe_label, pe_value) {
    fp_roe <- future_eps_b * pe_value
    fp_eps <- future_eps_c * pe_value
    ret_roe <- ((fp_roe / current_price)^(1/years) - 1) * 100
    ret_eps <- ((fp_eps / current_price)^(1/years) - 1) * 100
    data.frame(
      Scenario = pe_label,
      `PE Used` = round(pe_value, 2),
      `Future Price (ROE Method)` = round(fp_roe, 2),
      `Expected Return (ROE Method)` = paste0(round(ret_roe, 2), "%"),
      `Future Price (EPS Method)` = round(fp_eps, 2),
      `Expected Return (EPS Method)` = paste0(round(ret_eps, 2), "%"),
      check.names = FALSE
    )
  }
  stage_e <- rbind(
    compute_e_row("Min Historical PE", pe_min),
    compute_e_row("Mean Historical PE", pe_mean),
    compute_e_row("Max Historical PE", pe_max),
    compute_e_row("Current PE", pe_to_use)
  )
  
  dividends <- project_dividends(stage_b, stage_c, current_eps = current_eps, years = years)
  stage_f <- stage_f_total_return(stage_b, stage_c, dividends, years = years)
  stage_g <- stage_g_total_verdict(stage_f)
  stage_h <- stage_h_pe_scenario_total_returns(stage_e, dividends, current_price = current_price, years = years)
  stage_i <- stage_i_buy_price_sensitivity(stage_b, stage_c, dividends, years = years)
  
  list(
    ticker = ticker,
    profile = profile,
    metrics = metrics,
    checklist = checklist,
    stage_a = stage_a,
    stage_b = stage_b,
    stage_c = stage_c,
    stage_d = stage_d,
    stage_e = stage_e,
    dividends = dividends,
    stage_f = stage_f,
    stage_g = stage_g,
    stage_h = stage_h,
    stage_i = stage_i
  )
}