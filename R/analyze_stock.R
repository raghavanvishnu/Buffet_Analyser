analyze_stock <- function(ticker, years = 10) {
  url <- paste0("https://www.screener.in/company/", ticker, "/consolidated/")
  page <- read_html(url)
  
  metrics <- get_buffett_metrics(ticker)
  profile <- get_company_profile(ticker)
  checklist <- buffett_checklist(metrics)
  
  ratios_text <- page %>% html_elements("#top-ratios li") %>% html_text2()
  current_pe <- ratios_text[grepl("Stock P/E", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  historical_pe <- get_historical_pe(ticker, metrics)
  historical_pe <- historical_pe[is.finite(historical_pe) & historical_pe > 0]
  avg_historical_pe <- mean(historical_pe, na.rm = TRUE)
  pe_to_use <- min(avg_historical_pe, current_pe, na.rm = TRUE)
  
  stage_a <- stage_a_initial_return(metrics, page)
  stage_b <- stage_b_roe_method(metrics, page, pe_to_use = pe_to_use, years = years)
  stage_c <- stage_c_eps_method(metrics, page, pe_to_use = pe_to_use, years = years)
  stage_d <- stage_d_verdict(stage_b, stage_c)
  stage_e <- stage_e_pe_scenarios(ticker, metrics, stage_b, stage_c, years = years)
  dividends <- project_dividends(stage_b, stage_c, current_eps = stage_a$current_eps, years = years)
  stage_f <- stage_f_total_return(stage_b, stage_c, dividends, years = years)
  stage_g <- stage_g_total_verdict(stage_f)
  stage_h <- stage_h_pe_scenario_total_returns(stage_e, dividends, current_price = stage_b$current_price, years = years)
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