stage_c_eps_method <- function(metrics, page, years = 10) {
  eps_vals <- as.numeric(metrics$eps)
  
  # EPS growth rate: CAGR from first to last available year
  eps_start <- eps_vals[1]
  eps_end <- eps_vals[length(eps_vals)]
  n_periods <- length(eps_vals) - 1
  eps_growth_rate <- ((eps_end / eps_start)^(1/n_periods) - 1) * 100
  
  # Future EPS (10 years from now)
  future_eps <- eps_end * (1 + eps_growth_rate / 100)^years
  
  # P/E ratio (reuse from top-ratios)
  ratios_text <- page %>% html_elements("#top-ratios li") %>% html_text2()
  pe_ratio <- ratios_text[grepl("Stock P/E", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  current_price <- ratios_text[grepl("Current Price", ratios_text)] %>% gsub("[^0-9.]", "", .) %>% as.numeric()
  
  # Future stock price
  future_price <- future_eps * pe_ratio
  
  # Expected rate of return
  expected_return <- ((future_price / current_price)^(1/years) - 1) * 100
  
  list(
    current_eps = eps_end,
    eps_growth_rate = eps_growth_rate,
    future_eps = future_eps,
    pe_ratio = pe_ratio,
    future_price = future_price,
    current_price = current_price,
    expected_return_eps_method = expected_return
  )
}