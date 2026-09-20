stage_f_total_return <- function(stage_b, stage_c, dividends, years = 10) {
  current_price <- stage_b$current_price
  
  total_value_roe <- stage_b$future_price + dividends$total_dividend_roe_method
  total_value_eps <- stage_c$future_price + dividends$total_dividend_eps_method
  
  return_roe <- ((total_value_roe / current_price)^(1/years) - 1) * 100
  return_eps <- ((total_value_eps / current_price)^(1/years) - 1) * 100
  
  data.frame(
    Method = c("ROE Method", "EPS Growth Method"),
    `Current Price` = round(current_price, 2),
    `Future Stock Price` = round(c(stage_b$future_price, stage_c$future_price), 2),
    `Total Dividends (10yr)` = round(c(dividends$total_dividend_roe_method, dividends$total_dividend_eps_method), 2),
    `Total Value` = round(c(total_value_roe, total_value_eps), 2),
    `Total Expected Return` = paste0(round(c(return_roe, return_eps), 2), "%"),
    check.names = FALSE
  )
}