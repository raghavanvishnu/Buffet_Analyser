stage_i_buy_price_sensitivity <- function(stage_b, stage_c, dividends, years = 10,
                                          offsets = c(-30, -20, -10, 0, 10, 20)) {
  current_price <- stage_b$current_price
  future_price_roe <- stage_b$future_price
  future_price_eps <- stage_c$future_price
  total_div_roe <- dividends$total_dividend_roe_method
  total_div_eps <- dividends$total_dividend_eps_method
  
  buy_prices <- current_price * (1 + offsets / 100)
  
  total_value_roe <- future_price_roe + total_div_roe
  total_value_eps <- future_price_eps + total_div_eps
  
  return_roe <- ((total_value_roe / buy_prices)^(1/years) - 1) * 100
  return_eps <- ((total_value_eps / buy_prices)^(1/years) - 1) * 100
  
  scenario_label <- ifelse(
    offsets == 0, "Today's Price",
    paste0(ifelse(offsets < 0, "Drops ", "Rises "), abs(offsets), "% before buying")
  )
  
  data.frame(
    Scenario = scenario_label,
    `Buy Price` = round(buy_prices, 2),
    `Total Return (ROE Method)` = paste0(round(return_roe, 2), "%"),
    `Total Return (EPS Method)` = paste0(round(return_eps, 2), "%"),
    check.names = FALSE
  )
}