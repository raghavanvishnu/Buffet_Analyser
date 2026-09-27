stage_h_pe_scenario_total_returns <- function(stage_e, dividends, current_price, years = 10) {
  total_div_roe <- dividends$total_dividend_roe_method
  total_div_eps <- dividends$total_dividend_eps_method
  
  future_price_roe <- stage_e[["Future Price (ROE Method)"]]
  future_price_eps <- stage_e[["Future Price (EPS Method)"]]
  
  total_value_roe <- future_price_roe + total_div_roe
  total_value_eps <- future_price_eps + total_div_eps
  
  return_roe <- ((total_value_roe / current_price)^(1/years) - 1) * 100
  return_eps <- ((total_value_eps / current_price)^(1/years) - 1) * 100
  
  data.frame(
    Scenario = stage_e$Scenario,
    `PE Used` = stage_e[["PE Used"]],
    `Total Value (ROE Method)` = round(total_value_roe, 2),
    `Total Return (ROE Method)` = paste0(round(return_roe, 2), "%"),
    `Total Value (EPS Method)` = round(total_value_eps, 2),
    `Total Return (EPS Method)` = paste0(round(return_eps, 2), "%"),
    check.names = FALSE
  )
}