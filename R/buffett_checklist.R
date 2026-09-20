buffett_checklist <- function(metrics) {
  roe_vals <- as.numeric(metrics$roe)
  eps_vals <- as.numeric(metrics$eps)
  roce_vals <- as.numeric(metrics$roce)
  debt_vals <- as.numeric(metrics$debt_to_net_income)
  fcf_vals <- as.numeric(metrics$free_cash_flow)
  
  list(
    ticker = metrics$ticker,
    eps_upward_trend = eps_vals[length(eps_vals)] > eps_vals[1],
    roe_above_15 = mean(roe_vals, na.rm = TRUE) > 15,
    roic_above_12 = mean(roce_vals, na.rm = TRUE) > 12,
    debt_below_5x_ni = all(debt_vals < 5, na.rm = TRUE),
    fcf_positive = all(fcf_vals > 0, na.rm = TRUE)
  )
}