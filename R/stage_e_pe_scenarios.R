stage_e_pe_scenarios <- function(ticker, metrics, stage_b, stage_c, years = 10) {
  historical_pe <- get_historical_pe(ticker, metrics)
  historical_pe <- historical_pe[is.finite(historical_pe) & historical_pe > 0]
  
  pe_min <- min(historical_pe, na.rm = TRUE)
  pe_mean <- mean(historical_pe, na.rm = TRUE)
  pe_max <- max(historical_pe, na.rm = TRUE)
  
  current_price <- stage_b$current_price
  
  compute_row <- function(pe_label, pe_value) {
    future_price_roe <- stage_b$future_eps * pe_value
    future_price_eps <- stage_c$future_eps * pe_value
    
    return_roe <- ((future_price_roe / current_price)^(1/years) - 1) * 100
    return_eps <- ((future_price_eps / current_price)^(1/years) - 1) * 100
    
    data.frame(
      Scenario = pe_label,
      `PE Used` = round(pe_value, 2),
      `Future Price (ROE Method)` = round(future_price_roe, 2),
      `Expected Return (ROE Method)` = paste0(round(return_roe, 2), "%"),
      `Future Price (EPS Method)` = round(future_price_eps, 2),
      `Expected Return (EPS Method)` = paste0(round(return_eps, 2), "%"),
      check.names = FALSE
    )
  }
  
  rbind(
    compute_row("Min Historical PE", pe_min),
    compute_row("Mean Historical PE", pe_mean),
    compute_row("Max Historical PE", pe_max),
    compute_row("Current PE", stage_b$pe_ratio)
  )
}