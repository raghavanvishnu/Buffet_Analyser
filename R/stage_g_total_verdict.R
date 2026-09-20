stage_g_total_verdict <- function(stage_f) {
  returns <- as.numeric(gsub("%", "", stage_f[["Total Expected Return"]]))
  avg_return <- mean(returns)
  
  verdict <- if (avg_return > 20) {
    "FANTASTIC"
  } else if (avg_return >= 12) {
    "GOOD"
  } else {
    "UNACCEPTABLE"
  }
  
  list(
    avg_total_expected_return = avg_return,
    verdict = verdict
  )
}