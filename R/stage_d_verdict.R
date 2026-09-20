stage_d_verdict <- function(stage_b, stage_c) {
  avg_return <- (stage_b$expected_return_roe_method + stage_c$expected_return_eps_method) / 2
  
  verdict <- if (avg_return > 20) {
    "FANTASTIC"
  } else if (avg_return >= 12) {
    "GOOD"
  } else {
    "UNACCEPTABLE"
  }
  
  list(
    avg_expected_return = avg_return,
    verdict = verdict
  )
}