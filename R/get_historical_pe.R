get_historical_pe <- function(ticker, metrics) {
  prices <- tidyquant::tq_get(paste0(ticker, ".NS"), from = "2014-01-01", to = Sys.Date())
  
  eps_vals <- as.numeric(metrics$eps)
  year_labels <- names(metrics$eps)
  year_ends <- as.Date(paste0("31 ", year_labels), format = "%d %b %Y")
  
  pe_vals <- sapply(seq_along(year_ends), function(i) {
    target_date <- year_ends[i]
    nearby <- prices[abs(as.numeric(prices$date - target_date)) <= 10, ]
    if (nrow(nearby) == 0) return(NA)
    closest <- nearby[which.min(abs(as.numeric(nearby$date - target_date))), ]
    closest$close / eps_vals[i]
  })
  
  names(pe_vals) <- year_labels
  pe_vals
}