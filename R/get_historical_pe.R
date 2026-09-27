get_historical_pe <- function(ticker, metrics, yahoo_symbol = NULL, year_end_dates = NULL) {
  if (is.null(yahoo_symbol)) yahoo_symbol <- paste0(ticker, ".NS")
  prices <- tidyquant::tq_get(yahoo_symbol, from = "2010-01-01", to = Sys.Date())
  
  eps_vals <- as.numeric(metrics$eps)
  year_labels <- names(metrics$eps)
  
  if (is.null(year_end_dates)) {
    year_ends <- as.Date(paste0("31 ", year_labels), format = "%d %b %Y")
  } else {
    year_ends <- year_end_dates
  }
  
  pe_vals <- sapply(seq_along(year_ends), function(i) {
    target_date <- year_ends[i]
    if (is.na(target_date)) return(NA)
    nearby <- prices[abs(as.numeric(prices$date - target_date)) <= 10, ]
    if (nrow(nearby) == 0) return(NA)
    closest <- nearby[which.min(abs(as.numeric(nearby$date - target_date))), ]
    closest$close / eps_vals[i]
  })
  
  names(pe_vals) <- year_labels
  pe_vals
}