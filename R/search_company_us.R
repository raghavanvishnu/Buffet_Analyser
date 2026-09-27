#' Search roic.ai for a US company by name and get matching tickers
#'
#' Uses roic.ai's authenticated ticker search endpoint. Requires a free
#' API key stored as the ROIC_API_KEY environment variable (see .Renviron) -
#' never hardcode the key in this file or commit it to version control.
#'
#' @param query Company name or partial name, e.g. "Apple"
#' @return A data.frame with columns: name, ticker (empty if no matches
#'   or if the request fails)
search_company_us <- function(query) {
  if (is.null(query) || nchar(trimws(query)) < 2) {
    return(data.frame(name = character(0), ticker = character(0)))
  }
  
  api_key <- Sys.getenv("ROIC_API_KEY")
  if (api_key == "") {
    warning("ROIC_API_KEY is not set - US company search is unavailable.")
    return(data.frame(name = character(0), ticker = character(0)))
  }
  
  result <- tryCatch({
    resp <- httr::GET(
      "https://api.roic.ai/v2/tickers/search",
      query = list(query = query, search_by = "name", limit = 10, apikey = api_key)
    )
    if (httr::status_code(resp) != 200) {
      return(data.frame(name = character(0), ticker = character(0)))
    }
    parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    
    # Response shape may be a plain list/data.frame, or wrapped under a
    # "data"/"results" field - handle both without assuming which.
    if (is.data.frame(parsed)) {
      df <- parsed
    } else if (!is.null(parsed$data)) {
      df <- parsed$data
    } else if (!is.null(parsed$results)) {
      df <- parsed$results
    } else {
      return(data.frame(name = character(0), ticker = character(0)))
    }
    
    if (nrow(df) == 0) return(data.frame(name = character(0), ticker = character(0)))
    
    name_col <- intersect(c("name", "companyName", "company_name"), names(df))[1]
    ticker_col <- intersect(c("ticker", "symbol"), names(df))[1]
    if (is.na(name_col) || is.na(ticker_col)) {
      return(data.frame(name = character(0), ticker = character(0)))
    }
    
    result_df <- data.frame(name = df[[name_col]], ticker = df[[ticker_col]], stringsAsFactors = FALSE)
    # Keep only primary NYSE/Nasdaq listings - foreign listings on roic.ai
    # use a dot suffix (e.g. AAPL.MX, AAPL.DE, AAPL.TO) which this drops
    result_df[!grepl("\\.", result_df$ticker), ]
  }, error = function(e) {
    data.frame(name = character(0), ticker = character(0))
  })
  
  result
}