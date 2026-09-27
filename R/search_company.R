search_company <- function(query) {
  if (is.null(query) || nchar(trimws(query)) < 2) {
    return(data.frame(name = character(0), ticker = character(0)))
  }
  
  url <- paste0("https://www.screener.in/api/company/search/?q=", URLencode(query))
  
  result <- tryCatch({
    resp <- httr::GET(url, httr::add_headers("User-Agent" = "Mozilla/5.0"))
    if (httr::status_code(resp) != 200) {
      return(data.frame(name = character(0), ticker = character(0)))
    }
    parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    if (length(parsed) == 0 || is.null(parsed$name)) {
      return(data.frame(name = character(0), ticker = character(0)))
    }
    ticker <- gsub("^/company/([^/]+)/.*$", "\\1", parsed$url)
    data.frame(name = parsed$name, ticker = ticker, stringsAsFactors = FALSE)
  }, error = function(e) {
    data.frame(name = character(0), ticker = character(0))
  })
  
  result
}