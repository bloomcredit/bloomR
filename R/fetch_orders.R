fetch_orders <- function(audience, auth_token) {

  #library(httr)

  url <- "https://sandbox.bloom.dev/v2/data-access/orders"

  headers <- c("accept" = "application/json", "Authorization" = paste("Bearer", auth_token))

  response <- GET(url, add_headers(headers))

  result <- content(response)

}
