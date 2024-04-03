#' Order Credit Data
#'
#' @param url `character` URL to endpoint.
#' @param consumer_id `character` Consumer ID.
#' @param portfolio_id `character` Portfolio ID.
#' @param sku `character` SKU enabled for your account. E.g: "equifax-gold-soft-fico-internet".
#' @param auth_token `character` Oauth2 token returned by [bloomR::fetch_auth_token].
#'
#' @return Response from placing a credit order.
#'
#' @examples
#' \dontrun{
#' credit_data <- order_credit_data(
#'   audience = "dev",
#'   consumer_id = "your_consumer_id",
#'   portfolio_id = "your_portfolio_id",
#'   sku = "sku_you_want_to_order",
#'   auth_token = "your_auth_token"
#' )
#' }
#'
#' @export
order_credit_data <- function(url = set_order_credit_data_url("dev"), consumer_id, portfolio_id, sku, auth_token) {

# create attributes list --------------------------------------------------
  stopifnot(
    "`consumer_id` must be of type character" = is.character(consumer_id),
    "`portfolio_id` must be of type character" = is.character(portfolio_id),
    "`sku` must be of type character" = is.character(sku),
    "`url` cannot be null." = !is.null(url)
  )

  attributes = list(
    consumer_id = consumer_id,
    portfolio_id = portfolio_id,
    sku = sku
  )

# create body -------------------------------------------------------------
  body <- list(
    data = list(
      type = "order",
      attributes = attributes
    )
  )

# send request ------------------------------------------------------------
  response <- httr::POST(
    url,
    httr::add_headers("Authorization" = paste("Bearer", auth_token), "Content-Type" = "application/json"),
    body = body,
    encode = "json"
  )


# handle request ----------------------------------------------------------
  httr::content(response)

}
