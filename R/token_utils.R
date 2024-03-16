#' Set the API URL based on audience.
#'
#' @param audience `character` Must be one of: "dev" or "prod", corresponding to the Audience Parameter.
#' See more here: https://developers.bloomcredit.io/docs/environments-1
#'
#' @examples
#' url <- set_url("dev")
#'
#' @keywords internal
#'
#' @return `character` URL for Oauth2 token API endpoint.
set_token_url <- function(audience) {
  if (tolower(audience) == "dev") {
    url <- "https://auth.bloom.dev/oauth/token"
  }

  if (tolower(audience) == "prod") {
    url <- "https://auth.bloomcredit.io/oauth/token"
  }

  return(url)
}
