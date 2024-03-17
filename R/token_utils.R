#' Set your `client_id` using `options()`.
#'
#' @param client_id `character` Your Bloom Credit API `client_id`.
#' @export
set_client_id <- function(client_id) {

  proceed <- usethis::ui_yeah(
    x = "Using `set_client_id()` will store your `client_id` in your R options. Continue?"
  )

  if (proceed) {
    options("bloom_api_client_id" = client_id)
    cli::cli_alert_success("{.arg client_id} set.")
    cli::cli_alert_info("Run {.code getOption('bloom_api_client_id')} to view your `client_id`.")
  }
}

#' Set your `client_secret` using `options()`.
#'
#' @param client_secret `character` Your Bloom Credit API `client_secret`.
#' @export
set_client_secret <- function(client_secret) {

  proceed <- usethis::ui_yeah(
    x = "Using `set_client_secret()` will store your `client_secret` in your R options. Continue?"
  )

  if (proceed) {
    options("bloom_api_client_secret" = client_secret)
    cli::cli_alert_success("{.arg client_secret} set.")
    cli::cli_alert_info("Run {.code getOption('bloom_api_client_secret')} to view your `client_secret`.")
  }
}
