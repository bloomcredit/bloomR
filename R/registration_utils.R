#' Make body for API call to register a consumer.
#'
#' @param consumer_info `list` containing the required named fields:
#' - `first_name`:
#' - `last_name`:
#' - `city`:
#' - `line1`:
#' - `state_code`:
#' - `zipcode`: (character) must be exactly 5 digits, e.g.: "90210".
#' - `address_primary`: (boolean) must be `TRUE` or `FALSE`.
#'
#' @return `list` body of registration request.
#' @keywords internal
make_registration_body <- function(consumer_info) {

  required_fields <- c("first_name",
                       "last_name",
                       "city",
                       "line1",
                       "state_code",
                       "zipcode",
                       "address_primary")


# consumer_list data checking ---------------------------------------------

  # all required fields are found in `consumer_info` parameter
  if (!all(required_fields %in% names(consumer_info))) {
    cli::cli_abort("Required fields missing - one of: {required_fields}")
  }

  # test that `address_primary` is boolean
  if (!is.logical(consumer_info$address_primary)) {
    cli::cli_abort("{.var consumer_info$address_primary must be TRUE or FALSE}")
  }


# construct sub-lists -----------------------------------------------------

  # handle address null values
  addresses <- list(
    list(
      city = consumer_info$city,
      hash = consumer_info$hash,
      line1 = consumer_info$line1,
      line2 = consumer_info$line2,
      primary = consumer_info$address_primary,
      state_code = consumer_info$state_code,
      type = consumer_info$address_type,
      zipcode = consumer_info$zipcode
    )
  ) %>%
    purrr::map( ~ .x %>% discard(is.null))

  # handle emails null values
  emails <- list(
    email_address = consumer_info$email_address,
    primary = consumer_info$email_primary,
    type = consumer_info$email_type
  ) %>%
    purrr::map( ~ .x %>% discard(is.null))

  # handle name null values
  name <- list(
    first_name = consumer_info$first_name,
    middle_name = consumer_info$middle_name,
    last_name = consumer_info$last_name,
    generation_code = consumer_info$generation_code
  ) %>%
    purrr::map( ~ .x %>% discard(is.null))

  # handle phones null values
  phones <- list(
    phone_number = consumer_info$phone_number,
    primary = consumer_info$phone_primary,
    type = consumer_info$phone_type
  ) %>%
    purrr::map( ~ .x %>% discard(is.null))

  # handle top-level attributes null values
  attributes <- list(
    ssn = consumer_info$ssn,
    addresses = addresses,
    date_of_birth = consumer_info$date_of_birth,
    emails = emails,
    income = consumer_info$income,
    ip_address = consumer_info$ip_address,
    name = name,
    phones = phones
  ) %>%
    purrr::map( ~ .x %>% discard(is.null)) %>%
    purrr::compact()


# construct body ----------------------------------------------------------
  body <- list(
    data = list(
      type = "consumers",
      attributes = attributes
    )
  )

  return(body)

}
