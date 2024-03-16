make_registration_body <- function(consumer_info) {

  consumer_info <- list(
    ssn = "",
    date_of_birth = "",
    first_name = "",
    last_name = "",
    line1 = "",
    city = "",
    state_code = "",
    zipcode = "",
    primary = ""
  )

  required_fields <- c("first_name",
                       "last_name",
                       "city",
                       "line1",
                       "state_code",
                       "zipcode")

  stopifnot(
    all(required_fields) %in% names(consumer_info)
  )

  data <- list(
    data = list(
      type = "consumers",
      attributes = list(
        ssn = "123456789",                 # character (format: 9 digits only, space and "-" allowed)
        date_of_birth = "1964-03-15",      # character (format: yyyy-MM-dd or yyyyMMdd)
        name = list(
          first_name = "Michael",          # character (required)
          middle_name = NULL,              # character
          last_name = "Scott",             # character (required)
          generation_code = NULL           # character
        ),
        addresses = list(
          list(
            city = "Scranton",             # character (required)
            hash = NULL,                   # character
            line1 = "1725 Slough Avenue",  # character (required)
            line2 = NULL,                  # character
            primary = NULL,                # boolean
            state_code = "PA",             # character (required)
            type = NULL,                   # character
            zipcode = "18503"              # character (required - 5 digit)
          )
        )
      )
    )
  )

}
