consumer_info <- list(
  "first_name" = "x",
  "last_name" = "x",
  "city" = "x",
  "line1" = "x",
  "state_code" = "x",
  "zipcode" = "x",
  "address_primary" = TRUE
)

test_that("expected fields are caught", {

  required_fields <- c("first_name",
                       "last_name",
                       "city",
                       "line1",
                       "state_code",
                       "zipcode",
                       "address_primary")


  for (i in names(consumer_info)) {
    ci <- consumer_info
    ci[[i]] <- NULL
    expect_error(make_registration_body(ci))
  }

})


test_that("address_primary field boolean value is caught", {
  ci <- consumer_info
  ci$address_primary <- "x"

  expect_error(make_registration_body(ci))
})



