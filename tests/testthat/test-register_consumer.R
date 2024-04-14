consumer_info <- list(
  ssn = "123456789",
  city = "Scranton",
  line1 = "1725 Slough Avenue",
  state_code = "PA",
  zipcode = "18503",
  date_of_birth = "1964-03-15",
  first_name = "Michael",
  last_name = "Scott",
  address_primary = TRUE
)

test_that("url cannot be null", {

  expect_error(
    register_consumer(
      url = NULL,
      consumer_info = consumer_info,
      auth_token = "123"
      )
  )

})


test_that("consumer info must be a list", {
  expect_error(
    register_consumer(
      consumer_info = data.frame(a = 1),
      auth_token = "123"
    )
  )
})


test_that("invalid auth_token returns correct response code", {
  expect_error(
    register_consumer(
      consumer_info = consumer_info,
      auth_token = "123"
      )
  )
})
