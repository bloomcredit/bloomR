# stopifnot(
#   "`consumer_id` must be of type character" = is.character(consumer_id),
#   "`portfolio_id` must be of type character" = is.character(portfolio_id),
#   "`sku` must be of type character" = is.character(sku),
#   "`url` cannot be null." = !is.null(url)
# )

test_that("consumer_id must be character type", {

  expect_error(
    order_credit_data(consumer_id = 123, portfolio_id = "123", sku = "123", auth_token = "123"),
    "`consumer_id` must be of type character"
  )

})

test_that("portfolio_id must be character type", {

  expect_error(
    order_credit_data(consumer_id = "123", portfolio_id = 123, sku = "123", auth_token = "123"),
    "`portfolio_id` must be of type character"
  )

})

test_that("sku must be character type", {

  expect_error(
    order_credit_data(consumer_id = "123", portfolio_id = "123", sku = 123, auth_token = "123"),
    "`sku` must be of type character"
  )

})

test_that("url cannot be null", {

  expect_error(
    order_credit_data(url = NULL, consumer_id = "123", portfolio_id = "123", sku = "123", auth_token = "123")
  )

})

test_that("incorrect data returns error", {
  expect_error(
    order_credit_data(consumer_id = "123", portfolio_id = "123", sku = "123", auth_token = "123")
  )
})
