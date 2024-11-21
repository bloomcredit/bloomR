test_that("set_token_url returns correct URL for dev and prod", {

  expect_equal(
    set_token_url(audience = "dev"),
    "https://authn.bloom.dev/oauth2/token"
  )

  expect_equal(
    set_token_url(audience = "prod"),
    "https://authn.bloomcredit.io/oauth2/token"
  )

})

test_that("set_registration_url returns correct URL for dev and prod", {
  expect_equal(
    set_registration_url(audience = "dev"),
    "https://sandbox.bloom.dev/v2/core/consumers"
  )

  expect_equal(
    set_registration_url(audience = "prod"),
    "https://api.bloomcredit.io/v2/core/consumers"
  )
})

test_that("set_order_credit_data_url returns correct URL for dev and prod", {
  expect_equal(
    set_order_credit_data_url(audience = "dev"),
    "https://sandbox.bloom.dev/v2/data-access/orders/"
  )

  expect_equal(
    set_order_credit_data_url(audience = "prod"),
    "https://api.bloomcredit.io/v2/data-access/orders/"
  )
})

test_that("set_get_credit_data_url returns correct URL for dev and prod", {
  expect_equal(
    set_get_credit_data_url(audience = "dev", order_id = "123"),
    "https://sandbox.bloom.dev/v2/data-access/orders/123/full-report"
  )

  expect_equal(
    set_get_credit_data_url(audience = "prod", order_id = "123"),
    "https://api.bloomcredit.io/v2/data-access/orders/123/full-report"
  )
})
