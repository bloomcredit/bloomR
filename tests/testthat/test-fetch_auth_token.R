test_that("fetch_auth_token audience parameter errors out as intended", {
  # audience parameter must be one of: "dev" or "prod"
  expect_error(fetch_auth_token(url = "a"))
})

test_that("improper credentials return error", {
  expect_error(fetch_auth_token(url = "dev", client_id = "a", client_secret = "a"))
})
