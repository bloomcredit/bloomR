

test_credit_data <- list(
  attributes = structure(
    list(
      order_id = "123123123",
      total_tradelines = "000000004",
      open_tradelines = "000000003",
      tradeline_open_last_3_months = "000000000",
      tradeline_open_last_6_months = "000000000",
      tradeline_open_last_9_months = "000000000",
      tradeline_open_last_12_months = "000000000",
      months_oldest_tradeline_opened = "000000341",
      months_recent_tradeline_opened = "000000154",
      months_recent_delinquency = "00000120",
      active_revolving_open_ended_tradelines_balance_opened_last_12_m = "000000000",
      maximum_bank_card_utilization = "000000249",
      months_oldest_revolving_tradeline_opened = "000000341",
      open_revolving_tradelines = "000000003",
      revolving_tradelines_opened_last_6_months = "000000000",
      total_revolving_tradelines = "000000004",
      months_oldest_mortgage_tradeline_opened = "000000998",
      open_mortgage_tradelines = "000000098",
      total_mortgage_tradelines = "000000000",
      months_recent_auto_tradeline_opened = "000000998",
      open_autotradelines = "000000098",
      total_auto_tradelines = "000000000",
      active_installment_tradelines = "000000000",
      active_installment_tradelines_opened_last_3_months = "000000098",
      months_recent_unsecured_installment_tradeline_delinquency = "000000996",
      months_recent_unsecured_installment_tradelines_opened = "000000998",
      open_unsecured_installment_tradelines = "000000098",
      unsecured_installment_tradelines_opened_6_months = "000000098",
      outstanding_balance_open_student_loan_tradelines = "999999998",
      total_student_loan_tradelines = "000000000",
      foreclosures = "000000000",
      months_recent_public_record = "000000999",
      months_recent_third_party_collection = "000000998",
      percent_opened_trades_last_24_months = "000000000",
      worst_rating_all_tradelines_last_12_months = "000000001"
    ),
    row.names = c(NA,-1L),
    class = c("tbl_df", "tbl", "data.frame")
  ),
  credit_data = structure(
    list(
      order_date = "2024-04-03",
      order_id = "123",
      consumer_id = "6abc123"
    ),
    class = c("tbl_df",
              "tbl", "data.frame"),
    row.names = c(NA,-1L)
  ),
  credit_scores = structure(
    list(
      id = c(
        "abc123",
        "xyz123"
      ),
      narrative = c(
        "Too few accounts currently paid as agreed",
        "Lack of recent installment loan information"
      ),
      score_id = c(
        "aaa",
        "aaa"
      ),
      model = c("FICO10",
                "FICO10"),
      value = c(750, 750),
      credit_data_id = c(
        "bbb",
        "bbb"
      )
    ),
    row.names = c(NA,-2L),
    class = c("tbl_df", "tbl", "data.frame")
  ),
  mla_statuses = structure(
    list(
      id = "abc",
      order_id = "def",
      referral_contact_number = "888-123-4567",
      regulated_identifier = "RG",
      covered_borrower_status = "Y"
    ),
    row.names = c(NA,-1L),
    class = c("tbl_df",
              "tbl", "data.frame")
  ),
  ofac_statuses = structure(
    list(
      reference = "",
      order_id = "aaaaaa",
      issue_source = "",
      id = "fffff"
    ),
    row.names = c(NA,-1L),
    class = c("tbl_df", "tbl", "data.frame")
  ),
  tradelines = structure(
    list(
      bureau = c(
        "EQUIFAX",
        "EQUIFAX",
        "EQUIFAX",
        "EQUIFAX",
        "EQUIFAX",
        "EQUIFAX"
      ),
      id = c(
        "xxxxxxx",
        "xxxxxxx",
        "xxxxxxx",
        "xxxxxxx",
        "xxxxxxx",
        "xxxxxxx"
      ),
      name.x = c(
        "JPMCB - CARD SERVICE",
        "EQUIFAX TEST DATA",
        "EQUIFAX TEST DATA",
        "EQUIFAX TEST DATA",
        "EQUIFAX TEST DATA",
        "EQUIFAX TEST DATA"
      ),
      account_number = c(
        "1234567891011",
        "1234567891012",
        "1234567891013",
        "1234567891014",
        "1234567891015",
        "1234567891016"
      ),
      date_opened = c(
        "2014-02-01",
        "2011-06-19",
        "1995-11-27",
        "2010-09-23",
        "2014-02-01",
        "2000-01-20"
      ),
      id.y = c(
        "1",
        "2",
        "3",
        "4",
        "5",
        "6"
      ),
      name.y = c("", "", "", "", "", ""),
      type = c(
        "REVOLVING",
        "REVOLVING",
        "REVOLVING",
        "REVOLVING",
        "REVOLVING",
        "REVOLVING"
      ),
      type_description = c(
        "Flexible Spending Credit Card",
        "Charge Account",
        "Credit Card",
        "Credit Card",
        "Flexible Spending Credit Card",
        "Charge Account"
      ),
      date_closed = c(NA, NA, NA, NA, NA, "2000-01-01")
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA,-6L)
  )
)


test_that("approved decision is correctly returned", {
  expect_equal(
    get_decision(credit_data = test_credit_data),
    "decision: approved"
  )
})


test_that("denied decision is correctly returned", {
  test <- test_credit_data
  test$credit_scores$value <- 500
  test$attributes$months_recent_delinquency <- 5

  expect_equal(
    get_decision(credit_data = test),
    "decision: denied"
  )

})


test_that("manual review decision is correctly returned", {

  test <- test_credit_data
  test$credit_scores$value <- 500

  expect_equal(
    get_decision(credit_data = test),
    "decision: manual review required"
  )

})


test_that("error is returned when insufficient data is provided", {
  test <- test_credit_data
  test$credit_scores <- NULL

  expect_error(get_decision(test))
})
