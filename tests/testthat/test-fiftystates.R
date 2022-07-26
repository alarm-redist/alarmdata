test_that("50states_plans works", {
    x <- alarm_50state_plans('WY')
    expect_s3_class(x, 'data.frame')

})

test_that("50states_stats works", {
    x <- alarm_50state_stats("WY")
    expect_s3_class(x, 'data.frame')
})

test_that("50states_map works", {
    x <- alarm_50state_map('WY')
    expect_s3_class(x, 'data.frame')
})
