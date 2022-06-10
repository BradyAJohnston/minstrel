testthat::test_that("Test that it reads", {
  testthat::expect_success({
    fl <- system.file("extdata", "test_plate.xml", package = "minstrel")
    minstrel::read_recipe(fl)
    testthat::expect(TRUE, failure_message = "Failed to read recipe.")
  })
})
