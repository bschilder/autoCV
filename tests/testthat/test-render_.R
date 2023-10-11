test_that("render_ functions work", {
  
  file <- get_template()
  file_html <- render_cv(file = file)
  testthat::expect_true(file.exists(file_html))
})
