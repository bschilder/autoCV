test_that("render_ functions work", {
  
  file <- get_template(save_dir = tempdir(),
                       force_new = TRUE)
  testthat::expect_true(file.exists(file))
  # browseURL(file)
  
  file_html <- render_cv(file = file)
  testthat::expect_true(file.exists(file_html))
})
