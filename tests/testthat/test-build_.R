test_that("build_ functions work", {
  
  files <- get_data(dir_only = FALSE)
  # setwd(d)
  
  txt <- capture.output(
    build_footer()
  )
  testthat::expect_true(grepl("^<p style='color:",txt))
  
  txt <- build_skill_bars(percent = "50")
  testthat::expect_true(grepl("^<div class = 'skill-bar'",txt))
  
  gg <- build_skills_plot(file = files$skills)
  testthat::expect_s3_class(gg,"gg")
  
  # testthat::test_path("cv_data")
  txt <- capture.output(
    build_summary(files = files)
  )
  testthat::expect_true(grepl('^<svg aria-hidden="true" role="img"',txt))
  
  txt <- capture.output(
    build_title()
  )
  testthat::expect_true(grepl('^## Leonardo da Vinci',txt[[1]]))  
  testthat::expect_true(grepl('^### My Degrees',txt[[7]]))  
  
  txt <- capture.output(
    build_toc()
  )
  testthat::expect_true(grepl('^<h4><svg aria-hidden="true" role="img',txt))  
  
  # library(textnets) ### Important
  # vn <- build_network()  
})
