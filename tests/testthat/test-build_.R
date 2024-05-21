test_that("build_ functions work", {
  
  wd <- get_data() 
  
  txt <- capture.output(
    build_footer()
  )
  testthat::expect_true(grepl("^<p style='color:",txt))
  
  txt <- build_skill_bars(percent = "50")
  testthat::expect_true(grepl("^<div class = 'skill-bar'",txt))
  
  bsp <- build_skills_plot(wd = wd)
  testthat::expect_s3_class(bsp$plot,"gg")
  
  # testthat::test_path("cv_data")
  txt <- capture.output(
    build_summary(wd = wd)
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
