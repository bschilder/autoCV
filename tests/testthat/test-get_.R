test_that("get_ functions work", {
  
  css <- get_css()
  testthat::expect_true(file.exists(css))
  testthat::expect_equal(basename(css),"override.css")
  
  d <- get_data() 
  testthat::expect_true(file.exists(d))
  testthat::expect_equal(basename(d),"cv_data")
  
  files <- get_data(dir_only = FALSE) 
  testthat::expect_true(all(sapply(files,file.exists))) 
  
  files2 <- get_data(dir_only = FALSE, 
                     dir_manual = here::here()) 
  testthat::expect_equal(basename(unlist(files)),
                         basename(unlist(files2)))
  
  logo <- get_logo()
  testthat::expect_true(grepl("<a href='https://github.com/bschilder/autoCV'",logo))
  
  template <- get_template(save_dir = tempdir()) 
  testthat::expect_true(file.exists(template)) 
})
