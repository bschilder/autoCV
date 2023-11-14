test_that("parse_name works", {
  
  #### parse_name ####
  text <- "I am LD vinci the painter."
  name <- "Leonardo da Vinci"
  text2 <- parse_name(name=name,
                      text=text)
  testthat::expect_equal(
    text2,
    "I am **Ld Vinci** the painter."
  )
  text3 <- parse_name(name=name)
  testthat::expect_length(text3,8)
})
