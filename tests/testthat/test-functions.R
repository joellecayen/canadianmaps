# canadianmaps testing #

#context("check-functions")  # The file is called "test-functions.R"
library(testthat)        # load testthat package
library(canadianmaps)    # load my package



#scale_fill_map
test_that("check-functions: scale_fill_map() behaves like a ggplot manual fill object", {

  output <- scale_fill_map(palette = "Kelly", num = 5)
  output$train(LETTERS[1:5])
  values <- output$map(c(LETTERS[1:3], NA))
  expect_equal(values, c("#B0DFE5", "#D9EEB4", "#FDB462", "grey90"))
})

#scale_color_map
test_that("check-functions: scale_color_map() behaves like a ggplot manual fill object", {

  output <- scale_color_map(palette = "Kelly", num = 5)
  output$train(LETTERS[1:5])
  values <- output$map(c(LETTERS[1:3], NA))
  expect_equal(values, c("#B0DFE5", "#D9EEB4", "#FDB462", "grey90"))

})


#coord_transform
test_that("check-functions: coord_transform() returns a dataframe", {

  lat <- c(2,3,4)
  long <- c(5,6,7)

  data <- data.frame(lat, long)

  output <- data %>%
    coord_transform(long = "long", lat = "lat")

  expect_s3_class(output, "data.frame")


})

#FSA
test_that("check-functions: FSA returns a dataframe", {

  output <- FSA

  expect_s3_class(output, "data.frame")

})

#PROV
test_that("check-functions: PROV returns a dataframe", {

  output <- PROV

  expect_s3_class(output, "data.frame")

})

#REG
test_that("check-functions: REG returns a dataframe", {

  output <- REG

  expect_s3_class(output, "data.frame")

})


