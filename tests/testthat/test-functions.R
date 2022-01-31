# canadianmaps testing #

context("check-functions")  # The file is called "test-functions.R"
library(testthat)        # load testthat package
library(canadianmaps)    # load my package



#scale_fill_map
test_that("scale_fill_map() returns a ggplot manual fill object", {

  output <- scale_fill_map(palette = "Kelly", num = 5)

  expect_equal(output$scale_name, "manual")

})

#scale_color_map
test_that("scale_color_map() returns a ggplot manual fill object", {

  output <- scale_color_map(palette = "Kelly", num = 5)

  expect_equal(output$scale_name, "manual")

})


#coord_transform
test_that("coord_transform() returns a dataframe", {

  lat <- c(2,3,4)
  long <- c(5,6,7)

  data <- data.frame(lat, long)

  output <- data %>%
    coord_transform(long = "long", lat = "lat")

  expect_s3_class(output, "data.frame")


})

#FSA
test_that("FSA returns a dataframe", {

  output <- FSA

  expect_s3_class(output, "data.frame")

})

#PROV
test_that("PROV returns a dataframe", {

  output <- PROV

  expect_s3_class(output, "data.frame")

})

#REG
test_that("REG returns a dataframe", {

  output <- REG

  expect_s3_class(output, "data.frame")

})


