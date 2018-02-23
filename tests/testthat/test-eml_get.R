library(testthat)
library(eml2)

context("eml_get")


test_that("eml_get works", {
  f <- system.file("xsd/test", "eml-datasetWithUnits.xml", package = "EML")
  eml <- read_eml(f)
  x <- eml_get(eml, "physical")
  expect_s3_class(x, "emld")
  y <- eml_get(eml, "attributeList")
  expect_length(y,2)
  expect_s3_class(y, "emld")
})
