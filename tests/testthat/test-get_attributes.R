testthat::context("get_attributes")


test_that("We can get attributes out as a data.frame", {


  eml <-
    read_eml(system.file(
      "xsd/test/eml-datasetWithAttributelevelMethods.xml",
      package = "EML"
    ))
  A <- eml_get(eml, "attributeList")
  #df <- get_attributes(A)

  eml <-
    read_eml(system.file("xsd/test/eml-i18n.xml", package = "EML"))
  A <- eml_get(eml, "attributeList")


})
