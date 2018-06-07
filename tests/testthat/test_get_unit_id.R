context("get_unit_id")

test_that("allow for many unit input formats",
          {

form_2.2.0 <- "kilometerPerSecondSquared"
form_2.1.0 <- "kilometersPerSquareSecond"

expect_equal(form_2.2.0,  suppressWarnings(get_unit_id(form_2.1.0, "eml-2.2.0")))
expect_equal(form_2.1.0,  suppressWarnings(get_unit_id(form_2.1.0, "eml-2.1.0")))

expect_equal(form_2.2.0,  suppressWarnings(get_unit_id(form_2.2.0, "eml-2.2.0")))
expect_equal(form_2.1.0,  suppressWarnings(get_unit_id(form_2.2.0, "eml-2.1.0")))

expect_equal(form_2.2.0,  suppressWarnings(get_unit_id('km/s^2', "eml-2.2.0")))
expect_equal(form_2.1.0,  suppressWarnings(get_unit_id('km/s^2', "eml-2.1.0")))

expect_equal(form_2.2.0,  suppressWarnings(get_unit_id('km s-2', "eml-2.2.0")))
expect_equal(form_2.1.0,  suppressWarnings(get_unit_id('km s-2', "eml-2.1.0")))

expect_equal(form_2.2.0,  suppressWarnings(get_unit_id('km s-\u00B2', "eml-2.2.0")))
expect_equal(form_2.1.0,  suppressWarnings(get_unit_id('km s-\u00B2', "eml-2.1.0")))

expect_equal(form_2.2.0,  suppressWarnings(get_unit_id('s-2 /     kilometers-1', "eml-2.2.0")))
expect_equal(form_2.1.0,  suppressWarnings(get_unit_id('s-2 /     kilometers-1', "eml-2.1.0")))

expect_equal(form_2.2.0,  suppressWarnings(get_unit_id('km/kg km/(s^2*km)*kg^4/(kg Kilograms kg seconds) sec(s)(s)(Seconds μs)/(μs s^3)', "eml-2.2.0")))
expect_equal(form_2.1.0,  suppressWarnings(get_unit_id('km/kg km/(s^2*km)*kg^4/(kg Kilograms kg seconds) sec(s)(s)(Seconds μs)/(μs s^3)', "eml-2.1.0")))
})
