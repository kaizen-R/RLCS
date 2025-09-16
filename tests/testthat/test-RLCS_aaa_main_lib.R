## Dummy tests...
test_that("New rule creates an object of class rlcs_rule", {
  expect_equal(class(.new_rlcs_rule("0100", "yes")), "rlcs_rule")
})
test_that("New pop creates an object of class rlcs_population", {
  expect_equal(class(.new_rlcs_population()), "rlcs_population")
})

## Validate rule input controls
test_that("Correct format input for States", {
  expect_error(.validate_state_string(), regexp="length")
  expect_error(.validate_state_string(0.1), regexp="string")
  expect_error(.validate_state_string("1"), regexp="length")
  expect_error(.validate_state_string("01a"), regexp="alphabet")
  expect_equal(.validate_state_string("01"), T)
})
