test_that("Basic Functionality of SL training", {
  expect_error(rlcs_train_sl(data.frame(state=c("0", "11"), class=c("yes", "no")), RLCS_hyperparameters()))
  expect_error(rlcs_train_sl(data.frame(states=c("00", "11"), class=c("yes", "no")), RLCS_hyperparameters()), "state.*column")
  expect_equal(class(rlcs_train_sl(data.frame(state=c("00", "11"), class=c("yes", "no")), RLCS_hyperparameters())), "rlcs_population")
})
