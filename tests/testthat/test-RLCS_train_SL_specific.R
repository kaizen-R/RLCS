test_that("Basic Functionality of SL training", {
  expect_equal(class(rlcs_train_sl(data.frame(state=c("00", "11"), class=c("yes", "no")), RLCS_hyperparameters())), "rlcs")
})

test_that("Basic Functionality of SL training - Bad Input Controls", {
  expect_error(rlcs_train_sl(data.frame(state=c("0", "11"), class=c("yes", "no")), RLCS_hyperparameters()))
  expect_error(rlcs_train_sl(data.frame(states=c("00", "11"), class=c("yes", "no")), RLCS_hyperparameters()), "state.*column")
  expect_null(rlcs_train_sl(data.frame(state=c("11", "11"), class=c(1,0)),
                            RLCS_hyperparameters(
                              wildcard_prob = 0.5,
                              n_epochs = 20,
                              deletion_trigger = 5)))
})
