library(RLCS)

demo_env1 <- rlcs_demo_secret1()
sample_of_rows <- sample(1:nrow(demo_env1), 10, replace=F)
print(demo_env1[sample_of_rows,], row.names = F)

## Defaults will work but make things quite slow...
## Tuning some hyper-parameters makes it faster:
demo_params <- RLCS_hyperparameters(
  n_epochs = 280,
  deletion_trigger = 40,
  deletion_threshold = 0.9)

rlcs_model1 <- rlcs_train_sl(demo_env1, demo_params, NULL, F)

## That's it!
print(rlcs_model1)
plot(rlcs_model1)

## Which rule of the model would match the following "state"?
get_match_set("00101", rlcs_model1)

## Let's try a second example:
demo_params <- RLCS_hyperparameters(
  n_epochs = 400,
  deletion_trigger = 40,
  deletion_threshold = 0.9)
demo_env2 <- rlcs_demo_secret2()
## Have a look at the exercise:
print(demo_env2)

rlcs_model2 <- rlcs_train_sl(demo_env2, demo_params, NULL, F)
print(rlcs_model2)
plot(rlcs_model2)

## Last example - SLOWER, and as always, non-deterministic:
demo_params <- RLCS_hyperparameters(
  wildcard_prob = 0.6,
  n_epochs = 800,
  deletion_trigger = 80,
  deletion_threshold = 0.95)

demo_env3 <- rlcs_mux6()
rlcs_model3 <- rlcs_train_sl(demo_env3, demo_params, NULL, F)
print(rlcs_model3)
plot(rlcs_model3)
