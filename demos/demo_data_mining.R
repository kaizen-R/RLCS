library(RLCS)

demo_env1 <- rlcs_demo_secret1()
sample_of_rows <- sample(1:nrow(demo_env1), 10, replace=F)
print(demo_env1[sample_of_rows,], row.names = F)

## Defaults will work but make things quite slow...
## Tuning some hyper-parameters makes it faster:
demo_params <- RLCS_hyperparameters(
  n_epochs = 300,
  deletion_trigger = 150,
  deletion_threshold = 0.99)

rlcs_model1 <- rlcs_train_sl(demo_env1, demo_params,
                             pre_trained_lcs = NULL,
                             verbose = F)

## That's it!
print(rlcs_model1)
plot(rlcs_model1)

## Which rule of the model would match the following "state"?
get_match_set("00101", rlcs_model1)

## Let's try a second example:
demo_env2 <- rlcs_demo_secret2()
## Have a look at the exercise:
print(demo_env2)
## Using defaults for Hyper parameters can work,
## it's just not necessarily a great idea, either for speed or accuracy...
rlcs_model2 <- rlcs_train_sl(demo_env2)
print(rlcs_model2)
plot(rlcs_model2)

## Last example - SLOWER, and as always, non-deterministic:
demo_params <- RLCS_hyperparameters(
  wildcard_prob = 0.5,
  n_epochs = 800,
  deletion_trigger = 80,
  deletion_threshold = 0.95)

demo_env3 <- rlcs_mux6()
rlcs_model3 <- rlcs_train_sl(demo_env3, demo_params)
print(rlcs_model3)
plot(rlcs_model3)

## Impossible mining
## This should return NULL instead of a model...
demo_env4 <- data.frame(state=c("11", "11"), class=c(1,0))
demo_params <- RLCS_hyperparameters(
  wildcard_prob = 0.5,
  n_epochs = 20,
  deletion_trigger = 5)
rlcs_model4 <- rlcs_train_sl(demo_env4, demo_params)
print(rlcs_model4)
