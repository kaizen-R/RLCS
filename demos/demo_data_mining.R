library(RLCS)

demo_env1 <- rlcs_demo_secret1()
sample_of_rows <- sample(1:nrow(demo_env1), 10, replace=F)
print(demo_env1[sample_of_rows,], row.names = F)

## Defaults will work but make things quite slow...
## Tuning some hyper-parameters makes it faster:
demo_params <- RLCS_hyperparameters(
  wildcard_prob = .5,
  rd_trigger = 15,
  mutation_probability = .2,
  tournament_pressure = 8,
  n_epochs = 90,
  deletion_trigger = 30,
  deletion_threshold = 0.95)

rlcs_model1 <- rlcs_train_sl(demo_env1, demo_params,
                             pre_trained_lcs = NULL,
                             verbose = F)
print(rlcs_model1$pop)
plot(rlcs_model1)

## Which rule of the model would match the following "state"?
get_match_set("00101", rlcs_model1)
rlcs_predict_sl(data.frame(state="00101", class="1", predicted=-1), rlcs_model1)

## Let's try a second example:
demo_env2 <- rlcs_demo_secret2()
## Have a look at the exercise:
print(demo_env2)
## Using defaults for Hyper parameters can work,
## it's just not necessarily a great idea, either for speed or accuracy...
rlcs_model2 <- rlcs_train_sl(demo_env2)
print(rlcs_model2)
plot(rlcs_model2)

## We could make it just as good on average, but a bit faster
demo_params <- RLCS_hyperparameters(
  wildcard_prob = .4,
  rd_trigger = 25,
  mutation_probability = .2,
  n_epochs = 200,
  deletion_trigger = 10,
  deletion_threshold = 0.6,
  max_pop_size = 500)
rlcs_model2 <- rlcs_train_sl(demo_env2, demo_params)
print(rlcs_model2)
plot(rlcs_model2)

## Last example - SLOWER, and as always, non-deterministic:
demo_params <- RLCS_hyperparameters(
  wildcard_prob = 0.5,
  n_epochs = 800,
  deletion_trigger = 200,
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
## Plotting NULL is pointless... plot(rlcs_model4)
