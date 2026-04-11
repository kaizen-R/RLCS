## RLCS supports using foreach() and %dopar% if available.
## For supervised learning, it then chooses to train N parallel models.
## And picks the best one by accuracy as a result.

library(RLCS)

demo_env <- rlcs_mux6()

## Let's compare two runs, one traditional, one parallel:

demo_params_single <- RLCS_hyperparameters(
  wildcard_prob = 0.5,
  n_epochs = 800,
  deletion_trigger = 80,
  deletion_threshold = 0.95)

t_start_single <- Sys.time()
rlcs_model_single <- rlcs_train_sl(demo_env, demo_params_single)
t_stop_single <- Sys.time()
print(paste("Single-process runtime:", t_stop_single-t_start_single))
print(rlcs_model_single)
plot(rlcs_model_single)

## Training several agents, one can possibly reduce the runtime of each,
## While still get good results:
library(foreach)
library(doParallel)
## RLCS checks for availability of both packages in namespace and
## falls back to normal single-thread processing
n_cores <- detectCores()
## More cores would only make sense with more data!
run_par_count <- max(1, n_cores-1)
cluster <- makeCluster(run_par_count)
registerDoParallel(cluster)
demo_params_parallel <- RLCS_hyperparameters(
  wildcard_prob = 0.5,
  n_epochs = 400,
  deletion_trigger = 20,
  deletion_threshold = 0.95)

set.seed(12345) ## There is a possibility that an execution fails.
## Detected in v0.1.6 and marked to be reworked.
## Error is related to t_shuffle_set in train SL Specific #759
t_start_parallel <- Sys.time()
rlcs_model_parallel <- rlcs_train_sl(demo_env,
                                     demo_params_parallel,
                                     n_agents=run_par_count,
                                     use_validation = T,
                                     merge_best_n = min(4, run_par_count),
                                     second_evolution_iterations = 2)
t_stop_parallel <- Sys.time()

stopCluster(cluster) ## Don't forget that :)

print(paste("Parallel runtime:", t_stop_parallel-t_start_parallel))
print(rlcs_model_parallel)
plot(rlcs_model_parallel)

run_par_count <- max(1, n_cores-1)
cluster <- makeCluster(run_par_count)
registerDoParallel(cluster)
demo_params_parallel <- RLCS_hyperparameters(
  wildcard_prob = 0.3,
  n_epochs = 100,
  deletion_trigger = 5,
  deletion_threshold = 0.99)

demo_env <- rlcs_mux11()

set.seed(12345) ## There is a possibility that an execution fails.
## Detected in v0.1.6 and marked to be reworked.
## Error is related to t_shuffle_set in train SL Specific #759
t_start_parallel <- Sys.time()
rlcs_model_parallel <- rlcs_train_sl(demo_env,
                                     demo_params_parallel,
                                     n_agents=run_par_count,
                                     use_validation = T,
                                     merge_best_n = min(2, run_par_count),
                                     second_evolution_iterations = 3)
t_stop_parallel <- Sys.time()

stopCluster(cluster) ## Don't forget that :)

print(paste("Parallel runtime:", t_stop_parallel-t_start_parallel))
print(rlcs_model_parallel)
plot(rlcs_model_parallel)

#### CAREFUL WITH THIS DEMO: IT MODIFIES your installed packages.
#### TESTED IN RStudio as restarting R is needed.

## What happens if you do not have the required package and yet try to run
## parallel agents as in the above?
remove.packages("foreach")
.rs.restartR()
requireNamespace("foreach", quietly = TRUE)==T
requireNamespace("doParallel", quietly = TRUE)==T
library(RLCS)
rlcs_model_parallel <- rlcs_train_sl(demo_env, demo_params_single, n_agents=run_par_count)
print(rlcs_model_parallel)
plot(rlcs_model_parallel)

## Test re-adding the package in namespace:
install.packages("foreach")
requireNamespace("foreach", quietly = TRUE)==T
requireNamespace("doParallel", quietly = TRUE)==T
library(foreach)
library(doParallel)
n_cores <- detectCores()
## More cores would only make sense with more data!
run_par_count <- max(1, n_cores-1)
cluster <- makeCluster(run_par_count)
registerDoParallel(cluster)
demo_env <- rlcs_mux6()
## Poor results here, taking the best of n agents, but with fast-bad parameters
rlcs_model_parallel <- rlcs_train_sl(demo_env, demo_params_parallel,
                                     n_agents=run_par_count)
print(rlcs_model_parallel)
plot(rlcs_model_parallel)

## Use a validation set, now. And selection of best agents, and iterations:
rlcs_model_parallel <- rlcs_train_sl(demo_env, demo_params_parallel,
                                     n_agents=run_par_count,
                                     use_validation=T,
                                     merge_best_n = min(2, run_par_count),
                                     second_evolution_iterations = 3)
print(rlcs_model_parallel)
plot(rlcs_model_parallel)

stopCluster(cluster) ## Don't forget that :)
