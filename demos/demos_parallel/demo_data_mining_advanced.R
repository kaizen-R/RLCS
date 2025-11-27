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
t_start_parallel <- Sys.time()
rlcs_model_parallel <- rlcs_train_sl(demo_env,
                                     demo_params_parallel,
                                     n_agents=run_par_count)
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

rlcs_model_parallel <- rlcs_train_sl(demo_env, demo_params_parallel,
                                     n_agents=run_par_count)
print(rlcs_model_parallel)
plot(rlcs_model_parallel)
t_stop_parallel <- Sys.time()

stopCluster(cluster) ## Don't forget that :)

## Use a validation set, now.
## More cores would only make sense with more data!
cluster <- makeCluster(run_par_count)
registerDoParallel(cluster)

rlcs_model_parallel <- rlcs_train_sl(demo_env, demo_params_parallel,
                                     n_agents=run_par_count,
                                     use_validation=T)
print(rlcs_model_parallel)
plot(rlcs_model_parallel)
t_stop_parallel <- Sys.time()

# ## Impossible mining
# ## This should return NULL instead of a model, parallel or not...
demo_env4 <- data.frame(state=c("11", "11"), class=c(1,0))
demo_params <- RLCS_hyperparameters(
  wildcard_prob = 0.5,
  n_epochs = 20,
  deletion_trigger = 5)
rlcs_model4 <- rlcs_train_sl(demo_env4, demo_params,
                             n_agents=run_par_count)
print(rlcs_model4)

stopCluster(cluster) ## Don't forget that :)
