library(RLCS) ## Assuming you've gotten the package installed by now...

## First let's review the base dataset Iris, just in case:
# head(iris, n=3)
plot(iris, col=as.factor(iris$Species))

## The dataset needs encoding for RLCS input compatibility
## NOT part of the LCS Algorithm per-se, provided only for demo!
rlcs_iris <- rlcs_rosetta_stone(iris, class_col=5)
# head(rlcs_iris$model, n=3)

## For later:
full_dataset <- cbind(iris, rlcs_iris$model)

set.seed(123) ## Only for "reproducibility", as the algorithm is stochastic
## Let's shuffle the data a bit:
full_dataset <- full_dataset[sample(1:nrow(full_dataset), nrow(full_dataset), replace = F), ]
# head(full_dataset, n=3)

## Train-test separation:
train_set <- sample(1:nrow(full_dataset),size = round(0.8*nrow(full_dataset)), replace = F)
train_environment <- full_dataset[train_set,]
test_environment <- full_dataset[-train_set,]
# head(test_environment, n=3)

## Hyperparameters are key for performance of RLCS:
iris_hyperparameters <- RLCS_hyperparameters(
  wildcard_prob = 0.3, ## Probability that covering will choose a wildcard char
  rd_trigger = 20, ## Smaller means more rules generated through GA tournament
  mutation_probability = 0.1,
  parents_selection_mode <- "tournament",
  tournament_pressure = 6,
  ## Most important parameters to vary so far:
  n_epochs = 800, ## Epochs to repeat process on train set
  deletion_trigger = 80, ## Number of epochs in between subsumption & deletion
  deletion_threshold = 0.95
)

## Doubling process with intermediate cleanup
t_start <- Sys.time()

## This here is the training. That's all there is to it!
iris_classifier <- rlcs_train_sl(train_environment,
                              iris_hyperparameters,
                              pre_trained_lcs = NULL)

t_end <- Sys.time()
print(t_end - t_start) ## Training Runtime.

## Let's see how we could do testing:
test_environment$predicted <- -1 ## Stands for not found
test_environment$predicted <- rlcs_predict_sl(test_environment, iris_classifier, verbose=F)

# head(test_environment)
table(test_environment[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_environment), \(i) {
  ifelse(test_environment[i, "class"] == test_environment[i, "predicted"], 1, 0)
}))/nrow(test_environment), 2)))
length(iris_classifier$condition_strings)


## Training several agents, one can also
## compensate somewhat for stochastic aspects,
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


t_start <- Sys.time()

## This here is the training. That's all there is to it!
iris_classifier_parallel <- rlcs_train_sl_parallel_search_space(
  train_environment,
  iris_hyperparameters,
  pre_trained_lcs = NULL,
  n_agents=run_par_count,
  use_validation=T)

t_end <- Sys.time()
print(t_end - t_start) ## Training Runtime.

## Let's see how we could do testing:
test_environment$predicted <- -1 ## Stands for not found
test_environment$predicted <- rlcs_predict_sl(test_environment, iris_classifier_parallel, verbose=F)

# head(test_environment)
table(test_environment[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_environment), \(i) {
  ifelse(test_environment[i, "class"] == test_environment[i, "predicted"], 1, 0)
}))/nrow(test_environment), 2)))
length(iris_classifier_parallel$condition_strings)


t_start <- Sys.time()

## This here is the training. That's all there is to it!
iris_classifier_parallel <- rlcs_train_sl_parallel_search_space(
  train_environment,
  iris_hyperparameters,
  pre_trained_lcs = NULL,
  n_agents=run_par_count,
  # use_validation=T,
  merge_best_n = 3
)

t_end <- Sys.time()
print(t_end - t_start) ## Training Runtime.

## Let's see how we could do testing:
test_environment$predicted <- -1 ## Stands for not found
test_environment$predicted <- rlcs_predict_sl(test_environment, iris_classifier_parallel, verbose=F)

# head(test_environment)
table(test_environment[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_environment), \(i) {
  ifelse(test_environment[i, "class"] == test_environment[i, "predicted"], 1, 0)
}))/nrow(test_environment), 2)))
length(iris_classifier_parallel$condition_strings)

stopCluster(cluster) ## Don't forget that :)
