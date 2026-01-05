## Running demo_sl_iris.R first to select train_set/test_set, then come here

library(RLCS) ## Assuming you've gotten the package installed by now...
library(rpart)
library(randomForest)
library(neuralnet)
# library(caret)
library(ggplot2)

rlcs_means <- rpart_means <- rf_means <- nnet_means <- c()


## First let's review the base dataset Iris, just in case:
# head(iris, n=3)
plot(iris, col=as.factor(iris$Species))

## The dataset needs encoding for RLCS input compatibility
## NOT part of the LCS Algorithm per-se, provided only for demo!
rlcs_iris <- rlcs_rosetta_stone(iris, class_col=5)
# head(rlcs_iris$model, n=3)

## For later:
full_dataset <- cbind(iris, rlcs_iris$model)

## Now this is NOT to be made part of the RLCS package, but it's cool:
library(foreach)
library(doParallel) ## could be mirai, this is just one example
n_cores <- detectCores()
## More cores would only make sense with more data!
run_par_count <- min(8, n_cores-1)
cluster <- makeCluster(run_par_count)
registerDoParallel(cluster)

temp_seeds <- sample(1:1000, 2, replace = F)



## Training RLCS on this combination of training/testing dataset
## Hyperparameters are key for performance of RLCS:
## We make it particularly... Short, this time, see next:
iris_hyperparameters_1 <- RLCS_hyperparameters(
  wildcard_prob = 0.2, ## Probability that covering will choose a wildcard char
  rd_trigger = 10, ## Smaller means more rules generated through GA tournament
  mutation_probability = 0.2,
  parents_selection_mode <- "tournament",
  tournament_pressure = 10,
  ## Most important parameters to vary so far:
  n_epochs = 400, ## Epochs to repeat process on train set
  deletion_trigger = 200, ## Number of epochs in between subsumption & deletion
  deletion_threshold = 0.75,
  max_pop_size=1200
)
## Then make it faster
iris_hyperparameters_2 <- RLCS_hyperparameters(
  wildcard_prob = 0.3, ## Probability that covering will choose a wildcard char
  rd_trigger = 25, ## Smaller means more rules generated through GA tournament
  mutation_probability = 0.05,
  parents_selection_mode <- "tournament",
  tournament_pressure = 5,
  ## Most important parameters to vary so far:
  n_epochs = 40, ## Epochs to repeat process on train set
  deletion_trigger = 20, ## Number of epochs in between subsumption & deletion
  deletion_threshold = 0.95,
  max_pop_size=400
)


t_start <- Sys.time()
results <- list()
results_l <- 1

for(i in temp_seeds) {
  seeds_iter <- i
  set.seed(seeds_iter) ## Only for "reproducibility", as the algorithm is stochastic

  ## Let's shuffle the data a bit:
  full_dataset <- full_dataset[sample(1:nrow(full_dataset), nrow(full_dataset), replace = F), ]

  ## Train-test separation:
  train_set <- sample(1:nrow(full_dataset),size = round(0.8*nrow(full_dataset)), replace = F)
  train_environment <- full_dataset[train_set,]
  test_environment <- full_dataset[-train_set,]


  ## Would a second layer of evolutive selection work?
  t_start_iter <- Sys.time()
  iris_classifier <- NULL

  # second_evolution_iterations = 1,
  # second_evolution_run_params = NULL

  ## New: Validation subset, so that we can compare accuracy / F1 score...
  ## Of different agents, and then keep and consolidate each one.
  iris_classifier <- rlcs_train_sl(
      train_environment,
      run_params = iris_hyperparameters_1,
      # pre_trained_lcs = iris_classifier,
      verbose = FALSE,
      n_agents = run_par_count, use_validation = T, merge_best_n = min(4, run_par_count),
      second_evolution_iterations = 2,
      second_evolution_run_params = iris_hyperparameters_2
    )
    print(length(iris_classifier))
    # print(head(print(iris_classifier), 5))

  ## Simpler better?
  # iris_classifier <- rlcs_train_sl(
  #       train_environment,
  #       run_params = iris_hyperparameters_1
  # )

  # ## Not so simple?
  # iris_classifier <- rlcs_train_sl(
  #       train_environment,
  #       run_params = iris_hyperparameters_1,
  #       n_agents = 6,
  #       use_validation = T,
  #       merge_best_n = 3
  #     )

  ## Let's see how we could do testing:
  test_environment$predicted <- -1 ## Stands for not found
  test_environment$predicted <- rlcs_predict_sl(test_environment, iris_classifier, verbose=F)

  # head(test_environment)
  print(Sys.time() - t_start_iter)
  print("RLCS")
  print(table(test_environment[, c("predicted", "class")]))
  rlcs_accuracy <- round(sum(sapply(1:nrow(test_environment), \(i) {
    ifelse(test_environment[i, "class"] == test_environment[i, "predicted"], 1, 0)
  }))/nrow(test_environment), 4)
  # print(paste("Accuracy:", rlcs_accuracy))
  length(iris_classifier)



  ## Testing RF on this training/testing subsets
  set.seed(seeds_iter) ## Only for "reproducibility", as the algorithm is stochastic
  iris_rpart <- rpart(Species~.,data=train_environment[,1:5])
  pred_rpart_num <- predict(iris_rpart,newdata=test_environment[,1:5])
  irisPred <- c("setosa", "versicolor", "virginica")[
    sapply(1:nrow(pred_rpart_num), \(x)
           { which(pred_rpart_num[x, ] == max(pred_rpart_num[x, ])) })
  ]
  print("Rpart")
  print(table(irisPred, test_environment$Species))
  rpart_accuracy <- round(sum(sapply(1:nrow(test_environment), \(i) {
    ifelse(test_environment[i, "Species"] == irisPred[i], 1, 0)
  }))/nrow(test_environment), 4)

  ## Testing RF on this training/testing subsets
  set.seed(seeds_iter) ## Only for "reproducibility", as the algorithm is stochastic
  iris_rf <- randomForest(Species~., data = train_environment[,1:5], ntree=300, proximity=TRUE)
  irisPred <- predict(iris_rf,newdata=test_environment[,1:5])
  print("RF")
  print(table(irisPred, test_environment$Species))
  rf_accuracy <- round(sum(sapply(1:nrow(test_environment), \(i) {
    ifelse(test_environment[i, "Species"] == irisPred[i], 1, 0)
  }))/nrow(test_environment), 4)


  ## Testing Nnet on this training/testing subsets
  set.seed(seeds_iter) ## Only for "reproducibility", as the algorithm is stochastic
  nnet=neuralnet(Species~., train_environment[,1:5],
                 hidden = c(4,4,3),
                 stepmax = 5e+05, ## Giving more chance here
                 linear.output = FALSE)

  ypred = neuralnet::compute(nnet, test_environment[,1:5])
  yhat = ypred$net.result
  yhat=data.frame("yhat"=ifelse(max.col(yhat[ ,1:3])==1, "setosa",
                                ifelse(max.col(yhat[ ,1:3])==2, "versicolor", "virginica")))
  print("Nnet")
  print(table(yhat$yhat, test_environment$Species))
  nnet_accuracy <- round(sum(sapply(1:nrow(test_environment), \(i) {
    ifelse(test_environment[i, "Species"] == yhat$yhat[i], 1, 0)
  }))/nrow(test_environment), 4)

  results[[results_l]] <- list(seeds_iter = seeds_iter,
              rlcs_accuracy = rlcs_accuracy,
              rpart_accuracy = rpart_accuracy,
              rf_accuracy = rf_accuracy,
              nnet_accuracy = nnet_accuracy)
  results_l <- results_l + 1

  rlcs_means <- sapply(results, \(x) x$rlcs_accuracy)
  rpart_means <- sapply(results, \(x) x$rpart_accuracy)
  rf_means <- sapply(results, \(x) x$rf_accuracy)
  nnet_means <- sapply(results, \(x) x$nnet_accuracy)
  nreps <- length(rlcs_means)
  print(length(results))

  res_df <- data.frame(accuracy=c(rlcs_means, rpart_means, rf_means, nnet_means),
                       algo = c(rep("rlcs", nreps),
                                rep("rpart", nreps),
                                rep("RF", nreps),
                                rep("Nnet", nreps)),
                       iteration = 1:nreps)
  # res_df
  print(paste("RLCS mean Accuracy:", round(mean(rlcs_means), 4)))
  print(paste("RPART mean Accuracy:", round(mean(rpart_means), 4)))
  print(paste("RF mean Accuracy:", round(mean(rf_means), 4)))
  print(paste("Nnet mean Accuracy:", round(mean(nnet_means), 4)))

  plot(ggplot(res_df)+
    geom_line(aes(x=iteration, y=accuracy, colour=algo)))

  Sys.sleep(2)
}

stopCluster(cluster) ## Don't forget that :)

t_end <- Sys.time()
print(t_end - t_start) ## Training Runtime.
