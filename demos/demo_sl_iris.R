library(RLCS) ## Assuming you've gotten the package installed by now...

## First let's review the base dataset Iris, just in case:
head(iris, n=3)
plot(iris, col=as.factor(iris$Species))

## The dataset needs encoding for RLCS input compatibility
## NOT part of the LCS Algorithm per-se, provided only for demo!
rlcs_iris <- rlcs_rosetta_stone(iris, class_col=5)
head(rlcs_iris$model, n=3)

## For later:
full_dataset <- cbind(iris, rlcs_iris$model)

set.seed(12345) ## Only for "reproducibility", as the algorithm is stochastic
## Let's shuffle the data a bit:
full_dataset <- full_dataset[sample(1:nrow(full_dataset), nrow(full_dataset), replace = F), ]
head(full_dataset)

## Train-test separation:
train_set <- sample(1:nrow(full_dataset),size = round(0.8*nrow(full_dataset)), replace = F)
train_environment <- full_dataset[train_set,]
test_environment <- full_dataset[-train_set,]
head(test_environment)

## Hyperparameters are key for performance of RLCS:
iris_hyperparameters <- RLCS_hyperparameters(
  wildcard_prob = 0.3, ## Probability that covering will choose a wildcard char
  rd_trigger = 20, ## Smaller means more rules generated through GA tournament
  mutation_probability = 0.1,
  parents_selection_mode <- "tournament",
  tournament_pressure = 6,
  ## Most important parameters to vary so far:
  n_epochs = 250, ## Epochs to repeat process on train set
  deletion_trigger = 50, ## Number of epochs in between subsumption & deletion
  deletion_threshold = 0.9
)

## Doubling process with intermediate cleanup
t_start <- Sys.time()

## This here is the training. That's all there is to it!
iris_classifier <- rlcs_train_sl(train_environment,
                              iris_hyperparameters,
                              pre_trained_lcs = NULL)

# ## SECRET TRICK: You can keep only the best rules of your model.
# ## (IF you're willing to accept the cost on Accuracy...)
# iris_classifier <- RLCS:::.apply_deletion_sl(
#   iris_classifier,
#   deletion_limit = 0.95,
#   max_pop_size = 100)

t_end <- Sys.time()
print(t_end - t_start) ## Training Runtime.

## Let's see how we could do testing:
test_environment$predicted <- -1 ## Stands for not found
test_environment$predicted <- rlcs_predict_sl(test_environment, iris_classifier, verbose=F)

head(test_environment)
table(test_environment[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_environment), \(i) {
  ifelse(test_environment[i, "class"] == test_environment[i, "predicted"], 1, 0)
}))/nrow(test_environment), 2)))
length(iris_classifier)

## So what does it all look like?
print(iris_classifier[[1]])
head(print(iris_classifier))
plot(iris_classifier)

## Visualize how some of the generated rules correspond to the actual data:
## Here one proposed approach. Just for demo purposes.
library(ggplot2)

## Let's look at three example rules:
for(example in c(1, 2, 3)) {
  sample_result_set <- reverse_match_set(iris_classifier[[example]], full_dataset)
  full_dataset$Match <- "No"
  full_dataset$Match[sample_result_set] <- "Yes"

  g <- ggplot(full_dataset) +
    geom_point(aes(x=Petal.Length,
                   y=Petal.Width,
                   # y=Petal.Width,
                   colour = class,
                   shape=factor(Match),
                   alpha=.2,
                   # fill=NA,
                   size=.8)) +
    theme_bw()
  plot(g)
  full_dataset$Match <- NULL
}

## Sorted by accuracy and generality, the LCS first few rules are informative:
head(print(iris_classifier), 10)


# ## DECODING RESULTS FOR INTERPRETATION
# ## *Soon-to-be* function to reverse the Rosetta-Stone information to facilitate
# ## Rules readability:
# print(rlcs_iris$var_names)
# print(RLCS:::.Gray_strings)
# head(full_dataset, 1)
# rlcs_iris$cuts
# head(print(iris_classifier), 1)

test_environment[1,]

get_match_set(test_environment[1, "state"], iris_classifier)
## Use 1 of the matches, then:
print(iris_classifier[[9]])
rlcs_rosetta_decode_rule(iris_classifier[[9]], rlcs_iris)

head(print(iris_classifier), 9)
rlcs_rosetta_decode_rule(iris_classifier[[1]], rlcs_iris)
