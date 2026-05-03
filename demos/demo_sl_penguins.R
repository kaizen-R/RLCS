library(palmerpenguins)
library(RLCS)
plot(penguins, col=as.factor(penguins$species))
rlcs_penguins <- rlcs_rosetta_stone(penguins[complete.cases(penguins),], class_col=1)


full_dataset <- cbind(as.data.frame(penguins[complete.cases(penguins),]), rlcs_penguins$model)

set.seed(1234) ## Only for "reproducibility", as the algorithm is stochastic
## Let's shuffle the data a bit:
full_dataset <- full_dataset[sample(1:nrow(full_dataset), nrow(full_dataset), replace = F), ]
# head(full_dataset, n=3)

## Train-test separation:
train_set <- sample(1:nrow(full_dataset),size = round(0.8*nrow(full_dataset)), replace = F)
train_environment <- full_dataset[train_set,]
test_environment <- full_dataset[-train_set,]
# head(test_environment, n=3)

## Hyperparameters are key for performance of RLCS:
penguins_hyperparameters <- RLCS_hyperparameters(
  wildcard_prob = 0.2, ## Probability that covering will choose a wildcard char
  rd_trigger = 15, ## Smaller means more rules generated through GA tournament
  mutation_probability = 0.2,
  parents_selection_mode <- "tournament",
  tournament_pressure = 4,
  ## Most important parameters to vary so far:
  n_epochs = 600, ## Epochs to repeat process on train set
  deletion_trigger = 20, ## Number of epochs in between subsumption & deletion
  deletion_threshold = 0.99,
  max_pop_size = 650
)

## Doubling process with intermediate cleanup
t_start <- Sys.time()

## This here is the training. That's all there is to it!
penguins_classifier <- rlcs_train_sl(train_environment,
                                 penguins_hyperparameters,
                                 pre_trained_lcs = NULL)

# ## SECRET TRICK: You can keep only the best rules of your model.
# ## (IF you're willing to accept the cost on Accuracy...)
# penguins_classifier <- RLCS:::.apply_deletion_sl(
#   penguins_classifier,
#   deletion_limit = 0.99,
#   max_pop_size = 400)

t_end <- Sys.time()
print(t_end - t_start) ## Training Runtime.

## Let's see how we could do testing:
test_environment$predicted <- -1 ## Stands for not found
test_environment$predicted <- rlcs_predict_sl(test_environment, penguins_classifier, verbose=F)

head(test_environment)
table(test_environment[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_environment), \(i) {
  ifelse(test_environment[i, "class"] == test_environment[i, "predicted"], 1, 0)
}))/nrow(test_environment), 2)))
length(penguins_classifier$pop)

head(print(penguins_classifier$pop))
get_match_set(test_environment[1, "state"], penguins_classifier)
penguins_classifier$pop[[get_match_set(test_environment[1, "state"], penguins_classifier)[1]]]

for(example in c(1, 3, 5)) {
  sample_result_set <- reverse_match_set(penguins_classifier$pop[[example]], full_dataset)
  full_dataset$Match <- "No"
  full_dataset$Match[sample_result_set] <- "Yes"

  g <- ggplot(full_dataset) +
    geom_point(aes(x=bill_length_mm,
                   y=bill_depth_mm,
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
reverse_match_set(penguins_classifier$pop[[1]], full_dataset)
rlcs_rosetta_decode_rule(penguins_classifier$pop[[1]], rlcs_penguins)
