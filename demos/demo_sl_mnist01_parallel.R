## A more comprehensive example and hints at future work, too!

library(RLCS) ## Well, yes...

##
## PART ONE
##

##
## PRE-REQUISITES specific to this particular Dataset example:
## Support functions for data visualizations.
## You can skip to "PART TWO" below.
##

## Poorly put together, quick and dirty
rlcs_visualize_predict_mnist49b <- function(test_env_df, pop) {
  example_visual <- test_env_df
  example_visual_v <- strsplit(example_visual$sharp_image, "", fixed=T)[[1]]
  example_visual_m <- matrix(1-as.integer(example_visual_v), nrow=28, byrow = F)

  correct_class <- rlcs_predict_sl(test_env_df, pop, verbose=F)

  match_set <- get_match_set(test_env_df$state, pop)
  res_pos <- sapply(match_set, \(i) {
    if(pop[[i]]$action == correct_class) return(T)
    F
  })
  res_pos <- match_set[which(res_pos)]

  res <- pop[res_pos]
  # res <- res[res$action == correct_class]
  print(paste("Matched Rules Tot.:", length(match_set)))
  print(paste("Of which correct classification:", length(res_pos)))

  t_string <- test_env_df$state
  t_row <- strsplit(t_string, "", fixed=T)[[1]]
  x_clean <- sapply(t_row, \(item) {
    return(ifelse(item == 1, 1, 0))
  })
  t_m_orig <- matrix(x_clean, nrow=7, byrow = T)

  # browser()
  t_m <- sapply(res, \(x) {
    t_string <- x$condition_string
    t_numerosity <- x$numerosity
    t_row <- strsplit(t_string, "", fixed=T)[[1]]
    x_clean <- sapply(t_row, \(item) {
      if(item == '0') return(t_numerosity)
      return(0)
    })
    as.numeric(x_clean)
  })
  t_m <- sapply(1:nrow(t_m), \(i) sum(t_m[i, ]))
  t_mm <- matrix(t_m, nrow=7, byrow = T)
  t_mm <- t_mm + min(t_mm)
  t_m_0 <- round((t_mm - mean(t_mm))/(max(t_mm)-min(t_mm))*10)

  t_m <- sapply(res, \(x) {
    t_string <- x$condition_string
    t_numerosity <- x$numerosity
    t_row <- strsplit(t_string, "", fixed=T)[[1]]
    x_clean <- sapply(t_row, \(item) {
      if(item == '1') return(t_numerosity);
      return(0)
    })
    as.numeric(x_clean)
  })
  t_m <- sapply(1:nrow(t_m), \(i) sum(t_m[i, ]))
  t_mm <- matrix(t_m, nrow=7, byrow = T)
  t_mm <- t_mm + min(t_mm)
  t_m_1 <- round((t_mm - mean(t_mm))/(max(t_mm)-min(t_mm))*10)


  t_m <- sapply(res, \(x) {
    t_string <- x$condition_string
    t_numerosity <- x$numerosity
    t_row <- strsplit(t_string, "", fixed=T)[[1]]
    x_clean <- sapply(t_row, \(item) {
      if(item == '#') return(0);
      return(ifelse(item == 1, -t_numerosity, t_numerosity))
    })
    as.numeric(x_clean)
  })
  t_m <- sapply(1:nrow(t_m), \(i) sum(t_m[i, ]))
  t_mm <- matrix(t_m, nrow=7, byrow = T)
  t_mm <- t_mm + min(t_mm)
  t_mm_z <- round((t_mm - mean(t_mm))/(max(t_mm)-min(t_mm))*10)

  my_pal_1 <- RColorBrewer::brewer.pal(9,"Greys")
  my_pal_2 <- RColorBrewer::brewer.pal(11,"BrBG")

  mat <- matrix(c(1, 2, 3,  # First, second
                  1, 5, 4), # and third plot
                nrow = 2, ncol = 3,
                byrow = TRUE)

  layout(mat = mat)

  ## Support function to cleaner (no need for additional package) plot
  tf <- function(m) t(m)[, nrow(m):1]
  ## Support function to cleaner (no need for additional package) plot
  imageM <- function(m, col = c(0, 1), ...) {
    grid = max(dim(m)) <= 25
    asp = (nrow(m)-1)/(ncol(m)-1)
    image(tf(m), col = col, asp = asp, axes = FALSE, ...)
  }

  imageM(example_visual_m, main = "MNIST formatted 28x28 binary")


  imageM(t_m_orig, col=my_pal_1, main="Compressed 7x7")
  imageM(t_m_0, col=my_pal_1, main="Pixel=0")
  imageM(t_m_1, col=my_pal_1, main="Pixel=1")
  imageM(t_mm_z, col=my_pal_2, main=paste(correct_class,"Conf.:", round(100*length(res_pos)/length(match_set),2), '%'))

  t_mm
}

print_mnist_number <- function(mnist_string) {
  mnist_vec <- unlist(strsplit(mnist_string, ""))
  mnist_vec[which(mnist_vec == '#')] <- " "

  for(i in 1:28) {
    print(paste(mnist_vec[(28*(i-1)+1):(28*(i-1)+28)], collapse=""))
  }
}

print_mnist_number_49b <- function(mnist_string) {
  mnist_vec <- unlist(strsplit(mnist_string, ""))
  mnist_vec[which(mnist_vec == '#')] <- " "

  for(i in 1:7) {
    print(paste(mnist_vec[(7*(i-1)+1):(7*(i-1)+7)], collapse=""))
  }
}

## RLCS package comes with a sample dataset of simplified MNIST images
## Here is a simple look at some of it:
print_mnist_number(mnist_string = mnist_bin01_49b[1, "sharp_image"])
print_mnist_number_49b(mnist_string = mnist_bin01_49b[1, "state"])

##
## PART TWO
##

##
## MAIN CODE
##

## Let's train a model on our demo simplified-MNIST dataset:
set.seed(12345)
train_set <- sample(1:nrow(mnist_bin01_49b),size = round(0.7*nrow(mnist_bin01_49b)), replace = F)
train_mnist_bin01_49b <- mnist_bin01_49b[train_set[1:800],]
test_mnist_bin01_49b <- mnist_bin01_49b[(1:nrow(mnist_bin01_49b))[-train_set],]
test_mnist_bin01_49b$predicted <- -1 ## Stands for not found

## Key to adapt to each problem...
## Acceptable hyperparameters for this particular scenario:
mnist_hyperparameters <- RLCS_hyperparameters(
  wildcard_prob = .4,
  rd_trigger = 20,
  mutation_probability = .1,
  ## parents_selection_mode,
  tournament_pressure = 5,
  n_epochs = 50,
  deletion_trigger = 10,
  deletion_threshold = .9
)

######
## One-thread training - About
######
t_start <- Sys.time()

mnist01_classifier <- structure(list(), class="rlcs_population")
for(i in 1:1) { ## Can be double pass for example, but still sequential
  mnist01_classifier <- rlcs_train_sl(train_mnist_bin01_49b,
                                      mnist_hyperparameters,
                                      pre_trained_lcs = mnist01_classifier)
}

t_end <- Sys.time()

print(t_end - t_start)

## Check resulting model:

## Let's run our trained Classifiers Set on test mnist_bin01_49b:
test_mnist_bin01_49b$predicted <- -1 ## Stands for not found
test_mnist_bin01_49b$predicted <- rlcs_predict_sl(test_mnist_bin01_49b, mnist01_classifier)

table(test_mnist_bin01_49b[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_mnist_bin01_49b), \(i) {
  ifelse(test_mnist_bin01_49b[i, "class"] == test_mnist_bin01_49b[i, "predicted"], 1, 0)
}))/nrow(test_mnist_bin01_49b), 2)))

length(mnist01_classifier)

## This is tailored to this specific example, but hopefully it helps show
## What the "model", as a set of rules, is actually "thinking"

res <- rlcs_visualize_predict_mnist49b(test_mnist_bin01_49b[7,], mnist01_classifier)
res <- rlcs_visualize_predict_mnist49b(test_mnist_bin01_49b[nrow(test_mnist_bin01_49b)-15,], mnist01_classifier)

## Example wrong classification. Note number of disagreeing rules, though...
## Wrongly classified. See HOW THE MODEL TELLS YOU it's doubting?
disagreed <- which(test_mnist_bin01_49b$class != test_mnist_bin01_49b$predicted)
res <- rlcs_visualize_predict_mnist49b(test_mnist_bin01_49b[disagreed[1],],
                                       mnist01_classifier)

## Visualize LCS
par(mfrow=c(1,1))
plot(mnist01_classifier)

# ## Visualize LCS per Class: 0
# plot(mnist01_classifier[which(sapply(mnist01_classifier, \(x) {if(x$action == 0) return(T); F}))])
# length(mnist01_classifier[which(sapply(mnist01_classifier, \(x) {if(x$action == 0) return(T); F}))])
# print(mnist01_classifier[which(sapply(mnist01_classifier, \(x) {if(x$action == 0) return(T); F}))])
#
# ## Visualize LCS per Class: 1
# plot(mnist01_classifier[which(sapply(mnist01_classifier, \(x) {if(x$action == 1) return(T); F}))])
# length(mnist01_classifier[which(sapply(mnist01_classifier, \(x) {if(x$action == 1) return(T); F}))])
# print(mnist01_classifier[which(sapply(mnist01_classifier, \(x) {if(x$action == 1) return(T); F}))])


##
## PART THREE
##

######
## PARALLEL RUNNING
######
## The MODEL is a set of Classifiers.
## It's easy to "merge" more than one model!

## Now this is NOT to be made part of the RLCS package, but it's cool:
library(foreach)
library(doParallel) ## could be mirai, this is just one example
n_cores <- detectCores()
n_cores
run_par_count <- n_cores-1
cluster <- makeCluster(run_par_count)
registerDoParallel(cluster)

## We will repeat this here JUST TO facilitate running the parallel example
## Separately from the simpler mono-core/thread run

library(RLCS)
## Seeding is the same as above, for results comparison.
set.seed(12345)
train_set <- sample(1:nrow(mnist_bin01_49b),size = round(0.7*nrow(mnist_bin01_49b)), replace = F)
train_mnist_bin01_49b <- mnist_bin01_49b[train_set[1:800],]
test_mnist_bin01_49b <- mnist_bin01_49b[(1:nrow(mnist_bin01_49b))[-train_set],]
test_mnist_bin01_49b$predicted <- -1 ## Stands for not found


t_start_par <- Sys.time()
sets_size <- floor(nrow(train_mnist_bin01_49b) / run_par_count)
results <- list()

mnist01_par_classifier <- list() ## COULD be pre-trained already...
## This is superfluous here, but just making a point...

results <- foreach(i = 1:run_par_count
                   # , .verbose=T
                   # , .combine=cbind
) %dopar% {
  ## Let's take a SUBSET of the training data
  ## NOTE: You could imagine more complex subsetting, with overlaps, etc.
  ## That might greatly help quality!
  ## But here we're demo-ing speed comparisons only.
  sets_size <- floor(nrow(train_mnist_bin01_49b) / run_par_count)
  sub_start <- (i-1)*sets_size+1
  sub_end <- i*sets_size

  ## dopar requires to reload context data:
  library(RLCS)
  mnist_hyperparameters <- RLCS_hyperparameters(
    wildcard_prob = .4,
    rd_trigger = 20,
    mutation_probability = .1,
    ## parents_selection_mode,
    tournament_pressure = 5,
    n_epochs = 50,
    deletion_trigger = 10,
    deletion_threshold = .9
  ) ## Same a single-core run, only to compare speed
  ## But you could imagine running much more comprehensive training per-subset!

  # Now do the actual training
  par_pop <- rlcs_train_sl(train_mnist_bin01_49b[sub_start:sub_end,],
             mnist_hyperparameters,
             pre_trained_lcs = mnist01_par_classifier) ## Optional pre-trained

  ## Implicit return of the following in the results list:
  ## Keep only the very best of each population of classifier
  ##  for later compaction, which will keep things running a bit faster...
  RLCS:::.apply_deletion_sl(par_pop, deletion_limit = 0.95, max_pop_size = 700)
}


## Back to main processing:
## Recollect all sub-lcs
mnist01_par_classifier <- list()
for(j in 1:length(results)) {
  print(paste("result set", j, "length", length(results[[j]])))
  for(i in 1:length(results[[j]])) {
    mnist01_par_classifier[[length(mnist01_par_classifier)+1]] <- results[[j]][[i]]
  }
}

## Compaction of sorts:
## Now that only is more useful if there was a better overlap among the training
## subsets!
## If no overlap, the resulting LCS will have more rules than a single-core
## approach because each ruleset will be tailored to each subset of the training
## data
print(length(mnist01_par_classifier))
## Unnecessary:
# mnist01_par_classifier <- RLCS:::.remove_duplicate_rules(mnist01_par_classifier)
# print(length(mnist01_par_classifier))
## Barely useful:
mnist01_par_classifier <- RLCS:::.apply_subsumption_whole_pop_sl(mnist01_par_classifier)
print(length(mnist01_par_classifier))

stopCluster(cluster) ## Don't forget that :)

t_end_par <- Sys.time() ## Let's compare with single-core runtime:
print(t_end_par - t_start_par)

## Now how does this new approach for the LCS fare...?

test_mnist_bin01_49b$predicted <- -1 ## Stands for not found
test_mnist_bin01_49b$predicted <- rlcs_predict_sl(test_mnist_bin01_49b, mnist01_par_classifier)

table(test_mnist_bin01_49b[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_mnist_bin01_49b), \(i) {
  ifelse(test_mnist_bin01_49b[i, "class"] == test_mnist_bin01_49b[i, "predicted"], 1, 0)
}))/nrow(test_mnist_bin01_49b), 2)))



##
## BONUS
##

## OK, finally, let's see a bit about the LCS itself.
## This would apply to either single-core/thread or parallel processing.

## This is how one rule looks like, in a given classifier:
print_mnist_number_49b(mnist01_par_classifier[[457]]$condition_string)
## Can you tell what class it matches?
## Hint: a 0 will have an empty middle across the central lines...
mnist01_par_classifier[[457]]$action

## We have seen a visual, but a less visual option is to ask for rules scoring:
print_mnist_number(test_mnist_bin01_49b$sharp_image[5])
rlcs_predict_sl(test_mnist_bin01_49b[5,], mnist01_par_classifier, verbose = T)
print_mnist_number(test_mnist_bin01_49b$sharp_image[nrow(test_mnist_bin01_49b)-5])
rlcs_predict_sl(test_mnist_bin01_49b[nrow(test_mnist_bin01_49b)-5,], mnist01_par_classifier, verbose = T)

