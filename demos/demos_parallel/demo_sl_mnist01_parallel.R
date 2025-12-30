
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
train_mnist_bin01_49b <- mnist_bin01_49b[train_set[1:800],] ## REDUX: 800 samples!
test_mnist_bin01_49b <- mnist_bin01_49b[-train_set,] ## REDUX: 30% of total!
test_mnist_bin01_49b$predicted <- -1 ## Stands for not found

## Key to adapt to each problem...

######
## PARALLEL RUNNING in a DIFFERENT way!
## Here we decide, instead of doing some boosting, we have enough
## samples in the environment to work on ENVIRONMENT SUBSETS
## in parallel.
######
## The MODEL is a set of Classifiers.
## It's easy to "merge" more than one model!

## Now this is NOT to be made part of the RLCS package, but it's cool:
library(foreach)
library(doParallel) ## could be mirai, this is just one example
n_cores <- detectCores()
## More cores would only make sense with more data!
run_par_count <- max(1, n_cores-1)
cluster <- makeCluster(run_par_count)
registerDoParallel(cluster)

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

t_start_par <- Sys.time()
## Parallel processing has a few options of its own...
mnist01_classifier <- rlcs_train_sl(train_mnist_bin01_49b,mnist_hyperparameters,
                                    n_agents=run_par_count,
                                    use_validation = F,
                                    merge_best_n = 2,
                                    second_evolution_iterations = 2)

t_end_par <- Sys.time() ## Let's compare with single-core runtime:
print(t_end_par - t_start_par)

stopCluster(cluster) ## Don't forget that :)

## Check resulting model:

## Let's run our trained Classifiers Set on test mnist_bin01_49b:
test_mnist_bin01_49b$predicted <- -1 ## Stands for not found
test_mnist_bin01_49b$predicted <- rlcs_predict_sl(test_mnist_bin01_49b, mnist01_classifier)

table(test_mnist_bin01_49b[, c("class", "predicted")])
print(paste("Accuracy:", round(sum(sapply(1:nrow(test_mnist_bin01_49b), \(i) {
  ifelse(test_mnist_bin01_49b[i, "class"] == test_mnist_bin01_49b[i, "predicted"], 1, 0)
}))/nrow(test_mnist_bin01_49b), 2)))

length(mnist01_classifier$pop)


