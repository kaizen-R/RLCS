######
## SUB FUNCTIONS
## Better self contained like this for the time being.
######

## Why not use S3 OO, as we want to use plot(), print()...
.new_rlcs_rule <- function(condition_string, action) {
  ## NEW!
  zeros_vector <- ones_vector <- rep(0, nchar(condition_string))
  which_zeros <- which(strsplit(condition_string, "", fixed=T)[[1]] == "0")
  which_ones <- which(strsplit(condition_string, "", fixed=T)[[1]] == "1")
  zeros_vector[which_zeros] <- 1
  ones_vector[which_ones] <- 1

  length_fixed_bits <- length(which_zeros)+length(which_ones)

  t_rule <- list(condition_string = condition_string,
       condition_length = nchar(condition_string),
       condition_list = list("0" = which(strsplit(condition_string, "", fixed=T)[[1]] == "0"),
                             "1" = which(strsplit(condition_string, "", fixed=T)[[1]] == "1")),
       ## NEW!
       zeros_pos_vector = zeros_vector,
       ones_pos_vector = ones_vector,
       length_fixed_bits = length_fixed_bits,

       action = action,
       total_reward = 5, ## Force initial exploration
       action_count = 1, ## For Reinforcement Learning

       accuracy = 1,
       match_count = 1,
       correct_count = 1, ## For Supervised Learning/Data Mining

       numerosity = 1,
       first_seen = 1)
  class(t_rule) <- "rlcs_rule"
  t_rule
}

.new_rlcs_population <- function(x = list()) {
  structure(x, class = "rlcs_population")
}

## VERY basic error generation and processing stop.
## Simply put, if input strings are not right, nothing would work...
## So no need to tryCatch(), as STOPPING would be needed anyway.
## At least in current implementation.
.validate_state_string <- function(state_string = "") {
    if(is.null(state_string) || !is.character(state_string)) stop("Input States must be strings.")
    if(nchar(state_string) < 2) stop("Input States strings must be of length >= 2.")
    ## Only basic ternary alphabet is accepted for now
    t_chars <- strsplit(state_string, "")[[1]]
    if(any(!(t_chars %in% c("0", "1")))) stop("Current implementation works with ternary alphabet. Input States must contain only characters 0 and 1.")
    T ## implicit return
}

## Create Cover for a yet to be found state
## state_string must contain at LEAST two characters for this to work
.generate_cover_rule_for_unmatched_instance <- function(state_string = "",
                                                       wildcard_prob = 0.5) {

  ## Hidden function, so instead I contorl at Pop level for SL.
  ## For RL, this is optional probably as it adds runtime.
  # if(!.validate_state_string(state_string)) return(NULL)
  len_state <- nchar(state_string)

  if(wildcard_prob > 1 || wildcard_prob < 0)
    return(NULL)

  ## ROUGH approximation to the correct count of wildcard. Should use runif()...
  n_wildcards <- round(len_state*wildcard_prob)

  ## Set lower and upper limits to number of wildcards
  if(n_wildcards == len_state) n_wildcards <- len_state-1

  t_pos <- sample(1:len_state, n_wildcards, replace = FALSE)

  state_vec <- strsplit(state_string, "", fixed = T)[[1]]
  state_vec[t_pos]<- '#'
  paste(state_vec, collapse='')
}

.recalculate_pop_matrices <- function(pop) {
  # browser()
  zeros_matrix <- t(as.matrix(sapply(pop, \(x) x$zeros_pos_vector)))
  ones_matrix <- t(as.matrix(sapply(pop, \(x) x$ones_pos_vector)))
  list(zeros_matrix, ones_matrix)
}

.recalculate_pop_matrices_new_rule <- function(t_matrices, condition_string) {
  # browser()
  ## I just will want to add a row to either matrices!!
  zeros_vector <- ones_vector <- rep(0, nchar(condition_string))
  which_zeros <- which(strsplit(condition_string, "", fixed=T)[[1]] == "0")
  which_ones <- which(strsplit(condition_string, "", fixed=T)[[1]] == "1")
  zeros_vector[which_zeros] <- 1
  ones_vector[which_ones] <- 1

  zeros_matrix <- rbind(t_matrices[[1]], zeros_vector)
  ones_matrix <- rbind(t_matrices[[2]], ones_vector)

  list(zeros_matrix, ones_matrix)
}

.lengths_fixed_bits <- function(pop) {
  sapply(pop, \(x) x$length_fixed_bits)
}

.lengths_fixed_zeros <- function(pop) {
  sapply(pop, \(x) length(x$cond$condition_list$"0"))
}

.lengths_fixed_ones <- function(pop) {
  sapply(pop, \(x) length(x$cond$condition_list$"1"))
}

.lengths_fixed_bits_new_rule <- function(t_lengths, condition_string) {
  zeros_vector <- ones_vector <- rep(0, nchar(condition_string))
  which_zeros <- which(strsplit(condition_string, "", fixed=T)[[1]] == "0")
  which_ones <- which(strsplit(condition_string, "", fixed=T)[[1]] == "1")

  c(t_lengths, length(which_zeros)+length(which_ones))
}

## Function to add rule to a population.
## date_rule_born will be useful stat for future, setting as parameter for now.
.add_valid_rule_to_pop <- function(pop, condition_string,
                                  action, date_rule_born = 0,
                                  match_count = 1,
                                  correct_count = 1,
                                  accuracy = 1,
                                  numerosity = 1) {
  t_rule <- .new_rlcs_rule(condition_string, action)

  ## Key here is creating a population structure
  if(is.null(pop) || length(pop) == 0) {
    pop <- structure(list(t_rule), class = "rlcs_population")
    return(pop)
  }

  t_rule$match_count <- match_count
  t_rule$correct_count <- correct_count
  t_rule$accuracy <- accuracy
  t_rule$numerosity <- numerosity

  t_rule$first_seen <- date_rule_born
  pop[[length(pop)+1]] <- t_rule

  # pop <- .recalculate_pop_matrices(pop)
  # class(pop) <- "rlcs_population"
  pop
}

## FUNCTION FACTORY!
## Often needed, is to update by increase of 1 one parameter of an LCS rule
.inc_param_count <- function(param) {
  # param <- as.name(param)
  f_param <- param

  function(pop) {
    inc_param_count_cpp(pop, f_param)
    # lapply(pop, \(x) {
    #   x[[param]] <- x[[param]] + 1
    #   x
    # })
  }
}

## Augment match count of a set of classifiers
# .inc_match_count <- function(M_pop) {
#   lapply(M_pop, \(x) {
#     x$match_count <- x$match_count + 1
#     # x$accuracy <- x$correct_count / x$match_count
#     x
#   })
# }
.inc_match_count <- .inc_param_count("match_count")

.inc_numerosity_by_condition <- function(pop, item) {
  lapply(pop, \(x, item) {
    if(x$condition_string == item) x$numerosity <- x$numerosity + 1
    x
  }, item)
}

## Augment correct count of a set of classifiers
.update_matched_accuracy <- function(match_pop) {

  # ## Still a bit inefficient this, but that's what comes with using lists...
  # lapply(match_pop, \(x) {
  #   # x$accuracy <- round(x$correct_count / x$match_count, 5)
  #   x$accuracy <- x$correct_count / x$match_count
  #   ## TODO Could run in problems for VERY high numbers divisions...
  #   ## Consider for next versions
  #   x
  # })
  update_matched_accuracy_cpp(match_pop)
}

#' Get the subset of a Population of Classifiers that matches a given State
#'
#' @param instance_state A state from the RLCS environment
#' @param pop A population of Classifiers
#'
#' @returns Numeric vector, of indices of the matching Classifiers
#' @export
#'
#' @examples
#' demo_env1 <- rlcs_demo_secret1()
#' demo_params <- RLCS_hyperparameters(n_epochs = 280, deletion_trigger = 40, deletion_threshold = 0.9)
#' rlcs_model1 <- rlcs_train_sl(demo_env1, demo_params)
#' print(rlcs_model1)
#' get_match_set("00101", rlcs_model1)
#'
get_match_set <- function(instance_state, pop) {
  if(length(pop) > 0) {
    # Only part relevant for matching
    ti_cond <- as.integer(strsplit(instance_state, "", fixed = T)[[1]])

    ## Former version, was bottleneck of the overall runtimes in several tests:
    # match_set <- which(sapply(pop, \(item, ti_cond) {
    #     rule <- item$condition_list
    #     !(any(ti_cond[rule$'0'] != 0) || any(ti_cond[rule$'1'] != 1))
    # }, ti_cond))


    # ## Simple C++ version:
    match_set <- get_match_set_cpp(pop, ti_cond)

    ## Tried to vectorize, no improvement...
    # if(length(pop) < 8) {
    ## EDIT FOR RCpp compatible. Quite a bit faster, this:
    #   match_set <- get_match_set_cpp(pop, ti_cond)
    # } else {
    #
    #   match_set <- unlist(sapply(0:3, \(i) {
    #     indices <- which((1:length(pop)%%4) == i)
    #     indices[get_match_set_cpp(pop[indices], ti_cond)]
    #   }))
    #
    #   # print(new_match_set)
    #   # match_set <- new_match_set
    # }



    if(length(match_set) > 0)
      return(match_set)
  }

  NULL ## implicit return
}

.get_match_set_mat <- function(instance_state, pop, t_matrices, t_lengths) {
  if(length(pop) > 0) {
    # Only part relevant for matching
    ti_cond <- as.integer(strsplit(instance_state, "", fixed = T)[[1]])


    ## Matrices approach?
    ## THIS CAN BE MOVED TO REDUCE ITERATIONS A LOT!

    matched_zeros <- t_matrices[[1]] %*% (1-ti_cond)
    matched_ones <- t_matrices[[2]] %*% ti_cond
    matched_lengths <- matched_zeros + matched_ones
    match_set <- which(matched_lengths == t_lengths)
    # browser()

    if(length(match_set) > 0)
      return(match_set)
  }

  NULL ## implicit return
}

#' Returns all rlcs_environment entries (data frame row indices) that match a Classifier/Rule
#'
#' @param rlcs_classifier A Population of Classifiers, pre-trained.
#' @param rlcs_environment An RLCS environment (data frame inc. a "state" variable)
#'
#' @returns Data Frame Row Indices from an environment that match a Classifier
#' @export
#'
#' @examples
#' ## See Iris Example in GitHub for detailed example. NOT RUN
reverse_match_set <- function(rlcs_classifier, rlcs_environment) {
  print(rlcs_classifier)

  rule_0 <- rlcs_classifier$condition_list$'0'
  rule_1 <- rlcs_classifier$condition_list$'1'

  match_set <- which(sapply(rlcs_environment$state, \(item, rule_0, rule_1) {
    env_entry <- as.integer(strsplit(item, "", fixed = T)[[1]])
    !(any(env_entry[rule_0] != 0) || any(env_entry[rule_1] != 1))
  }, rule_0, rule_1))

  # match_set <- get_match_set_cpp(pop, ti_cond)

  if(length(match_set) > 0)
    return(match_set)
  NULL ## implicit return
}


.found_same_condition <- function(pop, item) {
  any(sapply(pop, \(x, item) {
    if(x$condition_string == item) return(TRUE)
    FALSE
  }, item))
}

.apply_deletion_no_threshold <- function(pop) {

  ## Works nicely with subsumption to remove unnecessary classifiers:
  survivors_set <- which(sapply(pop, \(x) {
    if(x$numerosity > 0) return(TRUE)
    FALSE
  }))
  if(length(survivors_set) == 0) return(NULL)
  ## Ensure you keep class here.
  pop <- pop[survivors_set]
  # pop <- .recalculate_pop_matrices(pop)

  structure(pop, class = "rlcs_population")
}

## Bad: Old doesn't mean it should be discarded.
# keep_only_newer_individuals <- function(pop, first_seen_threshold, accuracy=1) {
#
#   ## Works nicely with subsumption to remove unnecessary classifiers:
#   survivors_set <- which(sapply(pop, \(x) {
#     if(x$first_seen > first_seen_threshold && x$accuracy >= accuracy) return(TRUE)
#     FALSE
#   }))
#   if(length(survivors_set) == 0) return(NULL)
#   ## Ensure you keep class here.
#   structure(pop[c(survivors_set)], class = "rlcs_population")
# }

## Particularly useful function for parallel runs, which is not a default.
.remove_duplicate_rules <- function(pop) {
  ## Somewhat expensive function, but... Doesn't show up all that much in profvis
  if(length(pop) > 1) {
    # pop <- lcs_best_sort(pop) ## Not needed here
    for(item in 1:(length(pop)-1)) {

      cond_string <- pop[[item]]$condition_string
      cond_lab <- pop[[item]]$action

      if(pop[[item]]$numerosity > 0) {
        ## Showing progress points. As this could take a while...
        if(item %% 100 == 0) print(item)

        pop[item:length(pop)] <- lapply(item:length(pop), \(x, t_cond, t_lab, ref_num) {
          if(x > ref_num &&
             pop[[x]]$numerosity > 0 &&
             pop[[x]]$condition_string == t_cond &&
             pop[[x]]$action == t_lab) {
            pop[[x]]$numerosity <- 0
          }

          pop[[x]]
        }, cond_string, cond_lab, item)
      }
    }

    ## Removing duplicates
    pop <- .apply_deletion_no_threshold(pop)
  }

  pop
}
