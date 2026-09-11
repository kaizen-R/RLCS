######
## SUB FUNCTIONS
## Better self contained like this for the time being.
######

## New: Focus on matrices & vectors as central processing for all, not lists
## env must contain an object "lcs".
## This is expected from within RLCS processing subroutines.
## IMPORTANT: Here we assume ALL CONDITION STRINGS ARE THE SAME LENGTH!!
.new_rlcs_rule_env_mat <- function(env, condition_string, action, date_rule_born) {

  ## This function should never happen "in concurrency" with other calls!

  action <- as.character(action)
  ## NEW!
  condition_length <- env$lcs$conditions_length

  # browser()

  # print(condition_length)
  zeros_vector <- numeric(condition_length)
  ones_vector <- rep(0, condition_length) ## Equivalent

  t_vec <- strsplit(condition_string, "", fixed=T)[[1]]

  zeros_vector[t_vec == "0"] <- 1
  ones_vector[t_vec == "1"] <- 1

  # length_fixed_bits <- length(which_zeros)+length(which_ones)
  length_fixed_bits <- sum(zeros_vector) + sum(ones_vector)
  ## Locate empty slot for new rule insertion:

  pos_to_update <- which(env$lcs$numerosities == 0)[1]
  # pos_to_update <- which(!env$lcs$valid_rules)[1]

  # cat('\n', pos_to_update, '\n')

  ## If NO empty slot, add to all components, n_entries by n_entries for efficiency:
  n_entries <- max(10, round(env$lcs$model_initial_space / 10, 0))

  if(!any(env$lcs$numerosities == 0)) {
  # if(length(pos_to_update) == 0) {
    ## Prepare next position insert:
    pos_to_update <- length(env$lcs$numerosities) + 1

    ## Prepare space for upcoming new rule entries
    env$lcs$condition_strings <- c(env$lcs$condition_strings, character(n_entries))
    env$lcs$actions <- c(env$lcs$actions, character(n_entries))
    env$lcs$rule_first_seens <- c(env$lcs$rule_first_seens, numeric(n_entries))

    env$lcs$match_counts <- c(env$lcs$match_counts, rep(1, n_entries)) ## could be integer
    env$lcs$correct_counts <- c(env$lcs$correct_counts, rep(1, n_entries))
    env$lcs$numerosities <- c(env$lcs$numerosities, numeric(n_entries)) ## Default to 0
    env$lcs$accuracies <- c(env$lcs$accuracies, rep(1, n_entries)) ## numeric


    ## For RL: Total Reward starts at 5, not at 0:
    env$lcs$action_counts <- c(env$lcs$action_counts, rep(1, n_entries))
    env$lcs$total_rewards <- c(env$lcs$total_rewards, rep(5, n_entries))

    ## New. Found in some LCS explanations out there... Just wasn't in RLCS
    ## yet.
    env$lcs$coverage_epoch_correct_count <- c(env$lcs$coverage_epoch_correct_count, rep(1L, n_entries))

    ## Now add space to matching matrices
    env$lcs$matrix_conditions_vecs <- rbind(env$lcs$matrix_conditions_vecs,
                                            matrix(numeric(condition_length*n_entries), byrow = T, nrow=n_entries))
    env$lcs$matrix_match_0s <- rbind(env$lcs$matrix_match_0s,
                                     matrix(numeric(condition_length*n_entries), byrow = T, nrow=n_entries))
    env$lcs$matrix_match_1s <- rbind(env$lcs$matrix_match_1s,
                                     matrix(numeric(condition_length*n_entries), byrow = T, nrow=n_entries))
    ## Faster to compare later
    env$lcs$lengths_fixed_bits <- c(env$lcs$lengths_fixed_bits, rep(-1, n_entries))
    # env$lcs$valid_rules <- c(env$lcs$valid_rules, logical(n_entries)) ## Equivalent to rep F 10
  } else {
    pos_to_update <- which(env$lcs$numerosities == 0)[1]
  }

  env$lcs$condition_strings[pos_to_update] <- condition_string
  env$lcs$actions[pos_to_update] <- action
  env$lcs$rule_first_seens[pos_to_update] <- date_rule_born

  # env$lcs$match_counts[pos_to_update] <- 1 ## Already the default
  # env$lcs$correct_counts[pos_to_update] <- 1 ## Already the default
  # env$lcs$accuracies[pos_to_update] <- 1 ## Already the default
  #
  # For RL:
  # env$lcs$action_counts[pos_to_update] <- 1 ## Already the default
  # env$lcs$total_rewards[pos_to_update] <- 5 ## Already the default
  #
  # env$lcs$coverage_epoch_correct_count[pos_to_update] <- 1

  # print(t_vec)
  # print(pos_to_update)
  # print(env$lcs$matrix_conditions_vecs[pos_to_update, ])
  env$lcs$matrix_conditions_vecs[pos_to_update, ] <-  t_vec
  env$lcs$lengths_fixed_bits[pos_to_update] <- length_fixed_bits
  env$lcs$matrix_match_0s[pos_to_update, ] <- zeros_vector
  env$lcs$matrix_match_1s[pos_to_update, ] <- ones_vector

  env$lcs$numerosities[pos_to_update] <- 1 ## ACTIVATE THIS RULE
  # env$lcs$valid_rules[pos_to_update] <- TRUE ## ACTIVATE THIS RULE

  # print(env$lcs$total_rewards[pos_to_update])

  return(pos_to_update) ## Could be useful as a return
}

## Why not use S3 OO, as we want to use plot(), print()...
.new_rlcs <- function(sample_state_from_environment = "00000", n_entries = 500) {
    lcs <- list()
    lcs$model_initial_space <- n_entries
    ## We initialize an empty model with several spaces pre-reserved
    ## for processing efficiency reasons:


    ## Initialization of matrices requires known sample string length
    ## IMPORTANT: We consider a fixed length for all sample states
    condition_length <- nchar(sample_state_from_environment)

    lcs$conditions_length <- condition_length ## FIXED FOR ALL LCS MODEL HERE
    lcs$condition_strings <- character(n_entries)
    lcs$actions <- character(n_entries)
    lcs$rule_first_seens <- numeric(n_entries)

    lcs$match_counts <- rep(1, n_entries) ## could be integer
    lcs$correct_counts <- rep(1, n_entries)
    lcs$numerosities <- numeric(n_entries) ## Default to 0
    lcs$accuracies <- numeric(n_entries) ## numeric

    ## For RL: Total Reward starts at 5, not at 0:
    lcs$action_counts <- rep(1, n_entries)
    lcs$total_rewards <- rep(5, n_entries)

    ## New. Found in some LCS explanations out there... Just wasn't in RLCS
    ## yet.
    lcs$coverage_epoch_correct_count <- rep(1L, n_entries)

    ## Now add space to matching matrices
    lcs$matrix_conditions_vecs <- matrix(numeric(condition_length*n_entries), byrow = T, nrow=n_entries)
    lcs$matrix_match_0s <- matrix(numeric(condition_length*n_entries), byrow = T, nrow=n_entries)
    lcs$matrix_match_1s <- matrix(numeric(condition_length*n_entries), byrow = T, nrow=n_entries)
    ## Faster to compare later
    lcs$lengths_fixed_bits <- rep(-1, n_entries) #numeric(n_entries)
    # lcs$valid_rules <- logical(n_entries) ## equivalent to initialize to false

    lcs <- structure(lcs, class = "rlcs")
    return(lcs)
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

.lengths_fixed_bits_new_rule <- function(t_lengths, condition_string) {
  t_cond <- strsplit(condition_string, "", fixed=T)[[1]]

  zeros_vector <- ones_vector <- rep(0, nchar(condition_string))

  which_zeros <- which_cpp(t_cond == "0")
  which_ones <- which_cpp(t_cond == "1")

  c(t_lengths, length(which_zeros)+length(which_ones))
}

## Function to add rule to a population.
## date_rule_born will be useful stat for future, setting as parameter for now.
.add_valid_rule_to_lcs_env_mat <- function(env, condition_string,
                                           action, date_rule_born = 0,
                                           match_count = 1,
                                           correct_count = 1,
                                           accuracy = 1,
                                           numerosity = 1) {

  ## Key here is creating a population structure
  if(is.null(env$lcs)) {
    env$lcs <- .new_rlcs(condition_string, n_entries=100)
    return(NULL)
  }


  ## New rule with defaults
  t_rule_pos <- .new_rlcs_rule_env_mat(env, condition_string, action, date_rule_born)

  ## If not defaults, well:
  if(match_count != 1)
    env$lcs$match_counts[t_rule_pos] <- match_count
  if(correct_count != 1)
    env$lcs$correct_counts[t_rule_pos] <- correct_count
  if(match_count != 1 | correct_count != 1)
    env$lcs$accuracies[t_rule_pos] <- env$lcs$correct_counts[t_rule_pos] / env$lcs$match_counts[t_rule_pos]
  if(numerosity != 1)
    env$lcs$numerosities[t_rule_pos] <- numerosity

  return(t_rule_pos) ## Could be useful
}




## FUNCTION FACTORY!
## Often needed, is to update by increase of 1 one parameter of an LCS rule
# .inc_param_count <- function(param) {
#   f_param <- param
#
#   function(pop) {
#     inc_param_count_cpp(pop, f_param)
#   }
# }

## Augment match count of a set of classifiers
.inc_match_count_env3 <- function(env, match_set) {
  env$lcs$match_counts[match_set] <- env$lcs$match_counts[match_set]+1
}

.inc_correct_count_env3 <- function(env) {
  env$lcs$correct_counts[correct_set] <- env$lcs$correct_counts[correct_set]+1
}

.inc_numerosity_by_condition3 <- function(env, item) {
  t_positions <- (env$lcs$condition_strings == item)
  t_positions <- t_positions[which(env$lcs$numerosities[t_positions] > 0)]
  env$lcs$numerosities[t_positions] <- env$lcs$numerosities[t_positions]+1
}

.inc_numerosity3 <- function(env, positions) {
  env$lcs$numerosities[positions] <- env$lcs$numerosities[positions]+1
}


.update_accuracy_env3 <- function(env, positions) {
  ## TODO Could run in problems for VERY high numbers divisions...?
  env$lcs$accuracies[positions] <- env$lcs$correct_counts[positions] / env$lcs$match_counts[positions]
  NULL
}

.update_matched_accuracy_env3 <- function(env) {
  .update_accuracy_env3(env, env$match_set)
  NULL
}

# .update_pop_accuracy_env <- function(env) {
#   update_matched_accuracy_cpp2(env$lcs$pop)
#   NULL
# }

.update_pop_accuracy_env3 <- function(env) {
  # .update_accuracy_env3(env, which(env$lcs$numerosities > 0 & env$lcs$lengths_fixed_bits > 0))
  .update_accuracy_env3(env, which(env$lcs$numerosities > 0))
  NULL
}

## New full matrix / vectors approach
.get_match_set_mat_env3 <- function(ti_cond, env) {

  valid_set <- env$lcs$numerosities > 0
  if(any(valid_set)) {
    # if(env$use_gpu) {
    #   # print("Use Torch!")
    #   match_set <- which(torch::as_array(torch::torch_matmul(t_lcs$matrices[[3]],
    #                                                          torch::torch_tensor(1-ti_cond, dtype = torch::torch_uint8(), device=env$gpu_type)) +
    #                                        torch::torch_matmul(t_lcs$matrices[[4]],
    #                                                            torch::torch_tensor(ti_cond, dtype = torch::torch_uint8(), device=env$gpu_type))) ==
    #                        t_lcs$lengths)
    # } else {
    match_set <- (env$lcs$matrix_match_0s %*% (1-ti_cond) +
                          env$lcs$matrix_match_1s %*% ti_cond) ==
                         env$lcs$lengths_fixed_bits
    # }
    if(any(match_set)) {
      match_set_bool <- match_set[valid_set]

      if(any(match_set_bool)) {
        match_set <- which(match_set_bool)
        ## Saving a function call
        env$lcs$match_counts[match_set] <- env$lcs$match_counts[match_set]+1
        return(match_set)
      }
    }
  }
  NULL ## implicit return
}

## Version not updating the LCS match scores
.get_match_set_mat_env3_no_update <- function(ti_cond, env) {

  valid_set <- env$lcs$numerosities > 0
  if(any(valid_set)) {
    # if(env$use_gpu) {
    #   # print("Use Torch!")
    #   match_set <- which(torch::as_array(torch::torch_matmul(t_lcs$matrices[[3]],
    #                                                          torch::torch_tensor(1-ti_cond, dtype = torch::torch_uint8(), device=env$gpu_type)) +
    #                                        torch::torch_matmul(t_lcs$matrices[[4]],
    #                                                            torch::torch_tensor(ti_cond, dtype = torch::torch_uint8(), device=env$gpu_type))) ==
    #                        t_lcs$lengths)
    # } else {
    match_set <- (env$lcs$matrix_match_0s %*% (1-ti_cond) +
                    env$lcs$matrix_match_1s %*% ti_cond) ==
      env$lcs$lengths_fixed_bits
    # }
    if(any(match_set)) {
      match_set_bool <- match_set[valid_set]

      if(any(match_set_bool)) {
        match_set <- which(match_set_bool)
        return(match_set)
      }
    }
  }
  NULL ## implicit return
}
# ## Idea: Do matching once, multiple env samples at a time, how would that go?
# .get_match_set_mat_env3 <- function(sample_pos, env, train_count) {
#
#   env_subset_pos <- sample_pos:min(nrow(env$train_env_df), sample_pos+1)
#   ti_conds <- env$environment_conds_mat[, env_subset_pos]
#
#   # print(sample_pos)
#
#   match_sets <- NULL
#
#   if(length(env$lcs$pop) == 0) {
#     cover_rule <-
#       .generate_cover_rule_for_unmatched_instance(env$environment_states[sample_pos],
#                                                   env$run_params$get_wildcard_prob())
#     if(!is.null(cover_rule)) {
#       .add_valid_rule_to_lcs_env(env, cover_rule,
#                                  env$environment_classes[sample_pos],
#                                  train_count)
#     }
#     return(NULL)
#   }
#
#   # Only part relevant for matching
#   # # Matrices approach!
#   match_sets_vals <- matrix((env$lcs$matrices[[1]] %*% (1-ti_conds) +
#                               env$lcs$matrices[[2]] %*% ti_conds),
#                             byrow=F, ncol=nrow(env$lcs$matrices[[1]]))
#   match_sets_matrix <- which_cpp(match_sets_vals == env$lcs$lengths, arr.ind=T)
#   match_sets <- unique(match_sets_matrix[,1])
#   # browser()
#
#   # if(is.null(match_sets) | length(match_sets) == 0) {
#   #   message("Cover needed")
#   #   cover_rule <-
#   #     .generate_cover_rule_for_unmatched_instance(env$environment_states[sample_pos],
#   #                                                 env$run_params$get_wildcard_prob())
#   #   if(!is.null(cover_rule)) {
#   #     .add_valid_rule_to_lcs_env(env, cover_rule,
#   #                                env$environment_classes[sample_pos],
#   #                                train_count)
#   #   }
#   #   return(NULL)
#   # }
#
#   # %% nrow(env$lcs$matrices[[1]])
#
#   # print(env_subset_pos[-match_sets])
#   if(is.null(match_sets) | length(match_sets) == 0) {
#     match_env_subset_pos <- env_subset_pos
#   } else {
#     match_env_subset_pos <- env_subset_pos[-match_sets]
#   }
#
#   lapply(match_env_subset_pos,
#          \(i) {
#            cover_rule <-
#              .generate_cover_rule_for_unmatched_instance(env$environment_states[i],
#                                                          env$run_params$get_wildcard_prob())
#            if(!is.null(cover_rule)) {
#              .add_valid_rule_to_lcs_env(env, cover_rule,
#                                         env$environment_classes[i],
#                                         train_count)
#              NULL
#            }
#          })
#
#   # browser()
#
#   if(length(match_env_subset_pos) > 0)
#     if(match_env_subset_pos[1] == sample_pos)
#       return(NULL)
#
#   as.integer(match_sets_matrix[which_cpp(match_sets_matrix[,1] == 1), 2])
#   # env_subset_pos[match_sets] ## Returning positions matching!
# }

#' Get the subset of a Population of Classifiers that matches a given State
#'
#' @param instance_state A state from the RLCS environment
#' @param lcs A population of Classifiers
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
## Still works, mainly for end-user; but will eventually be discarded.
get_match_set <- function(instance_state, lcs) {
  lcs <- lcs
  .get_match_set_mat_env3(ti_cond = as.numeric(strsplit(instance_state, "")[[1]]), environment())
}

#' Returns all rlcs_environment entries (data frame row indices) that match a Classifier/Rule
#'
#' @param rlcs_classifier A Population of Classifiers, pre-trained.
#' @param position the rule identifier within the population you want to check
#' @param rlcs_environment An RLCS environment (data frame inc. a "state" variable)
#'
#' @returns Data Frame Row Indices from an environment that match a Classifier
#' @export
#'
#' @examples
#' ## See Iris Example in GitHub for detailed example. NOT RUN
reverse_match_set <- function(rlcs_classifier, position, rlcs_environment) {
  print(rlcs_classifier)

  rule_0 <- as.logical(rlcs_classifier$matrix_match_0s[position, ])
  rule_1 <- as.logical(rlcs_classifier$matrix_match_1s[position, ])

  match_set <- which(sapply(rlcs_environment$state, \(item, rule_0, rule_1) {
    env_entry <- as.integer(strsplit(item, "", fixed = T)[[1]])
    !(any(env_entry[rule_0] != 0) || any(env_entry[rule_1] != 1))
  }, rule_0, rule_1))

  if(length(match_set) > 0)
    return(match_set)
  NULL ## implicit return
}

.reverse_match_set_size3 <- function(env, rlcs_environment) {
  match_sets_lengths <- c()
  for(i in 1:length(env$lcs$condition_strings)) {
    rule_0 <- which(env$lcs$matrix_match_0s[i,] == 1)
    rule_1 <- which(env$lcs$matrix_match_1s[i,] == 1)

    rule_matches <- which_cpp(sapply(rlcs_environment$state, \(item, rule_0, rule_1) {
      env_entry <- as.integer(strsplit(item, "", fixed = T)[[1]])
      !(any(env_entry[rule_0] != 0) || any(env_entry[rule_1] != 1))
    }, rule_0, rule_1))

    match_sets_lengths <- c(match_sets_lengths, length(rule_matches))
  }

  match_sets_lengths
}


.found_same_condition3 <- function(env, correct_set, item) {
  matched_condition <- (env$lcs$condition_strings[correct_set] == item)

  if(any(matched_condition)) {
    # print(matched_condition)
    return(correct_set[matched_condition])
  }
  return(c())
}

.apply_deletion_no_threshold_env3 <- function(env) {
  ## Works nicely with subsumption to remove unnecessary classifiers:

  survivors_set <- which(env$lcs$numerosities > 0)
  survivors_set <- survivors_set[which(survivors_set %in% which(env$lcs$lengths_fixed_bits > 0))]
  # survivors_set <- which(env$lcs$valid_rules)
  # delete_set <- which(env$lcs$numerosities == 0)

  if(length(survivors_set) < 1) return(NULL)

  # env$lcs$conditions_length <- env$lcs$conditions_length ## Fixed, no update needed
  env$lcs$condition_strings <- env$lcs$condition_strings[survivors_set]
  env$lcs$actions <- env$lcs$actions[survivors_set]
  env$lcs$rule_first_seens <- env$lcs$rule_first_seens[survivors_set]

  env$lcs$match_counts <- env$lcs$match_counts[survivors_set]
  env$lcs$correct_counts <- env$lcs$correct_counts[survivors_set]

  env$lcs$numerosities <- env$lcs$numerosities[survivors_set]

  env$lcs$accuracies <- env$lcs$accuracies[survivors_set]

  ## For RL: Total Reward starts at 5, not at 0:
  env$lcs$action_counts <- env$lcs$action_counts[survivors_set]
  env$lcs$total_rewards <- env$lcs$total_rewards[survivors_set]

  ## New. Found in some LCS explanations out there... Just wasn't in RLCS
  ## yet.
  # env$lcs$coverage_epoch_correct_count <- rep(1L, n_entries)

  ## Now add space to matching matrices
  env$lcs$matrix_conditions_vecs <- env$lcs$matrix_conditions_vecs[survivors_set,]
  env$lcs$matrix_match_0s <- env$lcs$matrix_match_0s[survivors_set,]
  env$lcs$matrix_match_1s <- env$lcs$matrix_match_1s[survivors_set,]

  ## Faster to compare later
  env$lcs$lengths_fixed_bits <- env$lcs$lengths_fixed_bits[survivors_set]
  # env$lcs$valid_rules <- env$lcs$valid_rules[survivors_set]

  env$lcs <- structure(env$lcs, class = "rlcs")
}


# ## Particularly useful function for parallel runs, which is not a default.
# .remove_duplicate_rules <- function(pop) {
#   ## Somewhat expensive function, but... Doesn't show up all that much in profvis
#   if(length(pop) > 1) {
#     # pop <- lcs_best_sort(pop) ## Not needed here
#     for(item in 1:(length(pop)-1)) {
#
#       cond_string <- pop[[item]]$condition_string
#       cond_lab <- pop[[item]]$action
#
#       if(pop[[item]]$numerosity > 0) {
#         ## Showing progress points. As this could take a while...
#         if(item %% 100 == 0) print(item)
#
#         pop[item:length(pop)] <- lapply(item:length(pop), \(x, t_cond, t_lab, ref_num) {
#           if(x > ref_num &&
#              pop[[x]]$numerosity > 0 &&
#              pop[[x]]$condition_string == t_cond &&
#              pop[[x]]$action == t_lab) {
#             pop[[x]]$numerosity <- 0
#           }
#
#           pop[[x]]
#         }, cond_string, cond_lab, item)
#       }
#     }
#
#     ## Removing duplicates
#     pop <- .apply_deletion_no_threshold(pop)
#   }
#
#   pop
# }
