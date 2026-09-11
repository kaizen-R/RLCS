## Supervised Learning-specific versions of TRAINING functions

## CAREFUL HERE CORRECT SET IS NOT RELATIVE ANYMORE!!
.inc_correct_count_env3 <- function(env, correct_set) {
  env$lcs$correct_counts[correct_set] <- env$lcs$correct_counts[correct_set] + 1
}

.min_correct_count_env3 <- function(env, correct_set) {
  min(env$lcs$correct_counts[correct_set])
}

.get_correct_set_env3 <- function(t_instance_class, env, match_set) {
  # browser()
  if(length(match_set) > 0) {

    # correct_set <- which(env$lcs$actions_vec[match_set] == t_instance_class)
    correct_pos <- which(env$lcs$actions[match_set] == t_instance_class)
    correct_set <- match_set[correct_pos] ## NOT relative positions
    if(length(correct_set) > 0) {
      ## Now part of this function, because using env, makes more sense!
      env$lcs$correct_counts[correct_set] <- env$lcs$correct_counts[correct_set]+1
      return(correct_set)
    }
  }
  NULL ## implicit return
}

## Version not updating the LCS correct scores
.get_correct_set_env3_no_update <- function(t_instance_class, env, match_set) {
  if(length(match_set) > 0) { ## Match set is supposed to only contain valid entries
    correct_pos <- which(env$lcs$actions[match_set] == t_instance_class)
    correct_set <- match_set[correct_pos] ## NOT relative positions
    if(length(correct_set) > 0) {
      return(correct_set)
    }
  }
  NULL ## implicit return
}

## KEY function:
## Classifiers are better or worse. CHOOSING THE BEST ones is important
## For SL, accuracy is top priority, followed by generality
## This is a faster approach to calculation although arguably could be discussed
.lcs_best_sort_sl_env3 <- function(env) {

  if(is.null(env$lcs)) return(NULL)
  if(!any(env$lcs$numerosities > 0)) return(NULL) ## Nothing to sort...

  # browser()
  ## Now this here is important: Numerosity SHOULD play a role, shouldn't it?
  ranking <- env$lcs$accuracies - 0.01 * env$lcs$lengths_fixed_bits / env$lcs$conditions_length
  # ranking <- env$lcs$accuracies - 0.01 * env$lcs$lengths_fixed_bits / env$lcs$conditions_length + 0.01 * env$lcs$numerosities
  sorted_entries <- order(ranking, decreasing=T)

  env$lcs$condition_strings <- env$lcs$condition_strings[sorted_entries]
  env$lcs$actions <- env$lcs$actions[sorted_entries]
  env$lcs$rule_first_seens <- env$lcs$rule_first_seens[sorted_entries]

  env$lcs$match_counts <- env$lcs$match_counts[sorted_entries]
  env$lcs$correct_counts <- env$lcs$correct_counts[sorted_entries]
  env$lcs$numerosities <- env$lcs$numerosities[sorted_entries]
  env$lcs$accuracies <- env$lcs$accuracies[sorted_entries]

  ## For RL: Total Reward starts at 5, not at 0:
  env$lcs$action_counts <- env$lcs$action_counts[sorted_entries]
  env$lcs$total_rewards <- env$lcs$total_rewards[sorted_entries]

  ## New. Found in some LCS explanations out there... Just wasn't in RLCS
  ## yet.
  env$lcs$coverage_epoch_correct_count <- env$lcs$coverage_epoch_correct_count[sorted_entries]

  ## Now add space to matching matrices
  env$lcs$matrix_conditions_vecs <- env$lcs$matrix_conditions_vecs[sorted_entries,]
  env$lcs$matrix_match_0s <- env$lcs$matrix_match_0s[sorted_entries,]
  env$lcs$matrix_match_1s <- env$lcs$matrix_match_1s[sorted_entries,]
  ## Faster to compare later
  env$lcs$lengths_fixed_bits <- env$lcs$lengths_fixed_bits[sorted_entries]

  NULL
}

## Another key function here.
.apply_subsumption_whole_pop_sl <- function(env,deletion_limit = .6, max_pop_size = 10000) {

  if(is.null(env$lcs)) return(NULL)
  if(!any(env$lcs$numerosities > 0)) return(NULL) ## Nothing to sort...

  .apply_deletion_no_threshold_env3(env)
  .lcs_best_sort_sl_env3(env)

  ## Within this function in this case :)
  t_labs <- env$lcs$actions

  useful_pos <- which(env$lcs$numerosities > 0)
  if(length(useful_pos) < 2) {
    print("Not enough valid population to run subsumption")
    return(NULL)
  }

  subsumers_list <- list()

  for(t_pos in useful_pos[1:(length(useful_pos)-1)]) { ## If this all works, will move it to C++...
    ## For comparisons:
    rest_pos <- useful_pos[which(useful_pos > t_pos)] ## Careful, we don't want relative positions here!
    rest_t_matrices_zeros <- env$lcs$matrix_match_0s[rest_pos,]
    rest_t_matrices_ones <- env$lcs$matrix_match_1s[rest_pos,]

    ## Remember the rules are sorted by accuracy and generality already!
    subsumer_must_match_zeros <- rest_t_matrices_zeros  %*% env$lcs$matrix_match_0s[t_pos, ]
    subsumer_must_match_ones <- rest_t_matrices_ones  %*% env$lcs$matrix_match_1s[t_pos, ]

    subsumed_must_be_different_zero <-  rest_t_matrices_zeros %*%  env$lcs$matrix_match_1s[t_pos, ]
    subsumed_must_be_different_one <- rest_t_matrices_ones %*% env$lcs$matrix_match_0s[t_pos, ]

    pop_to_delete <- which(
      ((subsumer_must_match_zeros+subsumer_must_match_ones) == env$lcs$lengths_fixed_bits[t_pos]) &
        (subsumed_must_be_different_zero+subsumed_must_be_different_one == 0) &
        (t_labs[rest_pos] == t_labs[t_pos])
    )

    ## Return positions to be deleted from population
    if(!is.null(pop_to_delete) && length(pop_to_delete) > 0) {
      subsumers_list[[length(subsumers_list)+1]] <- (pop_to_delete + t_pos) ## Optimization. POSITIONS
    } else {
      subsumers_list[[length(subsumers_list)+1]] <- NA
    }
  }

  # subsumers_positions <- which(!is.na(subsumers_list))
  subsumers_positions <- which(!is.na(subsumers_list))
  if(length(subsumers_positions)>0) {
    env$lcs$numerosities[subsumers_positions] <- vapply(subsumers_positions, \(i) {
      env$lcs$numerosities[i] + length(subsumers_list[[i]])
    }, numeric(1)) ## Go this just once!

    pop_to_delete <- unique(unlist(subsumers_list)) ## Reduce operations
    env$lcs$numerosities[pop_to_delete] <- 0 ## Go this just once!
    ## New, to be reviewed: If I keep working with numerosities > 0, deletion is not useful:
    .apply_deletion_sl_env(env,
                            deletion_limit = deletion_limit,
                            max_pop_size = max_pop_size)
  }
}

.validate_SL_train_df <- function(train_env_df) {
  if(!("state" %in% names(train_env_df))) stop("Input Data Frame must contain a 'state' column.")
  if(!("class" %in% names(train_env_df))) stop("Input Data Frame must contain a 'class' column.")
  if(!all(vapply(train_env_df$state, .validate_state_string, logical(1)))) stop("SL: Training environment, wrong state found. STOP.")
}


.perfect_coverage_simplifier_sl_env3 <- function(env, train_env_df, t_classes_counts) {

  pop_actions <- env$lcs$actions

  if(length(pop_actions) == 0) return(NULL)
  if(length(unique(pop_actions)) == length(t_classes_counts)) {
    t_df <- data.frame(match_sizes = .reverse_match_set_size3(env, train_env_df),
                       match_class = pop_actions,
                       rule_id = 1:length(env$lcs$condition_strings))
    t_df <- merge(t_df, as.data.frame(t_classes_counts), by.x = "match_class", by.y = "Var1")
    t_df <- t_df[order(t_df$rule_id),]

    if(nrow(t_df) > 1) {
      for(i in 1:(nrow(t_df)-1)) {

        if(t_df$match_sizes[i] == t_df$Freq[i]) { ## Full Coverage!
          ## Then delete all entries that have the same match_class, as they are useless
          to_remove <- which(t_df[i+1:nrow(t_df), "match_class"] == t_df$match_class[i])
          env$lcs$numerosities[to_remove] <- 0
        }
      }

      use_gpu <- env$use_gpu
      if(use_gpu) gpu_type <- env$gpu_type

      .apply_deletion_sl_env(env)
    }
  }
}


#' Try to simplify a Learning Classifier System (LCS) population.
#'
#' By iterating over the rules of a population
#' assumed sorted already, going backwards, try to evaluate
#' the quality of the model and make sure you can maintain results while removing
#' rules one by one. Only remove rules that do not affect results.
#' @param rlcs_obj
#' An RLCS model object.
#' @param train_df
#' The dataset used for training the model. This COULD be a different dataset,
#' although results might vary then.
#'
#' @returns
#' An \R \code{RLCS Model} containing a shorter population of rules, ideally.
#' @export
#'
#' @examples
#' ## Supposing you have trained an RLCS model against the iris dataset as
#' ## proposed in the documented examples:
#' rlcs_iris <- rlcs_rosetta_stone(iris, class_col=5)
#' full_dataset <- cbind(iris, rlcs_iris$model)
#' full_dataset <- full_dataset[sample(1:nrow(full_dataset), nrow(full_dataset), replace = FALSE), ]
#' ## Train-test separation:
#' train_set <- sample(1:nrow(full_dataset), size = round(0.8*nrow(full_dataset)), replace = FALSE)
#' train_environment <- full_dataset[train_set,]
#' test_environment <- full_dataset[-train_set,]
#' iris_classifier <- rlcs_train_sl(train_environment)
#' cleaner_iris_classifier <- rlcs_simplify_pop(iris_classifier, train_environment)
#' print(cleaner_iris_classifier)
#' print(cleaner_iris_classifier)
#' plot(cleaner_iris_classifier)
rlcs_simplify_pop <- function(rlcs_obj, train_df) {

  train_df$predicted <- rlcs_predict_sl(train_df, rlcs_obj)
  t_res <- table(train_df[, c("class", "predicted")])
  print(t_res)
  base_accuracy_res <- round(sum(sapply(1:nrow(train_df), \(i) {
    ifelse(train_df[i, "class"] == train_df[i, "predicted"], 1, 0)
  }))/nrow(train_df), 4)
  print(base_accuracy_res)
  print(length(rlcs_obj$condition_strings))

  recalculated_df <- train_df
  backup_rlcs_obj <- rlcs_obj

  for(i in length(rlcs_obj$condition_strings):1) {
    cat("checking rule: ", i, '...')
    lcs <- rlcs_obj

    lcs$numerosities[i] <- 0
    .apply_deletion_no_threshold_env3(environment())

    train_df$predicted <- rlcs_predict_sl(recalculated_df, lcs)
    new_accuracy_res <- round(sum(sapply(1:nrow(train_df), \(i) {
      ifelse(train_df[i, "class"] == train_df[i, "predicted"], 1, 0)
    }))/nrow(train_df), 4)
    if(new_accuracy_res == base_accuracy_res) {
      cat("removing rule", i)
      rlcs_obj <- lcs
    }
    cat('\n')

  }
  rlcs_obj
}


.apply_deletion_sl_env <- function(env, deletion_limit = 0.6, max_pop_size = 10000) {

  env$lcs$numerosities[env$lcs$accuracies < deletion_limit] <- 0

  positions_nums <- which(env$lcs$numerosities > 0 & env$lcs$lengths_fixed_bits > 0)

  if(length(positions_nums) > max_pop_size) {
    env$lcs$numerosities[positions_nums[(max_pop_size+1):length(positions_nums)]] <- 0
  }

  .apply_deletion_no_threshold_env3(env)

  NULL
}


























## NEW: Try to use only matrices and vectors now...
.rlcs_train_one_instance_one_epoch_mat_env3 <- function(env,
                                                       sample_pos,
                                                       t_instance,
                                                       size_env, ## Used for Subsumption Freq.
                                                       n_epoch, ## Used for Subsumption Freq.
                                                       train_count, ## train_count
                                                       run_params ## Algorithm Hyperparameters
) {
  ######
  ## Main process for R LCS Training
  ######

  # ## Do we want to try using torch?
  # if(env$use_gpu) { use_gpu <- env$use_gpu; gpu_type <- env$gpu_type }

  ## ADD ERROR CONTROL
  t_instance_state <- env$environment_states[sample_pos] ## this is a vector
  t_instance_vec <- env$environment_conds_mat[, sample_pos] ## matrix version better?
  t_instance_class <- env$environment_classes[sample_pos]

  match_set <- .get_match_set_mat_env3(t_instance_vec, env)

  if(is.null(match_set) || length(match_set) == 0) { ## COVERING needed
    for(iter in 1:3) { ## Testing more covering for new niches?
      cover_rule <-
        .generate_cover_rule_for_unmatched_instance(t_instance_state,
                                                    run_params$get_wildcard_prob())
      if(!is.null(cover_rule)) {
        .add_valid_rule_to_lcs_env_mat(env, cover_rule,
                                   t_instance_class,
                                   train_count)
      }

    } ## END TESTING MORE COVERING FOR NEW NICHES
    return(NULL)
  }

  ## Faster to work with only match population until need to review overall population

  ## Now part of match set discovery!
  # .inc_match_count_env3(env, match_set)
  ## better because more contained!

  correct_set <- .get_correct_set_env3(t_instance_class, env, match_set)

  if(is.null(correct_set) || length(correct_set) == 0) { ## COVERING needed
    cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance_state,
                                                              run_params$get_wildcard_prob())
    if(!is.null(cover_rule)) {
      .add_valid_rule_to_lcs_env_mat(env, cover_rule, t_instance_class, train_count)
    }
    return(NULL) ## Not convinced about this one here...
  } else {

    ## Now part of correct set discovery!
    # .inc_correct_count_env3(env, correct_set)

    ## *Second* Rule Discovery HAPPENS HERE NOW
    ## Rule discovery happens only AFTER A RULE HAS HAD SOME TIME

    # if(round(.mean_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
    # if(round(.mean_match_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
    # if((.min_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
    # if((.min_correct_count(correct_pop) %% run_params$rd_trigger) == 0) {
    # if((.min_match_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
    if((.min_correct_count_env3(env, correct_set) %% run_params$rd_trigger) == 0) {

      ## New: Moved: The idea is to call this only when absolutely needed:
      .update_accuracy_env3(env, match_set) ## This really only needs to happen here


      ## The GA, basically, happens here: Cross-over & Mutation:
      children <- .cross_over_parents_strings_sl_env3(env, correct_set, run_params$get_sel_mode(),
                                       run_params$get_tournament_pressure()) |>
        sapply(.mutate_condition_string, t_instance_state, run_params$get_mut_prob())

      ## In some cases, we have only one child.
      for(child in children) {
        pos_duplicated <- .found_same_condition3(env, correct_set, child)

        if(length(pos_duplicated) > 0) { ## Duplicate rule
          env$lcs$numerosities[pos_duplicated] <- env$lcs$numerosities[pos_duplicated] + 1
        } else {
          .add_valid_rule_to_lcs_env_mat(env, child, t_instance_class, train_count)
        }
      }
    }
  }

  # ## NEW: More rule-discovery
  # ## OK, matched, correct set, but what if there is not enough correctset?
  # if(length(correct_set) < 2) { ## COVERING enforced
  #   cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance_state,
  #                                                             run_params$get_wildcard_prob())
  #   if(!is.null(cover_rule)) {
  #     # lcs <- .add_valid_rule_to_lcs(lcs, cover_rule,
  #     #                               t_instance$class, train_count)
  #     .add_valid_rule_to_lcs_env(env, cover_rule, t_instance$class, train_count)
  #   }
  # }

  ## Apply Deletion by reducing numerosity
  if((train_count %% (run_params$deletion_trigger*size_env)) == 0) {

    .update_pop_accuracy_env3(env)

    ## Subsumption is too important to skip, for speed reasons.
    .apply_subsumption_whole_pop_sl(env,
                                     deletion_limit = run_params$get_deletion_threshold(),
                                     max_pop_size = run_params$get_max_pop_size())


    ## Now implied in subsumption above
    # .apply_deletion_sl_env(env,
    #                        deletion_limit = run_params$get_deletion_threshold(),
    #                        max_pop_size = run_params$get_max_pop_size())
  }
  NULL
}

#' Train a Learning Classifier System (LCS).
#'
#' @param train_env_df
#' A data frame containing, specifically, one "state" and one
#' "class" column. The "state" column MUST contain strings made of ONLY 0 and 1,
#' such as: "00110101". This is a requirement for the current RLCS implementation.
#' @param run_params
#' An RLCS_hyperparameters object, for which an object construction is provided.
#' @param pre_trained_lcs
#' Optional. Can be used to EVOLVE a pre-trained LCS.
#' @param use_gpu
#' Defaults to FALSE. OPTIONAL. ONLY USED if torch is available. Defaults to CUDA if found.
#' Otherwise goes back to CPU (and is slower than not enabling the GPU option in the first place).
#'
#' @returns
#' An \R \code{RLCS Model} containing the proposed model, made of several classifiers.
#' @export
#'
#' @examples
#' ## Generate running hyperparameters
#' demo_params <- RLCS_hyperparameters(n_epochs = 400, deletion_trigger = 40, deletion_threshold = 0.9)
#' ## One demo dataset for data mining scenario
#' demo_env1 <- rlcs_demo_secret1()
#' ## Try to see for yourself what the dataset hides:
#' demo_env1
#' ## Generate the model with RLCS:
#' rlcs_model <- rlcs_train_sl(demo_env1, demo_params)
#' print(rlcs_model)
#' plot(rlcs_model)
rlcs_train_sl <- function(train_env_df,
                          run_params = RLCS_hyperparameters(),
                          pre_trained_lcs = NULL,
                          use_gpu = F) {

  ## Basic input controls:
  .validate_SL_train_df(train_env_df) ## Maybe put this in a decorator?
  ## TODO Add Running Params Checks here... Use decorators!

  ## Initialization:
  lcs <- .new_rlcs(train_env_df$state[1], nrow(train_env_df))
  ## Re-training, or "online" updates
  if(!is.null(pre_trained_lcs)) lcs <- pre_trained_lcs

  ## For torch use:
  backup_gpu_flag <- use_gpu
  if(use_gpu & requireNamespace("torch", quietly=T)) {
    use_gpu <- use_gpu
    gpu_type <- ifelse(torch::cuda_is_available(), "cuda", ifelse(torch::backends_mps_is_available(), "mps", "cpu"))
  }


  ##
  ## Case 1: Default: single-core, full data, basic processing:
  ##
  print("Running single-core/thread, sequential")
  size_env <- nrow(train_env_df)
  shuffle_indexes <- sample(1:nrow(train_env_df), nrow(train_env_df), replace = F)
  train_env_df <- train_env_df[shuffle_indexes, ]
  environment_conds_mat <- sapply(strsplit(train_env_df$state, "", fixed = T), \(x) as.integer(x))
  environment_states <- train_env_df$state
  environment_classes <- train_env_df$class

  t_classes_counts <- table(train_env_df$class) ## For Coverage!!

  ## Expose algorithm to training set:
  for(epoch in 1:(run_params$get_n_epochs())) {

    # print("lcs number of entries before epoch")
    # print(length(which(environment()$lcs$numerosities > 0)))

    for(i in 1:size_env) {
      ## Now this part of the algorithm is "necessarily" sequential...
      #lcs <-
      .rlcs_train_one_instance_one_epoch_mat_env3(environment(),
                                                 i,
                                                 train_env_df[i, ],
                                                 size_env,
                                                 epoch,
                                                 (epoch-1)*size_env+i, ## train_count
                                                 run_params)
      # browser()
    }

    # if(epoch %% 10 == 0)
    # plot(lcs) ## Let's monitor progress
    # Sys.sleep(0.1)

    ## RE-shuffling population, just in case...
    train_env_df <- train_env_df[sample(1:nrow(train_env_df),
                                        nrow(train_env_df),
                                        replace = F), ]
    environment_conds_mat <- sapply(strsplit(train_env_df$state, "", fixed = T), \(x) as.integer(x))
    environment_states <- train_env_df$state
    environment_classes <- train_env_df$class

    # Sys.sleep(0.5)
    cat('\r', paste("Complete:", round(100*epoch/run_params$get_n_epochs()), "%",
                    "| Epoch:", epoch,
                    "Progress Exposure:", (epoch)*size_env,
                    "Classifiers Count:", length(which(lcs$numerosities > 0)), "   "
    ))
  }

  cat('\n')

  ## Final simplification: Coverage: This is not mandatory, but could be useful.
  ## To be reworked.
  # .perfect_coverage_simplifier_sl_env3(environment(), train_env_df, t_classes_counts)

  ## Sometimes, deletion removes all rules as none are good enough!
  if(!any(lcs$numerosities > 0)) return(NULL)
  class(lcs) <- "rlcs"

  ## Compact the LCS by removing all entries that have numerosity 0:
  ## TBD
  lcs
}


















#### PARALLEL 1
#' Train a Learning Classifier System (LCS), but go faster by running
#' and merging several agents in parallel on smaller environment subsets.
#' EXPERIMENTAL.
#'
#' @param train_env_df
#' A data frame containing, specifically, one "state" and one
#' "class" column. The "state" column MUST contain strings made of ONLY 0 and 1,
#' such as: "00110101". This is a requirement for the current RLCS implementation.
#' @param run_params
#' An RLCS_hyperparameters object, for which an object construction is provided.
#' @param pre_trained_lcs
#' Optional. Can be used to EVOLVE a pre-trained LCS.
#' @param n_agents
#' Default is 0. OPTIONAL. ONLY USED if foreach and doParallel are available.
#' IF available, a number of parallel cores, as indicated PRIOR to calling RLCS
#' like so: makeCluster() registerDoParallel()
#' then RLCS will train n_agents in parallel.
#' @param split_horizontal
#' Defaults to T. If foreach and doParallel are available: Splits evenly across
#' N agents (N number of cores/threads) the input dataset.
#' Then trains N agents, and then merges the resulting data.
#' This can potentially speed-up the process, but will probably over-fit for each
#' subset, hence probably reducing overall model accuracy.
#' @param max_pop_size_parallel
#' Defaults to 10000. OPTIONAL. ONLY USED if foreach and doParallel are available
#' Applies as last step: additional deletion to contain population sizes after merging.
#' @param use_gpu
#' Defaults to FALSE. OPTIONAL. ONLY USED if torch is available. Defaults to CUDA if found.
#' Otherwise goes back to CPU (and is slower than not enabling the GPU option in the first place).
#'
#' @returns
#' An \R \code{RLCS Model} containing the proposed model, made of several classifiers.
#' @export
#'
#' @examples
#' ## Generate running hyperparameters
#' demo_params <- RLCS_hyperparameters(n_epochs = 400, deletion_trigger = 40, deletion_threshold = 0.9)
#' ## One demo dataset for data mining scenario
#' demo_env1 <- rlcs_demo_secret1()
#' ## Try to see for yourself what the dataset hides:
#' demo_env1
#' ## Generate the model with RLCS:
#' rlcs_model <- rlcs_train_sl(demo_env1, demo_params)
#' print(rlcs_model)
#' plot(rlcs_model)
rlcs_train_sl_horizontal_split <- function(train_env_df, run_params = RLCS_hyperparameters(),
                                           pre_trained_lcs = NULL,
                                           n_agents = 2,
                                           split_horizontal = T, ## That is but one option!
                                           max_pop_size_parallel = 10000,
                                           use_gpu = F) {

  ## Basic input controls:
  .validate_SL_train_df(train_env_df) ## Maybe put this in a decorator?
  ## TODO Add Running Params Checks here... Use decorators!

  ## Initialization:
  lcs <- .new_rlcs(train_env_df$state[1])
  ## Re-training, or "online" updates
  if(!is.null(pre_trained_lcs)) lcs <- pre_trained_lcs

  ## For torch use:
  backup_gpu_flag <- use_gpu
  if(use_gpu & requireNamespace("torch", quietly=T)) {
    use_gpu <- use_gpu
    gpu_type <- ifelse(torch::cuda_is_available(), "cuda", ifelse(torch::backends_mps_is_available(), "mps", "cpu"))
  }

  ##
  ## Case 2: Parallel agents, each with a part of horizontal input data split.
  ##
  if(requireNamespace("foreach", quietly=T) & requireNamespace("doParallel", quietly=T) &
     n_agents > 1 & split_horizontal) { ## NEW! Parallel processing support

    `%dopar%` <- foreach::`%dopar%` ## not required anymore?

    agents <- foreach::foreach(i = 1:n_agents) %dopar% { ## Train N agents
          # , .export = c("use_gpu", "train_env_df", "n_agents", "lcs", "run_params")
      sets_size <- floor(nrow(train_env_df) / n_agents)
      sub_start <- (i-1)*sets_size+1
      sub_end <- i*sets_size

      ## Shuffling population, just in case...
      shuffle_indexes <- sample(1:nrow(train_env_df), nrow(train_env_df), replace = F)
      train_env_df <- train_env_df[shuffle_indexes, ]
      sub_df <- train_env_df[sub_start:sub_end,]

      library(RLCS) ## Assuming you've gotten the package installed by now...

      size_env <- nrow(sub_df)
      sub_lcs <- lcs ; lcs <- sub_lcs

      lcs <- rlcs_train_sl(sub_df,
                           run_params = run_params,
                           pre_trained_lcs = sub_lcs,
                           use_gpu = use_gpu)


      # if(use_gpu & requireNamespace("torch", quietly=T)) {
      #   use_gpu <- use_gpu; gpu_type <- gpu_type;
      # }
      #

      ## Sometimes, deletion removes all rules as none are good enough!
      if(is.null(lcs)) return(NULL)
      # sub_lcs <- .perfect_coverage_simplifier_sl(sub_lcs, sub_df, t_classes_counts)
      class(lcs) <- "rlcs"
      return(lcs)
    } ## End dopar

    new_lcs <- .new_rlcs(train_env_df$state[1], n_entries = 1)
    for(j in 1:length(agents)) {
      # new_lcs$conditions_length <- condition_length ## FIXED FOR ALL LCS MODEL HERE
      new_lcs$condition_strings <- c(new_lcs$condition_strings, agents[[j]]$condition_strings)
      new_lcs$actions <- c(new_lcs$actions, agents[[j]]$actions)
      new_lcs$rule_first_seens <- c(new_lcs$rule_first_seens, agents[[j]]$rule_first_seens)

      new_lcs$match_counts <- c(new_lcs$match_counts, agents[[j]]$match_counts)
      new_lcs$correct_counts <- c(new_lcs$correct_counts, agents[[j]]$correct_counts)
      new_lcs$numerosities <- c(new_lcs$numerosities, agents[[j]]$numerosities)
      new_lcs$accuracies <- c(new_lcs$accuracies, agents[[j]]$accuracies)

      ## For RL: Total Reward starts at 5, not at 0:
      new_lcs$action_counts <- c(new_lcs$action_counts, agents[[j]]$action_counts)
      new_lcs$total_rewards <- c(new_lcs$total_rewards, agents[[j]]$total_rewards)

      ## New. Found in some LCS explanations out there... Just wasn't in RLCS
      ## yet.
      new_lcs$coverage_epoch_correct_count <- c(new_lcs$coverage_epoch_correct_count, agents[[j]]$coverage_epoch_correct_count)

      ## Now add space to matching matrices
      new_lcs$matrix_conditions_vecs <- rbind(new_lcs$matrix_conditions_vecs, agents[[j]]$matrix_conditions_vecs)
      new_lcs$matrix_match_0s <- rbind(new_lcs$matrix_match_0s, agents[[j]]$matrix_match_0s)
      new_lcs$matrix_match_1s <- rbind(new_lcs$matrix_match_1s, agents[[j]]$matrix_match_1s)
      ## Faster to compare later
      new_lcs$lengths_fixed_bits <- c(new_lcs$lengths_fixed_bits, agents[[j]]$lengths_fixed_bits)

    }

    lcs <- new_lcs

    .apply_subsumption_whole_pop_sl(environment())


    .apply_deletion_sl_env(environment(),
                            # deletion_limit = run_params$get_deletion_threshold(),
                            max_pop_size = max_pop_size_parallel)

    print(length(lcs$condition_strings))

    return(lcs) ## End here
  }

  ## Fallback:
  print('Missing packages')
  return(NULL)
}


#### PARALLEL 2
#' Train a Learning Classifier System (LCS). But try to cover more search space
#' by running and merging several agents in parallel, for the same iterations.
#' EXPERIMENTAL.
#'
#' @param train_env_df
#' A data frame containing, specifically, one "state" and one
#' "class" column. The "state" column MUST contain strings made of ONLY 0 and 1,
#' such as: "00110101". This is a requirement for the current RLCS implementation.
#' @param run_params
#' An RLCS_hyperparameters object, for which an object construction is provided.
#' @param pre_trained_lcs
#' Optional. Can be used to EVOLVE a pre-trained LCS.
#' @param n_agents
#' Default is 2. If foreach and doParallel are available, a number of parallel
#' cores, as indicated PRIOR to calling RLCS like so:
#' makeCluster() registerDoParallel()
#' then RLCS will train n_agents in parallel.
#' @param use_validation
#' Default is FALSE. OPTIONAL. ONLY USED if foreach and doParallel are available.
#' When training several models in parallel, this parameter modifies selection of best
#' one by running a test in a validation set of 10 percent of samples, which is
#' first removed from the training set.
#' @param merge_best_n
#' Default is 0. OPTIONAL.ONLY USED if foreach and doParallel are available.
#' Choose to merge and compact the best n (1 < n < n_agents) of your parallelly trained
#' agents. This includes a compaction previous to returning results, but will most
#' probably return a larger population as a trade-off for expecting better accuracy.
#' @param second_evolution_iterations
#' Defaults to 1. OPTIONAL.ONLY USED if foreach and doParallel are available and
#' used. On top of the above, it will run a second "era", whereby only best agents
#' are surviving and competing again, only to then be merged.
#' @param second_evolution_run_params
#' Defaults to NULL. OPTIONAL. ONLY USED if second_evolution_iterations is bigger
#' than 1. The idea here is that after a slower, more exploratory first era, a
#' second (and more) era(s) can be used to push more generalization.
#' @param max_pop_size_parallel
#' Defaults to 10000. OPTIONAL. ONLY USED if foreach and doParallel are available
#' Applies as last step: additional deletion to contain population sizes after merging.
#' @param use_gpu
#' Defaults to FALSE. OPTIONAL. ONLY USED if torch is available. Defaults to CUDA if found.
#' Otherwise goes back to CPU (and is slower than not enabling the GPU option in the first place).
#'
#' @returns
#' An \R \code{RLCS Model} containing the proposed model, made of several classifiers.
#' @export
#'
#' @examples
#' ## Generate running hyperparameters
#' demo_params <- RLCS_hyperparameters(n_epochs = 400, deletion_trigger = 40, deletion_threshold = 0.9)
#' ## One demo dataset for data mining scenario
#' demo_env1 <- rlcs_demo_secret1()
#' ## Try to see for yourself what the dataset hides:
#' demo_env1
#' ## Generate the model with RLCS:
#' rlcs_model <- rlcs_train_sl(demo_env1, demo_params)
#' print(rlcs_model)
#' plot(rlcs_model)
rlcs_train_sl_parallel_search_space <- function(train_env_df, run_params = RLCS_hyperparameters(),
                                                 pre_trained_lcs = NULL,
                                                 n_agents = 2,
                                                 use_validation=F,
                                                 merge_best_n = 0,
                                                 second_evolution_iterations = 1,
                                                 second_evolution_run_params = NULL,
                                                 max_pop_size_parallel = 10000,
                                                 use_gpu = F) {

  ## Basic input controls:
  .validate_SL_train_df(train_env_df) ## Maybe put this in a decorator?
  ## TODO Add Running Params Checks here... Use decorators!

  ## Initialization:
  lcs <- .new_rlcs(train_env_df$state[1])
  ## Re-training, or "online" updates
  if(!is.null(pre_trained_lcs)) lcs <- pre_trained_lcs

  ## For torch use:
  backup_gpu_flag <- use_gpu
  if(use_gpu & requireNamespace("torch", quietly=T)) {
    use_gpu <- use_gpu
    gpu_type <- ifelse(torch::cuda_is_available(), "cuda", ifelse(torch::backends_mps_is_available(), "mps", "cpu"))
  }

  ##
  ## Case 3: Parallel, but not horizontal input data split.
  ##
  ## Instead, full coverage in each agent of all the data. Not faster per-se, but
  ## hopefully better coverage across agent and merging possible with fewer
  ## epochs.
  if(requireNamespace("foreach", quietly=T) & requireNamespace("doParallel", quietly=T) &
     n_agents > 1) {

    use_gpu <- backup_gpu_flag
    if(use_gpu & requireNamespace("torch", quietly=T)) {
      use_gpu <- use_gpu; gpu_type <- gpu_type;
      # use_gpu <- use_gpu
      # gpu_type <- ifelse(torch::cuda_is_available(), "cuda", ifelse(torch::backends_mps_is_available(), "mps", "cpu"))
    }

    `%dopar%` <- foreach::`%dopar%` ## not required anymore?

    for(second_evol_iter in 1:second_evolution_iterations) {

      if(second_evol_iter > 1 && !is.null(second_evolution_run_params))
        run_params <- second_evolution_run_params

      print(paste("Using foreach() %dopar% to train up to", n_agents, "parallel agents."))

      ## Shuffling population, just in case...
      # shuffle_indexes <- sample(1:nrow(train_env_df), nrow(train_env_df), replace = F)
      # train_env_df <- train_env_df[shuffle_indexes, ]

      if(use_validation) {
        validation_set <- sample(1:nrow(train_env_df), max(round(.1*nrow(train_env_df)), 1), replace = F)
        sub_train_environment <- train_env_df[-validation_set,]
      } else { sub_train_environment <- train_env_df }

      agents <- foreach::foreach(i = 1:n_agents
                                 #, .export = c("use_gpu")
                                 , .packages = c("torch")
      ) %dopar% { ## Train N agents

        t_shuffle_set <- sample(1:nrow(sub_train_environment),
                                nrow(sub_train_environment),
                                replace = F)
        sub_train_environment_shuffle <- sub_train_environment[t_shuffle_set, ]


        library(RLCS) ## Assuming you've gotten the package installed by now...

        # if(backup_gpu_flag & requireNamespace("torch", quietly=T)) {
        #   use_gpu <- NULL
        #   use_gpu <- backup_gpu_flag
        #   gpu_type <- ifelse(torch::cuda_is_available(), "cuda", ifelse(torch::backends_mps_is_available(), "mps", "cpu"))
        # }

        size_env <- nrow(sub_train_environment)
        sub_lcs <- lcs ; lcs <- sub_lcs

        lcs <- rlcs_train_sl(sub_train_environment,
                              run_params = run_params,
                              pre_trained_lcs = sub_lcs,
                              use_gpu = use_gpu)


        ## Sometimes, deletion removes all rules as none are good enough!
        if(is.null(lcs)) return(NULL)
        ##
        ## Final simplification: Coverage
        ##
        # lcs <- .perfect_coverage_simplifier_sl(lcs, sub_train_environment_shuffle, t_classes_counts)

        class(lcs) <- "rlcs"
        return(lcs)
      }
      print("Now checking agents quality")

      agents_quality <- list()
      for(j in 1:n_agents) {
        ## ADD CHECK HERE FOR !is.null(agents[[j]])
        if(!is.null(agents[[j]])) {
          ## Let's see how we could do testing:
          ## We calculate accuracy BOTH for training...
          # print(nrow(sub_train_environment))
          validation_environment <- sub_train_environment[sample(1:nrow(sub_train_environment), min(1, round(0.1*nrow(train_env_df))), replace = F),]
          validation_environment$predicted <- -1 ## Stands for not found
          validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]])

          agents_quality[[j]] <- round(sum(vapply(1:nrow(validation_environment), \(i) {
            ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
          }, numeric(1)))/nrow(validation_environment), 4)

          # print(agents_quality[[j]])
          if(use_validation) {
            ## AND validation:
            validation_environment <- train_env_df[validation_set,]
            validation_environment$predicted <- -1 ## Stands for not found
            validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]])

            agents_quality[[j]] <- (agents_quality[[j]] +
                                      round(sum(sapply(1:nrow(validation_environment), \(i) {
                                        ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
                                      }))/nrow(validation_environment), 4))/2
            # print(agents_quality[[j]])
          }
        } else
          agents_quality[[j]] <- 0
      }

      print(unlist(agents_quality))

      if((merge_best_n > 1) & (merge_best_n <= n_agents)) {
        best_agents <- order(unlist(agents_quality), decreasing = TRUE)[1:merge_best_n]
        print(best_agents)

        ## Recollect all sub-lcs
        compacted_classifier <- list()
        agents <- agents[best_agents]


        new_lcs <- .new_rlcs(train_env_df$state[1], n_entries = 1)
        for(j in 1:length(agents)) {
          # new_lcs$conditions_length <- condition_length ## FIXED FOR ALL LCS MODEL HERE
          new_lcs$condition_strings <- c(new_lcs$condition_strings, agents[[j]]$condition_strings)
          new_lcs$actions <- c(new_lcs$actions, agents[[j]]$actions)
          new_lcs$rule_first_seens <- c(new_lcs$rule_first_seens, agents[[j]]$rule_first_seens)

          new_lcs$match_counts <- c(new_lcs$match_counts, agents[[j]]$match_counts)
          new_lcs$correct_counts <- c(new_lcs$correct_counts, agents[[j]]$correct_counts)
          new_lcs$numerosities <- c(new_lcs$numerosities, agents[[j]]$numerosities)
          new_lcs$accuracies <- c(new_lcs$accuracies, agents[[j]]$accuracies)

          ## For RL: Total Reward starts at 5, not at 0:
          new_lcs$action_counts <- c(new_lcs$action_counts, agents[[j]]$action_counts)
          new_lcs$total_rewards <- c(new_lcs$total_rewards, agents[[j]]$total_rewards)

          ## New. Found in some LCS explanations out there... Just wasn't in RLCS
          ## yet.
          new_lcs$coverage_epoch_correct_count <- c(new_lcs$coverage_epoch_correct_count, agents[[j]]$coverage_epoch_correct_count)

          ## Now add space to matching matrices
          new_lcs$matrix_conditions_vecs <- rbind(new_lcs$matrix_conditions_vecs, agents[[j]]$matrix_conditions_vecs)
          new_lcs$matrix_match_0s <- rbind(new_lcs$matrix_match_0s, agents[[j]]$matrix_match_0s)
          new_lcs$matrix_match_1s <- rbind(new_lcs$matrix_match_1s, agents[[j]]$matrix_match_1s)
          ## Faster to compare later
          new_lcs$lengths_fixed_bits <- c(new_lcs$lengths_fixed_bits, agents[[j]]$lengths_fixed_bits)

        }

        lcs <- new_lcs
      } else {
        best_agent <- order(unlist(agents_quality), decreasing = TRUE)[1]
        print(best_agent)
        lcs <- agents[[best_agent]]
      }
    }

    .apply_subsumption_whole_pop_sl(environment())

    .apply_deletion_sl_env(environment(),
                            # deletion_limit = run_params$get_deletion_threshold(),
                            max_pop_size = max_pop_size_parallel)

    print(length(lcs$condition_strings))

    return(lcs) ## End here
  }

  ## Fallback:
  print('Missing packages')
  return(NULL)
}

####----

## Working on new version to make things somehow faster, although this will
## require some validation for sure...


####----
