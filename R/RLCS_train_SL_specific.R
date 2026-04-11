## Supervised Learning-specific versions of TRAINING functions

## Note:
## Slowly replacing functions with hidden functions (.<name>)...

.inc_correct_count <- .inc_param_count("correct_count") ## Factory
.inc_correct_count_env <- function(env) {
  inc_param_count_cpp(env$correct_pop, "correct_count")
}
.mean_correct_count <- function(pop) {
  # sum(sapply(pop, \(x) x$correct_count)) / length(pop)
  sum(vapply(pop, \(x) x$correct_count, numeric(1))) / length(pop)
}

.mean_match_count <- function(pop) {
  # experienced_classifiers <- which(
  #   sapply(pop, \(x) {
  #     if(x$match_count > 5)
  #       return(T)
  #     return(F)
  #   })
  # )
  # sum(sapply(pop[experienced_classifiers], \(x) x$match_count)) / length(pop[experienced_classifiers])
  # sum(sapply(pop, \(x) x$match_count)) / length(pop)
  sum(vapply(pop, \(x) x$match_count, numeric(1))) / length(pop)
}

.min_correct_count <- function(pop) {
  min(vapply(pop, \(x) x$correct_count, numeric(1)))
}

# .min_correct_count_env <- function(env) {
  # min(vapply(env$correct_pop, \(x) x$correct_count, numeric(1)))
# }
# .min_correct_count_env <- function(env) {
#   min(vapply(env$correct_pop, \(x) x[[7]], numeric(1)))
# }
.min_correct_count_env <- function(env) {
  min_param_count_cpp(env$correct_pop, "correct_count")
}


.min_match_count <- function(pop) {
  min(vapply(pop, \(x) x$match_count, numeric(1)))
}

.get_correct_set <- function(t_instance, match_pop) {
  if(!is.null(match_pop) & (length(match_pop) > 0)) {
    # ti_class <- t_instance$class
    #
    # correct_set <- which(vapply(match_pop, \(item) {
    #   ti_class == item$action
    # }, logical(1)))

    # browser()
    correct_set <- get_correct_set_cpp(match_pop, as.character(t_instance$class))
    if(length(correct_set) > 0) return(correct_set)
  }
  NULL ## implicit return
}

.get_correct_set_env <- function(t_instance_class, env, match_set) {
  # browser()
  if(length(match_set) > 0) {

    correct_set <- which(env$lcs$actions_vec[match_set] == t_instance_class)
    if(length(correct_set) > 0) return(correct_set)
  }
  NULL ## implicit return
}


## KEY function:
## Classifiers are better or worse. CHOOSING THE BEST ones is important
## For SL, accuracy is top priority, followed by generality
## This is a faster approach to calculation although arguably could be discussed
.lcs_best_sort_sl <- function(pop) {
  pop <- unclass(pop)
  if(length(pop) == 0) return(NULL)

  ranking <- vapply(pop, \(x) {
    # x$accuracy + 0.01 * (1 - (length(x$condition_list$"0")+length(x$condition_list$"1")) / x$condition_length)
    # x$accuracy - 0.01 * (length(x$condition_list$"0")+length(x$condition_list$"1")) / x$condition_length
    x$accuracy - 0.01 * x$length_fixed_bits / x$condition_length

  }, numeric(1)) ## For same accuracy, more general rules will be preferred.
  pop[order(ranking, decreasing=T)]
}

.lcs_best_sort_sl_env <- function(env) {
  env$lcs$pop <- unclass(env$lcs$pop)
  if(length(env$lcs$pop) == 0) return(NULL)

  ranking <- vapply(env$lcs$pop, \(x) {
    # x$accuracy + 0.01 * (1 - (length(x$condition_list$"0")+length(x$condition_list$"1")) / x$condition_length)
    # x$accuracy - 0.01 * (length(x$condition_list$"0")+length(x$condition_list$"1")) / x$condition_length
    x$accuracy - 0.01 * x$length_fixed_bits / x$condition_length
  }, numeric(1)) ## For same accuracy, more general rules will be preferred.
  env$lcs$pop <- env$lcs$pop[order(ranking, decreasing=T)]
  NULL
}


## Another key function here.
.apply_subsumption_whole_pop_sl <- function(pop) {

  if(length(pop) <= 1) return(pop)

  # print(length(pop))

  pop <- .lcs_best_sort_sl(pop)

  # .lcs_best_sort_sl_env(environment())
  pop <- .apply_deletion_no_threshold(pop)
  if(length(pop) <= 1) return(pop)

  # print(length(pop))

  ## Within this function in this case :)
  t_matrices <- .recalculate_pop_matrices(pop)

  # print(length(pop))
  # print(nrow(t_matrices))

  # browser()
  t_labs <- sapply(pop, \(x) { x$action })

  pop_size <- length(pop)
  subsumers_list <- lapply(1:(pop_size-1), \(item) {

    # if(pop[[item]]$numerosity > 0) { ## Now superfluous in vectorized approach

    # t_zero <- pop[[item]]$condition_list$"0"
    # t_one <- pop[[item]]$condition_list$"1"
    # correct_length_values_set <- length(pop[[item]]$condition_list$"0") +
    #   length(pop[[item]]$condition_list$"1")

    correct_length_values_set <- pop[[item]]$length_fixed_bits
    # cond_string <- pop[[item]]$condition_string
    # cond_lab <- pop[[item]]$action

    # pop_to_delete <- NULL

    # # ## Alternative approach now, let's see!

    rest_t_matrices_zeros <- t_matrices[[1]][(item+1):pop_size,]
    rest_t_matrices_ones <- t_matrices[[2]][(item+1):pop_size,]
    ## Remember the rules are sorted by accuracy and generality already!
    subsumer_must_match_zeros <- rest_t_matrices_zeros  %*% t_matrices[[1]][item,]
    subsumer_must_match_ones <- rest_t_matrices_ones  %*% t_matrices[[2]][item,]

    subsumed_must_be_different_zero <-  rest_t_matrices_zeros %*% t_matrices[[2]][item,]
    subsumed_must_be_different_one <- rest_t_matrices_ones %*% t_matrices[[1]][item,]

    pop_to_delete <- which(
      ((subsumer_must_match_zeros+subsumer_must_match_ones) == correct_length_values_set) &
        (subsumed_must_be_different_zero+subsumed_must_be_different_one == 0) &
        (t_labs[(item+1):pop_size] == t_labs[item])
    )

    if(!is.null(pop_to_delete) && length(pop_to_delete) > 0) {
      pop_to_delete <- pop_to_delete + item ## Optimization. POSITIONS
      return(pop_to_delete)
    }

    return(c()) ## Default: Nothing to delete
  })

  # print(length(pop))

  ## Update numerosity for each subsumer
  ## Before vectorization
  # for(i in 1:length(subsumers_list)) {
  #   pop[[i]]$numerosity <- pop[[i]]$numerosity+length(subsumers_list[[i]])
  # } ## Probably can be vectorized, this too...
  ## Vectorized version
  subsumers_positions <- which(sapply(subsumers_list, \(x) !is.null(x)))
  pop[subsumers_positions] <- lapply(subsumers_positions, \(i) {
    pop[[i]]$numerosity <- pop[[i]]$numerosity + length(subsumers_list[[i]])
    pop[[i]]
  }) ## Go this just once!

  # print(length(pop))

  pop_to_delete <- unique(unlist(subsumers_list)) ## Reduce operations
  pop[pop_to_delete] <- lapply(pop[pop_to_delete], \(item_to_clean) {
    item_to_clean$numerosity <- 0
    item_to_clean
  }) ## Go this just once!

  # print(length(pop))

  pop <- .apply_deletion_no_threshold(pop)

  # print(length(pop))

  pop
}


.apply_subsumption_whole_pop_sl_env <- function(env) {


  browser()
  if(length(env$lcs$pop) > 1) {
    # pop <- .lcs_best_sort_sl(pop)
    .lcs_best_sort_sl_env(env)
    .apply_deletion_no_threshold_env(env)

    ## Within this function in this case :)
    t_matrices <- .recalculate_pop_matrices(env$lcs$pop)

    # browser()
    t_labs <- sapply(env$lcs$pop, \(x) { x$action })
    subsumers_list <- lapply(1:(length(env$lcs$pop)-1), \(item) {

      # if(pop[[item]]$numerosity > 0) { ## Now superfluous in vectorized approach

      t_zero <- env$lcs$pop[[item]]$condition_list$"0"
      t_one <- env$lcs$pop[[item]]$condition_list$"1"
      # cond_string <- pop[[item]]$condition_string
      # cond_lab <- pop[[item]]$action

      pop_to_delete <- NULL

      # # ## Alternative approach now, let's see!


      ## Remember the rules are sorted by accuracy and generality already!
      subsumer_must_match_zeros <- t_matrices[[1]][(item+1):length(env$lcs$pop),]  %*% t_matrices[[1]][item,]
      subsumer_must_match_ones <- t_matrices[[2]][(item+1):length(env$lcs$pop),]  %*% t_matrices[[2]][item,]

      subsumed_must_be_different_zero <-  t_matrices[[1]][(item+1):length(env$lcs$pop),] %*% t_matrices[[2]][item,]
      subsumed_must_be_different_one <- t_matrices[[2]][(item+1):length(env$lcs$pop),] %*% t_matrices[[1]][item,]

      pop_to_delete <- which(
        ((subsumer_must_match_zeros+subsumer_must_match_ones) == (length(t_zero)+length(t_one))) &
          (subsumed_must_be_different_zero+subsumed_must_be_different_one == 0) &
          (t_labs[(item+1):length(env$lcs$pop)] == t_labs[item])
      )

      if(!is.null(pop_to_delete) && length(pop_to_delete) > 0) {
        pop_to_delete <- pop_to_delete + item ## Optimization. POSITIONS
        return(pop_to_delete)
      }

      return(c()) ## Default: Nothing to delete
    })

    ## Update numerosity for each subsumer
    ## Before vectorization
    # for(i in 1:length(subsumers_list)) {
    #   pop[[i]]$numerosity <- pop[[i]]$numerosity+length(subsumers_list[[i]])
    # } ## Probably can be vectorized, this too...
    ## Vectorized version
    subsumers_positions <- which(sapply(subsumers_list, \(x) !is.null(x)))
    pop[subsumers_positions] <- lapply(subsumers_positions, \(i) {
      env$lcs$pop[[i]]$numerosity <- env$lcs$pop[[i]]$numerosity + length(subsumers_list[[i]])
      env$lcs$pop[[i]]
    }) ## Go this just once!

    pop_to_delete <- unique(unlist(subsumers_list)) ## Reduce operations
    env$lcs$pop[pop_to_delete] <- lapply(env$lcs$pop[pop_to_delete], \(item_to_clean) {
      item_to_clean$numerosity <- 0
      item_to_clean
    }) ## Go this just once!

    env$lcs$pop <- .apply_deletion_no_threshold(env$lcs$pop)
  }

  # pop
}


.apply_deletion_sl <- function(lcs, deletion_limit = 0.6, max_pop_size = 10000) {

  lcs$pop <- lapply(lcs$pop, \(x) {
    if(x$accuracy < deletion_limit) x$numerosity <- 0
    x
  })

  if(length(lcs$pop) > max_pop_size) {
    lcs$pop <- .lcs_best_sort_sl(lcs$pop)
    lcs$pop[max_pop_size:length(lcs$pop)] <- lapply(lcs$pop[max_pop_size:length(lcs$pop)], \(x) {
      x$numerosity <- 0
      x
    })
  }

  lcs$pop <- .apply_deletion_no_threshold(lcs$pop)
  lcs$matrices <- .recalculate_pop_matrices(lcs$pop)
  lcs$lengths <- .lengths_fixed_bits(lcs$pop)
  lcs$actions_vec <- .recalculate_actions_vec(lcs$pop)

  lcs
}


######
## KEY FUNCTION: Train binary classifier LCS
######
.rlcs_train_one_instance_one_epoch_mat <- function(lcs,
                                               t_instance,
                                               size_env, ## Used for Subsumption Freq.
                                               n_epoch, ## Used for Subsumption Freq.
                                               train_count, ## train_count
                                               run_params ## Algorithm Hyperparameters
) {
  ######
  ## Main process for R LCS Training
  ######
  # list(pop = list(), matrices = list(), lengths = 0)

  ## ADD ERROR CONTROL
  match_set <- .get_match_set_mat(t_instance$state, lcs)
  # browser()
  if(is.null(match_set) || length(match_set) == 0) { ## COVERING needed
    cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance$state,
                                                              run_params$get_wildcard_prob())

    if(!is.null(cover_rule)) {
      # browser()
      lcs <- .add_valid_rule_to_lcs(lcs, cover_rule,
                                    t_instance$class, train_count)

      # print("no match, added rule")
      # print(lcs$pop)
      return(lcs)
    }
  } else {
    # print("Matched")
    ## Faster to work with only match population until need to review overall population
    match_pop <- .inc_match_count(lcs$pop[c(match_set)])

    correct_set <- .get_correct_set(t_instance, match_pop)
    if(is.null(correct_set) || length(correct_set) == 0) { ## COVERING needed
      cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance$state,
                                                                run_params$get_wildcard_prob())
      if(!is.null(cover_rule)) {
        lcs <- .add_valid_rule_to_lcs(
          lcs, cover_rule, t_instance$class, train_count)
      }
    } else {
      correct_pop <- match_pop[c(correct_set)]
      match_pop[c(correct_set)] <- .inc_correct_count(correct_pop)

      ## *Second* Rule Discovery HAPPENS HERE NOW
      ## Rule discovery happens only AFTER A RULE HAS HAD SOME TIME
      # if(round(.mean_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
      # if(round(.mean_match_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
      if((.min_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
      # if((.min_match_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {

        ## The GA, basically, happens here: Cross-over & Mutation:
        # print("Triggered Mutation")
        children <- correct_pop |>
          .cross_over_parents_strings_sl(run_params$get_sel_mode(),
                                         run_params$get_tournament_pressure()) |>
          sapply(.mutate_condition_string, t_instance$state, run_params$get_mut_prob())

        ## In some cases, we have only one child.
        for(child in children) {
          if(.found_same_condition(correct_pop, child)) ## Duplicate rule
            match_pop <- .inc_numerosity_by_condition(match_pop, child)
          else {
            lcs <- .add_valid_rule_to_lcs(lcs, child, t_instance$class, train_count)
          }

        }
      }
    }

    ## Update Matched Population statistics into main population
    lcs$pop[c(match_set)] <- .update_matched_accuracy(match_pop)

    # ## NEW: More rule-discovery
    # ## OK, matched, correct set, but what if there is not enough correctset?
    # if(length(correct_set) < 2) { ## COVERING enforced
    #   cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance$state,
    #                                                             run_params$get_wildcard_prob())
    #   if(!is.null(cover_rule)) {
    #     lcs <- .add_valid_rule_to_lcs(lcs, cover_rule,
    #                                   t_instance$class, train_count)
    #   }
    # }
  }

  ## Apply Deletion by reducing numerosity
  # if((train_count %% (run_params$get_deletion_trigger()*size_env)) == 0) {
  if((train_count %% (run_params$deletion_trigger*size_env)) == 0) {
    ## Subsumption is too important to skip, for speed reasons.
    lcs$pop <- .apply_subsumption_whole_pop_sl(lcs$pop)

    lcs <- .apply_deletion_sl(lcs,
                              deletion_limit = run_params$get_deletion_threshold(),
                              max_pop_size = run_params$get_max_pop_size())

    print(paste("Epoch:", n_epoch,
                "Progress Exposure:", train_count,
                "Classifiers Count:", length(lcs$pop)))
  }

  lcs
}

.validate_SL_train_df <- function(train_env_df) {
  if(!("state" %in% names(train_env_df))) stop("Input Data Frame must contain a 'state' column.")
  if(!("class" %in% names(train_env_df))) stop("Input Data Frame must contain a 'class' column.")
  if(!all(vapply(train_env_df$state, .validate_state_string, logical(1)))) stop("SL: Training environment, wrong state found. STOP.")
}

.perfect_coverage_simplifier_sl <- function(lcs, train_env_df, t_classes_counts) {
  # print(as.data.frame(t_classes_counts))
  pop_actions <- sapply(lcs$pop, \(x) x$action)
  # print(table(pop_actions))
  # print(length(t_classes_counts))
  if(length(unique(pop_actions)) == length(t_classes_counts)) {
    t_df <- data.frame(match_sizes = .reverse_match_set_size(lcs$pop, train_env_df),
                       match_class = pop_actions,
                       rule_id = 1:length(lcs$pop))
    t_df <- merge(t_df, as.data.frame(t_classes_counts), by.x = "match_class", by.y = "Var1")
    t_df <- t_df[order(t_df$rule_id),]
    # print(head(t_df, 10))

    if(nrow(t_df) > 1) {
      for(i in 1:(nrow(t_df)-1)) {

        if(t_df$match_sizes[i] == t_df$Freq[i]) { ## Full Coverage!
          ## Then delete all entries that have the same match_class, as they are useless
          to_remove <- which(t_df[i+1:nrow(t_df), "match_class"] == t_df$match_class[i])
          lcs$pop[to_remove] <- lapply(lcs$pop[to_remove], \(item) {
            item$numerosity <- 0
            item
          })
        }
      }
      lcs <- .apply_deletion_sl(lcs)
      # pop_actions <- sapply(lcs$pop, \(x) x$action)
      # print(table(pop_actions))
    }
  }

  lcs
}

.apply_deletion_sl_env <- function(env, deletion_limit = 0.6, max_pop_size = 10000) {

  env$lcs$pop <- lapply(env$lcs$pop, \(x) {
    if(x$accuracy < deletion_limit) x$numerosity <- 0
    x
  })

  if(length(env$lcs$pop) > max_pop_size) {
    #env$lcs$pop <-
    .lcs_best_sort_sl_env(env)
    env$lcs$pop[max_pop_size:length(env$lcs$pop)] <- lapply(env$lcs$pop[max_pop_size:length(env$lcs$pop)], \(x) {
      x$numerosity <- 0
      x
    })
  }

  #env$lcs$pop <- .apply_deletion_no_threshold(env$lcs$pop)
  .apply_deletion_no_threshold_env(env)
  env$lcs$matrices <- .recalculate_pop_matrices(env$lcs$pop)
  env$lcs$lengths <- .lengths_fixed_bits(env$lcs$pop)
  env$lcs$actions_vec <- .recalculate_actions_vec(env$lcs$pop)

  NULL
}

.rlcs_train_one_instance_one_epoch_mat_env <- function(env,
                                                   t_instance,
                                                   size_env, ## Used for Subsumption Freq.
                                                   n_epoch, ## Used for Subsumption Freq.
                                                   train_count, ## train_count
                                                   run_params ## Algorithm Hyperparameters
) {
  ######
  ## Main process for R LCS Training
  ######
  # list(pop = list(), matrices = list(), lengths = 0)

  ## ADD ERROR CONTROL
  t_instance_state <- t_instance$state
  t_instance_vec <- as.integer(strsplit(t_instance_state, "", fixed = T)[[1]])

  # match_set <- .get_match_set_mat(t_instance_state, env$lcs)
  # match_set <- .get_match_set_mat_env(t_instance_state, env)
  match_set <- .get_match_set_mat_env2(t_instance_vec, env)
  # browser()
  if(is.null(match_set) || length(match_set) == 0) { ## COVERING needed



    for(iter in 1:5) { ## Testing more covering for new niches?

      cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance_state,
                                                              run_params$get_wildcard_prob())

      if(!is.null(cover_rule)) {
        # browser()
        #env$lcs <-
        .add_valid_rule_to_lcs_env(env, cover_rule,
                                   t_instance$class, train_count)

        # print("no match, added rule")
        # print(lcs$pop)

      }
      return(NULL)
    } ## END TESTING MORE COVERING FOR NEW NICHES
  } else {
    # print("Matched")
    ## Faster to work with only match population until need to review overall population
    # match_pop <- .inc_match_count(env$lcs$pop[c(match_set)])
    # match_pop <- .inc_match_count_env(env, match_set)
    match_pop <- env$lcs$pop[c(match_set)]
    match_pop <- .inc_match_count_env(environment())

    # correct_set <- .get_correct_set(t_instance, match_pop)
    correct_set <- .get_correct_set_env(t_instance$class, env, match_set)

    if(is.null(correct_set) || length(correct_set) == 0) { ## COVERING needed
      cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance_state,
                                                                run_params$get_wildcard_prob())
      if(!is.null(cover_rule)) {
        #env$lcs <-
        .add_valid_rule_to_lcs_env(env, cover_rule, t_instance$class, train_count)
      }
    }
    else {
      correct_pop <- match_pop[c(correct_set)]
      # match_pop[c(correct_set)] <- .inc_correct_count(correct_pop)
      # correct_pop <- .inc_correct_count(correct_pop)
      correct_pop <- .inc_correct_count_env(environment())

      ## *Second* Rule Discovery HAPPENS HERE NOW
      ## Rule discovery happens only AFTER A RULE HAS HAD SOME TIME
      # if(round(.mean_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
      # if(round(.mean_match_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {

      # if((.min_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
      # if((.min_correct_count(correct_pop) %% run_params$rd_trigger) == 0) {
      if((.min_correct_count_env(environment()) %% run_params$rd_trigger) == 0) {
        # if((.min_match_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {

        ## The GA, basically, happens here: Cross-over & Mutation:
        # print("Triggered Mutation")
        children <- correct_pop |>
          .cross_over_parents_strings_sl(run_params$get_sel_mode(),
                                         run_params$get_tournament_pressure()) |>
          sapply(.mutate_condition_string, t_instance_state, run_params$get_mut_prob())

        ## In some cases, we have only one child.
        for(child in children) {
          if(.found_same_condition(correct_pop, child)) ## Duplicate rule
            # match_pop <- .inc_numerosity_by_condition(match_pop, child)
            correct_pop <- .inc_numerosity(correct_pop)
          else {
            #env$lcs <-
            .add_valid_rule_to_lcs_env(env, child, t_instance$class, train_count)
          }
        }
      }
      match_pop[c(correct_set)] <- correct_pop
    }

    ## Update Matched Population statistics into main population
    # env$lcs$pop[c(match_set)] <- .update_matched_accuracy(match_pop)
    .update_matched_accuracy_env(environment())
    env$lcs$pop[c(match_set)] <- match_pop
    # ## NEW: More rule-discovery
    # ## OK, matched, correct set, but what if there is not enough correctset?
    # if(length(correct_set) < 2) { ## COVERING enforced
    #   cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance_state,
    #                                                             run_params$get_wildcard_prob())
    #   if(!is.null(cover_rule)) {
    #     lcs <- .add_valid_rule_to_lcs(lcs, cover_rule,
    #                                   t_instance$class, train_count)
    #   }
    # }
  }

  ## Apply Deletion by reducing numerosity
  # if((train_count %% (run_params$get_deletion_trigger()*size_env)) == 0) {
  if((train_count %% (run_params$deletion_trigger*size_env)) == 0) {
    ## Subsumption is too important to skip, for speed reasons.
    env$lcs$pop <- .apply_subsumption_whole_pop_sl(env$lcs$pop)

    #env$lcs <-
    .apply_deletion_sl_env(env,
                           deletion_limit = run_params$get_deletion_threshold(),
                           max_pop_size = run_params$get_max_pop_size())

    # print(paste("Epoch:", n_epoch,
    #             "Progress Exposure:", train_count,
    #             "Classifiers Count:", length(env$lcs$pop)))
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
#' @param verbose
#' Default is FALSE. I strongly suggest you do not use that parameter.
#' @param n_agents
#' Default is 0. OPTIONAL. ONLY USED if foreach and doParallel are available.
#' IF available, a number of parallel cores, as indicated PRIOR to calling RLCS
#' like so: makeCluster() registerDoParallel()
#' then RLCS will train n_agents in parallel.
#' @param split_horizontal
#' Defaults to FALSE. OPTIONAL. ONLY USED if foreach and doParallel are available.
#' WARNING: If used, EXCLUDES OTHER Parallelizing options!!
#' Splits evenly across N agents (N number of cores/threads) the input dataset.
#' Then trains N agents, and then merges the resulting data.
#' This can potentially speed-up the process, but will probably over-fit for each
#' subset, hence probably reducing overall model accuracy.
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
rlcs_train_sl <- function(train_env_df, run_params = RLCS_hyperparameters(),
                          pre_trained_lcs = NULL, verbose=FALSE,
                          n_agents = 0,
                          split_horizontal = F, ## That is but one option!
                          use_validation=F,
                          merge_best_n = 0,
                          second_evolution_iterations = 1,
                          second_evolution_run_params = NULL,
                          max_pop_size_parallel = 10000) {
  ## Initialization:
  lcs <- .new_rlcs()
  .validate_SL_train_df(train_env_df)

  ## TODO Add Running Params Checks here...
  ## Further testing will surface issues here.

  ## Re-training, or "online" updates
  if(!is.null(pre_trained_lcs)) lcs <- pre_trained_lcs

  ## NEW
  lcs$matrices <- .recalculate_pop_matrices(lcs$pop) ## Poor naming...
  # lcs$lengths <- sapply(lcs$pop, \(x) x$length_fixed_bits) ## Poor naming...
  lcs$lengths <- vapply(lcs$pop, \(x) x$length_fixed_bits, numeric(1)) ## Poor naming...
  lcs$actions_vec <- .recalculate_actions_vec(lcs$pop)

  ## Shuffling population, just in case...
  train_env_df <- train_env_df[sample(1:nrow(train_env_df),
                                      nrow(train_env_df),
                                      replace = F), ]

  if(requireNamespace("foreach", quietly=T) &
     requireNamespace("doParallel", quietly=T) &
     n_agents > 1) { ## NEW!

    if(split_horizontal) {

      agents <- foreach::foreach(i = 1:n_agents) %dopar% { ## Train N agents

        sets_size <- floor(nrow(train_env_df) / n_agents)
        sub_start <- (i-1)*sets_size+1
        sub_end <- i*sets_size
        sub_df <- train_env_df[sub_start:sub_end,]
        t_classes_counts <- table(sub_df$class) ## For Coverage!!

        library(RLCS) ## Assuming you've gotten the package installed by now...

        size_env <- nrow(sub_df)
        sub_lcs <- lcs
        lcs <- sub_lcs
        # sub_lcs <- lcs

        for(epoch in 1:(run_params$get_n_epochs())) {
          for(i in 1:size_env) {
            #sub_lcs <-
            .rlcs_train_one_instance_one_epoch_mat_env(environment(),
                                                              sub_df[i, ],
                                                              size_env,
                                                              epoch,
                                                              (epoch-1)*size_env+i, ## train_count
                                                              run_params)
          }

        }

        ## Sometimes, deletion removes all rules as none are good enough!
        if(is.null(lcs)) return(NULL)
        # sub_lcs <- .perfect_coverage_simplifier_sl(sub_lcs, sub_df, t_classes_counts)
        class(lcs) <- "rlcs"
        return(lcs)
      }

      for(j in 1:length(agents)) {
        print(paste("agent set", j, "length", length(agents[[j]]$pop)))
        for(i in 1:length(agents[[j]]$pop)) {
          lcs$pop[[length(lcs$pop)+1]] <- agents[[j]]$pop[[i]]
        }
      }

      lcs$pop <- .apply_subsumption_whole_pop_sl(lcs$pop)
      # .apply_subsumption_whole_pop_sl_env(environment())
      print(length(lcs$pop))
      lcs <- .apply_deletion_sl(lcs, max_pop_size = max_pop_size_parallel)
      # lcs <- .perfect_coverage_simplifier_sl(lcs, train_env_df, t_classes_counts)
      print(length(lcs$pop))
      # lcs <- .apply_deletion_sl(lcs, max_pop_size = max_pop_size_parallel)
      # return(lcs)
    } else {
      for(second_evol_iter in 1:second_evolution_iterations) {

        if(second_evol_iter > 1 && !is.null(second_evolution_run_params))
          run_params <- second_evolution_run_params

        print(paste("Using foreach() %dopar% to train up to", n_agents, "parallel agents."))
        `%dopar%` <- foreach::`%dopar%`

        # browser()
        if(use_validation) {
          validation_set <- sample(1:nrow(train_env_df), max(round(.1*nrow(train_env_df)), 1), replace = F)
          sub_train_environment <- train_env_df[-validation_set,]
        } else {
          sub_train_environment <- train_env_df
        }

        agents <- foreach::foreach(i = 1:n_agents) %dopar% { ## Train N agents

          t_shuffle_set <- sample(1:nrow(sub_train_environment),
                                  nrow(sub_train_environment),
                                  replace = F)
          sub_train_environment_shuffle <- sub_train_environment[t_shuffle_set, ]
          # t_classes_counts <- table(sub_train_environment_shuffle$class) ## For Coverage!!
          library(RLCS) ## Assuming you've gotten the package installed by now...

          size_env <- nrow(sub_train_environment)
          sub_lcs <- lcs
          lcs <- sub_lcs

          for(epoch in 1:(run_params$get_n_epochs())) {
            for(i in 1:size_env) {
              #lcs <-
              .rlcs_train_one_instance_one_epoch_mat_env(environment(),
                                                         sub_train_environment_shuffle[i, ],
                                                         size_env,
                                                         epoch,
                                                         (epoch-1)*size_env+i, ## train_count
                                                         run_params)
            }


          }
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
            validation_environment <- sub_train_environment[sample(1:nrow(sub_train_environment), min(1, round(0.1*nrow(train_env_df))), replace = F),]
            validation_environment$predicted <- -1 ## Stands for not found
            validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]])

            agents_quality[[j]] <- round(sum(vapply(1:nrow(validation_environment), \(i) {
              ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
            }, numeric(1)))/nrow(validation_environment), 4)

            if(use_validation) {
              ## AND validation:
              validation_environment <- train_env_df[validation_set,]
              validation_environment$predicted <- -1 ## Stands for not found
              validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]])

              agents_quality[[j]] <- (agents_quality[[j]] +
                                        round(sum(sapply(1:nrow(validation_environment), \(i) {
                                          ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
                                        }))/nrow(validation_environment), 4))/2
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
          # t_start_1 <- Sys.time()
          for(j in 1:length(agents)) {
            for(k in 1:length(agents[[j]]$pop)) {
              compacted_classifier[[length(compacted_classifier)+1]] <- agents[[j]]$pop[[k]]
            }
          }

          compacted_classifier <- .apply_subsumption_whole_pop_sl(compacted_classifier)
          lcs$pop <- compacted_classifier
          lcs$matrices <- .recalculate_pop_matrices(lcs$pop) ## Poor naming...
          lcs$lengths <- vapply(lcs$pop, \(x) x$length_fixed_bits, numeric(1)) ## Poor naming...
          lcs$actions_vec <- .recalculate_actions_vec(lcs$pop)
        } else {
          best_agent <- order(unlist(agents_quality), decreasing = TRUE)[1]
          print(best_agent)
          lcs <- agents[[best_agent]]
        }

        # lcs <- .perfect_coverage_simplifier_sl(lcs, train_env_df, t_classes_counts)
      }
    }

    # lcs <- .apply_deletion_sl(lcs, max_pop_size = max_pop_size_parallel)
    # return(lcs)
  } else {
    print("Running single-core/thread, sequential")
    size_env <- nrow(train_env_df)
    t_classes_counts <- table(train_env_df$class) ## For Coverage!!

    ## Expose algorithm to training set:
    for(epoch in 1:(run_params$get_n_epochs())) {

      for(i in 1:size_env) {
        ## Now this part of the algorithm is "necessarily" sequential...
        #lcs <-
        .rlcs_train_one_instance_one_epoch_mat_env(environment(),
                                                   train_env_df[i, ],
                                                   size_env,
                                                   epoch,
                                                   (epoch-1)*size_env+i, ## train_count
                                                   run_params)
        if(verbose) { ## Truly not recommended!
          if(!is.null(lcs))
            class(lcs) <- "rlcs"
          message(print(lcs))
        }
      }
      cat('\r', paste("Complete:", round(100*epoch/run_params$get_n_epochs()), "%",
                      "| Epoch:", epoch,
                      "Progress Exposure:", (epoch)*size_env,
                      "Classifiers Count:", length(lcs$pop), "   "
                      ))
    }
    ##
    ## Final simplification: Coverage
    ##
    lcs <- .perfect_coverage_simplifier_sl(lcs, train_env_df, t_classes_counts)
  }

  ## Sometimes, deletion removes all rules as none are good enough!
  if(is.null(lcs$pop)) return(NULL)
  class(lcs) <- "rlcs"

  print('')
  lcs
}



.rlcs_train_sl_old <- function(train_env_df, run_params = RLCS_hyperparameters(),
                              pre_trained_lcs = NULL, verbose=FALSE,
                              n_agents = 0,
                              split_horizontal = F, ## That is but one option!
                              use_validation=F,
                              merge_best_n = 0,
                              second_evolution_iterations = 1,
                              second_evolution_run_params = NULL,
                              max_pop_size_parallel = 10000) {
  ## Initialization:
  lcs <- .new_rlcs()
  .validate_SL_train_df(train_env_df)

  ## TODO Add Running Params Checks here...
  ## Further testing will surface issues here.

  ## Re-training, or "online" updates
  if(!is.null(pre_trained_lcs)) lcs <- pre_trained_lcs

  ## NEW
  lcs$matrices <- .recalculate_pop_matrices(lcs$pop) ## Poor naming...
  # lcs$lengths <- sapply(lcs$pop, \(x) x$length_fixed_bits) ## Poor naming...
  lcs$lengths <- vapply(lcs$pop, \(x) x$length_fixed_bits, integer(1)) ## Poor naming...
  lcs$actions_vec <- .recalculate_actions_vec(lcs$pop)

  ## Shuffling population, just in case...
  train_env_df <- train_env_df[sample(1:nrow(train_env_df),
                                      nrow(train_env_df),
                                      replace = F), ]

  t_classes_counts <- table(train_env_df$class) ## For Coverage!!

  if(requireNamespace("foreach", quietly=T) &
     requireNamespace("doParallel", quietly=T) &
     n_agents > 1) { ## NEW!

    if(split_horizontal) {

      agents <- foreach::foreach(i = 1:n_agents) %dopar% { ## Train N agents

        sets_size <- floor(nrow(train_env_df) / n_agents)
        sub_start <- (i-1)*sets_size+1
        sub_end <- i*sets_size
        sub_df <- train_env_df[sub_start:sub_end,]
        t_classes_counts <- table(sub_df$class) ## For Coverage!!

        library(RLCS) ## Assuming you've gotten the package installed by now...

        size_env <- nrow(sub_df)

        sub_lcs <- lcs

        for(epoch in 1:(run_params$get_n_epochs())) {
          for(i in 1:size_env) {
            sub_lcs <- .rlcs_train_one_instance_one_epoch_mat(sub_lcs,
                                                              sub_df[i, ],
                                                              size_env,
                                                              epoch,
                                                              (epoch-1)*size_env+i, ## train_count
                                                              run_params)
          }
        }

        ## Sometimes, deletion removes all rules as none are good enough!
        if(is.null(sub_lcs)) return(NULL)
        # sub_lcs <- .perfect_coverage_simplifier_sl(sub_lcs, sub_df, t_classes_counts)
        class(sub_lcs) <- "rlcs"
        return(sub_lcs)
      }

      for(j in 1:length(agents)) {
        print(paste("agent set", j, "length", length(agents[[j]]$pop)))
        for(i in 1:length(agents[[j]]$pop)) {
          lcs$pop[[length(lcs$pop)+1]] <- agents[[j]]$pop[[i]]
        }
      }

      lcs$pop <- .apply_subsumption_whole_pop_sl(lcs$pop)
      lcs <- .apply_deletion_sl(lcs, max_pop_size = max_pop_size_parallel)
      # lcs <- .perfect_coverage_simplifier_sl(lcs, train_env_df, t_classes_counts)

      print(length(lcs$pop))
      # lcs <- .apply_deletion_sl(lcs, max_pop_size = max_pop_size_parallel)
      # return(lcs)
    } else {
      for(second_evol_iter in 1:second_evolution_iterations) {

        if(second_evol_iter > 1 && !is.null(second_evolution_run_params))
          run_params <- second_evolution_run_params

        print(paste("Using foreach() %dopar% to train up to", n_agents, "parallel agents."))
        `%dopar%` <- foreach::`%dopar%`

        # browser()
        if(use_validation) {
          validation_set <- sample(1:nrow(train_env_df), max(round(.1*nrow(train_env_df)), 1), replace = F)
          sub_train_environment <- train_env_df[-validation_set,]
        } else {
          sub_train_environment <- train_env_df
        }

        agents <- foreach::foreach(i = 1:n_agents) %dopar% { ## Train N agents

          sub_train_environment_shuffle <- sub_train_environment[sample(1:nrow(sub_train_environment),
                                                                        nrow(sub_train_environment),
                                                                        replace = F), ]
          t_classes_counts <- table(sub_train_environment_shuffle$class) ## For Coverage!!
          library(RLCS) ## Assuming you've gotten the package installed by now...

          size_env <- nrow(sub_train_environment)

          for(epoch in 1:(run_params$get_n_epochs())) {
            for(i in 1:size_env) {
              lcs <- .rlcs_train_one_instance_one_epoch_mat(lcs,
                                                            sub_train_environment_shuffle[i, ],
                                                            size_env,
                                                            epoch,
                                                            (epoch-1)*size_env+i, ## train_count
                                                            run_params)
            }


          }
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
            validation_environment <- sub_train_environment[sample(1:nrow(sub_train_environment), min(1, round(0.1*nrow(train_env_df))), replace = F),]
            validation_environment$predicted <- -1 ## Stands for not found
            validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]])

            agents_quality[[j]] <- round(sum(vapply(1:nrow(validation_environment), \(i) {
              ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
            }, numeric(1)))/nrow(validation_environment), 4)

            if(use_validation) {
              ## AND validation:
              validation_environment <- train_env_df[validation_set,]
              validation_environment$predicted <- -1 ## Stands for not found
              validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]])

              agents_quality[[j]] <- (agents_quality[[j]] +
                                        round(sum(sapply(1:nrow(validation_environment), \(i) {
                                          ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
                                        }))/nrow(validation_environment), 4))/2
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
          # t_start_1 <- Sys.time()
          for(j in 1:length(agents)) {
            for(k in 1:length(agents[[j]]$pop)) {
              compacted_classifier[[length(compacted_classifier)+1]] <- agents[[j]]$pop[[k]]
            }
          }

          compacted_classifier <- .apply_subsumption_whole_pop_sl(compacted_classifier)
          lcs$pop <- compacted_classifier
          lcs$matrices <- .recalculate_pop_matrices(lcs$pop) ## Poor naming...
          # lcs$lengths <- sapply(lcs$pop, \(x) x$length_fixed_bits) ## Poor naming...
          lcs$lengths <- vapply(lcs$pop, \(x) x$length_fixed_bits, integer(1)) ## Poor naming...
          lcs$actions_vec <- .recalculate_actions_vec(lcs$pop)
        } else {
          best_agent <- order(unlist(agents_quality), decreasing = TRUE)[1]
          print(best_agent)
          lcs <- agents[[best_agent]]
        }

        # lcs <- .perfect_coverage_simplifier_sl(lcs, train_env_df, t_classes_counts)
      }
    }

    # lcs <- .apply_deletion_sl(lcs, max_pop_size = max_pop_size_parallel)
    # return(lcs)
  } else {
    print("Running single-core/thread, sequential")
    size_env <- nrow(train_env_df)
    ## Expose algorithm to training set:
    for(epoch in 1:(run_params$get_n_epochs())) {
      for(i in 1:size_env) {
        ## Now this part of the algorithm is "necessarily" sequential...
        lcs <- .rlcs_train_one_instance_one_epoch_mat(lcs,
                                                      train_env_df[i, ],
                                                      size_env,
                                                      epoch,
                                                      (epoch-1)*size_env+i, ## train_count
                                                      run_params)
        if(verbose) { ## Truly not recommended!
          if(!is.null(lcs))
            class(lcs) <- "rlcs"
          message(print(lcs))
        }
      }

    }
    ##
    ## Final simplification: Coverage
    ##
    lcs <- .perfect_coverage_simplifier_sl(lcs, train_env_df, t_classes_counts)
  }

  ## Sometimes, deletion removes all rules as none are good enough!
  if(is.null(lcs$pop)) return(NULL)
  class(lcs) <- "rlcs"

  lcs
}
