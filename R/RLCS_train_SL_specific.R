## Supervised Learning-specific versions of TRAINING functions

## Note:
## Slowly replacing functions with hidden functions (.<name>)...

.inc_correct_count <- .inc_param_count("correct_count") ## Factory

.mean_correct_count <- function(pop) {
  sum(sapply(pop, \(x) x$correct_count)) / length(pop)
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
  sum(sapply(pop, \(x) x$match_count)) / length(pop)
}

.min_correct_count <- function(pop) {
  min(sapply(pop, \(x) x$correct_count))
}

.min_match_count <- function(pop) {
  min(sapply(pop, \(x) x$match_count))
}

.get_correct_set <- function(t_instance, match_pop) {
  if(length(match_pop) > 0) {

    correct_set <- which(sapply(match_pop, \(item, ti_class) {
      ti_class == item$action
    }, t_instance$class))

    if(length(correct_set) > 0)
      return(correct_set)
  }
  NULL ## implicit return
}

## KEY function:
## Classifiers are better or worse. CHOOSING THE BEST ones is important
## For SL, accuracy is top priority, followed by generality
## This is a faster approach to calculation although arguably could be discussed
.lcs_best_sort_sl <- function(pop) {
  if(length(pop) == 0) return(NULL)

  ranking <- sapply(pop, \(x) {
    x$accuracy +
      0.01 * (1 - (length(x$condition_list$"0")+length(x$condition_list$"1")) / x$condition_length)
  }) ## For same accuracy, more general rules will be preferred.
  pop[order(ranking, decreasing=T)]
}

## Another key function here.
.apply_subsumption_whole_pop_sl <- function(pop) {

  if(length(pop) > 1) {
    pop <- .lcs_best_sort_sl(pop)
    pop <- .apply_deletion_no_threshold(pop)

    ## Within this function in this case :)
    t_matrices <- .recalculate_pop_matrices(pop)
    t_labs <- sapply(pop, \(x) { x$action })

    for(item in 1:(length(pop)-1)) {

      if(pop[[item]]$numerosity > 0) {

        t_zero <- pop[[item]]$condition_list$"0"
        t_one <- pop[[item]]$condition_list$"1"
        cond_string <- pop[[item]]$condition_string
        cond_lab <- pop[[item]]$action

        pop_to_delete <- NULL

        # ## Quick and dirty. I don't need the length subsetting here! to be reviewed!
        # all_ones_mat <- matrix(rep(1, nchar(pop[[1]]$condition_string)*length(pop[(item+1):length(pop)])),
        #                        nrow=length(pop[(item+1):length(pop)]))

        ## The old ways were... Slower, by a lot!
        # pop_to_delete <-
        #   ## RELATIVE POSITIONS!! we need item as base.
        #   which(sapply(pop[(item+1):length(pop)],
        #                \(x, t_cond, t_lab, t_zero, t_one) {
        #
        #                  if(x$numerosity > 0 && x$action == t_lab) {
        #                    t_other_cond_0 <- x$condition_list$"0"
        #                    t_other_cond_1 <- x$condition_list$"1"
        #
        #                    return(all((length(t_zero) <= length(t_other_cond_0)) ||
        #                          (length(t_one) <= length(t_other_cond_1)),
        #                          t_zero %in% t_other_cond_0,
        #                          t_one %in% t_other_cond_1))
        #                  }
        #
        #                  return(F)
        #                }, cond_string, cond_lab, t_zero, t_one))

        # ## Alternative approach now, let's see!
        ti_cond <- strsplit(cond_string, "", fixed = T)[[1]]
        ti_cond[which(ti_cond == "#")] <- -1
        ti_cond <- as.integer(ti_cond)
        ## To be cleaned here too:
        zero_vec <- rep(0, length(ti_cond))
        subsumer_must_be_zero <- zero_vec
        subsumer_must_be_zero[which(ti_cond == 0)] <- 1
        subsumer_must_be_one <- zero_vec
        subsumer_must_be_one[which(ti_cond == 1)] <- 1

        ## Remember the rules are sorted by accuracy and generality already!
        subsumer_must_match_zeros <- t_matrices[[1]][(item+1):length(pop),]  %*% subsumer_must_be_zero
        subsumer_must_match_ones <- t_matrices[[2]][(item+1):length(pop),]  %*% subsumer_must_be_one

        subsumed_must_be_different_zero <-  t_matrices[[1]][(item+1):length(pop),] %*% subsumer_must_be_one
        subsumed_must_be_different_one <- t_matrices[[2]][(item+1):length(pop),] %*% subsumer_must_be_zero

        new_pop_to_delete <- which(
          ((subsumer_must_match_zeros+subsumer_must_match_ones) == (length(t_zero)+length(t_one))) &
            (subsumed_must_be_different_zero+subsumed_must_be_different_one == 0) &
            (t_labs[(item+1):length(pop)] == cond_lab)
        )

        pop_to_delete <- new_pop_to_delete
        # browser()

        if(!is.null(pop_to_delete) && length(pop_to_delete) > 0) {

          pop_to_delete <- pop_to_delete + item ## Optimization. POSITIONS

          pop[[item]]$numerosity <- pop[[item]]$numerosity+length(pop_to_delete)

          pop[pop_to_delete] <- lapply(pop[pop_to_delete], \(item_to_clean) {
            item_to_clean$numerosity <- 0
            item_to_clean
          })
          # print(sapply(pop[pop_to_delete], \(x) x$numerosity))
        }
      }
    }

    pop <- .apply_deletion_no_threshold(pop)
  }
  pop
}

.apply_deletion_sl <- function(pop, deletion_limit = 0.6, max_pop_size = 10000) {

  pop <- lapply(pop, \(x) {
    if(x$accuracy < deletion_limit) x$numerosity <- 0
    x
  })

  if(length(pop) > max_pop_size) {
    pop <- .lcs_best_sort_sl(pop)
    pop[max_pop_size:length(pop)] <- lapply(pop[max_pop_size:length(pop)], \(x) {
      x$numerosity <- 0
      x
    })
  }

  .apply_deletion_no_threshold(pop)
}


######
## KEY FUNCTION: Train binary classifier LCS
######
.rlcs_train_one_instance_one_epoch_mat <- function(pop,
                                               t_instance,
                                               size_env, ## Used for Subsumption Freq.
                                               n_epoch, ## Used for Subsumption Freq.
                                               train_count, ## train_count
                                               run_params ## Algorithm Hyperparameters
) {
  ######
  ## Main process for R LCS Training
  ######

  ## ADD ERROR CONTROL
  match_set <- .get_match_set_mat(t_instance$state, pop, t_matrices, t_lengths)
  if(is.null(match_set) || length(match_set) == 0) { ## COVERING needed
    cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance$state,
                                                              run_params$get_wildcard_prob())

    if(!is.null(cover_rule)) {
      pop <- .add_valid_rule_to_pop(pop, cover_rule,
                                    t_instance$class, train_count)

      t_matrices <<- .recalculate_pop_matrices(pop)
      t_lengths <<- .lengths_fixed_bits(pop)

      return(pop)
    }
  } else {
    ## Faster to work with only match population until need to review overall population
    match_pop <- .inc_match_count(pop[c(match_set)])

    correct_set <- .get_correct_set(t_instance, match_pop)
    if(is.null(correct_set) || length(correct_set) == 0) { ## COVERING needed
      cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance$state,
                                                                run_params$get_wildcard_prob())
      if(!is.null(cover_rule)) {
        pop <- .add_valid_rule_to_pop(pop, cover_rule,
                                      t_instance$class, train_count)
        ## I don't like doing this, but it's a temporary thing
        t_matrices <<- .recalculate_pop_matrices_new_rule(t_matrices, cover_rule)
        t_lengths <<- .lengths_fixed_bits_new_rule(t_lengths, cover_rule)
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
        children <- correct_pop |>
          .cross_over_parents_strings_sl(run_params$get_sel_mode(),
                                         run_params$get_tournament_pressure()) |>
          sapply(.mutate_condition_string, t_instance$state, run_params$get_mut_prob())

        ## In some cases, we have only one child.
        for(child in children) {
          if(.found_same_condition(correct_pop, child)) ## Duplicate rule
            match_pop <- .inc_numerosity_by_condition(match_pop, child)
          else {
            # browser()
            pop <- .add_valid_rule_to_pop(pop,
                                          child,
                                          t_instance$class,
                                          train_count)
            # t_matrices <<- .recalculate_pop_matrices(pop)
            t_matrices <<- .recalculate_pop_matrices_new_rule(t_matrices, child)
            t_lengths <<- .lengths_fixed_bits_new_rule(t_lengths, child)
          }

        }
      }
    }

    ## Update Matched Population statistics into main population
    pop[c(match_set)] <- .update_matched_accuracy(match_pop)
  }

  ## Apply Deletion by reducing numerosity

  if((train_count %% (run_params$get_deletion_trigger()*size_env)) == 0) {
    ## Subsumption is too important to skip, for speed reasons.
    pop <- .apply_subsumption_whole_pop_sl(pop)
    pop <- .apply_deletion_sl(pop,
                              deletion_limit = run_params$get_deletion_threshold(),
                              max_pop_size = run_params$get_max_pop_size())

    ## I don't like doing this, envs-wise, but it's a temporary thing
    t_matrices <<- .recalculate_pop_matrices(pop)
    t_lengths <<- .lengths_fixed_bits(pop)

    print(paste("Epoch:", n_epoch,
                "Progress Exposure:", train_count,
                "Classifiers Count:", length(pop)))
  }

  pop
}

.validate_SL_train_df <- function(train_env_df) {
  if(!("state" %in% names(train_env_df))) stop("Input Data Frame must contain a 'state' column.")
  if(!("class" %in% names(train_env_df))) stop("Input Data Frame must contain a 'class' column.")
  if(!all(sapply(train_env_df$state, .validate_state_string))) stop("SL: Training environment, wrong state found. STOP.")
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
#' then RLCS will train n_agents parallel model and select the best one, by accuracy.
#' @param use_validation
#' Default is FALSE. OPTIONAL.ONLY USED if foreach and doParallel are available.
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
                       n_agents = 0, use_validation=F,
                       merge_best_n = 0,
                       second_evolution_iterations = 1,
                       second_evolution_run_params = NULL) {
  ## Initialization:
  lcs <- .new_rlcs_population()
  # structure(list(), class = "rlcs_population")

  .validate_SL_train_df(train_env_df)

  ## TODO Add Running Params Checks here...
  ## Further testing will surface issues here.


  ## Re-training, or "online" updates
  if(!is.null(pre_trained_lcs)) lcs <- pre_trained_lcs

  ## Shuffling population, just in case...
  train_env_df <- train_env_df[sample(1:nrow(train_env_df),
                                      nrow(train_env_df),
                                      replace = F), ]


  ## NEW
  ## "Parent" env variables...
  ## This should become part of the population
  ## But that means changing a lot of code right now, so I'll take it slow...
  t_matrices <- .recalculate_pop_matrices(lcs) ## Poor naming...
  t_lengths <- sapply(lcs, \(x) x$length_fixed_bits) ## Poor naming...

  t_classes_counts <- table(train_env_df$class) ## Poor naming... For Coverage

  if(requireNamespace("foreach", quietly=T) &
     requireNamespace("doParallel", quietly=T) &
     n_agents > 1) { ## NEW!

    for(second_evol_iter in 1:second_evolution_iterations) {

      if(second_evol_iter > 1 && !is.null(second_evolution_run_params))
        run_params <- second_evolution_run_params

      print(paste("Using foreach() %dopar% to train up to", n_agents, "parallel agents."))
      `%dopar%` <- foreach::`%dopar%`

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
        class(lcs) <- "rlcs_population"
        return(lcs)
      }


      agents_quality <- list()
      for(j in 1:n_agents) {

        ## ADD CHECK HERE FOR !is.null(agents[[j]])
        if(!is.null(agents[[j]])) {
          if(use_validation) { ## Let's see how we could do testing:

            ## We calculate accuracy BOTH for training...
            validation_environment <- train_env_df
            validation_environment$predicted <- -1 ## Stands for not found
            validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]], verbose=F)

            agents_quality[[j]] <- round(sum(sapply(1:nrow(validation_environment), \(i) {
              ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
            }))/nrow(validation_environment), 4)

            ## AND validation:
            validation_environment <- train_env_df[validation_set,]
            validation_environment$predicted <- -1 ## Stands for not found
            validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]], verbose=F)

            agents_quality[[j]] <- (agents_quality[[j]] + round(sum(sapply(1:nrow(validation_environment), \(i) {
              ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
            }))/nrow(validation_environment), 4))/2
          } else {
            ## Otherwise just training env:
            validation_environment <- train_env_df
            validation_environment$predicted <- -1 ## Stands for not found
            validation_environment$predicted <- rlcs_predict_sl(validation_environment, agents[[j]], verbose=F)

            agents_quality[[j]] <- round(sum(sapply(1:nrow(validation_environment), \(i) {
              ifelse(validation_environment[i, "class"] == validation_environment[i, "predicted"], 1, 0)
            }))/nrow(validation_environment), 4)
          }

        } else
          agents_quality[[j]] <- 0
      }

      if((merge_best_n > 1) & (merge_best_n <= n_agents)) {

        best_agents <- order(unlist(agents_quality), decreasing = TRUE)[1:merge_best_n]
        print(unlist(agents_quality))
        print(best_agents)

        ## Recollect all sub-lcs
        compacted_classifier <- list()
        agents <- agents[best_agents]
        for(j in 1:length(agents)) {
          for(k in 1:length(agents[[j]])) {
            compacted_classifier[[length(compacted_classifier)+1]] <- agents[[j]][[k]]
          }
        }

        compacted_classifier <- .apply_subsumption_whole_pop_sl(compacted_classifier)
        lcs <- compacted_classifier
        # return(compacted_classifier)
      } else {
        best_agent <- order(unlist(agents_quality), decreasing = TRUE)[1]
        # print(unlist(agents_quality))
        # print(best_agents)
        lcs <- agents[[best_agent]]
        # return(agents[[best_agent]])
      }
    }

    return(lcs)
  } else {
    print("Running single-core/thread, sequential")
    size_env <- nrow(train_env_df)
    ## Expose algorithm to training set:
    for(epoch in 1:(run_params$get_n_epochs())) {


      for(i in 1:size_env) {
        ## Now this part of the algorithm is "necessarily" sequential...
        # lcs <- .rlcs_train_one_instance_one_epoch(lcs,
        ## NEW!
        lcs <- .rlcs_train_one_instance_one_epoch_mat(lcs,
                                                      train_env_df[i, ],
                                                      size_env,
                                                      epoch,
                                                      (epoch-1)*size_env+i, ## train_count
                                                      run_params)
        if(verbose) { ## Truly not recommended!
          if(!is.null(lcs))
            class(lcs) <- "rlcs_population"
          message(print(lcs))
        }
      }
    }
  }


  ## Sometimes, deletion removes all rules as none are good enough!
  if(is.null(lcs)) return(NULL)
  class(lcs) <- "rlcs_population"

  lcs
}
