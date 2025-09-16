## Supervised Learning-specific versions of TRAINING functions

## Note:
## Slowly replacing functions with hidden functions (.<name>)...

# .inc_correct_count <- function(C_pop) {
#   lapply(C_pop, \(x) {
#     x$correct_count <- x$correct_count + 1
#     x
#   })
# }
.inc_correct_count <- .inc_param_count("correct_count") ## Factory

.mean_correct_count <- function(pop) {
  sum(sapply(pop, \(x) x$correct_count)) / length(pop)
}

.min_correct_count <- function(pop) {
  min(sapply(pop, \(x) x$correct_count))
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

# lcs_best_sort_sl <- function(pop) {
#   if(length(pop) == 0) return(NULL)
#   # print(unclass(pop))
#
#   df <- plyr::rbind.fill(lapply(1:length(pop), \(i) {
#     t_c <- pop[[i]]
#     data.frame(correct_count = t_c$correct_count,
#                accuracy = t_c$accuracy,
#                n_wildcard = t_c$condition_length -
#                  (length(t_c$condition_list$"0")+length(t_c$condition_list$"1")),
#                numerosity = t_c$numerosity)
#   }))
#   t_sort <- order(df$accuracy, df$n_wildcard,
#                   df$correct_count, df$numerosity, decreasing = T)
#
#   pop[t_sort]
# }

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

    for(item in 1:(length(pop)-1)) {

      if(pop[[item]]$numerosity > 0) {

        t_zero <- pop[[item]]$condition_list$"0"
        t_one <- pop[[item]]$condition_list$"1"
        cond_string <- pop[[item]]$condition_string
        cond_lab <- pop[[item]]$action

        pop_to_delete <- NULL

        pop_to_delete <-
          # which(sapply(1:length(pop),  ## OPTIMIZATION HERE... Don't iterate all!
          which(sapply((item+1):length(pop),
                       \(x, t_cond, t_lab, t_zero, t_one, ref_num) {
                         # print(x)
                         if(x > ref_num &&
                            pop[[x]]$numerosity > 0 && pop[[x]]$action == t_lab) {
                           t_other_cond_0 <- pop[[x]]$condition_list$"0"
                           t_other_cond_1 <- pop[[x]]$condition_list$"1"

                           if(#pop[[x]]$condition_string == t_cond  ||
                             all((length(t_zero) <= length(t_other_cond_0)) ||
                                 (length(t_one) <= length(t_other_cond_1)),
                                 t_zero %in% t_other_cond_0,
                                 t_one %in% t_other_cond_1))
                             return(T)
                         }

                         return(F)
                       }, cond_string, cond_lab, t_zero, t_one, item))

        if(length(pop_to_delete) > 0) {

          pop_to_delete <- pop_to_delete + item ## Optimization

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

  # ## Works nicely with subsumption to remove unnecessary classifiers:
  # survivors_bool <- sapply(pop, \(x) {
  #   if(x$numerosity > 0) return(TRUE)
  #   FALSE
  # })
  #
  # if(any(survivors_bool)) {
  #   survivors_set <- which(survivors_bool)
  #   return(structure(pop[c(survivors_set)], class="rlcs_population"))
  # }
  # structure(list(), class="rlcs_population")

  .apply_deletion_no_threshold(pop)
}

## Support function for human-compatible printing:
## Discarded in favor of S3 methods
# make_pop_printable_sl <- function(pop) {
#   if(length(pop) == 0) return(NULL)
#
#   pop <- .lcs_best_sort_sl(pop)
#   plyr::rbind.fill(lapply(1:length(pop), \(i) {
#     t_c <- pop[[i]]
#     data.frame(condition = t_c$condition_string,
#                action = t_c$action,
#                match_count = t_c$match_count,
#                correct_count = t_c$correct_count,
#                accuracy = t_c$accuracy,
#                numerosity = t_c$numerosity,
#                first_seen = t_c$first_seen)
#   }))
# }
#


######
## KEY FUNCTION: Train binary classifier LCS
######
.rlcs_train_one_instance_one_epoch <- function(pop,
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
  match_set <- get_match_set(t_instance$state, pop)
  if(is.null(match_set) || length(match_set) == 0) { ## COVERING needed
    cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance$state,
                                                             run_params$get_wildcard_prob())

    if(!is.null(cover_rule)) {
      pop <- .add_valid_rule_to_pop(pop, cover_rule,
                                   t_instance$class, train_count)
    }
  } else {
    ## Faster to work with only match population until need to review overall population
    match_pop <- .inc_match_count(pop[c(match_set)])

    correct_set <- .get_correct_set(t_instance, match_pop)
    if(is.null(correct_set) || length(correct_set) == 0) { ## COVERING needed
      cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance$state,
                                                               run_params$get_wildcard_prob())
      if(!is.null(cover_rule))
        pop <- .add_valid_rule_to_pop(pop, cover_rule,
                                     t_instance$class, train_count)
    } else {
      correct_pop <- match_pop[c(correct_set)]

      match_pop[c(correct_set)] <- .inc_correct_count(correct_pop)

      ## *Second* Rule Discovery HAPPENS HERE NOW
      ## Rule discovery happens only AFTER A RULE HAS HAD SOME TIME
      # if(round(.mean_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
      if((.min_correct_count(correct_pop) %% run_params$get_rd_trigger()) == 0) {
        ## The GA, basically, happens here: Cross-over & Mutation:
        children <- correct_pop |>
          .cross_over_parents_strings_sl(run_params$get_sel_mode(),
                                        run_params$get_tournament_pressure()) |>
          sapply(.mutate_condition_string, t_instance$state, run_params$get_mut_prob())

        ## In some cases, we have only one child.
        for(child in children) {
          if(.found_same_condition(correct_pop, child)) ## Duplicate rule
            match_pop <- .inc_numerosity_by_condition(match_pop, child)
          else
            pop <- .add_valid_rule_to_pop(pop,
                                         child,
                                         t_instance$class,
                                         train_count)
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
    pop <- .apply_deletion_sl(pop, deletion_limit = run_params$get_deletion_threshold())

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

rlcs_train <- function(train_env_df, run_params,
                       pre_trained_lcs = NULL, verbose=F) {
  ## Initialization:
  lcs <- .new_rlcs_population()
  # structure(list(), class = "rlcs_population")

  .validate_SL_train_df(train_env_df)

  ## TODO Add Running Params Checks here...
  ## Further testing will surface issues here.


  ## Re-training, or "online" updates
  if(!is.null(pre_trained_lcs)) lcs <- pre_trained_lcs

  size_env <- nrow(train_env_df)

  ## Expose algorithm to training set:
  for(epoch in 1:(run_params$get_n_epochs())) {
    for(i in 1:size_env) {
      ## Now this part of the algorithm is "necessarily" sequential...
      lcs <- .rlcs_train_one_instance_one_epoch(lcs,
                        train_env_df[i, ],
                        size_env,
                        epoch,
                        (epoch-1)*size_env+i, ## train_count
                        run_params)
      if(verbose) { ## Truly not recommended!
        class(lcs) <- "rlcs_population"
        message(print(lcs))
      }
    }
  }

  ## Sometimes, deletion removes all rules as none are good enough!
  if(is.null(lcs)) return(NULL)
  class(lcs) <- "rlcs_population"

  lcs
}
