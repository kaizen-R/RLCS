## Reinforcement Learning-specific versions of TRAINING functions
inc_action_count <- .inc_param_count("action_count")
## Was:
# inc_action_count <- function(A_pop) {
#   lapply(A_pop, \(x) {
#     x$action_count <- x$action_count + 1
#     x
#   })
# }

.mean_action_count <- function(pop) {
  sum(sapply(pop, \(x) x$action_count)) / length(pop)
}

.min_action_count <- function(pop) {
  min(sapply(pop, \(x) x$action_count))
}

.get_action_set <- function(chosen_action, match_pop) {
  if(length(match_pop) > 0)
    return(which(sapply(match_pop, \(item) {
      if(item$action == chosen_action) return(T)
      F
    })))
  NULL ## implicit return
}

## Similar to get_action_set in a way
.get_rule_to_be_updated <- function(t_instance_string, chosen_action, pop) {
  which(sapply(pop, \(x) {
    ## Because there was an action, there was a t_instance_string
    if(x$action == chosen_action &&
       !is.null(get_match_set(t_instance_string, list(x))))
      return(T)
    F
  }))
}


.lcs_best_sort_rl <- function(pop) {
  if(length(pop) == 0) return(NULL)
  ranking <- sapply(pop, \(x) {
    x$total_reward +
      0.01 * (1 - (length(x$condition_list$"0")+length(x$condition_list$"1")) / x$condition_length)
  })
  # browser()
  pop[order(ranking, decreasing=T)]
}

.apply_subsumption_rl <- function(pop) {
  ## Careful here: It's dangerous to sort action population without a thought!
  ## Population here is from the action set.
  ## But it's also in this case a *subset of the match set*.
  ## Sorting would change rules order within the match set and overall population.
  ## Thankfully, action population should never be very large...
  ## And yes, that makes this function intricately dependent on the rest... :(

  if(length(pop) > 1) { ## Useless otherwise, but should be > 20 in fact :D

    ## TRICK HERE!! See comments above
    pop <- .lcs_best_sort_rl(pop)

    ## Well, in later steps:
    ## I overwrite match_pop[c(action_set)] with newly sorted action_pop!!

    for(item in 1:(length(pop)-1)) {
      if(pop[[item]]$numerosity > 0) { ## Skip rules already "deleted"

        t_zero <- pop[[item]]$condition_list$"0"
        t_one <- pop[[item]]$condition_list$"1"
        cond_string <- pop[[item]]$condition_string
        cond_lab <- pop[[item]]$action

        t_rew <- pop[[item]]$total_reward

        pop_to_delete <-
          which(sapply((item+1):length(pop),
                       \(x, t_cond, t_lab, t_zero, t_one, t_rew, ref_num) {

                         if(x != ref_num &&
                            pop[[x]]$numerosity > 0 &&
                            pop[[x]]$action == t_lab &&
                            pop[[x]]$total_reward < t_rew) {
                           t_other_cond_0 <- pop[[x]]$condition_list$"0"
                           t_other_cond_1 <- pop[[x]]$condition_list$"1"
                           if(pop[[x]]$condition_string == t_cond  ||
                              all((length(t_zero) <= length(t_other_cond_0)) ||
                                  (length(t_one) <= length(t_other_cond_1)),
                                  t_zero %in% t_other_cond_0,
                                  t_one %in% t_other_cond_1))
                             return(T)
                         }

                         return(F)
                       }, cond_string, cond_lab, t_zero, t_one, t_rew, item
          ))

        if(length(pop_to_delete) > 0) { ## Subsumption applied!
          print(pop[[item]]$action) ## For visual progress tracking

          pop_to_delete <- pop_to_delete + item ## Vectorized, start at item position

          pop[[item]]$numerosity <- pop[[item]]$numerosity + length(pop_to_delete)

          pop[pop_to_delete] <- lapply(pop[pop_to_delete],
                                       \(item) {
                                         item$numerosity <- 0
                                         item })
        }
      }
    }
  }
  pop
}

.apply_deletion_rl <- function(pop, deletion_limit = 0.0, max_pop_size = 10000) {

  pop <- lapply(pop, \(x) {
    if(x$total_reward < deletion_limit) x$numerosity <- 0
    x
  })

  if(length(pop) > max_pop_size) {
    pop <- .lcs_best_sort_rl(pop) ## Should this happen more often??
    pop[max_pop_size:length(pop)] <- lapply(pop[max_pop_size:length(pop)], \(x) {
      x$numerosity <- 0
      x
    })
  }

  ## Works nicely with subsumption to remove unnecessary classifiers:
  survivors_set <- which(sapply(pop, \(x) {
    if(x$numerosity > 0) return(TRUE)
    FALSE
  }))

  structure(pop[c(survivors_set)], class="rlcs_population")
}

## Implementation of TD, with alpha 0.1
.update_action_reward_td <- function(A_pop, reward, alpha = 0.1) {
  lapply(A_pop, \(x) {
    x$total_reward <- x$total_reward + alpha * (reward - x$total_reward)
    x
  })
}

## Sample Average Reward update
.update_action_reward_sa <- function(A_pop, reward) {
  lapply(A_pop, \(x) {
    x$total_reward <- x$total_reward + 1/x$action_count * (reward - x$total_reward)
    x
  })
}

## Implementation of TD, with alpha 0.1
.update_last_action_reward_td <- function(last_pop, action_pop, alpha = 0.1) {
  current_action_set_reward <- mean(sapply(action_pop, \(x) x$total_reward))

  lapply(last_pop, \(x) {
    x$total_reward <- x$total_reward + alpha * (current_action_set_reward - x$total_reward)
    x
  })
}

## Sample Average Reward update
.update_last_action_reward_sa <- function(last_pop, action_pop) {
  ## My judgement call here, really...
  current_action_set_reward <- mean(sapply(action_pop, \(x) x$total_reward))
  lapply(last_pop, \(x) {
    x$total_reward <- x$total_reward + 1/x$action_count * (current_action_set_reward - x$total_reward)
    x
  })
}
