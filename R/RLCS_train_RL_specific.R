## Reinforcement Learning-specific versions of TRAINING functions
.inc_action_count <- .inc_param_count("action_count")

.mean_action_count <- function(pop) {
  sum(sapply(pop, \(x) x$action_count)) / length(pop)
}

.min_action_count <- function(pop) {
  min(sapply(pop, \(x) x$action_count))
}

.get_action_set <- function(chosen_action, match_pop) {
  if(length(match_pop) > 0)
    return(which(sapply(match_pop, \(item) {
      item$action == chosen_action
    })))
  NULL ## implicit return
}

## Similar to get_action_set in a way
.get_rule_to_be_updated <- function(t_instance_string, chosen_action, pop) {
  which(sapply(pop, \(x) {
    ## Because there was an action, there was a t_instance_string
    # if(x$action == chosen_action &&
    #    !is.null(get_match_set(t_instance_string, list(x))))
    #   return(T)
    # F
    return(x$action == chosen_action &&
             !is.null(get_match_set(t_instance_string, list(x))))
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
          which(sapply(pop[(item+1):length(pop)],
                       # \(x, t_cond, t_lab, t_zero, t_one, t_rew, ref_num) {
                       \(x, t_cond, t_lab, t_zero, t_one, t_rew) {
                         if(x$numerosity > 0 &&
                            x$action == t_lab &&
                            x$total_reward < t_rew) {
                           t_other_cond_0 <- x$condition_list$"0"
                           t_other_cond_1 <- x$condition_list$"1"

                           # if(pop[[x]]$condition_string == t_cond  ||
                           #    all((length(t_zero) <= length(t_other_cond_0)) ||
                           #        (length(t_one) <= length(t_other_cond_1)),
                           #        t_zero %in% t_other_cond_0,
                           #        t_one %in% t_other_cond_1))
                           #   return(T)
                           return(x$condition_string == t_cond  ||
                                    all((length(t_zero) <= length(t_other_cond_0)) ||
                                          (length(t_one) <= length(t_other_cond_1)),
                                        t_zero %in% t_other_cond_0,
                                        t_one %in% t_other_cond_1))
                         }

                         return(F)
                       }, cond_string, cond_lab, t_zero, t_one, t_rew))


        pop_to_delete <-
          ## RELATIVE POSITIONS!! we need item as base.
          which(sapply(pop[(item+1):length(pop)],
                       \(x, t_cond, t_lab, t_zero, t_one) {

                         if(x$numerosity > 0 && x$action == t_lab &&
                            x$total_reward < t_rew) {
                           t_other_cond_0 <- x$condition_list$"0"
                           t_other_cond_1 <- x$condition_list$"1"

                           # if(all((length(t_zero) <= length(t_other_cond_0)) ||
                           #       (length(t_one) <= length(t_other_cond_1)),
                           #       t_zero %in% t_other_cond_0,
                           #       t_one %in% t_other_cond_1))
                           #   return(T)
                           return(all((length(t_zero) <= length(t_other_cond_0)) ||
                                        (length(t_one) <= length(t_other_cond_1)),
                                      t_zero %in% t_other_cond_0,
                                      t_one %in% t_other_cond_1))
                         }

                         return(F)
                       }, cond_string, cond_lab, t_zero, t_one))

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
    # if(x$numerosity > 0) return(TRUE)
    # FALSE
    x$numerosity > 0
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

.rlcs_rl_one_movement_mat <- function(t_step, agents, world,
                                      possible_actions,
                                      explore_turn,
                                      last_instance_string = NULL,
                                      explore_exploit_mechanism = 1,
                                      warm_up = 2000,
                                      verbose = FALSE) {

  if(verbose == T) {
    Sys.sleep(0.2)
    # dev.off()
    world$get_world_plot()
  }

  n_agents <- length(agents)
  # print(n_agents)
  i <- t_step
  for(j in 1:n_agents) {
    # print(j)
    t_agent <- 1000+j
    ## Adding MEMORY
    if(!is.null(agents[[j]]$lcs) && !is.null(agents[[j]]$chosen_action)) { ## There was an action before
      agents[[j]]$last_action <- which(sapply(agents[[j]]$lcs$pop, \(x) {
        ## Because there was an action, there was a last_instance_string
        if(x$action == agents[[j]]$chosen_action && !is.null(get_match_set(last_instance_string, list(x))))
          return(T)
        F
      }))
    }

    # browser()
    ## t_agent, agents, agent_env_to_state() compatible with world$get_agent_env(agent_num)
    action_set <- c()

    t_instance_string <- world$get_agent_env(t_agent)

    match_set <- .get_match_set_mat(t_instance_string, agents[[j]]$lcs)
    # print(is.null(match_set))
    # print(match_set)

    # match_set <- get_match_set(t_instance_string, agents[[j]]$lcs$pop)
    # print(class(match_set))
    # print(match_set)

    train_count <- n_epoch <- i

    subsumption_applied <- F



    ## Not part of LCS, supplementary mechanism to favor exploration
    if(agents[[j]]$internal_status > agents[[j]]$max_internal_status)
      agents[[j]]$internal_status <- agents[[j]]$max_internal_status

    agents[[j]]$internal_status <- agents[[j]]$internal_status - 1

    ## Alternative choice to decide to explore more or less:
    decide_explore <- F
    curiosity <- 20 ## Default curiosity
    if (i <= warm_up) curiosity <- 10 ## Warm up steps
    if((explore_exploit_mechanism == 2) && (i > warm_up)) {
      if(agents[[j]]$internal_status > agents[[j]]$internal_threshold_exploit) {
        curiosity <- 3 ## Well fed: Become somewhat more curious
      }
      if(agents[[j]]$internal_status > agents[[j]]$internal_threshold_explore) {
        curiosity <- 30 ## Expert: Become less curious
      }
    }
    if((explore_exploit_mechanism == 2) && (i %% curiosity == 0))
      decide_explore = T


    if (is.null(match_set) || length(match_set) == 0 || ## COVERING needed
        ((explore_exploit_mechanism == 1) && (i %% explore_turn == 0)) || ## Exploration Turn
        decide_explore) { ## Agent is "not hungry"

      cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance_string, wildcard_prob)

      if(!is.null(cover_rule)) {
        if(is.null(match_set) || length(match_set) == 0) ## COVERING needed
          agents[[j]]$chosen_action <- sample(possible_actions, 1)
        if(((explore_exploit_mechanism == 1) && (i %% explore_turn == 0)) || ## Exploration Turn
           decide_explore) { ## Agent is "not hungry"
          match_pop <- agents[[j]]$lcs$pop[c(match_set)]
          all_tested_actions <- sapply(match_pop, \(x) x$action) |> unique()

          ## Cleverer than random exploration:
          not_tested_yet <- !(possible_actions %in% all_tested_actions)
          if(any(not_tested_yet))
            agents[[j]]$chosen_action <- sample(possible_actions[which(not_tested_yet)], 1)
          else {
            recommended_action <- rlcs_predict_rl(match_pop)
            not_recommended_actions <- !(possible_actions %in% recommended_action)
            agents[[j]]$chosen_action <- sample(possible_actions[which(not_recommended_actions)], 1)
          }
        }

        agents[[j]]$lcs$pop <- .add_valid_rule_to_pop(agents[[j]]$lcs$pop, cover_rule, agents[[j]]$chosen_action, train_count)
        # if(length(agents[[j]]$lcs$lengths) == 0) {
        #   agents[[j]]$lcs$matrices <-.recalculate_pop_matrices_new_rule(t_matrices, cover_rule)
        #   agents[[j]]$lcs$lengths <- .lengths_fixed_bits_new_rule(t_lengths, cover_rule)
        # } else {
        agents[[j]]$lcs$matrices <- .recalculate_pop_matrices(agents[[j]]$lcs$pop)
        agents[[j]]$lcs$lengths <- .lengths_fixed_bits(agents[[j]]$lcs$pop)
        # }


        reward <- world$move_agent_and_get_reward(t_agent, agents[[j]]$chosen_action)
        ## Not part of LCS, instead creating an internal "state" of the agent:
        agents[[j]]$internal_status <- agents[[j]]$internal_status + reward

        rule_to_be_updated <- .get_rule_to_be_updated(t_instance_string, agents[[j]]$chosen_action, agents[[j]]$lcs$pop)

        ## Trick: Set alpha to 1.0 to take max reward directly
        # agents[[j]]$lcs$pop[rule_to_be_updated] <- .update_action_reward_td(agents[[j]]$lcs$pop[rule_to_be_updated], reward, alpha=1)
        agents[[j]]$lcs$pop[rule_to_be_updated] <- .update_action_reward_sa(agents[[j]]$lcs$pop[rule_to_be_updated], reward)
      }
    } else { ## Exploit known Actions
      ## Faster to work with only match population until need to review overall population
      # browser()

      match_pop <- .inc_match_count(agents[[j]]$lcs$pop[c(match_set)])

      agents[[j]]$chosen_action <- rlcs_predict_rl(match_pop)

      # browser()
      action_set <- .get_action_set(agents[[j]]$chosen_action, match_pop)

      action_pop <- .inc_action_count(match_pop[c(action_set)])
      match_pop[c(action_set)] <- action_pop

      ## *Second* Rule Discovery HAPPENS HERE NOW
      ## Rule discovery happens only AFTER A RULE HAS HAD SOME TIME
      if(round(.mean_action_count(action_pop) %% rd_trigger) == 0) {
        # print("Kicking GA")
        ## The GA, basically, happens here: Cross-over & Mutation:
        children <- action_pop |>
          .cross_over_parents_strings_rl(parents_selection_mode, tournament_pressure)
        children <- children |>
          sapply(.mutate_condition_string, t_instance_string, mutation_probability)

        ## In some cases, we have only one child.
        for(child in children) {
          if(.found_same_condition(action_pop, child))  ## Duplicate rule
            match_pop[c(action_set)] <- action_pop |>
              .inc_numerosity_by_condition(child)
          else {
            agents[[j]]$lcs$pop <- .add_valid_rule_to_pop(agents[[j]]$lcs$pop,
                                                             child, agents[[j]]$chosen_action,
                                                             train_count)
            # agents[[j]]$lcs$matrices <-.recalculate_pop_matrices_new_rule(t_matrices, child)
            # agents[[j]]$lcs$lengths <- .lengths_fixed_bits_new_rule(t_lengths, child)
            agents[[j]]$lcs$matrices <- .recalculate_pop_matrices(agents[[j]]$lcs$pop)
            agents[[j]]$lcs$lengths <- .lengths_fixed_bits(agents[[j]]$lcs$pop)
          }
        }
      }

      if(length(action_pop) > 20) {
        subsumption_applied <- T
        action_pop |> .apply_subsumption_rl() -> action_pop
      }

      reward <- world$move_agent_and_get_reward(t_agent, agents[[j]]$chosen_action)
      ## Not part of LCS, instead creating an internal "state" of the agent:
      agents[[j]]$internal_status <- agents[[j]]$internal_status + reward

      ## Trick: Set alpha to 1.0 to take max reward directly
      # match_pop[c(action_set)] <- .update_action_reward_td(action_pop, reward, alpha = 1)
      match_pop[c(action_set)] <- .update_action_reward_sa(action_pop, reward)

      ## Update Matched Population statistics into main population
      agents[[j]]$lcs$pop[c(match_set)] <- .update_matched_accuracy(match_pop)

      ## Leveraging Memory to pass on reward... One step in the past, given
      ## the agent only "sees" two steps ahead MAX.
      # agents[[j]]$lcs$pop[agents[[j]]$last_action] <- .update_last_action_reward_td(agents[[j]]$lcs$pop[agents[[j]]$last_action], action_pop, alpha = 0.1)
      agents[[j]]$lcs$pop[agents[[j]]$last_action] <- .update_last_action_reward_sa(agents[[j]]$lcs$pop[agents[[j]]$last_action], action_pop)
    }

    if((subsumption_applied && length(which(sapply(agents[[j]]$lcs$pop, \(x) x$numerosity == 0))) > 0) ||
       ((i %% 1000) == 0 && length(agents[[j]]$lcs$pop) > max_pop_size)) {
      before_deletion <- length(agents[[j]]$lcs$pop)
      agents[[j]]$lcs$pop <- .apply_deletion_rl(agents[[j]]$lcs$pop, deletion_threshold, max_pop_size)

      agents[[j]]$lcs$matrices <- .recalculate_pop_matrices(agents[[j]]$lcs$pop)
      agents[[j]]$lcs$lengths <- .lengths_fixed_bits(agents[[j]]$lcs$pop)

      print(paste("Deletion Applied", before_deletion, " -> ", length(agents[[j]]$lcs$pop)))
    }
  }

  # world$get_world_matrix()

  return(list(agents = agents, world = world, last_instance_string = t_instance_string))
}
