## Reinforcement Learning-specific versions of TRAINING functions

.inc_action_count_env <- function(env, action_set) {
  env$lcs$action_counts[action_set] <- env$lcs$action_counts[action_set]+1
}

.mean_action_count_env <- function(env, action_set) {
  if(length(action_set) < 1) return(-1)
  mean(env$lcs$action_counts[action_set])
}

.get_action_set_env <- function(chosen_action, env, match_set) {
  if(length(match_set) > 0)
    return(match_set[which(env$lcs$actions[match_set] == chosen_action)])
    # return(which(sapply(match_pop, \(item) {
    #   item$action == chosen_action
    # })))
  NULL ## implicit return
}

## Similar to get_action_set in a way
.get_rule_to_be_updated_env <- function(t_instance_string, chosen_action, env) {

  match_set <- .get_match_set_mat_env3(as.numeric(strsplit(t_instance_string, "")[[1]]), env)
  # match_set <- get_match_set(t_instance_string, lcs)
  if(is.null(match_set)) return(c())

  # also_good_action <- which(sapply(env$lcs$pop[match_set], \(x) return(x$action == chosen_action)))
  also_good_action <- which(env$lcs$actions[match_set] == chosen_action)
  return(match_set[also_good_action])
}

.lcs_best_sort_rl_env3 <- function(env) {

  if(is.null(env$lcs)) return(NULL)
  if(!any(env$lcs$numerosities > 0)) return(NULL) ## Nothing to sort...

  # browser()
  ## Now this here is important: Numerosity SHOULD play a role, shouldn't it?
  ranking <- env$lcs$total_rewards - 0.01 * env$lcs$lengths_fixed_bits / env$lcs$conditions_length
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
  # print(sorted_entries)
  # print(env$lcs$total_rewards)
  # if(any(!is.numeric(env$lcs$total_rewards))) browser()

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
.apply_subsumption_whole_pop_rl <- function(env,deletion_limit = .6, max_pop_size = 10000) {

  if(is.null(env$lcs)) return(NULL)
  if(!any(env$lcs$numerosities > 0)) return(NULL) ## Nothing to sort...

  # browser()

  # print(env$lcs)

  .apply_deletion_no_threshold_env3(env)
  .lcs_best_sort_rl_env3(env)

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
        (t_labs[rest_pos] == t_labs[t_pos]) & (env$lcs$total_rewards[rest_pos] < env$lcs$total_rewards[t_pos])
    )

    ## Return positions to be deleted from population
    if(!is.null(pop_to_delete) && length(pop_to_delete) > 0) {
      subsumers_list[[length(subsumers_list)+1]] <- (pop_to_delete + t_pos) ## Optimization. POSITIONS
    } else {
      subsumers_list[[length(subsumers_list)+1]] <- NA
    }
  }

  # browser()
  subsumers_positions <- which(!is.na(subsumers_list))
  if(length(subsumers_positions)>0) {
    env$lcs$numerosities[subsumers_positions] <- vapply(subsumers_positions, \(i) {
      env$lcs$numerosities[i] + length(subsumers_list[[i]])
    }, numeric(1)) ## Go this just once!

    pop_to_delete <- unique(unlist(subsumers_list[which(!is.na(subsumers_list))])) ## Reduce operations
    env$lcs$numerosities[pop_to_delete] <- 0 ## Go this just once!
    ## New, to be reviewed: If I keep working with numerosities > 0, deletion is not useful:
  }

  .apply_deletion_rl_env(env,
                         deletion_limit = deletion_limit,
                         max_pop_size = max_pop_size)
}

.apply_deletion_rl_env <- function(env, deletion_limit = 0.6, max_pop_size = 10000) {

  env$lcs$numerosities[env$lcs$total_rewards < deletion_limit] <- 0

  # positions_nums <- which(env$lcs$numerosities > 0 & env$lcs$lengths_fixed_bits > 0)
  positions_nums <- which(env$lcs$numerosities > 0)
  # positions_nums <- which(env$lcs$valid_rules)

  if(length(positions_nums) > max_pop_size) {
    env$lcs$numerosities[positions_nums[(max_pop_size+1):length(positions_nums)]] <- 0
    # env$lcs$valid_rules[positions_nums[(max_pop_size+1):length(positions_nums)]] <- FALSE
  }


  #env$lcs$pop <- .apply_deletion_no_threshold(env$lcs$pop)
  .apply_deletion_no_threshold_env3(env)


  NULL
}


## Implementation of TD, with alpha 0.1
# .update_action_reward_td <- function(A_pop, reward, alpha = 0.1) {
#   lapply(A_pop, \(x) {
#     x$total_reward <- x$total_reward + alpha * (reward - x$total_reward)
#     x
#   })
# }

## Sample Average Reward update
.update_action_reward_sa_env <- function(env, action_set, reward) {
  # print("In .update_action_reward_sa_env, before")
  # print(reward)
  # print(action_set)
  # print(env$lcs$total_rewards[action_set])

  if(!is.null(action_set) && length(action_set) > 0) {
    env$lcs$total_rewards[action_set] <- env$lcs$total_rewards[action_set] +
      ((reward - env$lcs$total_rewards[action_set]) /
         env$lcs$action_counts[action_set])
  }
  #
  # print("In .update_action_reward_sa_env, after")
  # print(env$lcs$total_rewards[action_set])

}

## Implementation of TD, with alpha 0.1
# .update_last_action_reward_td <- function(last_pop, action_pop, alpha = 0.1) {
#   current_action_set_reward <- mean(sapply(action_pop, \(x) x$total_reward))
#
#   lapply(last_pop, \(x) {
#     x$total_reward <- x$total_reward + alpha * (current_action_set_reward - x$total_reward)
#     x
#   })
# }

## Sample Average Reward update
.update_last_action_reward_sa <- function(env, last_action_set, action_set) {
  ## My judgement call here, really...
  ## Instead of updating current move with future reward, I'm doing it backwards
  ## If rules have disappeared, this might be better in fact...
  if(length(last_action_set) > 0) {

    current_action_set_reward <- round(mean(env$lcs$total_rewards[action_set]), 8)

    if(!is.nan(current_action_set_reward)) {
      # print("In .update_last_ action_reward_sa_env, before")
      # print(current_action_set_reward)
      # print(action_set)
      # print(last_action_set)
      # print(env$lcs$total_rewards[last_action_set])

      env$lcs$total_rewards[last_action_set] <- env$lcs$total_rewards[last_action_set] +
        0.5 *
        ## We're updating a past action, so current reward should participate only a little...
        ## Then again, that's not quite correct in any specific way either...
        ((current_action_set_reward - env$lcs$total_rewards[last_action_set]) /
           env$lcs$action_counts[last_action_set])
      #
      # print("In .update_last_ action_reward_sa_env, after")
      # print(env$lcs$total_rewards[last_action_set])
    }


  }
}

.update_accuracy_rl_env3 <- function(env, positions) {
  ## TODO Could run in problems for VERY high numbers divisions...?
  env$lcs$accuracies[positions] <- env$lcs$action_counts[positions] / env$lcs$match_counts[positions]
  NULL
}

.update_matched_accuracy_rl_env3 <- function(env) {
  .update_accuracy_rl_env3(env, env$match_set)
  NULL
}

.rlcs_rl_one_movement_mat_env <- function(t_step, agents, world,
                                      possible_actions,
                                      explore_turn,
                                      last_instance_string = NULL,
                                      explore_exploit_mechanism = 1,
                                      warm_up = 2000,
                                      verbose = FALSE,
                                      use_gpu = F) {

  if(verbose == T) {
    Sys.sleep(0.2)
    world$get_world_plot()
  }

  n_agents <- length(agents)
  i <- t_step
  for(j in 1:n_agents) {

    t_agent <- 1000+j
    lcs <- agents[[j]]$lcs

    ## Adding MEMORY for backwards reward updating...
    if(!is.null(lcs) &&
       any(lcs$numerosities > 0) &&
       !is.null(agents[[j]]$chosen_action)) {
      ## There was an action before
      t_match_set <- .get_match_set_mat_env3(as.numeric(strsplit(last_instance_string, "")[[1]]), environment())
      agents[[j]]$last_action <- t_match_set[which(lcs$actions[t_match_set] == agents[[j]]$chosen_action)]
    }

    ## Using ENVIRONMENTS to save on sending large LCS objects back and forth:
    action_set <- c()
    reward <- 0

    t_instance_string <- world$get_agent_env(t_agent)

    match_set <- .get_match_set_mat_env3(as.numeric(strsplit(t_instance_string, "")[[1]]),
                                         environment())

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

    if((explore_exploit_mechanism == 2) && (i %% curiosity == 0)) {
      decide_explore = T
    }

    if (is.null(match_set) || length(match_set) == 0 || ## COVERING needed
        ((explore_exploit_mechanism == 1) && (i %% explore_turn == 0)) || ## Exploration Turn
        decide_explore) { ## Agent is "not hungry"

      cover_rule <- .generate_cover_rule_for_unmatched_instance(t_instance_string, wildcard_prob)

      if(!is.null(cover_rule)) {
        if(is.null(match_set) || length(match_set) == 0) ## COVERING needed
          agents[[j]]$chosen_action <- sample(possible_actions, 1)

        if(((explore_exploit_mechanism == 1) && (i %% explore_turn == 0)) || ## Exploration Turn
           decide_explore) { ## Agent is "not hungry"

          all_tested_actions <- lcs$actions[match_set] |> unique()

          if(any(is.na(lcs$total_rewards))) browser()

          ## Cleverer than random exploration:
          not_tested_yet <- !(possible_actions %in% all_tested_actions)

          if(any(not_tested_yet))
            agents[[j]]$chosen_action <- sample(possible_actions[which(not_tested_yet)], 1)
          else {
            recommended_action <- rlcs_predict_rl(lcs, match_set)
            not_recommended_actions <- !(possible_actions %in% recommended_action)
            agents[[j]]$chosen_action <- sample(possible_actions[which(not_recommended_actions)], 1)
          }
        }

        added_rule_pos <- .add_valid_rule_to_lcs_env_mat(environment(), cover_rule, agents[[j]]$chosen_action, train_count)

        reward <- world$move_agent_and_get_reward(t_agent, agents[[j]]$chosen_action)

        ## Working more locally here, on the new rule only:
        .update_action_reward_sa_env(environment(), c(added_rule_pos), reward)
      }
    } else { ## Exploit known Actions
      ## Faster to work with only match population until need to review overall population
      agents[[j]]$chosen_action <- rlcs_predict_rl(lcs, match_set)

      action_set <- .get_action_set_env(agents[[j]]$chosen_action, environment(), match_set)

      .inc_action_count_env(environment(), action_set)

      ## *Second* Rule Discovery HAPPENS HERE NOW
      ## Rule discovery happens only AFTER A RULE HAS HAD SOME TIME
      if(round(.mean_action_count_env(environment(), action_set) %% rd_trigger) == 0) {
        # print("Kicking GA")
        ## The GA, basically, happens here: Cross-over & Mutation:
        children <- .cross_over_parents_strings_rl_env3(environment(), action_set, parents_selection_mode,
                                                        tournament_pressure)

        if(!is.null(children)) {
          children <- sapply(children, .mutate_condition_string, t_instance_string, mutation_probability)

          ## In some cases, we have only one child.
          for(child in children) {
            pos_duplicated <- .found_same_condition3(environment(), action_set, child)
            if(length(pos_duplicated) > 0) { ## Duplicate rule
              lcs$numerosities[pos_duplicated] <- lcs$numerosities[pos_duplicated] + 1
            } else {
              .add_valid_rule_to_lcs_env_mat(environment(), child, agents[[j]]$chosen_action, train_count)
            }
          }
        }
      }

      reward <- world$move_agent_and_get_reward(t_agent, agents[[j]]$chosen_action)

      ## Trick: Set alpha to 1.0 to take max reward directly
      # match_pop[c(action_set)] <- .update_action_reward_td(action_pop, reward, alpha = 1)
      .update_action_reward_sa_env(environment(), action_set, reward)

      ## Leveraging Memory to pass on reward... One step in the past, given
      ## the agent only "sees" two steps ahead MAX.
      .update_last_action_reward_sa(environment(), agents[[j]]$last_action, action_set)
      ## That is in fact equivalent to updating current move with future rewards,
      ## I'm just doing it backwards

      ## Subsumption affects lists of rules,
      ## !! if you apply before last action set update, you break it...!!
      if(length(action_set) > 20) {
        subsumption_applied <- T
        .apply_subsumption_whole_pop_rl(environment())
      }
    }

    ## Not part of LCS, instead creating an internal "state" of the agent:
    agents[[j]]$internal_status <- max(-1, agents[[j]]$internal_status + reward)

    if(!subsumption_applied && ## Otherwise would be redundant...
       ((i %% 1000) == 0 || length(lcs$condition_strings) > max_pop_size)) {
      .apply_deletion_rl_env(environment(), deletion_threshold, max_pop_size)
    }

    if(!is.null(lcs)) class(lcs) <- "rlcs"

    agents[[j]]$lcs <- lcs
  }

  return(list(agents = agents, world = world, last_instance_string = t_instance_string))
}
