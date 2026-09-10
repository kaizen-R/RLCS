## Genetic Algorithm functions
.mutate_condition_string <- function(cond_string, t_instance_state, mut_prob) {

  mut_point <- which(stats::runif(nchar(t_instance_state)) < mut_prob)
  if(length(mut_point) > 0) {
    t_state <- strsplit(t_instance_state, "", fixed = T)[[1]]
    t_cond <- strsplit(cond_string, "", fixed = T)[[1]]

    for(i in mut_point) {
      t_cond[i] <- ifelse(t_cond[i] == "#",
                          t_state[i], ## SPECIFY
                          '#') ## GENERALIZE
    }

    if(all(t_cond == "#")) return(cond_string) ## DISCARD ALL WILDCARD
    cond_string <- paste(t_cond, collapse = "")
  }

  return(cond_string)
}


.ga_run_one_tournament_sl3b <- function(env, correct_set, tournament_pressure) {

  if(length(correct_set) == 1) {
    return(env$lcs$condition_strings[correct_set])
  }

  positive_numerosities_positions <- which(env$lcs$numerosities > 0)
  valid_set <- positive_numerosities_positions[positive_numerosities_positions %in% correct_set]

  if(length(valid_set) < 2)  return(env$lcs$condition_strings[valid_set])

  t_numerosities <- env$lcs$numerosities[valid_set]
  n_elements <- sum(t_numerosities)

  t_pop <- env$lcs$condition_strings[valid_set]

  extract_n <- min(n_elements, tournament_pressure)

  ranking <- env$lcs$accuracies[valid_set] - (0.01 * env$lcs$lengths_fixed_bits[valid_set] / env$lcs$conditions_length)
  # ranking <- env$lcs$accuracies[valid_set] - 0.01 * env$lcs$lengths_fixed_bits[valid_set] / env$lcs$condition_length + 0.01 * env$lcs$numerosities[valid_set]
  ranking <- order(ranking, decreasing=T)
  t_pop <- t_pop[ranking]

  sorted_numerosities <- env$lcs$numerosities[valid_set][ranking]
  sorted_numerosities <- sorted_numerosities[!is.na(sorted_numerosities)]

  pop_indices_parents_pop <- unlist(sapply(1:length(sorted_numerosities), \(x) rep(x, sorted_numerosities[x])))

  p1_index <- min(sample(pop_indices_parents_pop, extract_n))
  # p2_index <- min(sample(pop_indices_parents_pop, extract_n))
  ## Now forcing mixing parents more often:
  t_remove <- which(pop_indices_parents_pop == p1_index)
  # cat('\n', t_remove, extract_n, length(t_remove), '\n')
  p2_index <- min(sample(array(pop_indices_parents_pop[-t_remove]), min(extract_n, length(pop_indices_parents_pop[-t_remove]))))

  parents_condition_strings <- c(t_pop[p1_index], t_pop[p2_index])

  return(parents_condition_strings)
} ## I really would need to revisit this some day!




.cross_over_parents_strings_sl_env3 <- function(env, correct_set, sel_mode,
                                           tournament_pressure) {
  ## Sometimes only one individual is in Correct population
  if(length(correct_set) == 1)
    return(env$lcs$condition_strings[correct_set]) #### IF SOMETHING BREAKS...

  if(sel_mode == "tournament") {
    parents <- .ga_run_one_tournament_sl3b(env, correct_set, tournament_pressure)
  }

  max_bits <- nchar(parents[1])
  cut_point <- floor(stats::runif(1, min = 1, max = max_bits))

  child1 <- paste0(substr(parents[1], 1, cut_point),
                   substr(parents[2], cut_point+1, max_bits))
  child2 <- paste0(substr(parents[2], 1, cut_point),
                   substr(parents[1], cut_point+1, max_bits))

  if(child1 == child2) return(child1)
  ## remove children if all wildcards!
  if(child1 == paste(rep('#', nchar(parents[1])), collapse = ''))
    return(child2)
  if(child2 == paste(rep('#', nchar(parents[1])), collapse = ''))
    return(child1)

  return(c(child1, child2))
}

## This will eventually go into a Function Factory
.ga_run_one_tournament_rl3b <- function(env, action_set, tournament_pressure) {

  if(length(action_set) == 1) {
    return(env$lcs$condition_strings[action_set])
  }

  positive_numerosities_positions <- which(env$lcs$numerosities > 0)
  valid_set <- positive_numerosities_positions[positive_numerosities_positions %in% action_set]

  if(length(valid_set) < 2)  return(env$lcs$condition_strings[valid_set])

  t_numerosities <- env$lcs$numerosities[valid_set]
  n_elements <- sum(t_numerosities)

  t_pop <- env$lcs$condition_strings[valid_set]

  extract_n <- min(n_elements, tournament_pressure)

  ranking <- env$lcs$total_rewards[valid_set] - (0.01 * env$lcs$lengths_fixed_bits[valid_set] / env$lcs$conditions_length)
  ranking <- order(ranking, decreasing=T)
  t_pop <- t_pop[ranking]

  sorted_numerosities <- env$lcs$numerosities[valid_set][ranking]
  sorted_numerosities <- sorted_numerosities[!is.na(sorted_numerosities)]

  pop_indices_parents_pop <- unlist(sapply(1:length(sorted_numerosities), \(x) rep(x, sorted_numerosities[x])))

  p1_index <- min(sample(pop_indices_parents_pop, extract_n))
  p2_index <- min(sample(pop_indices_parents_pop, extract_n))
  # ## Now forcing mixing parents more often:
  # t_remove <- which(pop_indices_parents_pop == p1_index)
  # # cat('\n', t_remove, extract_n, length(t_remove), '\n')
  # p2_index <- min(sample(array(pop_indices_parents_pop[-t_remove]), min(extract_n, length(pop_indices_parents_pop[-t_remove]))))

  parents_condition_strings <- c(t_pop[p1_index], t_pop[p2_index])

  return(parents_condition_strings)
} ## I really would need to revisit this some day!


.cross_over_parents_strings_rl_env3 <- function(env, action_set, sel_mode,
                                                tournament_pressure) {
  ## Sometimes only one individual is in Correct population
  if(length(action_set) == 0) return(NULL)
  if(length(action_set) == 1)
    return(env$lcs$condition_strings[action_set]) #### IF SOMETHING BREAKS...

  if(sel_mode == "tournament") {
    parents <- .ga_run_one_tournament_rl3b(env, action_set, tournament_pressure)
  }

  max_bits <- nchar(parents[1])
  cut_point <- floor(stats::runif(1, min = 1, max = max_bits))

  child1 <- paste0(substr(parents[1], 1, cut_point),
                   substr(parents[2], cut_point+1, max_bits))
  child2 <- paste0(substr(parents[2], 1, cut_point),
                   substr(parents[1], cut_point+1, max_bits))

  if(child1 == child2) return(child1)
  ## remove children if all wildcards!
  if(child1 == paste(rep('#', nchar(parents[1])), collapse = ''))
    return(child2)
  if(child2 == paste(rep('#', nchar(parents[1])), collapse = ''))
    return(child1)

  return(c(child1, child2))
}
