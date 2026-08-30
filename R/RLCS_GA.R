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

.ga_run_one_tournament_sl3 <- function(new_pop, tournament_pressure) {
  if(length(new_pop) == 1) {
    return(new_pop[[1]]$condition_string)
  }

  # n_elements <- sum(sapply(new_pop, \(x) { x$numerosity }))
  t_pop <- lapply(new_pop, \(x) { if(x$numerosity == 0) return(NULL); x })

  # ranking <- sapply(t_pop, \(x) {
  #   x$accuracy * x$numerosity + 100 *
  #     (x$condition_length - (length(x$condition_list$"0")+length(x$condition_list$"1")))
  # })
  # t_pop <- t_pop[order(ranking, decreasing=T)]
  t_pop <- .lcs_best_sort_sl(t_pop)
  n_elements <- sum(vapply(t_pop, \(x) { x$numerosity }, numeric(1)))

  extract_n <- min(n_elements, tournament_pressure)

  # p1_index <- min(sample.int(n_elements, extract_n))
  # p2_index <- min(sample.int(n_elements, extract_n))
  # parents_indices <- sort(c(p1_index, p2_index))

  parents_condition_strings <- c("", "")

  pop_indices_parents_pop <- unlist(lapply(1:length(t_pop), \(i) {
    rep(i, t_pop[[i]]$numerosity)
  }))

  # cat('\n', pop_indices_parents_pop, '\n')
  p1_index <- min(sample(pop_indices_parents_pop, extract_n))
  ## Now forcing mixing parents more often:
  t_remove <- which(pop_indices_parents_pop == p1_index)
  # cat('\n', t_remove, extract_n, length(t_remove), '\n')
  p2_index <- min(sample(pop_indices_parents_pop[-t_remove], min(extract_n, length(pop_indices_parents_pop[-t_remove]))))
  # cat('\n', p1_index, p2_index, '\n')
  # parents_indices <- sort(c(p1_index, p2_index))
  parents_condition_strings <- c(t_pop[[p1_index]]$condition_string,
                                 t_pop[[p2_index]]$condition_string)
  # for(j in 1:2) {
  #   temp_index <- parents_indices[j]
  #
  #   t_pop[[1]]$rank <- 0
  #
  #   for(i in 2:length(t_pop)) {
  #
  #     if(i == length(t_pop)) { ## We're at last item
  #       parents_condition_strings[j] <- t_pop[[i]]$condition_string
  #       if(j == 1) {
  #         parents_condition_strings[2] <- t_pop[[i]]$condition_string
  #       }
  #       return(parents_condition_strings)
  #     }
  #
  #     last_rank <- t_pop[[i-1]]$rank + 1
  #     new_rank <-  last_rank + t_pop[[i-1]]$numerosity
  #
  #     if(last_rank <= temp_index && new_rank > temp_index) {
  #       parents_condition_strings[j] <- t_pop[[i-1]]$condition_string
  #       break ## Continue to next j
  #     }
  #     t_pop[[i]]$rank <- new_rank
  #   }
  # }

  return(parents_condition_strings)
}

.cross_over_parents_strings_sl <- function(parents_pop, sel_mode,
                                          tournament_pressure) {
  ## Sometimes only one individual is in Correct population
  if(length(parents_pop) == 1)
    return(parents_pop[[1]]$condition_string) #### IF SOMETHING BREAKS...

  if(sel_mode == "tournament") {
    parents <- .ga_run_one_tournament_sl3(parents_pop, tournament_pressure)
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





.ga_run_one_tournament_sl3b <- function(env, correct_set, tournament_pressure) {
  # browser()
  if(length(correct_set) == 1) {
    return(env$lcs$condition_strings[correct_set])
  }

  # if(length(correct_set) > 1) browser()
  # n_elements <- sum(sapply(new_pop, \(x) { x$numerosity }))
  # n_elements <- sum(vapply(new_pop, \(x) { x$numerosity }, numeric(1)))

  positive_numerosities_positions <- which(env$lcs$numerosities > 0)
  # browser()
  # positive_numerosities_positions <- which(env$lcs$valid_rules)
  valid_set <- positive_numerosities_positions[positive_numerosities_positions %in% correct_set]

  if(length(valid_set) < 2)  return(env$lcs$condition_strings[valid_set])

  # if(!is.integer(valid_set)) browser()
  t_numerosities <- env$lcs$numerosities[valid_set]
  n_elements <- sum(t_numerosities)

  # t_pop <- lapply(new_pop, \(x) { if(x$numerosity == 0) return(NULL); x })
  t_pop <- env$lcs$condition_strings[valid_set]
  # t_pop_ranks <- rep(0, length(t_pop))


  extract_n <- min(n_elements, tournament_pressure)


  ranking <- env$lcs$accuracies[valid_set] - (0.01 * env$lcs$lengths_fixed_bits[valid_set] / env$lcs$conditions_length)
  # ranking <- env$lcs$accuracies[valid_set] - 0.01 * env$lcs$lengths_fixed_bits[valid_set] / env$lcs$condition_length + 0.01 * env$lcs$numerosities[valid_set]
  ranking <- order(ranking, decreasing=T)
  t_pop <- t_pop[ranking]

  sorted_numerosities <- env$lcs$numerosities[valid_set][ranking]
  sorted_numerosities <- sorted_numerosities[!is.na(sorted_numerosities)]
  # cat('\n', valid_set, '--', sorted_numerosities, '\n')
  # if(length(valid_set) > 1) browser()
  # parents_condition_strings <- c("", "")
  pop_indices_parents_pop <- unlist(sapply(1:length(sorted_numerosities), \(x) rep(x, sorted_numerosities[x])))
  # cat('\n', pop_indices_parents_pop, '\n')

  p1_index <- min(sample(pop_indices_parents_pop, extract_n))
  ## Now forcing mixing parents more often:
  t_remove <- which(pop_indices_parents_pop == p1_index)
  # cat('\n', t_remove, extract_n, length(t_remove), '\n')
  p2_index <- min(sample(array(pop_indices_parents_pop[-t_remove]), min(extract_n, length(pop_indices_parents_pop[-t_remove]))))
  # cat('\n', p1_index, p2_index, '\n')
  # parents_indices <- sort(c(p1_index, p2_index))
  parents_condition_strings <- c(t_pop[p1_index], t_pop[p2_index])

  # print(parents_condition_strings)

  # # p1_index <- min(sample.int(n_elements, extract_n))
  # # p2_index <- min(sample.int(n_elements, extract_n))
  # #
  # p1_index <- min(sample.int(n_elements, extract_n))
  # t_p2_index <- sample.int(n_elements, extract_n)
  # p2_index <- min(t_p2_index) ## Default
  # # if(n_elements > 1) {
  # #   p2_index <-
  # # }
  # parents_indices <- sort(c(p1_index, p2_index))
  #
  # parents_condition_strings <- c("", "")
  #
  # for(j in 1:2) {
  #   temp_index <- parents_indices[j]
  #
  #   t_pop_ranks[1] <- 0
  #
  #   for(i in 2:length(t_pop)) {
  #
  #     if(i == length(t_pop)) { ## We're at last item
  #       parents_condition_strings[j] <- t_pop[i]
  #
  #       if(j == 1) { ## Last item AND first parent, means second parent is the same:
  #         parents_condition_strings[2] <- t_pop[i]
  #       }
  #
  #       return(parents_condition_strings)
  #     }
  #
  #     last_rank <- t_pop_ranks[i-1] + 1
  #     new_rank <-  last_rank + t_numerosities[i-1]
  #
  #     if(last_rank <= temp_index && new_rank > temp_index) {
  #       parents_condition_strings[j] <- t_pop[i-1]
  #       break ## Continue to next j
  #     }
  #     t_pop_ranks[i] <- new_rank
  #   }
  # } ## I really need to review this logic...

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
.ga_run_one_tournament_rl3 <- function(new_pop, tournament_pressure) {
  if(length(new_pop) == 1) {
    return(new_pop[[1]]$condition_string)
  }

  n_elements <- sum(sapply(new_pop, \(x) { x$numerosity }))

  t_pop <- lapply(new_pop, \(x) { if(x$numerosity == 0) return(NULL); x })

  # ranking <- sapply(t_pop, \(x) {
  #   #x$total_reward * x$numerosity + 100 *
  #   # (x$condition_length - (length(x$condition_list$"0")+length(x$condition_list$"1")))
  #   x$total_reward
  #   # -
  #   #   (length(x$condition_list$"0")+length(x$condition_list$"1"))
  #
  # })
  # t_pop <- t_pop[order(ranking, decreasing=T)]
  t_pop <- .lcs_best_sort_rl(t_pop)

  extract_n <- min(n_elements, tournament_pressure)

  p1_index <- min(sample.int(n_elements, extract_n))
  p2_index <- min(sample.int(n_elements, extract_n))
  parents_indices <- sort(c(p1_index, p2_index))

  parents_condition_strings <- c("", "")

  for(j in 1:2) {
    temp_index <- parents_indices[j]

    t_pop[[1]]$rank <- 0

    for(i in 2:length(t_pop)) {

      if(i == length(t_pop)) { ## We're at last item
        parents_condition_strings[j] <- t_pop[[i]]$condition_string
        if(j == 1) {
          parents_condition_strings[2] <- t_pop[[i]]$condition_string
        }
        return(parents_condition_strings)
      }

      last_rank <- t_pop[[i-1]]$rank + 1
      new_rank <-  last_rank + t_pop[[i-1]]$numerosity

      if(last_rank <= temp_index && new_rank > temp_index) {
        parents_condition_strings[j] <- t_pop[[i-1]]$condition_string
        break ## Continue to next j
      }
      t_pop[[i]]$rank <- new_rank
    }
  }

  return(parents_condition_strings)
}

.cross_over_parents_strings_rl <- function(parents_pop, sel_mode,
                                           tournament_pressure) {
  ## Sometimes only one individual is in Correct population
  if(length(parents_pop) == 1)
    return(parents_pop[[1]]$condition_string) #### IF SOMETHING BREAKS...

  if(sel_mode == "tournament") {
    parents <- .ga_run_one_tournament_rl3(parents_pop, tournament_pressure)
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



## Old approach had an unnecessary dependency
# .ga_run_one_tournament_rl <- function(colec_df, tournament_pressure) {
#   parents <- NULL
#   n_elements <- nrow(colec_df)
#   for(i in 1:2) {
#     t_p <- sample(1:n_elements, min(n_elements, tournament_pressure))
#     t_p <- colec_df[t_p,]
#     t_p <- t_p[order(t_p$total_reward, t_p$accuracy, t_p$n_wildcard,
#                      #t_p$numerosity,
#                      decreasing = T), ]
#     if(i == 1) parents <- t_p[1,]
#     else parents <- rbind(parents, t_p[1, ])
#   }
#
#   parents
# }
#
# .cross_over_parents_strings_rl <- function(action_pop, sel_mode,
#                                           tournament_pressure) {
#   ## Sometimes only one individual is in Action population
#   if(length(action_pop) == 1)
#     return(action_pop[[1]]$condition)
#
#   colec_df <- plyr::rbind.fill(lapply(action_pop, \(x) {
#     data.frame(condition = rep(x$condition_string, x$numerosity),
#                total_reward = x$total_reward,
#                accuracy = x$accuracy,
#                n_wildcard = x$condition_length -
#                  (length(x$condition_list$"0")+length(x$condition_list$"1")),
#                numerosity = x$numerosity)
#
#   }))
#
#   if(sel_mode == "tournament") {
#     parents <- .ga_run_one_tournament_rl(colec_df, tournament_pressure)
#   }
#
#   max_bits <- nchar(parents$condition)
#   cut_point <- floor(stats::runif(1, min = 1, max = max_bits))
#   child1 <- paste0(substr(parents$condition[1], 1, cut_point),
#                    substr(parents$condition[2], cut_point+1, max_bits))
#   child2 <- paste0(substr(parents$condition[2], 1, cut_point),
#                    substr(parents$condition[1], cut_point+1, max_bits))
#
#   if(child1 == child2) return(c(child1))
#   ## remove children if all wildcards!
#   if(child1 == paste(rep('#', nchar(parents$condition[1])), collapse = ''))
#     return(child2)
#   if(child2 == paste(rep('#', nchar(parents$condition[1])), collapse = ''))
#     return(child1)
#
#   return(c(child1, child2))
# }
