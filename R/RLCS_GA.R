## Genetic Algorithm functions
.mutate_condition_string <- function(cond_string, t_instance_state, mut_prob) {
  mut_point <- which(runif(nchar(t_instance_state)) < mut_prob)
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

  n_elements <- sum(sapply(new_pop, \(x) { x$numerosity }))

  t_pop <- lapply(new_pop, \(x) { if(x$numerosity == 0) return(NULL); x })

  ranking <- sapply(t_pop, \(x) {
    x$accuracy * x$numerosity + 100 *
      (x$condition_length - (length(x$condition_list$"0")+length(x$condition_list$"1")))
  })
  t_pop <- t_pop[order(ranking, decreasing=T)]

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

.cross_over_parents_strings_sl <- function(parents_pop, sel_mode,
                                          tournament_pressure) {
  ## Sometimes only one individual is in Correct population
  if(length(parents_pop) == 1)
    return(parents_pop[[1]]$condition_string) #### IF SOMETHING BREAKS...

  if(sel_mode == "tournament") {
    parents <- .ga_run_one_tournament_sl3(parents_pop, tournament_pressure)
  }

  max_bits <- nchar(parents[1])
  cut_point <- floor(runif(1, min = 1, max = max_bits))

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

.ga_run_one_tournament_rl <- function(colec_df, tournament_pressure) {
  parents <- NULL
  n_elements <- nrow(colec_df)
  for(i in 1:2) {
    t_p <- sample(1:n_elements, min(n_elements, tournament_pressure))
    t_p <- colec_df[t_p,]
    t_p <- t_p[order(t_p$total_reward, t_p$accuracy, t_p$n_wildcard,
                     #t_p$numerosity,
                     decreasing = T), ]
    if(i == 1) parents <- t_p[1,]
    else parents <- rbind(parents, t_p[1, ])
  }

  parents
}

.cross_over_parents_strings_rl <- function(action_pop, sel_mode,
                                          tournament_pressure) {
  ## Sometimes only one individual is in Action population
  if(length(action_pop) == 1)
    return(action_pop[[1]]$condition)

  colec_df <- plyr::rbind.fill(lapply(action_pop, \(x) {
    data.frame(condition = rep(x$condition_string, x$numerosity),
               total_reward = x$total_reward,
               accuracy = x$accuracy,
               n_wildcard = x$condition_length -
                 (length(x$condition_list$"0")+length(x$condition_list$"1")),
               numerosity = x$numerosity)

  }))

  if(sel_mode == "tournament") {
    parents <- .ga_run_one_tournament_rl(colec_df, tournament_pressure)
  }

  max_bits <- nchar(parents$condition)
  cut_point <- floor(runif(1, min = 1, max = max_bits))
  child1 <- paste0(substr(parents$condition[1], 1, cut_point),
                   substr(parents$condition[2], cut_point+1, max_bits))
  child2 <- paste0(substr(parents$condition[2], 1, cut_point),
                   substr(parents$condition[1], cut_point+1, max_bits))

  if(child1 == child2) return(c(child1))
  ## remove children if all wildcards!
  if(child1 == paste(rep('#', nchar(parents$condition[1])), collapse = ''))
    return(child2)
  if(child2 == paste(rep('#', nchar(parents$condition[1])), collapse = ''))
    return(child1)

  return(c(child1, child2))
}
