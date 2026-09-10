#' Predict a Class for a given input set of states
#'
#' @param test_env_df The dataset. Must contain a compatible state column.
#' @param lcs A trained RLCS model, consisting of a population of classifiers.
#' @param verbose Detail or not the results? Defaults to FALSE.
#'
#' @returns A vector of values of classes.
#' @export
#'
rlcs_predict_sl <- function(test_env_df, lcs, verbose=F) {
  ret_set <- c()
  possible_classes <- unique(lcs$actions)

  ret_list <- lapply(1:nrow(test_env_df), \(i) {
    lcs <- lcs
    match_set <- .get_match_set_mat_env3(as.numeric(strsplit(test_env_df$state[i], "", fixed = T)[[1]]), environment())

    if(length(match_set) > 0) {
      t_recommendation <- c()

      for(k in 1:length(possible_classes)) {
        correct_set <- .get_correct_set_env3(possible_classes[k], environment(), match_set)
        t_recommendation[k] <- sum(lcs$accuracies[correct_set]*lcs$numerosities[correct_set])
      }

      predicted_actions <- as.character(possible_classes[which(t_recommendation == max(t_recommendation))])
      if(length(predicted_actions) > 1)
        return("rlcs_doubt")
      else
        return(predicted_actions)
    } else {
      if(verbose) print("NO suitable rule for this instance.")
      return("rlcs_no_match")
    }
  })

  return(unlist(ret_list))
}


#' Predict an Action for a given input set of states
#'
#' @param lcs A trained RLCS model, with a population of classifiers.
#' @param match_set positions for matching rules. In RL, we focus on this to limit processing times.
#' @param verbose Detail or not the results? Defaults to FALSE.
#' @param possible_actions List of acceptable actions in the given "world".
#'
#' @returns A vector of values of actions.
#' @export
#'
rlcs_predict_rl <- function(lcs, match_set, verbose=F, possible_actions = c("left", "right", "up", "down")) {

  lcs <- lcs
  ## Simple version that *only* works for our demo for RL!!
  t_df <- data.frame(action=possible_actions,
                     total_reward = 0,
                     n_entries = 0)

  for(i in match_set) {
    t_row <- which(t_df$action == lcs$actions[i])
    ## Key to selecting right action here:
    ## As total_reward is applied to rule directly, no need to consider accuracy...
    t_df$total_reward[t_row] <- t_df$total_reward[t_row] +
      lcs$total_rewards[i] * lcs$numerosities[i]

    t_df$n_entries[t_row] <- t_df$n_entries[t_row] + lcs$numerosities[i]
  }
  t_df <- t_df[t_df$n_entries > 0,]
  t_df$mean_reward_match <- t_df$total_reward / t_df$n_entries

  predicted_actions <- t_df[t_df$mean_reward_match == max(t_df$mean_reward_match), "action"]
  if(length(predicted_actions) > 1) {
    return(predicted_actions[sample(1:length(predicted_actions), 1)])
  }

  predicted_actions
}

## For other work, here some hints on text-based matching...
# rlcs_predict_log <- function(test_env_df, pop, verbose=T) {
#   ret_set <- c()
#   for(i in 1:nrow(test_env_df)) {
#     if(verbose) {
#       print("-------------")
#       cat(test_env_df$Log[i])
#       cat("\n")
#     }
#     match_set <- get_match_set(test_env_df$state[i], pop)
#     if(length(match_set) > 0) {
#       rec_0 <- sum(sapply(pop[match_set], \(x) {
#         if(x$action == 0)
#           return(x$numerosity*x$accuracy)
#         0
#       }))
#       rec_1 <- sum(sapply(pop[match_set], \(x) {
#         if(x$action == 1)
#           return(x$numerosity * x$accuracy)
#         0
#       }))
#
#       if(verbose) {
#         cat(paste(
#           sapply(pop[match_set], \(x) {
#             t_vec <- strsplit(x$condition_string, "", fixed = T)[[1]]
#             res_str <- c()
#             for(i in seq(1, length(t_vec), by=2)) {
#               res_str <- c(res_str,
#                            switch(paste(t_vec[i:(i+1)], collapse=""),
#                                   "00" = "1",
#                                   "01" = "A",
#                                   "0#" = "_",
#                                   "10" = " ",
#                                   "11" = "[",
#                                   "1#" = "*",
#                                   "#"))
#             }
#             return(paste(res_str, collapse=""))
#           }),
#           collapse="\n"))
#         cat("\n")
#         print(test_env_df$class[i])
#         print(paste("Recommend 0: ", rec_0))
#         print(paste("Recommend 1: ", rec_1))
#       }
#       ret_set <- c(ret_set, ifelse(rec_0 > rec_1, 0, 1))
#     } else {
#       if(verbose) print("NO suitable rule for this instance.")
#       ret_set <- c(ret_set, -1)
#     }
#   }
#   return(ret_set)
# }
#

