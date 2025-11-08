# rlcs_predict_simple <- function(test_env_df, pop, verbose=F) {
#   ret_set <- c()
#   for(i in 1:nrow(test_env_df)) {
#     if(verbose) {
#       print("-------------")
#       print(test_env_df$state[i])
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
#         print(test_env_df$class[i])
#         print(make_pop_printable(pop[match_set]))
#         print(paste("Recommend 0: ", rec_0))
#         print(paste("Recommend 1: ", rec_1))
#       }
#       ret_set <- c(ret_set, ifelse(rec_0 > rec_1, 0, 1))
#     } else {
#       if(verbose) print("NO suitable rule for this instance.")
#       ret_set <- c(ret_set, -1)
#     }
#
#   }
#   return(ret_set)
# }
#

#' Predict a Class for a given input set of states
#'
#'
#'
#' @param test_env_df The dataset. Must contain a compatible state column.
#' @param pop A trained RLCS model, consisting of a population of classifiers.
#' @param verbose Detail or not the results? Defaults to FALSE.
#'
#' @returns A vector of values of classes.
#' @export
#'
rlcs_predict_sl <- function(test_env_df, pop, verbose=F) {
  ret_set <- c()
  possible_classes <- unique(sapply(pop, \(x) x$action))

  for(i in 1:nrow(test_env_df)) {
    if(verbose) {
      print("-------------")
      print(test_env_df$state[i])
    }
    match_set <- get_match_set(test_env_df$state[i], pop)
    if(length(match_set) > 0) {
      t_recommendation <- c()

      for(k in 1:length(possible_classes)) {
        t_recommendation[k] <- sum(sapply(pop[match_set], \(x) {
          if(x$action == possible_classes[k])
            return(x$numerosity*x$accuracy)
          0
        }))
      }


      if(verbose) {
        print(test_env_df$class[i])
        print(possible_classes)
        print(pop[match_set])
        for(item in 1:length(possible_classes))
          print(paste("Recommend", possible_classes[item], ":", t_recommendation[item]))
        # print(paste("Recommend 1: ", rec_1))
        print(t_recommendation)

        print(max(t_recommendation))
        print(possible_classes[which(t_recommendation == max(t_recommendation))])
      }
      predicted_actions <- as.character(possible_classes[which(t_recommendation == max(t_recommendation))])
      if(length(predicted_actions) > 1) ret_set <- c(ret_set, "rlcs_doubt")
      else ret_set <- c(ret_set, predicted_actions)
    } else {
      if(verbose) print("NO suitable rule for this instance.")
      ret_set <- c(ret_set, "rlcs_no_match")
    }

  }
  return(ret_set)
}

rlcs_predict_simple_rl <- function(pop, verbose=F) {

  ## Simple version that *only* works for our demo for RL!!
  t_df <- data.frame(action=c("left", "right", "up", "down"),
                     total_reward = 0,
                     n_entries = 0)
  for(i in 1:length(pop)) {
    item <- pop[[i]]
    t_row <- which(t_df$action == item$action)
    ## Key to selecting right action here:
    ## As total_reward is applied to rule directly, no need to consider accuracy...
    t_df[t_row, "total_reward"] <- t_df[t_row, "total_reward"] +
      item$total_reward * item$numerosity

    t_df[t_row, "n_entries"] <- t_df[t_row, "n_entries"] + item$numerosity
  }
  t_df <- t_df[t_df$n_entries > 0,]
  t_df$mean_reward_match <- t_df$total_reward / t_df$n_entries

  predicted_actions <- t_df[t_df$mean_reward_match == max(t_df$mean_reward_match), "action"]

  if(length(predicted_actions) > 1) return(predicted_actions[sample(1:length(predicted_actions), 1)])
  predicted_actions
}

rlcs_SL_stats <- function(runtime, train_size, test_env_res) {
  print(paste("Training Runtime:", runtime))
  print(paste("Training Set Size:", train_size))
  print("Confusion 'Matrix' for Class 0:")
  class_0_numbers <- table(test_env_res[test_env_res$class == 0, "predicted"])
  print(class_0_numbers)
  print("Confusion 'Matrix' for Class 1:")
  class_1_numbers <- table(test_env_res[test_env_res$class == 1, "predicted"])
  print(class_1_numbers)
}

# rlcs_predict_mnist49b <- function(test_env_df, pop, verbose=T) {
#   ret_set <- c()
#   for(i in 1:nrow(test_env_df)) {
#     if(verbose) {
#       print("-------------")
#       print_mnist_number_49b(test_env_df$state[i])
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
#         print(test_env_df$class[i])
#         print(make_pop_printable_sl(pop[match_set]))
#         print(paste("Recommend 0: ", rec_0))
#         print(paste("Recommend 1: ", rec_1))
#       }
#       ret_set <- c(ret_set, ifelse(rec_0 > rec_1, 0, 1))
#     } else {
#       if(verbose) print("NO suitable rule for this instance.")
#       ret_set <- c(ret_set, -1)
#     }
#
#   }
#   return(ret_set)
# }
#



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
# rlcs_predict_yf <- function(test_env_df, pop, verbose=T) {
#   ret_set <- c()
#   for(i in 1:nrow(test_env_df)) {
#     if(verbose) {
#       print("-------------")
#       cat(test_env_df$state[i])
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
