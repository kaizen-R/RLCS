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
# rlcs_visualize_predict_mnist49b <- function(test_env_df, pop) {
#   example_visual <- test_env_df
#   example_visual_v <- strsplit(example_visual$sharp_image, "", fixed=T)[[1]]
#   example_visual_m <- matrix(1-as.integer(example_visual_v), nrow=28, byrow = F)
#
#   correct_class <- (rlcs_predict_mnist49b(test_env_df, pop, verbose=F))
#
#   match_set <- get_match_set(test_env_df$state, pop)
#   res <- make_pop_printable_sl(pop[match_set])
#   res <- res[res$action == correct_class, ]
#   print(paste("Matched Rules Tot.:", length(match_set)))
#
#   t_string <- test_env_df$state
#   t_row <- strsplit(t_string, "", fixed=T)[[1]]
#   x_clean <- sapply(t_row, \(item) {
#       return(ifelse(item == 1, 1, 0))
#     })
#   t_m_orig <- matrix(x_clean, nrow=7, byrow = T)
#
#   t_m <- sapply(1:nrow(res), \(x) {
#     t_string <- res$condition[x]
#     t_numerosity <- res$numerosity[x]
#     t_row <- strsplit(t_string, "", fixed=T)[[1]]
#     x_clean <- sapply(t_row, \(item) {
#       if(item == '0') return(t_numerosity);
#       return(0)
#     })
#     as.numeric(x_clean)
#   })
#   t_m <- sapply(1:nrow(t_m), \(i) sum(t_m[i, ]))
#   t_mm <- matrix(t_m, nrow=7, byrow = T)
#   t_mm <- t_mm + min(t_mm)
#   t_m_0 <- round((t_mm - mean(t_mm))/(max(t_mm)-min(t_mm))*10)
#
#   t_m <- sapply(1:nrow(res), \(x) {
#     t_string <- res$condition[x]
#     t_numerosity <- res$numerosity[x]
#     t_row <- strsplit(t_string, "", fixed=T)[[1]]
#     x_clean <- sapply(t_row, \(item) {
#       if(item == '1') return(t_numerosity);
#       return(0)
#     })
#     as.numeric(x_clean)
#   })
#   t_m <- sapply(1:nrow(t_m), \(i) sum(t_m[i, ]))
#   t_mm <- matrix(t_m, nrow=7, byrow = T)
#   t_mm <- t_mm + min(t_mm)
#   t_m_1 <- round((t_mm - mean(t_mm))/(max(t_mm)-min(t_mm))*10)
#
#   t_m <- sapply(1:nrow(res), \(x) {
#     t_string <- res$condition[x]
#     t_numerosity <- res$numerosity[x]
#     t_row <- strsplit(t_string, "", fixed=T)[[1]]
#     x_clean <- sapply(t_row, \(item) {
#       if(item == '#') return(0);
#       return(ifelse(item == 1, -t_numerosity, t_numerosity))
#       })
#     as.numeric(x_clean)
#   })
#   t_m <- sapply(1:nrow(t_m), \(i) sum(t_m[i, ]))
#   t_mm <- matrix(t_m, nrow=7, byrow = T)
#   t_mm <- t_mm + min(t_mm)
#   t_mm_z <- round((t_mm - mean(t_mm))/(max(t_mm)-min(t_mm))*10)
#
#   my_pal_1 <- brewer.pal(9,"Greys")
#   my_pal_2 <- brewer.pal(11,"BrBG")
#
#   mat <- matrix(c(1, 2, 3,  # First, second
#                   1, 5, 4), # and third plot
#                 nrow = 2, ncol = 3,
#                 byrow = TRUE)
#
#   layout(mat = mat)
#
#   ## Support function to cleaner (no need for additional package) plot
#   tf <- function(m) t(m)[, nrow(m):1]
#   ## Support function to cleaner (no need for additional package) plot
#   imageM <- function(m, col = c(0, 1), ...) {
#       grid = max(dim(m)) <= 25
#       asp = (nrow(m)-1)/(ncol(m)-1)
#       image(.self$tf(m), col = col, asp = asp, axes = FALSE, ...)
#   }
#
#   imageM(example_visual_m, main = "MNIST formatted 28x28 binary")
#
#
#   imageM(t_m_orig, col=my_pal_1, main="Compressed 7x7")
#   imageM(t_m_0, col=my_pal_1, main="Pixel=0")
#   imageM(t_m_1, col=my_pal_1, main="Pixel=1")
#   imageM(t_mm_z, col=my_pal_2, main=paste(correct_class,"Conf.:", round(100*nrow(res)/length(match_set),2), '%'))
#
#   t_mm
# }

# rlcs_predict_iris <- function(test_env_df, pop, verbose=F) {
#   ret_set <- c()
#   for(i in 1:nrow(test_env_df)) {
#     if(verbose) {
#       print("-------------")
#       print(test_env_df$state[i])
#     }
#
#     match_set <- get_match_set(test_env_df$state[i], pop)
#     if(length(match_set) > 0) {
#       rec_setosa <- sum(sapply(pop[match_set], \(x) {
#         if(x$action == "setosa")
#           return(x$numerosity*x$accuracy)
#         0
#       }))
#       rec_versicolor <- sum(sapply(pop[match_set], \(x) {
#         if(x$action == "versicolor")
#           return(x$numerosity * x$accuracy)
#         0
#       }))
#
#       rec_virginica <- sum(sapply(pop[match_set], \(x) {
#         if(x$action == "virginica")
#           return(x$numerosity * x$accuracy)
#         0
#       }))
#
#       if(verbose) {
#         print(test_env_df$class[i])
#         print(pop[match_set])
#         print(paste("Recommend setosa: ", rec_setosa))
#         print(paste("Recommend versicolor: ", rec_versicolor))
#         print(paste("Recommend virginica: ", rec_virginica))
#       }
#       if(rec_setosa > rec_versicolor && rec_setosa > rec_virginica) {
#         # print("setosa")
#         ret_set <- c(ret_set, "setosa")
#       } else if(rec_versicolor > rec_setosa && rec_versicolor > rec_virginica) {
#         # print("versicolor")
#         ret_set <- c(ret_set, "versicolor")
#       } else {
#         # print("virginica")
#         ret_set <- c(ret_set, "virginica")
#       }
#
#
#     } else {
#       if(verbose) print("NO suitable rule for this instance.")
#       ret_set <- c(ret_set, -1)
#     }
#   }
#   return(ret_set)
# }

# rlcs_SL_stats_iris <- function(runtime, train_size, test_env_res) {
#   print(paste("Training Runtime:", runtime))
#   print(paste("Training Set Size:", train_size))
#   print("Confusion 'Matrix' for Class setosa:")
#   class_setosa_numbers <- table(test_env_res[test_env_res$class == "setosa", "predicted"])
#   print(class_setosa_numbers)
#   print("Confusion 'Matrix' for Class versicolor:")
#   class_versicolor_numbers <- table(test_env_res[test_env_res$class == "versicolor", "predicted"])
#   print(class_versicolor_numbers)
#   print("Confusion 'Matrix' for Class virginica:")
#   class_virginica_numbers <- table(test_env_res[test_env_res$class == "virginica", "predicted"])
#   print(class_virginica_numbers)
# }


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
