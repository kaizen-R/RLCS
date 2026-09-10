## Helpers for S3 overwritten functions

## LCS Population print out:
#' @export
plot.rlcs <- function(x, ...) {

  if(is.null(x)) return(NULL)
  lcs <- x

  nbits <- lcs$conditions_length

  t_m <- matrix(rep(0, nbits^2), byrow = T, nrow=nbits)

  for(i in 1:length(lcs$condition_strings)) {
    t_cond_0 <- which(lcs$matrix_match_0s[i,] == 1)
    t_cond_1 <- which(lcs$matrix_match_1s[i,] == 1)

    filled_bits <- length(t_cond_0)+length(t_cond_1)
    for(j in 1:nbits) {
      if(j %in% c(t_cond_0, t_cond_1))
        t_m[filled_bits, j] <- t_m[filled_bits, j]+1
    }
  }

  stats::heatmap(t_m, Rowv=NA, Colv=NA, scale="none",
                 main="LCS focus", xlab="bit", ylab="# Used Bits",
                 col=grDevices::cm.colors(max(t_m)))
  graphics::persp(1:nbits, 1:nbits, t_m, theta = 150, phi = 30,
                  expand=0.5,
                  col="lightgreen",
                  shade=0.75,
                  ticktype = "detailed",
                  xlab = "# bits involved in rule", ylab="variable (bit)",
                  zlab = "# ocurrences of bit",
                  main="LCS Focus")
}

#' @export
print.rlcs <- function(x, ...) {
  ## x here is an rlcs LCS, which contains a population
  if(length(x$numerosities[x$numerosities > 0]) == 0) return(NULL)

  lcs <- x ## as we use environment's lcs object...

  if(any(x$total_rewards != 5)) { ## Pretty unlucky if that doesn't work for RL
    .lcs_best_sort_rl_env3(environment())
    valid_positions <- which(lcs$numerosities > 0)

    df <- data.frame(condition = lcs$condition_strings[valid_positions],
                     action = lcs$actions[valid_positions],
                     total_reward = lcs$total_rewards[valid_positions],
                     fixed_bits = lcs$lengths_fixed_bits[valid_positions],
                     match_count = lcs$match_counts[valid_positions],
                     action_count = lcs$action_counts[valid_positions],
                     numerosity = lcs$numerosities[valid_positions],
                     first_seen = lcs$rule_first_seens[valid_positions])

    names(df) <- c("condition", "action", "total_reward", "fixed_bits", "match_count",
                   "action_count", "numerosity", "first_seen")
  } else { ## Data Mining or Supervised Learning use-case

    .lcs_best_sort_sl_env3(environment())
    valid_positions <- which(lcs$numerosities > 0)

    df <- data.frame(condition = lcs$condition_strings[valid_positions],
                     action = lcs$actions[valid_positions],
                     accuracy = lcs$accuracies[valid_positions],
                     fixed_bits = lcs$lengths_fixed_bits[valid_positions],
                     match_count = lcs$match_counts[valid_positions],
                     correct_count = lcs$correct_counts[valid_positions],
                     numerosity = lcs$numerosities[valid_positions],
                     first_seen = lcs$rule_first_seens[valid_positions])

    names(df) <- c("condition", "action", "accuracy", "fixed_bits", "match_count",
                   "correct_count", "numerosity", "first_seen")
  }

  df
}
