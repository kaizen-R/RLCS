## Helpers for S3 overwritten functions

## LCS Population Rules print out:

#' @export
print.rlcs_rule <- function(x, ...) {
  print(paste(x$condition_string, x$action), ...)
}

## RLCS Population overwritten functions
#' @export
`[.rlcs_population` <- function(x, i) {
  # stopifnot(x, list)
  .new_rlcs_population(NextMethod())
}

#' @export
plot.rlcs_population <- function(x, ...) {

  nbits <- x[[1]]$condition_length

  t_m <- matrix(rep(0, nbits^2), byrow = T, nrow=nbits)

  for(i in 1:length(x)) {
    filled_bits <- length(x[[i]]$condition_list$"0")+length(x[[i]]$condition_list$"1")
    for(j in 1:nbits) {
      if(j %in% c(x[[i]]$condition_list$"0", x[[i]]$condition_list$"1"))
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
print.rlcs_population <- function(x, ...) {
  if(length(x) == 0) return(NULL)
  x <- .lcs_best_sort_sl(x)
  x <- unclass(x)
  l <- lapply(1:length(x), \(i) {
    t_c <- x[[i]]
    data.frame(condition = t_c$condition_string,
               action = t_c$action,
               match_count = t_c$match_count,
               correct_count = t_c$correct_count,
               accuracy = t_c$accuracy,
               numerosity = t_c$numerosity,
               reward = t_c$total_reward,
               first_seen = t_c$first_seen)
  })
  # plyr::rbind.fill(l) ## Faster, but adds plyr dependency :(
  ## Slower, but no dependency:
  df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=TRUE))
  names(df) <- c("condition", "action", "match_count", "correct_count", "accuracy", "numerosity", "reward", "first_seeen")
  df
}

