######
## SUB FUNCTIONS
## Better self contained like this for the time being.
######

## Why not use S3 OO, as we want to use plot(), print()...
.new_rlcs_rule <- function(condition_string, action) {
  t_rule <- list(condition_string = condition_string,
       condition_length = nchar(condition_string),
       condition_list = list("0" = which(strsplit(condition_string, "", fixed=T)[[1]] == "0"),
                             "1" = which(strsplit(condition_string, "", fixed=T)[[1]] == "1")),
       action = action,
       match_count = 1,
       correct_count = 1, ## For Supervised Learning/Data Mining
       accuracy = 1,
       action_count = 1, ## For Reinforcement Learning
       total_reward = 5, ## Force initial exploration
       numerosity = 1,
       first_seen = 1)
  class(t_rule) <- "rlcs_rule"
  t_rule
}

.new_rlcs_population <- function(x = list()) {
  structure(x, class = "rlcs_population")
}

## VERY basic error generation and processing stop.
## Simply put, if input strings are not right, nothing would work...
## So no need to tryCatch(), as STOPPING would be needed anyway.
## At least in current implementation.
.validate_state_string <- function(state_string = "") {
    if(is.null(state_string) || !is.character(state_string)) stop("Input States must be strings.")
    if(nchar(state_string) < 2) stop("Input States strings must be of length >= 2.")
    ## Only basic ternary alphabet is accepted for now
    t_chars <- strsplit(state_string, "")[[1]]
    if(any(!(t_chars %in% c("0", "1")))) stop("Current implementation works with ternary alphabet. Input States must contain only characters 0 and 1.")
    T ## implicit return
}

## Create Cover for a yet to be found state
## state_string must contain at LEAST two characters for this to work
.generate_cover_rule_for_unmatched_instance <- function(state_string = "",
                                                       wildcard_prob = 0.5) {

  ## Hidden function, so instead I contorl at Pop level for SL.
  ## For RL, this is optional probably as it adds runtime.
  # if(!.validate_state_string(state_string)) return(NULL)
  len_state <- nchar(state_string)

  if(wildcard_prob > 1 || wildcard_prob < 0)
    return(NULL)

  ## ROUGH approximation to the correct count of wildcard. Should use runif()...
  n_wildcards <- round(len_state*wildcard_prob)

  ## Set lower and upper limits to number of wildcards
  if(n_wildcards == len_state) n_wildcards <- len_state-1

  t_pos <- sample(1:len_state, n_wildcards, replace = FALSE)

  state_vec <- strsplit(state_string, "", fixed = T)[[1]]
  state_vec[t_pos]<- '#'
  paste(state_vec, collapse='')
}

## Function to add rule to a population.
## date_rule_born will be useful stat for future, setting as parameter for now.
.add_valid_rule_to_pop <- function(pop, condition_string,
                                  action, date_rule_born = 0,
                                  match_count = 1,
                                  correct_count = 1,
                                  accuracy = 1,
                                  numerosity = 1) {
  t_rule <- .new_rlcs_rule(condition_string, action)

  ## Key here is creating a population structure
  if(is.null(pop) || length(pop) == 0) {
    pop <- structure(list(t_rule), class = "rlcs_population")
    return(pop)
  }

  t_rule$match_count <- match_count
  t_rule$correct_count <- correct_count
  t_rule$accuracy <- accuracy
  t_rule$numerosity <- numerosity

  t_rule$first_seen <- date_rule_born
  pop[[length(pop)+1]] <- t_rule

  # class(pop) <- "rlcs_population"
  pop
}

## FUNCTION FACTORY!
## Often needed, is to update by increase of 1 one parameter of an LCS rule
.inc_param_count <- function(param) {
  param <- as.name(param)

  function(pop) {
    lapply(pop, \(x) {
      x[[param]] <- x[[param]] + 1
      x
    })
  }
}

## Augment match count of a set of classifiers
# .inc_match_count <- function(M_pop) {
#   lapply(M_pop, \(x) {
#     x$match_count <- x$match_count + 1
#     # x$accuracy <- x$correct_count / x$match_count
#     x
#   })
# }
.inc_match_count <- .inc_param_count("match_count")

.inc_numerosity_by_condition <- function(pop, item) {
  lapply(pop, \(x, item) {
    if(x$condition_string == item) x$numerosity <- x$numerosity + 1
    x
  }, item)
}

## Augment correct count of a set of classifiers
.update_matched_accuracy <- function(match_pop) {
  ## Still a bit inefficient this, but that's what comes with using lists...
  lapply(match_pop, \(x) {
    # x$accuracy <- round(x$correct_count / x$match_count, 5)
    x$accuracy <- x$correct_count / x$match_count
    ## TODO Could run in problems for VERY high numbers divisions...
    ## Consider for next versions
    x
  })
}

get_match_set <- function(instance_state, pop) {
  if(length(pop) > 0) {
    # Only part relevant for matching
    ti_cond <- as.integer(strsplit(instance_state, "", fixed = T)[[1]])

    ## Former version, was bottleneck of the overall runtimes in several tests:
    # match_set <- which(sapply(pop, \(item, ti_cond) {
    #     rule <- item$condition_list
    #     !(any(ti_cond[rule$'0'] != 0) || any(ti_cond[rule$'1'] != 1))
    # }, ti_cond))

    ## EDIT FOR RCpp compatible. Quite a bit faster, this:
    match_set <- get_match_set_cpp(pop, ti_cond)

    if(length(match_set) > 0)
      return(match_set)
  }

  NULL ## implicit return
}

.found_same_condition <- function(pop, item) {
  any(sapply(pop, \(x, item) {
    if(x$condition_string == item) return(TRUE)
    FALSE
  }, item))
}

.apply_deletion_no_threshold <- function(pop) {

  ## Works nicely with subsumption to remove unnecessary classifiers:
  survivors_set <- which(sapply(pop, \(x) {
    if(x$numerosity > 0) return(TRUE)
    FALSE
  }))
  if(length(survivors_set) == 0) return(NULL)
  ## Ensure you keep class here.
  structure(pop[c(survivors_set)], class = "rlcs_population")
}

## Bad: Old doesn't mean it should be discarded.
# keep_only_newer_individuals <- function(pop, first_seen_threshold, accuracy=1) {
#
#   ## Works nicely with subsumption to remove unnecessary classifiers:
#   survivors_set <- which(sapply(pop, \(x) {
#     if(x$first_seen > first_seen_threshold && x$accuracy >= accuracy) return(TRUE)
#     FALSE
#   }))
#   if(length(survivors_set) == 0) return(NULL)
#   ## Ensure you keep class here.
#   structure(pop[c(survivors_set)], class = "rlcs_population")
# }

# ## Particularly useful function for parallel runs, which is not a default.
# .remove_duplicate_rules <- function(pop) {
#   ## Somewhat expensive function, but... Doesn't show up all that much in profvis
#   if(length(pop) > 1) {
#     # pop <- lcs_best_sort(pop) ## Not needed here
#     for(item in 1:(length(pop)-1)) {
#
#       cond_string <- pop[[item]]$condition_string
#       cond_lab <- pop[[item]]$action
#
#       if(pop[[item]]$numerosity > 0) {
#         ## Showing progress points. As this could take a while...
#         if(item %% 100 == 0) print(item)
#
#         pop[item:length(pop)] <- lapply(item:length(pop), \(x, t_cond, t_lab, ref_num) {
#           if(x > ref_num &&
#              pop[[x]]$numerosity > 0 &&
#              pop[[x]]$condition_string == t_cond &&
#              pop[[x]]$action == t_lab) {
#             pop[[x]]$numerosity <- 0
#           }
#
#           pop[[x]]
#         }, cond_string, cond_lab, item)
#       }
#     }
#
#     ## Removing duplicates
#     pop <- apply_deletion_no_threshold(pop)
#   }
#
#   pop
# }
