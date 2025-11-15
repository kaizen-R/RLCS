#' RLCS Hyper-parameters preparatioin
#'
#' Generates an object of Hyperparameters required to run RLCS training.
#' All parameters have a default, although it is recommended each parameter is
#' reviewed and set depending on the use-case
#'
#' @field wildcard_prob numeric. Probability of a matching rule to set wildcards
#' instead of fixed bits.
#' @field rd_trigger numeric. When to trigger Genetic Algorithm.
#' The smaller this number, the more often new rules are generated
#' @field mutation_probability numeric. For each new rule generated, what's the
#' probability of each bit to be mutated.
#' @field parents_selection_mode character.
#' @field tournament_pressure numeric.
#' @field n_epochs numeric.
#' @field deletion_trigger numeric.
#' @field deletion_threshold numeric.
#'
#' @returns An RLCS Object with a set of hyperparameters for LCS Training.
#' @export
#'
#' @examples
#' default_rlcs_hyperparameters <- RLCS_hyperparameters()
RLCS_hyperparameters <- setRefClass(
  Class = "RLCS_hyperparameters",
  fields = list(wildcard_prob = "numeric",
                rd_trigger = "numeric", mutation_probability = "numeric",
                parents_selection_mode = "character", tournament_pressure = "numeric",
                n_epochs = "numeric", deletion_trigger = "numeric",
                deletion_threshold = "numeric",
                max_pop_size = "numeric"
  ),
  methods = list(
    initialize = function(wildcard_prob = 0.2,
                          rd_trigger = 25, mutation_probability = 0.05,
                          parents_selection_mode = "tournament", tournament_pressure = 5,
                          n_epochs = 1000, deletion_trigger = 200,
                          deletion_threshold = 0.75,
                          max_pop_size = 10000) {
      ## A tryCatch here would be perfect to control for invalid input.

      is_valid_probability_number <- function(t_num) {
        # if(class(t_num) == "numeric" && t_num <= 1 && t_num >= 0)
        if(is.numeric(t_num) && t_num <= 1 && t_num >= 0)
          return(TRUE)
        FALSE
      }

      is_valid_parent_selection_mode <- function(sel_mode) {
        ## Just the one implemented thus far...
        # if(class(sel_mode) == "character" && sel_mode == "tournament")
        if(is.character(sel_mode) && sel_mode == "tournament")
          return(TRUE)
        FALSE
      }

      if(is_valid_probability_number(wildcard_prob))
        .self$wildcard_prob <- wildcard_prob

      .self$rd_trigger <- rd_trigger

      if(is_valid_probability_number(mutation_probability))
        .self$mutation_probability <- mutation_probability

      if(is_valid_parent_selection_mode(parents_selection_mode))
        .self$parents_selection_mode <- parents_selection_mode

      .self$tournament_pressure <- tournament_pressure
      .self$n_epochs <- n_epochs
      .self$deletion_trigger <- deletion_trigger

      if(is_valid_probability_number(deletion_threshold))
        .self$deletion_threshold <- deletion_threshold

      if(is.numeric(max_pop_size) && max_pop_size > 1 && max_pop_size < 100000) {
        .self$max_pop_size <- round(max_pop_size)
      }
    },

    get_wildcard_prob = function() {
      .self$wildcard_prob
    },

    get_rd_trigger = function() {
      .self$rd_trigger
    },

    get_sel_mode = function() {
      .self$parents_selection_mode
    },

    get_tournament_pressure = function() {
      .self$tournament_pressure
    },

    get_mut_prob = function() {
      .self$mutation_probability
    },

    get_n_epochs = function() {
      .self$n_epochs
    },

    get_deletion_trigger = function() {
      .self$deletion_trigger
    },

    get_deletion_threshold = function() {
      .self$deletion_threshold
    },

    get_max_pop_size = function() {
      .self$max_pop_size
    }
  )
)
