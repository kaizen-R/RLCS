## Demo datasets

## Demo dataset: Fourth bit negated is the class
.rlcs_not_bit_4 <- function() {
  ## Modified from Help of intToBits()
  bitI_5b <- function(x) vapply(as.integer(x), function(x) {
    b <- substr(as.character(rev(intToBits(x))), 2L, 2L)
    paste0(b[28:32], collapse = "")
  }, "")

  full_dataset <- data.frame(state = bitI_5b(0:31), class = 0)
  full_dataset$class <- ifelse(substr(full_dataset$state, 4L, 4L) == '0', 1, 0)
  full_dataset
}

rlcs_not_bit_4_10 <- function() {
  ## Modified from Help of intToBits()
  bitI_10b <- function(x) vapply(as.integer(x), function(x) {
    b <- substr(as.character(rev(intToBits(x))), 2L, 2L)
    paste0(b[23:32], collapse = "")
  }, "")

  full_dataset <- data.frame(state = bitI_10b(0:1024), class = 0)
  full_dataset$class <- ifelse(substr(full_dataset$state, 4L, 4L) == '0', 1, 0)
  full_dataset
}

.rlcs_xor_bits_4_5 <- function() {
  ## Modified from Help of intToBits()
  bitI_5b <- function(x) vapply(as.integer(x), function(x) {
    b <- substr(as.character(rev(intToBits(x))), 2L, 2L)
    paste0(b[28:32], collapse = "")
  }, "")

  full_dataset <- data.frame(state = bitI_5b(0:31), class = 0)
  full_dataset$class <- ifelse(substr(full_dataset$state, 4L, 5L) == '01', 1,
                               ifelse(substr(full_dataset$state, 4L, 5L) == '10', 1, 0))
  full_dataset
}

## Demo dataset: Fourth bit negated is the class
rlcs_mux6 <- function() {
  ## Modified from Help of intToBits()
  bitI_6b <- function(x) vapply(as.integer(x), function(x) {
    b <- substr(as.character(rev(intToBits(x))), 2L, 2L)
    paste0(b[27:32], collapse = "")
  }, "")

  full_dataset <- data.frame(state = bitI_6b(0:63), class = 0)

  for(i in 1:nrow(full_dataset)) {
    full_dataset$class[i] <- switch(substr(full_dataset$state[i], 1L, 2L),
                                    '00' = substr(full_dataset$state[i], 3L, 3L),
                                    '01' = substr(full_dataset$state[i], 4L, 4L),
                                    '10' = substr(full_dataset$state[i], 5L, 5L),
                                    '11' = substr(full_dataset$state[i], 6L, 6L))
  }

  full_dataset
}


rlcs_example_secret1 <- function() {
  .rlcs_not_bit_4()
}
rlcs_example_secret2 <- function() {
  .rlcs_xor_bits_4_5()
}
