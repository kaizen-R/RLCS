## Rosetta Stone code
## Traditional binary is not optimum
# num_strings <- sapply(0:15, \(x) paste(rev(as.numeric(intToBits(x)))[-(1:28)], collapse=""))

## There are functions out there, see GA Package, but this removes a dependency
.Gray_strings <- function(nbits=4) {
  g_s <- list()

  g_s[[1]] <- c('0','1')
  g_s[[2]] <- c('00','01', '11', '10')
  g_s[[3]] <- c('000', '001', '011', '010', '110', '111', '101', '100')
  g_s[[4]] <- c('0000', '0001', '0011', '0010','0110', '0111', '0101',
                '0100', '1100', '1101', '1111', '1110', '1010', '1011',
                '1001', '1000')
  g_s[[5]] <- c('00000', '00001', '00011', '00010', '00110', '00111',
                '00101', '00100', '01100', '01101', '01111',
                '01110', '01010', '01011', '01001', '01000', '11000',
                '11001', '11011', '11010', '11110', '11111',
                '11101', '11100', '10100', '10101', '10111', '10110',
                '10010', '10011', '10001', '10000')
  g_s[[6]] <- c('000000', '000001', '000011', '000010', '000110', '000111',
                '000101', '000100', '001100', '001101',
                '001111', '001110', '001010', '001011', '001001', '001000',
                '011000', '011001', '011011', '011010',
                '011110', '011111', '011101', '011100', '010100', '010101',
                '010111', '010110', '010010', '010011',
                '010001', '010000', '110000', '110001', '110011', '110010',
                '110110', '110111', '110101', '110100',
                '111100', '111101', '111111', '111110', '111010', '111011',
                '111001', '111000', '101000', '101001',
                '101011', '101010', '101110', '101111', '101101', '101100',
                '100100', '100101', '100111', '100110',
                '100010', '100011', '100001', '100000')

  g_s[[nbits]]
}



## TODO: Characters & Factors
## TODO: Allow variable length encoding, instead of 4 bits always
#' Transforms numerical columns of a dataframe into RLCS compatible binary strings
#'
#' Transforms Numeric Columns each into 16 buckets, binary-encoded with Gray encoding.
#' THIS IS CURRENTLY Work In Progress and should not be used aside from the Iris example.
#'
#' @param input_df A dataframe with numeric or integers columns, and a "class" column
#' @param class_col The "class" column which is not to be transformed
#'
#' @returns a list with a vector of cut values per column, column names and assigned values per entry, per column
#' @export
#'
#' @examples
#' rlcs_iris <- rlcs_rosetta_stone(iris, class_col=5) ## NOT part of the LCS Algorithm!
#' head(rlcs_iris$model, n=3)
rlcs_rosetta_stone <- function(input_df, class_col=1) {
  quartiles_slicer_cuts <- function(input_vec, nbits = 4) {

    t_summary <- as.numeric(summary(input_vec)[c(2, 3, 5)])

    summary_16 <- c()

    t_vec <- input_vec[which(input_vec < t_summary[1])]
    summary_16 <- c(summary_16, as.numeric(summary(t_vec)[c(2,3,5)]))
    summary_16 <- c(summary_16, t_summary[1])
    t_vec <- input_vec[which(input_vec >= t_summary[1])]
    t_vec <- t_vec[which(t_vec < t_summary[2])]
    summary_16 <- c(summary_16, as.numeric(summary(t_vec)[c(2,3,5)]))
    summary_16 <- c(summary_16, t_summary[2])
    t_vec <- input_vec[which(input_vec >= t_summary[2])]
    t_vec <- t_vec[which(t_vec < t_summary[3])]
    summary_16 <- c(summary_16, as.numeric(summary(t_vec)[c(2,3,5)]))
    summary_16 <- c(summary_16, t_summary[3])
    t_vec <- input_vec[which(input_vec >= t_summary[3])]
    summary_16 <- c(summary_16, as.numeric(summary(t_vec)[c(2,3,5)]))
    return(summary_16)
  }

  quartiles_slicer_vals <- function(input_vec, summary_16) {
    .Gray_strings <- .Gray_strings()
    sapply(input_vec, \(x) {
      new_val <- '0000'
      for(i in 1:length(summary_16)) {
        if(x >= summary_16[i]) new_val <- .Gray_strings[i+1]
      }
      return(new_val)
    })
  }

  t_res <- list()
  for(t_col in 1:ncol(input_df)[-class_col]) {
    x <- input_df[,t_col]
    if(is.numeric(x) || is.integer(x)) {
      if(length(unique(x)) < 16) stop("Current implementation of Rosetta Stone requires 16+ unique values per column")
      t_cuts <- quartiles_slicer_cuts(input_df[, t_col])
      t_vals <- quartiles_slicer_vals(input_df[, t_col], t_cuts)
      t_name <- names(input_df)[t_col]
      t_name <- paste0("rlcs_", t_name)
      t_res[[length(t_res)+1]] <- list(cuts=t_cuts, vals=t_vals, name=t_name)
    }
  }

  output_df <- data.frame(lapply(t_res,\(x) x$vals))
  names(output_df) <- lapply(t_res,\(x) x$name)

  output_df$class <- as.character(input_df[, class_col])

  output_df$state <- sapply(1:nrow(output_df), \(i) {
    state_val <- paste(output_df[i, which(names(output_df) != "class")], collapse="")
    state_val
  })
  list(cuts=lapply(t_res, \(x) x$cuts),
       var_names = names(output_df)[grepl("^rlcs_.*$", names(output_df))],
       model=output_df)
}


#' To facilitate reading a rule in a decoded format.
#'
#' Rosetta Stone encodes numerical data into binary strings for compatibility
#' with RLCS. This helper function helps decode the generated model.
#'
#' @param rule A rule from an RLCS model
#' @param rosetta_stone_obj A list previously generated by rlcs_rosetta_stone()
#'
#' @export
#'
rlcs_rosetta_decode_rule <- function(rule, rosetta_stone_obj) {
  rule_cond <- rule$condition_string
  print(paste("Rule Condition String:", rule_cond))

  .Gray_strings <- .Gray_strings()
  # print(.Gray_strings)

  tnbits <- 4
  tncols <- nchar(rule_cond)/tnbits
  tbits <- strsplit(rule_cond, "")[[1]]
  for(i in 1:tncols) {
    # print("****")

    t_cuts <- rosetta_stone_obj$cuts[[i]]
    tbits_tcol <- tbits[(4*(i-1)+1):(4*i)]
    candidates <- .Gray_strings
    candidates_pos <- 1:length(candidates)
    # browser()
    for(j in 1:tnbits) {
      if(tbits_tcol[j] == 1) {
        candidates_pos <- candidates_pos[which(sapply(candidates, \(x) {
          strsplit(x, "")[[1]][j] == 1
        }))]
        candidates <- candidates[which(sapply(candidates, \(x) {
          strsplit(x, "")[[1]][j] == 1
        }))]

      }
      if(tbits_tcol[j] == 0) {
        candidates_pos <- candidates_pos[which(sapply(candidates, \(x) {
          strsplit(x, "")[[1]][j] == 0
        }))]
        candidates <- candidates[which(sapply(candidates, \(x) {
          strsplit(x, "")[[1]][j] == 0
        }))]

      }

    }

    if(length(candidates) == 16) {
      print(paste(rosetta_stone_obj$var_names[i], "can take any value"))
    } else {
      print(paste(rosetta_stone_obj$var_names[i], ":", paste(tbits[(4*(i-1)+1):(4*i)], collapse="")))
      # print("Cuts")
      # print(rosetta_stone_obj$cuts[[i]])
      # print(candidates_pos)

      if(all(candidates_pos == (candidates_pos[1]:candidates_pos[length(candidates_pos)]))) {
        text <- "-->"
        if(candidates_pos[1] == 1 && candidates_pos[length(candidates_pos)] != 1) text <- paste(text, paste(rosetta_stone_obj$var_names[i], "from less than", t_cuts[1]))
        else if(candidates_pos[1] == 1) text <- paste(text, paste(rosetta_stone_obj$var_names[i], "must be less than", t_cuts[1]))
        else text <- paste(text, paste(rosetta_stone_obj$var_names[i], "between", t_cuts[candidates_pos[1]]))

        if(candidates_pos[length(candidates_pos)] == 16 && candidates_pos[1] != 16) text <- paste(text, paste("up to greater than", t_cuts[15]))
        else if(candidates_pos[length(candidates_pos)] == 16) text <- paste(text, paste(rosetta_stone_obj$var_names[i], "must be greater than", t_cuts[15]))
        else text <- paste(text, paste("and", t_cuts[candidates_pos[length(candidates_pos)]]))

        print(text)
      } else
        sapply(candidates_pos, \(x) {
          if(x == 1) print(paste(rosetta_stone_obj$var_names[i], "less than", t_cuts[1]))
          if(x == 16) print(paste(rosetta_stone_obj$var_names[i], "greater than", t_cuts[15]))
          if(1 < x && x < 16) print(paste(rosetta_stone_obj$var_names[i], "between", t_cuts[x-1], "and", t_cuts[x]))
        })
    }

  }
}
