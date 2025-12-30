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
#' @param max_bits Max number of bits allowed for variables encoding
#'
#' @returns a list with a vector of cut values per column, column names and assigned values per entry, per column
#' @export
#'
#' @examples
#' rlcs_iris <- rlcs_rosetta_stone(iris, class_col=5) ## NOT part of the LCS Algorithm!
#' head(rlcs_iris$model, n=3)
rlcs_rosetta_stone <- function(input_df, class_col=1, max_bits=6) {

  if(max_bits > 6) {
    print("Max bits encoding: 6, reverting to that.")
    max_bits <- 6
  }

  .split_nbits <- function(input_vec) {
    if(length(input_vec) < 2) return(0) ## Only makes sense to encode if more than one value
    else if(length(input_vec) <= 2) nbits <- 1
    else if(length(input_vec) <= 4) nbits <- 2
    else if(length(input_vec) <= 8) nbits <- 3
    else if(length(input_vec) <= 16) nbits <- 4
    else if(length(input_vec) <= 32) nbits <- 5
    else nbits <- 6

    return(nbits)
  }

  .split_by_median_to_list <- function(vals_vec, nbits=NULL) {
    if(length(vals_vec) == 0) return(NULL)

    if(is.null(nbits)) nbits <- .split_nbits(unique(vals_vec))

    ## Recursion break cases:
    ## One value or no more breaking requested:
    if(nbits == 0) return(list(vec = vals_vec))

    cut_point <- median(unique(vals_vec))

    vec_1 <- vals_vec[which(vals_vec <= cut_point)]
    vec_2 <- vals_vec[which(vals_vec > cut_point)]

    return(c(.split_by_median_to_list(vec_1, nbits-1),
             list(cut_point = cut_point),
             .split_by_median_to_list(vec_2, nbits-1)))
  }

  .extract_cuts_and_sublists <- function(vals_vec, max_bits = 0) {
    if(length(vals_vec) <= 1) return(NULL)

    nbits <- .split_nbits(unique(vals_vec))
    if(max_bits > 0 && max_bits <= nbits) nbits <- max_bits

    print(nbits)
    print(max_bits)
    res_sublists <- .split_by_median_to_list(vals_vec, nbits)
    # print(res_sublists)
    # print(names(res_sublists[1]))
    res_sublists_vecs <- which(sapply(1:length(res_sublists), \(i) { (names(res_sublists[i]) == "vec") }))
    return(list(cut_points = as.numeric(res_sublists[-res_sublists_vecs]),
                vecs = res_sublists[res_sublists_vecs]))
  }

  .apply_t_bin_strings <- function(orig_vals, cut_points, t_bin_strings) {

    sapply(orig_vals, \(x) {
      new_val <- t_bin_strings[1]

      x <- as.numeric(x)
      for(next_lim in 1:length(cut_points)) {
        if(x >= cut_points[next_lim]) new_val <- t_bin_strings[next_lim+1]
      }
      new_val
    })
  }

  t_res <- list()
  output_df <- NULL

  for(t_col in which(1:ncol(input_df) != class_col)) {
    x <- input_df[,t_col]
    if(is.numeric(x) || is.integer(x)) {
      nbits_Gray <- .split_nbits(unique(x))
      vecs_cuts <- .extract_cuts_and_sublists(x, min(nbits_Gray, max_bits))

      t_bin_strings <- .Gray_strings(min(nbits_Gray, max_bits))
      t_name <- paste0("rlcs_", names(input_df)[t_col])

      # browser()
      Gray_vals_final_vec <- .apply_t_bin_strings(x, vecs_cuts$cut_points, t_bin_strings)
      t_res[[length(t_res)+1]] <- list(
        cuts = vecs_cuts$cut_points,
        vals = Gray_vals_final_vec,
        name = t_name,
        nbits = nbits_Gray)
      if(is.null(output_df)) {
        output_df <- data.frame(Gray_vals_final_vec)
        names(output_df) <- t_name
      } else {
        output_df[,t_name] <- Gray_vals_final_vec
      }
    }

  }

  output_df$state <- sapply(1:nrow(output_df), \(row_num) {
    paste(output_df[row_num, ], collapse="")
  })

  output_df$class <- as.character(input_df[, class_col])

  list(cuts=lapply(t_res, \(x) x$cuts),
       var_names = names(output_df)[grepl("^rlcs_.*$", names(output_df))],
       model=output_df,
       nbits=lapply(t_res, \(x) x$nbits))
}


#' To facilitate reading a rule in a decoded format. UNDER REVIEW!
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

  tncols <- length(rosetta_stone_obj$nbits)
  total_bits_covered <- 0
  tbits <- strsplit(rule_cond, "")[[1]]

  for(i in 1:length(rosetta_stone_obj$nbits)) {
    tnbits <- rosetta_stone_obj$nbits[[i]]
    tbits_tcol <- tbits[(total_bits_covered+1):(total_bits_covered + tnbits)]
    total_bits_covered <- total_bits_covered + tnbits
    .Gray_strings <- .Gray_strings(tnbits)


    t_cuts <- rosetta_stone_obj$cuts[[i]]

    candidates <- .Gray_strings
    candidates_pos <- 1:length(candidates)
    # browser()
    for(j in 1:tnbits) {
      if(tbits_tcol[j] == 1) {
        t_pointers <- which(sapply(candidates, \(x) {
          strsplit(x, "")[[1]][j] == 1
        }))
        candidates_pos <- candidates_pos[t_pointers]
        candidates <- candidates[t_pointers]
      }

      if(tbits_tcol[j] == 0) {
        t_pointers <- which(sapply(candidates, \(x) {
          strsplit(x, "")[[1]][j] == 0
        }))
        candidates_pos <- candidates_pos[t_pointers]
        candidates <- candidates[t_pointers]
      }

    }

    # browser()
    if(length(candidates) == 2^tnbits) {
      print(paste(rosetta_stone_obj$var_names[i], "can take any value"))
    } else {
      print(.Gray_strings)
      print(paste(rosetta_stone_obj$var_names[i], ":", paste(tbits_tcol, collapse="")))
      print(rosetta_stone_obj$cuts[[i]])
      print(candidates_pos)
    #
    #   if(all(candidates_pos == (candidates_pos[1]:candidates_pos[length(candidates_pos)]))) {
    #     text <- "-->"
    #     if(candidates_pos[1] == 1 && candidates_pos[length(candidates_pos)] != 1) text <- paste(text, paste(rosetta_stone_obj$var_names[i], "from less than", t_cuts[1]))
    #     else if(candidates_pos[1] == 1) text <- paste(text, paste(rosetta_stone_obj$var_names[i], "must be less than", t_cuts[1]))
    #     else text <- paste(text, paste(rosetta_stone_obj$var_names[i], "between", t_cuts[candidates_pos[1]]))
    #
    #     if(candidates_pos[length(candidates_pos)] == 16 && candidates_pos[1] != 16) text <- paste(text, paste("up to greater than", t_cuts[15]))
    #     else if(candidates_pos[length(candidates_pos)] == 16) text <- paste(text, paste(rosetta_stone_obj$var_names[i], "must be greater than", t_cuts[15]))
    #     else text <- paste(text, paste("and", t_cuts[candidates_pos[length(candidates_pos)]]))
    #
    #     print(text)
    #   } else
    #     sapply(candidates_pos, \(x) {
    #       if(x == 1) print(paste(rosetta_stone_obj$var_names[i], "less than", t_cuts[1]))
    #       if(x == 16) print(paste(rosetta_stone_obj$var_names[i], "greater than", t_cuts[15]))
    #       if(1 < x && x < 16) print(paste(rosetta_stone_obj$var_names[i], "between", t_cuts[x-1], "and", t_cuts[x]))
    #     })


      sapply(candidates_pos, \(x) {
        if(x == 1) print(paste(rosetta_stone_obj$var_names[i], "less than", t_cuts[1]))
        if(x == 2^tnbits) print(paste(rosetta_stone_obj$var_names[i], "greater than", t_cuts[2^tnbits-1]))
        if(1 < x && x < 2^tnbits) print(paste(rosetta_stone_obj$var_names[i], "between", t_cuts[x-1], "and", t_cuts[x]))
      })
    }
  }
}
