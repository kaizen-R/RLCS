## Rosetta Stone code


## TODO: Characters & Factors
## TODO: Allow variable length encoding, instead of 4 bits always
rlcs_rosetta_stone <- function(input_df, class_col=1) {
    quartiles_slicer <- function(input_vec, nbits = 2) {

        t_summary <- as.numeric(summary(input_vec)[c(1, 2, 3, 5, 6)])

        summary_16 <- min(input_vec) ## Better?

        for(i in 1:(length(t_summary)-1)) {

            t_vec <- input_vec[which(input_vec > t_summary[i])]
            t_vec <- t_vec[which(t_vec <= t_summary[i+1])]
            # print(t_vec)
            summary_16 <- c(summary_16, as.numeric(summary(t_vec)[c(2,3,5)]))
        }

        ## Traditional binary is not optimum
        # t_strings <- sapply(0:15, \(x) paste(rev(as.numeric(intToBits(x)))[-(1:28)], collapse=""))

        ## Instead, let's use Gray encoding
        t_strings <- c('0000', '0001', '0011', '0010',
                       '0110', '0111', '0101', '0100',
                       '1100', '1101', '1111', '1110',
                       '1010', '1011', '1001', '1000')
        sapply(1:length(input_vec), \(x) {
            new_val <- '0000'
            for(i in 1:length(summary_16)) {
                if(input_vec[x] >= summary_16[i]) new_val <- t_strings[i]
            }
            return(new_val)
        })
    }

    output_df <- data.frame(lapply(input_df[, -class_col], \(x) {
        if(class(x) == "numeric" || class(x) == "integer") {
            return(quartiles_slicer(x, 2))
        }

        return(x)
    }))

    names(output_df) <- paste0("rlcs_", names(output_df))

    output_df$class <- input_df[, class_col]
    output_df$state <- sapply(1:nrow(output_df), \(i) {
        state_val <- paste(output_df[i, which(names(output_df) != "class")], collapse="")
        state_val
    })
    output_df
}
