interlace <- function(vector_1, vector_2) {
    v1_len <- length(vector_1)
    v2_len <- length(vector_2)
    min_length <- min(v1_len, v2_len)
    i <- 0:(min_length * 2 - 1)

    index_sequence <- (i %% 2) * v1_len + (i %/% 2) + 1
    out_vector <- c(vector_1, vector_2)[index_sequence]

    if (v1_len > v2_len) {
        add_index <- (min_length + 1):(min_length + v1_len)
        out_vector <- c(out_vector, vector_1[add_index])
    } else if (v2_len > v1_len) {
        add_index <- (min_length + 1):v2_len
        out_vector <- c(out_vector, vector_2[add_index])
    }
    return(out_vector)
}