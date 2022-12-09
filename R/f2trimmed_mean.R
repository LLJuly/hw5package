#' Function of trimmed mean
#'
#'This function is used to calculate trimmed mean
#'
#'@param num_vec the numeric vector
#'@param s_smallest number of smallest numbers ignored
#'@param l_largest  number of largest numbers ignored
#'
#'@keywords trimmed mean
#'
#'@return the calculated trimmed mean
#'
#'@examples
#'f2trimmed_mean(c(1,7,3,2,5,0.5,9,10),1,2)
#'
#'@export
#'
#'


f2trimmed_mean <- function(num_vec, s_smallest, l_largest){
  if (length(num_vec) < s_smallest + l_largest + 1) {
    stop("error: The vector does not have enough elements.")
  } else {
    # Sort the vector ascendingly
    num_vec_sorted  <- sort(num_vec)
    # Dicard the s smallest numbers and l largest numbers
    num_vec_trimmed <- num_vec_sorted[(s_smallest + 1) : (length(num_vec_sorted) - l_largest)]
    trimmed_mean    <- mean(num_vec_trimmed)
    return(trimmed_mean)
  }
}

#f2trimmed_mean(c(1,7,3,2,5,0.5,9,10),1,2)
# return 3.6
