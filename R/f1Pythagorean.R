#' Function of Pythagorean theorem
#'
#'This function is used calculating the length of the third side,
#'given the lengths of two sides of the triangle.
#'
#'@param a the length of right-angle side
#'@param b the length of right-angle side
#'@param c the length of side opposite the right angle
#'
#'@keywords length of triangle
#'
#'@return the length of third side of this triangle
#'
#'@examples
#'f1Pythagorean(a=3,b=4)
#'
#'@export
#'
#'



f1Pythagorean   <- function(a = NULL,b = NULL,c =NULL){
  value_enter     <- c(a, b, c)
  if (!is.numeric(value_enter)){                                                                #non numeric values entered
    stop("error: Please enter numeric values")
  } else if (is.null(a) & is.null(b) || is.null(a) & is.null(c) || is.null(b) & is.null(c) ){   #less than two value entered
    stop("error: Please enter two values")
  } else if(!is.null(a) & !is.null(b) & !is.null(c)){                                           #three values entered
    stop("error: Please enter only two values")
  } else if (is.null(a)){
    a = sqrt(c*c - b*b)
    return(a)
  } else if (is.null(b)){
    b = sqrt(c*c - a*a)
    return(b)
  } else if (is.null(c)){
    c = sqrt(a*a + b*b)
    return(c)
  }

}


# f1Pythagorean(a=3,b=4)












