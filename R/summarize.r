#'Basic accounting for NA values
#'
#'All these functions performs the same as the basic version but first remove all \code{NA} elements. If all elements are \code{NA} then \code{NA} is returned, which is not always the case for the basic function.
#' @param x vector
#' @export
#' @importFrom dplyr last
#' @name NA omit and summarize 
#' @export
#' @aliases last_na_omit
last_na_omit <- function(x) {

	if(all(is.na(x))){
		x <- x[1]
	} else {
		x <- last(na.omit(x))
	}

	return(x)
}


#' @name NA omit and summarize
#' @export
#' @aliases any_na_omit
any_na_omit <- function(x) {
	any(x,na.rm=!all(is.na(x)))
}