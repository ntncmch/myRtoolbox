#'Basic accounting for NA values
#'
#'All these functions performs the same as the basic version (same name without \code{_na} at the end) but differ when all elements are \code{NA}. In this case, \code{NA} is returned in the same format as \code{x}, which is not always the case when the basic function is used with \code{\link[dplyr]{summarize}}.
#' @param x any object accepted by the basic function.
#' @param na_rm logical. If \code{TRUE}, \code{\link{na.omit}} is performed first. Default to \code{FALSE}
#' @export
#' @importFrom dplyr last
#' @name summarize with NA
#' @export
#' @aliases last_na
last_na <- function(x,na_rm=FALSE) {

	if(all(is.na(x))){
		return(x[1])
	} 

	if(na_rm){
		x <- na.omit(x)
	}
	
	return(last(x))	
}


#' @name summarize with NA
#' @export
#' @aliases all_na
all_na <- function(x,na_rm=FALSE) {

	if(all(is.na(x))){
		return(x[1])
	} 

	return(all(x,na.rm=na_rm))
}

#' @name summarize with NA
#' @export
#' @aliases any_na
any_na <- function(x,na_rm=FALSE) {

	if(all(is.na(x))){
		return(x[1])
	} 

	return(any(x,na.rm=na_rm))
}



#' @name summarize with NA
#' @export
#' @importFrom dplyr first
#' @aliases first_na
first_na <- function(x,na_rm=FALSE) {

	if(all(is.na(x))){
		return(x[1])
	} 

	if(na_rm){
		x <- na.omit(x)
	}
	
	return(first(x))	

}

#' @name summarize with NA
#' @export
#' @aliases max_na
max_na <- function(x,na_rm=FALSE) {

	if(all(is.na(x))){
		return(x[1])
	} 

	if(na_rm){
		x <- na.omit(x)
	}
	
	return(max(x))	

}


#' @name summarize with NA
#' @export
#' @aliases min_na
min_na <- function(x,na_rm=FALSE) {

	if(all(is.na(x))){
		return(x[1])
	} 

	if(na_rm){
		x <- na.omit(x)
	}
	
	return(min(x))	

}



