#'Remove characters from a string
#'
#'This function removes \code{char} from \code{x} and returns the new \code{x}. Vectorised over \code{x} and \code{char}.
#' @param x Vector of strings
#' @param char Vector of characters
#' @export
#' @import stringr
remove_char <- function(x,char=c(0:10," ","_")) {			
	
	char <- as.character(char)

	x <- str_split(x,"")
	
	x <- unlist(lapply(x,function(y) {str_trim(paste(y[!y%in%char],collapse=""))}))
	
	return(x)
}



#'Reduce duplicated characters in a string
#'
#'This function reduces several consecutive occurences of \code{char} in \code{x} to a single occurence of \code{char}. Vectorised over \code{x} and \code{char}.
#' @inheritParams remove_char
#' @export
#' @import stringr
#' @examples 
#' x <- c("toto_1__2___3....4","toto..2___3")
#' char <- c("_",".")
#' reduce_duplicated_char(x,char)
reduce_duplicated_char <- function(x,char="_") {

	for(my_char in char){

		# detect multiple . and replace by _
		pattern <- my_char
		while(any(str_detect(x,fixed(pattern)))){
			pattern <- str_join(pattern,my_char)
		}
		pattern <- str_sub(pattern,2L)
		while(str_length(pattern)){
			x <- str_replace_all(x,fixed(pattern),my_char)	
			pattern <- str_sub(pattern,2L)
		}
	}

	return(x)

}

#'Detect the presence or absence of pattern in a string
#'
#'This function does the same as \code{\link[stringr]{str_detect}} but allow for multiple pattern match and can return the values of matched elements.
#' @param  multi_pattern vector of patterns to look for.
#' @param  expression type of matching: POSIX regular expression (default), fixed, case insensitive and perl-compatible. 
#' @param  value  logical, if \code{TRUE} then the values of matched elements are returned. 
#' @inheritParams stringr::str_detect
#' @export
#' @import stringr
str_detect_multi_pattern <- function(string, multi_pattern, expression = c("regexp","fixed","ignore.case","perl"), value = FALSE) {

	expression <- match.arg(expression)

	x <- rep(FALSE,length(string))

	for(pattern in multi_pattern){

		if(expression == "regexp"){
			x <- x | str_detect(string,pattern)
		}else{
			x <- x | str_detect(string,do.call(expression,list(pattern)))			
		}
	}

	if(value){
		x <- string[x]
	}

	return(x)
}




