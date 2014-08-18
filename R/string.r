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

#'Compare two strings
#'
#'Compare \code{a} and \code{b} after removing all digits, space and & characters.
#'Return \code{TRUE} if \code{a} is a sub-string of \code{b} (or vice-versa).
#' @param a,b two strings
#' @export
#' @import stringr
str_compare <- function(a,b) {

	# lower case & remove digits & space & "_"
	a <- remove_char(tolower(a), char=c(0:10," ","_"))
	b <- remove_char(tolower(b), char=c(0:10," ","_"))

	return(str_detect(fixed(a),fixed(b)) | str_detect(fixed(b),fixed(a)))

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

#'Extract a substring
#'
#'Extract the part of \code{v_string} delimited by \code{my.split}, at position \code{position} from begining or end of \code{v_string}
#' @param v_string a vector of strings
#' @param my_split a character to delimite the substring to extract
#' @param position nuleric, position of the substring to be extracted
#' @param from character, \code{position} can be given from either the start (\code{first}) or end (\code{last}) of the \code{v_string}.
#' @export
extract_string <- function(v_string,my_split,position,from=c("first","last")){

	from <- match.arg(from)
	
	res <- sapply(v_string,function(x){

		tmp <- strsplit(as.character(x),split=my_split,fixed=TRUE)[[1]]
		
		if(from=="last"){
			position=length(tmp)-position+1
		}

		return(tmp[position])
	})

	return(res)
}



#'Capitalize every first letter of a word
#'
#'Split the word at each space (\code{" "}) and capitalize every sub-word.
#' @param words a vector of strings
#' @export
simpleCap <- function(words) {
	ans <- sapply(words, function(word) {
		if (is.na(word)) {
			return(word)
		}
		s <- strsplit(word, " ")[[1]]
		paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
	})
	return(ans)
}


#'Capitalize every first word letter of a string
#'
#'Split the string into words (must be separated by a signle space \code{" "}) and capitalize every word.
#' @param string a vector of string of characters.
#' @param strict logical. If \code{FALSE} (default) the case of the letters inside the words is not modified. If \code{TRUE}, all letters inside a word are lower case.
#' @export
capwords <- function(string, strict = FALSE) {

	cap <- function(s) { 
		paste(toupper(substring(s, 1, 1)), {s <- substring(s, 2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
	}

	sapply(strsplit(string, split = " "), cap, USE.NAMES = !is.null(names(string)))
}




