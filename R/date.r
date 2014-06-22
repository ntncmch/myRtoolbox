#'Format date variables in a data frame
#'
#'This functions detects all date variables matching \code{pattern} and then use \code{\link[lubridate]{parse_date_time}} with specified \code{orders} to parse them as POSIXct date-time objects.
#' @param df_2format A data frame
#' @param pattern Vector of strings to match the name of date variables
#' @param year_max Numeric. If a year exceeds this value, one century is subtracted to it.
#' @param as_date Logical. If \code{TRUE} dates are converted to class \code{"Date"}.
#' @inheritParams lubridate::parse_date_time
#' @note The \code{year_max} arguments is to handle the fact that date origin in \R is "1970-01-01" so dates like "08-05-45" will be parsed as "2045-05-08" instead of "1945-05-08" 
#' @export
#' @importFrom  lubridate parse_date_time
#' @importFrom  lubridate year
#' @importFrom  lubridate year<-
#' @importFrom  lubridate is.Date
#' @return A data frame with dates formatted.
format_date <- function(df_2format, pattern = "date", orders = "dmy", year_max = NULL, as_date = FALSE) {

	var_names <- names(df_2format)
	var_date <- str_detect_multi_pattern(var_names,pattern,expression="ignore.case",value=TRUE)

	if (length(var_date)) {

		message("Format the following date variables:")
		
		for(var in var_date){
			
			message(var)

			x <- df_2format[[var]]

			if(is.Date(x) && as_date){
				next
			}

			if(all(is.na(x) | as.character(x)=="")){

				x[] <- NA
				
			} else {

				if(is.Date(x)){
					x <- parse_date_time(x,orders="ymd")					
				} else {
					x <- parse_date_time(x,orders)					
				}

				if(all(is.na(x))){
					message("All format failed to parse for: ",var)
					print(summary(df_2format[[var]]))
				}

				if(!is.null(year_max) && length(ind <- which(year(x) > year_max))){
					message("Substract 1 century to the following dates:\n",paste(as.character(x[ind]),collapse="\n"))
					year(x[ind]) <- year(x[ind]) - 100				
				}
			}

			if(as_date){
				x <- as.Date(x)
			}

			df_2format[[var]] <- x 

		}

	} else {
		message("No match to ",sQuote("pattern")," in the data frame\n")
	}

	message("\n")

	return(df_2format)
}



