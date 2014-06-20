
#'Extract rows that differ between two data frames
#'
#'This function generalise the concept of \code{setdiff} to \code{data.frame}. Only rows of the columns present in both data frames are compared.
#' @param df1,df2 data frames
#' @export
diff_df <- function(df1, df2) {
	
	if(!nrow(df1)){return(df2)}
	if(!nrow(df2)){return(df1)}

	x <- intersect(names(df1),names(df2))
	
	df1$in_df1 <- TRUE
	df2$in_df2 <- TRUE

	res <- merge(df1, df2, all = T)

	res_diff <- subset(res, is.na(in_df1) | is.na(in_df2), select=x)
	
	return(res_diff)
}



#'Resolve duplicated rows of a data frame
#'
#'Resolve duplicated values of \code{var_duplicated} by removing \code{NA} values from other variables of the data frame \code{x} and merging similar values.
#' @param x data frame
#' @param var_duplicated variable that should be unique but that has potential duplicated values and thus need to be solved
#' @note Throw a warning if one of the other column can't be reduced because of different, non \code{NA}, values. The value is then merged by collapsing the values.
#' @export
#' @import plyr
resolve_duplicate <- function(x, var_duplicated) {

	df_unique <- ddply(x, var_duplicated, function(df) {
		
		if (nrow(df) > 1) {
			df2 <- data.frame(row.names=1,stringsAsFactors=FALSE)
			for (var in names(df)) {
				df_var <- df[, var]
				df2_var <- unique(df_var[!is.na(df_var)])
				if (length(df2_var) > 1) {
					warning("Can't resolve duplicate:",df2_var <- paste(df2_var, collapse = "/"))
				}
				df2[var] <- df2_var
			}
			return(df2)
		}

		return(df)
	})
	return(df_unique)
}
