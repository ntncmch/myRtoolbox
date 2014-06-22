
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
					warning("Can't resolve duplicate:",df2_var <- paste(df2_var, collapse = "/"),.call=FALSE)
				}
				df2[var] <- df2_var
			}
			return(df2)
		}

		return(df)
	})
	return(df_unique)
}



#'Extract from a data frame using prefix
#'
#'Extract only variables with a given \code{prefix}, allow for keeping variables without \code{prefix} by passing their names in \code{keep}.
#' @param df a data frame
#' @param  prefix character, prefix of the variables to be extracted
#' @param  keep character vector, specify variables without \code{prefix} to be returned
#' @export
#' @import plyr stringr
#' @return a \code{data.frame} with the variables renamed without the \code{prefix}.
extract_by_prefix <- function(df, prefix, keep=NULL) {

	df_sub_names <- grep(prefix,names(df),value=TRUE)
	df_sub <- df[c(keep,df_sub_names)]
	new_names <- names(df_sub)
	new_names <- unlist(llply(str_split(new_names,pattern=prefix),function(x) {rev(x)[1]}))	
	names(df_sub) <- new_names	

	return(df_sub)
}

