#'Merge a list of objects
#'
#'Merge a list of objects recursively using \code{\link{merge}}
#' @param .list List of objects (usually \code{data.frame}) to be merged
#' @param  ... Further arguments to be passed to \code{\link{merge}}
#' @export
merge_rec <- function(.list, ...) {
	if (length(.list) == 1) 
		return(.list[[1]])
	Recall(c(list(merge(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
}

#'Intersect a list of vectors
#'
#'Intersect a list of vectors recursively using \code{\link{intersect}}
#' @param .list List of vectors to be intersected.
#' @export
intersect_rec <- function(.list) {
	if (length(.list) == 1) 
		return(.list[[1]])
	Recall(c(list(intersect(.list[[1]], .list[[2]])), .list[-(1:2)]))
}
