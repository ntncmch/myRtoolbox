#'Write and reinstall packages
#'
#'These functions should be run immediately before and after upgrading \R as they allow to save all installed packages and reinstall them after upgrading.
#'\itemize{
#'	\item \code{write_installed_packages} write the name of all installed packages in a in a csv file "intalled_packages.csv".
#'	\item \code{read_and_install_packages} read the file and intall all packages with dependencies.
#'}
#' @param dir path of the directory to write the file (default to current working directory).
#' @export
#' @name write-and-reinstall-packages
#' @aliases write_installed_packages
write_installed_packages <- function(dir=getwd()) {

	tmp <- installed.packages()
	tmp <- as.data.frame(tmp)

	write.csv(tmp,file.path(dir,"installed_packages.csv"),row.names=FALSE)
}

#' @param file path of the directory containing the file.
#' @export
#' @name write-and-reinstall-packages
#' @aliases read_and_install_packages
read_and_install_packages <- function(file) {

	tmp <- read.csv(file)
	install.packages(as.character(tmp$Package),dependencies=TRUE)

}
