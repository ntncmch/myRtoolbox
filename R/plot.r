#'Viewport wrapper
#'
#' @param x row
#' @param y column
#' @export
#' @import grid
vplayout <- function(x, y){
	viewport(layout.pos.row = x, layout.pos.col = y)
}


#'Add letter
#'
#'Add letter to a ggplot
#' @param letter character
#' @param margin vector of size 4
#' @inheritParams ggplot2::element_text
#' @export
#' @import ggplot2
ggletter <- function(letter, margin=c(0,0.2,0,0), size= 20, hjust= -0.075, vjust = -0.1) {
	ggtitle(letter) + theme(plot.title = element_text(size = size, hjust = hjust, vjust = vjust, face = "bold"), plot.margin = unit(margin,"cm"))
}