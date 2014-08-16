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




#'Date axis in selected panels of a facet wrap
#'
#'This function allows to transform the numeric axis labels of some panels into date fomat. 
#' @param gplot a \code{ggplot} or \code{grob} object.
#' @param  panel numeric vector. Which panels should have date axis. Panels are numeroted from left to right and top to bottom.
#' @param  date_0 character. Date corresponding to the value 0 of the axis.
#' @param format character. The default for the format methods is \code{"\%b \%d"}, see \code{\link{strptime}} for other suitable \code{format}.
#' @param  n_ticks_max numeric. Maximum number of ticks on the axis. This is useful when the original axis has too many ticks and date labels overlap.
#' @export
#' @import gridExtra ggplot2
facet_wrap_date_waiver <- function(gplot, panel, date_0, format="%b %d", n_ticks_max=NULL) {

	gg <- gplot$grobs
	if(is.null(gg)){
		gplot <- ggplotGrob(gplot)
		gg <- gplot$grobs   
	}

	axis <- grep("axis_b", names(gg))

	for(ii in panel)  {

		x <- gg[[axis[ii]]]$children$axis$grobs[[2]]$label

		if(!is.null(n_ticks_max) && length(x)>n_ticks_max){
			# recompute ticks and labels
			x_old <- as.numeric(x)
			x <- seq(min(x_old),max(x_old),length=n_ticks_max)

			x_ticks <- gg[[axis[ii]]]$children$axis$grobs[[1]]$x			
			x_ticks_old <- unique(as.numeric(x_ticks))
			x_ticks <- approx(x_old,x_ticks_old,x)$y
			gg[[axis[ii]]]$children$axis$grobs[[1]]$x <- unit(rep(x_ticks,each=2),"native")
			
			y_ticks <- gg[[axis[ii]]]$children$axis$grobs[[1]]$y
			gg[[axis[ii]]]$children$axis$grobs[[1]]$y <- y_ticks[1:(2*n_ticks_max)]

			gg[[axis[ii]]]$children$axis$grobs[[2]]$x <- unit(x_ticks,"native")
		}

		x <- as.Date(date_0) + as.numeric(x)
		
		gg[[axis[ii]]]$children$axis$grobs[[2]]$label <- format(x, format)

	}

	gplot$grobs <- gg
	class(gplot) = c("arrange", "ggplot",class(gplot)) 
	
	return(gplot)
}



#'Change strip texts of a facet wrap
#'
#'This function allows to rename the strip of a facet wrap.
#' @param labels character vector. Names of the strips. Strips are renamed in the same order as they are plotted, from left to right and top to bottom.
#' @inheritParams facet_wrap_date_waiver
#' @export
#' @import gridExtra ggplot2
facet_wrap_labeller <- function(gplot,labels=NULL) {

	gg <- gplot$grobs
	if(is.null(gg)){
		gplot <- ggplotGrob(gplot)
		gg <- gplot$grobs   
	}

	strips <- grep("strip_t", names(gg))

	for(ii in seq_along(labels))  {
		modgrob <- getGrob(gg[[strips[ii]]], "strip.text", grep=TRUE, global=TRUE)
		gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
	}

	gplot$grobs <- gg
	class(gplot) = c("arrange", "ggplot",class(gplot)) 
	
	return(gplot)
}
