# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# August 2014, Jurgen Symynck
# Copyright 2014, Jurgen Symynck
#
# For more info, visit http://www.openreliability.org/
#
# For the latest version of this file, check the Subversion repository at
# http://r-forge.r-project.org/projects/abernethy/
#
# Disclaimer:
#    The author is not affiliated with Dr. Abernethy or Wes Fulton - CEO of
#    Fulton Findings(TM) and author of the software package SuperSMITH
#-------------------------------------------------------------------------------
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# +-----------------------------------+
# |  execute this software with R:    |
# |  http://www.r-project.org/        |
# +-----------------------------------+

contour.abrem <- function(x,...){
    # +------------------------------+
    # |  move abrem objects to list  |
    # |      of abrem objects        |
    # +------------------------------+
    if(identical(class(x),"abrem")) x <- list(x)
    if(!all(sapply(x,function(x)identical(class(x),"abrem")))){
        stop("Argument \"x\" is not of class \"abrem\" or ",
        "a list of \"abrem\" objects.")
    }
    # as of this point, x is always a list of one or more abrem objects
    
    # +------------------------------------+
    # |  create default options arguments  |
    # +------------------------------------+
    opa <- x[[1]]$options
    opa <- modifyList(opa, list(...))


    # +--------------------------+
    # |  create new plot canvas  |
    # +--------------------------+
    contourRanges <- findContourRanges(x,opa$verbosity)
    if(!is.null(contourRanges)){
        xlimits <- range(contourRanges[,1])
        ylimits <- range(contourRanges[,2])
        opanames <- names(opa)
        plotargs <- modifyList(opa[opanames %in% plot_default_args()],list(x=NA,axes=TRUE))
        plotargs$xlim <- xlimits
        plotargs$ylim <- ylimits
        plotargs$main <- opa$main.contour
        plotargs$sub  <- opa$sub.contour
        plotargs$log  <- ""
        plotargs$xlab <- "Eta"
        plotargs$ylab <- "Beta"

        do.call("plot.default",plotargs)
        if(opa$is.plot.grid){
            abline(
                h=pretty(contourRanges[,2],10),
                v=pretty(contourRanges[,1],10),
                col = opa$col.grid)
                # TODO: add userchoice in grid density here
        }
    }else message("contour.abrem: No contours available in (list of) abrem objects.")
#    r <- seq.log(opa$xlim[1]/10,opa$xlim[2]*10,c(1,5))

    # +------------------+
    # |  plot contours   |
    # +------------------+
    plotContours <- function(abrem){
        if(!is.null(abrem$fit)){
            plotContours2 <- function(fit){
                if(!is.null(fit$options)){
                    opafit <- modifyList(abrem$options,fit$options)
                }else{opafit <- abrem$options}
                is_MLE <- any(c("mle","mle-rba") %in% tolower(fit$options$method.fit))
                if(!is.null(fit$conf$blives)){
                    plotContours3 <- function(blicon){
                        if(!is.null(blicon$options)){
                            opaconf <- modifyList(opafit,blicon$options)
                        }else{opaconf <- opafit}
                        opaconf <- modifyList(opaconf,list(...))
                        if(!is.null(blicon$MLEXContour)){
                            if(!is_MLE)
                                points(blicon$MLEXContour$MLEpoint,pch=20,col=abrem$options$col)
                                # if MLE or MLE-RBA was used to calculate the distribution
                                # parameters, the omit plotting the MLEpoint. In all other cases,
                                # plot the MLEpoint because the distribution parameters will not match exactly
                                # the MLE point
                            if(all(c(!is.null(fit$beta),!is.null(fit$eta))))
                                points(x=fit$eta,y=fit$beta,pch=abrem$options$pch,col=abrem$options$col,
                                    lwd=abrem$options$lwd.points,cex=abrem$options$cex.points)
                            points(blicon$MLEXContour[[1]],type="l",lwd=opaconf$lwd,lty=opaconf$lty,col=opaconf$col)
                        }
                    }
                    #mtrace(plotContours3)
                    do.call("rbind",lapply(fit$conf$blives,plotContours3))
                        # combine the ranges from all MLEXContours
                        # found in the list of blicons
                }
            }
            do.call("rbind",lapply(abrem$fit,plotContours2))
                # combine the ranges from all MLEXContours
                # found in the list of fits
        }
    }
    if(!is.null(contourRanges)) lapply(x,plotContours)
    invisible()
}
