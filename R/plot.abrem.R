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

plot.abrem <- function(x,...){
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

    # +--------------------------------------+
    # |  dealing with  threshold parameters  |
    # +--------------------------------------+

    if(!is.null(list(...)$threshold))
        message("Currently, passing the \'threshold\' argument to plot.abrem is not supported. Proceeding...")
    
    # +--------------------------+
    # |  create new plot canvas  |
    # +--------------------------+
    if(tolower(opa$canvas)=="weibull") opa$log <- "x"
    if(tolower(opa$canvas)=="lognormal") opa$log <- "xy" 
        #   A character string describing the type of plotting canvas to use. 
        #   Possible values are:
        #       "x"   (weibull canvas)
        #       "xy"  (lognormal canvas)
        #       ""    (weibull Y axis, linear X axis)
        #       "y"   (lognormal Y axis, linear X axis).
    ra <- findMaxDataRange(x,opa$verbosity,opa$log)
        # NA values can be part of ra, when log scales are to be used
        # and there are negative failure times
    xlimits <- range(ra$xrange,na.rm=TRUE)
    ylimits <- range(ra$yrange,na.rm=TRUE)
    if(is.null(opa$xlim)){
        opa$xlim <- c(10^(log10(xlimits[1])-0.5),
            10^(log10(xlimits[2])+1))
    }
    if(is.null(opa$ylim)){
        opa$ylim <- c(0.01,0.99)
        if(ylimits[1] < 0.01 && ylimits[1] != -Inf) opa$ylim <- c(signif(ylimits[1],1),0.99)
        # do not care about the upper limit.
        # comparing to -Inf is needed for the case that no failures are in the event vectors,
        # like in the motors dataset fom package MASS for temp=150
    }
    opanames <- names(opa)
#    plotargs <- c(list(x=NA,axes=FALSE),opa[opanames %in% plot_default_args()])
    plotargs <- modifyList(opa[opanames %in% plot_default_args()],list(x=NA,axes=FALSE))
    if(!is.null(plotargs$ylim)){
        plotargs$ylim <- F0inv(plotargs$ylim,opa$log)
    }
    plotargs$main <- NULL
        # do not plot "main" just yet...
    if(!is.null(opa$mar))par(mar=opa$mar)
    if(!is.null(opa$mai))par(mai=opa$mai)
    do.call(plot.default,plotargs)
    if(opa$is.plot.grid){
        abline(
            h=F0inv(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10),opa$log),
                # TODO: the grid sometimes overplots the frame
            v=seq.log(opa$xlim[1]/10,opa$xlim[2]*10,seq(0,10,1)),
            col = opa$col.grid)
    }
    r <- seq.log(opa$xlim[1]/10,opa$xlim[2]*10,c(1,5))
    #lin <- 0.0
    for(t in c(1,3)){
    # TODO: rewrite as do.call() or apply()
        if(t %in% opa$axes){
            axis(t,at=seq.log(opa$xlim[1]/10,opa$xlim[2]*10,seq(0,10,0.2)),
                labels=NA,tcl=-0.25)#,line=0.0
                # plot top and bottom axis tickmarks
            axis(t,at=r,labels=r,tcl=-0.75)#,line=0.0
                # plot top and bottom axis labels
        }
    }
    r <- c(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10,c(1,2,5)),0.9)
    for(t in c(2,4)){
        if(t %in% opa$axes){
            axis(t,at=F0inv(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10),
                opa$log),labels=NA,tcl=-0.25)#,line=0.0
                # plot left and right axis tickmarks
            axis(t,at=F0inv(r,opa$log),
                labels=r*100,tcl=-0.75)#,line=0.0
                # plot left and right axis labels
        }else{
            #axis(t)
        }
    }
    abline(h=0,lty = 3,col = opa$col.grid)
        # plot the 63.2 [%] unreliability line
    title(main=opa$main,line=3)
    if(opa$frame.plot && opa$is.plot.grid)box()
        # because the grid overplots the earlier drawn frame...

    # +--------------------------+
    # |  plot confidence bounds  |
    # +--------------------------+
    lapply(x,plotConfsInAbrem,v=opa$verbosity,log=opa$log,...)

    # +-------------+
    # |  plot fits  |
    # +-------------+
    lapply(x,plotFitsInAbrem,v=opa$verbosity,xl=opa$xlim,yl=opa$ylim,log=opa$log,...)

    # +-----------------------------------+
    # |  plot probability plot positions  |
    # +-----------------------------------+
    lapply(x,plotSingleDataSet,opa$is.plot.ppp,log=opa$log,...)

    # +----------------+
    # |  plot legends  |
    # +----------------+
    lolegends <- NULL
    lolegends <- unlist(lapply(x,buildListOfLegends,
        v=opa$verbosity,
        isplotlegend=opa$is.plot.legend,...),FALSE)
        # TODO: likely, unlist is NOT the best way to go here, investigate
    lolegends <- lolegends[sapply(lolegends,function(lol)!is.null(lol))]
        # omit any list entries that contain only NULL
    if(!is.null(lolegends) && any(sapply(lolegends,function(lol)!is.null(lol)))){
        lx <- rep(lolegends[[1]]$rect$left,length(lolegends))
        ly <- lolegends[[1]]$rect$top +
            c(0,cumsum(sapply(lolegends,function(le)le$rect$h)[-1]))
        if(opa$log %in% c("x","xy","yx")) lx <- 10^lx
        if(opa$log %in% c("y","xy","yx")) ly <- 10^ly
            # TODO: F0(ly): looks very suspicious that this works -> investigate!
        for(i in 1:length(lolegends)){
            plotSingleLegend(lolegends[[i]],lx[i],ly[i])
            # TODO: replace with lapply
        }
    }else{
        if(opa$verbosity >= 1)message(
            "plot.abrem: There is no legend to plot.")
    }
    if(tolower(opa$canvas) == "weibull")
        legend("topleft",legend=NA,bg="white",cex=0.3,
            pch="W",inset=0,xpd=F)
    if(tolower(opa$canvas) == "lognormal")
        legend("topleft",legend=NA,bg="white",cex=0.3,
            pch="L",inset=0,xpd=F)
#    if(opa$log %in% c("","y")) legend("top",legend=NA,title="xxx",bg="white")
    invisible()
        # TODO: return the abrem object with updated graphical options
        # TODO: check if this makes sense when supplying a list
}
