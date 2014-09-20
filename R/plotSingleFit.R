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
#
plotSingleFit <- function(fit,v,opadata,...){
    opafit <- opadata
    if(!is.null(fit$options)){
        opafit <- modifyList(opadata,fit$options)
    }
    opafit <- modifyList(opafit,list(...))
    if(opafit$is.plot.fit){
        t0 <- 0
        if(is.logical(opafit$threshold))if(opafit$threshold){
            if(is.logical(opadata$threshold)){if(opadata$threshold)
                warning("opafit$threshold and opadata$threshold are logical values but numeric values were expected. Proceeding...")
            }else{
                # reuse the t0 value from the data level
                t0 <- opadata$threshold
            } 
        }          
        if(is.numeric(opafit$threshold))t0 <- opafit$threshold
        if(v >= 1)message("plotSingleFit: Adding fit...")
        fitt0 <- 0
        cret <- NULL
        if(!is.null(fit$t0)) fitt0 <- fit$t0
        if(!is.null(fit$beta) && !is.null(fit$eta))
            cret <- curve(F0inv(pweibull(x-fitt0+t0,fit$beta,fit$eta),opafit$log),
                add=TRUE,n=1001,
                    # n=1001 is needed for displaying the extreme
                    # curvature towards -Inf with low Beta values
                    # like 0.1
                col=opafit$col,lwd=opafit$lwd,lty=opafit$lty,
                xlim=getPlotRangeX(opafit$log),
                log=opafit$log)                
        if(!is.null(fit$mulog) && !is.null(fit$sigmalog))
            cret <- curve(F0inv(plnorm(x-fitt0+t0,fit$mulog,fit$sigmalog),opafit$log),
                add=TRUE,n=1001,
                col=opafit$col,lwd=opafit$lwd,lty=opafit$lty,
                xlim=getPlotRangeX(opafit$log),
                log=opafit$log)
        if(!is.null(cret)){
            cret$y[is.infinite(cret$y)] <- NA
                # works for weibull canvas
            cret$y[cret$y==0] <- NA
                # replacing zero's is needed for lognormal canvas.
            imin <- which.min(cret$y)
            lines(rep(cret$x[imin],2),
                y=c(cret$y[imin],getPlotRangeY(opafit$log)[1]),
                col=opafit$col,lwd=opafit$lwd,lty=opafit$lty)
                # plot vertical line towards -Inf
        }else{
            if(opafit$verbosity >= 1)message("plotSingleFit: No distribution parameters available for plotting the fit...")
        }
#        if(!is.null(fit$rate)){
#            ### exponential ###
#            if(opafit$verbosity >= 1)message(
#                "plotSingleFit: Adding Exponential fit ...")
#            curve(F0inv(pexp(x+t0,fit$rate),opafit$log),add=TRUE,
#                col=opafit$col,lwd=opafit$lwd,lty=opafit$lty,
#                xlim=getPlotRangeX(opafit$log),
#                log=opafit$log)
#        }
    }
    invisible()
}