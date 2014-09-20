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

plotSingleConfBound <- function(blc,opafit,opadatathreshold,...){
    if(!is.null(blc$options)){
        opaconf <- modifyList(opafit,blc$options)
    }else{opaconf <- opafit}
    opaconf <- modifyList(opaconf,list(...))
    if(opaconf$is.plot.cb){
        t0 <- 0
        if(is.logical(opadatathreshold))if(opafit$threshold)
            warning ("opadata$threshold is a logical value but numeric value was expected. Proceeding...")
        if(is.numeric(opadatathreshold))t0 <- opadatathreshold
            # efffectively ignore any threshold argument set at the conf level           

        if(!is.null(blc$bounds$datum))
            lines(y=F0inv(blc$bounds$unrel,opaconf$log),
                x=blc$bounds$datum-t0,
                col=opaconf$col,lwd=1,lty=2)
        if(!is.null(blc$bounds$lower))
            lines(y=F0inv(blc$bounds$unrel,opaconf$log),
                x=blc$bounds$lower-t0,col=opaconf$col,
                lwd=opaconf$lwd,lty=opaconf$lty)
        if(!is.null(blc$bounds$upper))
            lines(y=F0inv(blc$bounds$unrel,opaconf$log),
                x=blc$bounds$upper-t0,col=opaconf$col,
                lwd=opaconf$lwd,lty=opaconf$lty)
    }
}
