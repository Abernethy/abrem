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

plotSingleDataSet <- function(x,isplotppp,...){
    if(isplotppp){
        # TODO: possibly, this does not allow much flexibility in plotting.
        opadata <- modifyList(x$options,list(...))
        if(!is.null(x$data) &&
            !is.null(ti <- x$data$time) &&
            !is.null(ra <- x$data[,paste0("ppp.",tolower(opadata$ppos[1]))])){
            t0 <- 0
            if(is.logical(opadata$threshold))if(opadata$threshold)
                warning ("opadata$threshold is a logical value but numeric value was expected. Proceeding...")
            if(is.numeric(opadata$threshold))t0 <- opadata$threshold
            points(ti-t0,F0inv(ra,opadata$log),pch = opadata$pch,
                col = opadata$col,lwd = opadata$lwd.points,cex=opadata$cex.points)
        }else{
            stop("This Abrem object contains no probability plot positions.")
        }
    }
}