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

plotSingleLegend <- function(le,x,y){
    if(identical(label <- le$label,""))label <- NULL
    if(is.null(le$legend))le$legend <- ""
#    if(is.null(le$legend) || any(sapply(le$legend,function(fi)is.null(fi)))){
#        le$legend <- ""
#    }    
    legend(
        x=x,
        y=y,
        legend=le$legend,
        title=label,
        cex = le$legend.text.size,
        bg = "white",
        lty = unlist(le$lty),
        lwd = unlist(le$lwd),
        pch = unlist(le$pch),
        col = unlist(le$col),
        text.col = "black",
        xpd=TRUE
        )
        # TODO: Warning: unlist coerces numeric colors to character!
        # maybe as.graphicsAnnot to the rescue?
}