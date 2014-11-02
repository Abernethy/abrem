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

buildSingleDataLegend <- function(x,opadata,...){
    arg <- list(...)
    si <- function(number)signif(number,opadata$signif)
    li <- list()
    if(opadata$in.legend){
        li[[10]]    <- bsll(legend=paste0("ppos = ",opadata$ppos[1]),
            col=opadata$col,pch=opadata$pch,lwd=opadata$lwd.points)
        li[[15]]    <- bsll(legend=paste0("n (fail | susp.) = ",x$n,
            " (",x$fail," | ",x$susp,")"))
    }
    removeBadLegendEntries <- function(e){
        if(!is.null(e))!is.na(e$legend) else FALSE
    }
    if(length(li)>0){
        li <- li[sapply(li,removeBadLegendEntries)]
        ### Oct 2014:
        ### moved the following code inside the if(length(li)>0){statement
        fu  <- function(x,i){if(i %in% names(x))x[[i]]}
        fu2 <- function(i,x){lapply(x,fu,i=i)}
        items <- c("legend","lty","lwd","pch","col")
        le  <- lapply(items,fu2,li)
        names(le) <- items
        if(identical(label <- opadata$label,""))label <- NULL
        le$rect <- legend(
            "bottomright",
            legend=le$legend,
            title=label,
            cex = opadata$legend.text.size,
            plot=FALSE)$rect
        le$label <- opadata$label
        le$legend.text.size <- opadata$legend.text.size
    }else le <- NULL
    #else li <- ""
    # remove list items where the legend text = NA
    le
}
