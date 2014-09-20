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

bsll <- function(...){
    arg <- list(...)
    leline <- list(
        legend= NA,
        lty= NA,
        lwd= NA,
        pch= NA,
        col= NA)
    modifyList(leline,arg)
#    leline <- list(
#        legend= <- ifelse(is.null(arg$legend),NA,arg$legend)
#        title= <- ifelse(is.null(arg$title),NA,arg$title)
#        cex= <- ifelse(is.null(arg$cex),NA,arg$cex)
#        bg= <- ifelse(is.null(arg$bg),NA,arg$bg)
#        lty= <- ifelse(is.null(arg$lty),NA,arg$lty)
#        lwd= <- ifelse(is.null(arg$lwd),NA,arg$lwd)
#        pch= <- ifelse(is.null(arg$pch),NA,arg$pch)
#        col= <- ifelse(is.null(arg$col),NA,arg$col)
}
