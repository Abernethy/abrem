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

F0 <- function(q)
   1-exp(-exp(q))

F0inv <- function(p,log="x"){
    # transformation function to plot its argument
    # on the y-axis of the Weibull plot. This transformation function
    # lets the Weibull curve appear as a straight line on the weibull paper
    #
    # This is also the inverse Cumulative Distribution function of the
    # standardized Weibull plot with beta=eta=1
    # comparing  both implementationss of F0inv() with
    # system.time() does not show any significant difference
    #   log(log(1/(1-p)))}
    if(log %in% c("x",""))ret <- log(qweibull(p,1,1)) else ret <- qlnorm(p,0,1)
    ret
}