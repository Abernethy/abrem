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

bbb <- function(j,f,CL,beta,eta){
   # function to calculate Beta Binomial Confidence Bounds for B-lives.
   # j    : rank of failure
   # f    : number of failures (is NOT the same as sample size!) see
   #        'suspended items' fotr more info
   # CB   : Confidence Bound, Confidence Limit
   # beta : Weibull slope or scale parameter
   # eta  : Weibull shape paramater
   # see "The new Weibull handbook, fifth edition" p. 7-3
   # see "The new Weibull handbook, fifth edition" Appendix I
   # see also MS. Excel's and GNUmeric's BETAINV() function
   eta*(log(1/(1-qbeta(CL,j,f-j+1))))^(1/beta)}
