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

findRangeInAbrem <- function(abrem,v,log=""){
        if(!is.null(abrem$data)){
            if(!is.null(abrem$data$time)){
                ret <- data.frame(xrange=range(abrem$data$time,na.rm=TRUE))
            }else{
                stop("$data contains no \"$time\" column -> ",
                    "cannot create plot canvas.")
            }
            if(length(pppcolumns <- grep("ppp",names(abrem$data))) >= 1){
                ret <- cbind(ret,yrange=range(abrem$data[,pppcolumns],na.rm=TRUE))
            }else{
                stop("$data contains no ppp column(s) -> ",
                    "cannot create plot canvas.")
            }
        }else{stop('Argument \"x\" contains no \"$data\" dataframe.')}
        ret
    }