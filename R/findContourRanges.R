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

contourRange <- function(MLEXContour){
    ra <- do.call("rbind",MLEXContour)
    data.frame(range(ra[,1]),range(ra[,2]))
}

findContourRanges <- function(x,v){
    # +---------------------------------------------------+
    # |  find absolute maximum and minimum contour range  |
    # |     over the (list of) abrem objects              |
    # +---------------------------------------------------+
    # x is always a list of abrem object(s)

    findrange1 <- function(abrem){
        if(!is.null(abrem$fit)){
            findrange2 <- function(fit){
                if(!is.null(fit$conf$blives)){
                    findrange3 <- function(blicon){
                        if(!is.null(blicon$MLEXContour)){
                            # a contour is available
                            #if(deb)mtrace(contourRange)
                            contourRange(blicon$MLEXContour)
                        }
                    }
                    #if(deb)mtrace(findrange3)
                    do.call("rbind",lapply(fit$conf$blives,findrange3))
                        # combine the ranges from all MLEXContours
                        # found in the list of blicons
                }
            }
            #if(deb)mtrace(findrange2)
            do.call("rbind",lapply(abrem$fit,findrange2))
                # combine the ranges from all MLEXContours
                # found in the list of fits
        }
    }
    #if(deb)mtrace(findrange1)
    do.call("rbind",lapply(x,findrange1))
}
