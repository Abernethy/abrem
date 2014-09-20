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

Blifestring <- function(B,blicon,signif,...){
    # This functions creates a string for displaying the B-lives in the plot's
    # legend. missing input data result in an "NA". For example, the output
    # string could look like:
    #   "B10 = 9.86 | 50.13 | 103.4"
    # or
    #   "B1 = 9.86 | 50.13 | NA"
    si <- function(number)
        if(!is.null(number))signif(number,signif)
        else NA
      # shorthand writing of the signif() function
    qfun <- function(B,...){
        args <- as.list(unlist(...))

        ret <- NULL
        if(!is.null(args$beta) && !is.null(args$eta)){
            # the fit type was weibull
            ret <- qweibull(B,args$beta,args$eta)
            if(!is.null(args$t0)){
                # the fit type was weibull3p
                ret <- ret+args$t0

            }
        }
        if(!is.null(args$mulog) && !is.null(args$sigmalog)){
            # the fit type was lognormal
            ret <- qlnorm(B,args$mulog,args$sigmalog)
        }
        if(!is.null(args$rate)){
            # the fit type was exponential
            ret <- qexp(B,args$rate)
        }
        ret
    }
    id <- function(x,y)isTRUE(all.equal(x,y))
    c1 <- is.null(blicon$bounds) || is.null(blicon$bounds$lower)
    if(!c1) lo <- si(subset(blicon$bounds,
        sapply(blicon$bounds$unrel,id,B),lower))
    c2 <- is.null(blicon$bounds) || is.null(blicon$bounds$upper)
    if(!c2) up <- si(subset(blicon$bounds,
        sapply(blicon$bounds$unrel,id,B),upper))
    ret <- paste(sep = "","    B",signif(100*B)," = ",
        ifelse(c1,
           "NA",lo),
        " | ",si(qfun(B,...)),
        " | ",ifelse(c2,
           "NA",up))
    ret
#    NA
}
