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

abrem.fit <- function(x,...){
    # x is a single Abrem or a list of Abrem objects
    supported_dist <- c(
        "weibull","weibull2p","weibull3p",
        "lognormal","lognormal2p","lognormal3p")
    # supported_fit <-  c("rr","rr2","mle","mle2","mle3","mle-rba","mle2-rba","mle3-rba")
    supported_fit <-  c("rr","mle","mle-rba")
    if(missing(x)){
        stop("Argument \"x\" is missing.")
    }else{
        if(identical(class(x),"abrem")) x <- list(x)
        if(!all(sapply(x,function(x)identical(class(x),"abrem")))){
            stop("Argument \"x\" is not of class \"abrem\" or ",
            "a list of \"abrem\" objects.")
        }
        # from here on, x is a list of one or more abrem objects
        if(!is.null(x[[1]]$options)){
            opa <- x[[1]]$options
            # use the options of the first abrem object in the list
        }else stop("Abrem object has not options set.")
        opa <- modifyList(opa, list(...))
        if(is.null(opa$dist)){
            if(opa$verbosity >= 1)message("abrem.fit : ",
                "Target distribution defaults to weibull2p.")
                opa$dist <- "weibull2p"
        }else{
            if(length(opa$dist)>1)
                stop("Too many target distributions supplied.")
            if(!any(tolower(opa$dist) %in% supported_dist)){
                if(any(tolower(opa$dist) %in% c("gumbel"))){
                    stop("Gumbel is not yet a supported target fit method.")
                }else{
                    stop(paste0(opa$dist," is not a supported target fit method."))
                }
            }else{
                if(is.null(opa$method.fit)){
                    if(opa$verbosity >= 1)message("abrem.fit : ",
                        'Fit method defaults to \"rr\", \"xony\".')
                        opa$method.fit <- c("rr","xony")
                }else{
                    fits <- length(which(opa$method.fit %in% supported_fit))
                    if(fits > 1){
                        stop("Only one fit method should be supplied.")
                    }else{
                        if("rr" %in% tolower(opa$method.fit)){
                            if(!any(c("xony","yonx") %in%
                                tolower(opa$method.fit))){
                                if(opa$verbosity >= 1){
                                    message("abrem.fit : ",
                                        'Fit method \"rr\" defaults to \"xony\"')
                                    opa$method.fit <- c(opa$method.fit,"xony")
                                }
                            }
                        }
                        x <- lapply(x,calculateSingleFit,...)
                    }
                }
            }
        }
    }
    if(length(x)==1) x[[1]] else x
        # return list of abrem objects when argument x was a list
        # otherwise return single abrem object
}