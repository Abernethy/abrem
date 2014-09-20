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

params.to.ob <- function(dist, ... ){
    # function to generate test data that result in a perfect fit
    # (when using RR; X-on-Y, median plot positions)
    # beta,eta:  slope and shape parameters of 2 parameter Weibull
    # event: either an integer with the number of complete observations, or
    # a vector with censoring information (e.g.: c(1,1,1,0,0,0,1)

    opa <- options.abrem()
    opa <- modifyList(opa, list(...))
    if(!missing(dist)){
        if(!is.null(opa$n)){
            if(opa$n >= 2){
                if(is.null(opa$event)){
                    opa$event <- rep(1,opa$n)
                }else{
                    stop("Either Argument \"n\" or \"event\" ",
                        "should be supplied, not both.")
                }
            }else{
                stop("Argument \"n\" (number of failures) must be ",
                    "at least 2.")
            }
        }
        ### assuming opa$event is present
        if(!is.null(opa$event)){
            if(!(length(opa$event) >= 2)){
                stop("Argument \"n\" (number of failures) must be ",
                "at least 2.")
            }
        }else{
            stop("Argument \"event\" should have length of at least 2.")
        }
        if(all(opa$event==0)){
            stop("Argument \"event\"contains only censored events.")
        }
        if(any(c("rr","rr2") %in% tolower(opa$method.fit))){
            # TODO: currently, only the "rr2" method is supported. 
            # Should result in the same numbers though...
            # TODO: expand with the new abremPivotal argument "aranks"
            if(tolower(dist) %in% c("weibull","weibull2p")){
                if(!is.null(opa$beta) && !is.null(opa$eta)){
                    if(!is.null(opa$ppos)){
                        ppp <- rep(NA,length(opa$event))
                        ppp[opa$event==1] <- abremPivotals::getPPP(x=opa$event,ppos=opa$ppos[1])$ppp
                            # the above call SHOULD be correct (compated with output from pivotals
                            # 'mis'using which for getting timelike values from the event vector
                            # no need to use the real lifetime observations here,
                            # just getting the ppp
                        ret <- data.frame(time=qweibull(ppp,opa$beta,opa$eta),event=opa$event)
                            # a good thing that qweibull deals nicely with NA's!
                    }else{
                        stop("Argument \"ppos\" is missing; no ranking method supplied.")
                    }
                }else{stop("Arguments \"beta\" and/or \"eta\" not supplied.")}
            }
            if(tolower(dist) %in% c("lognormal","lognormal2p")){
                if(!is.null(opa$mulog) && !is.null(opa$sigmalog)){
                    if(!is.null(opa$ppos)){
                        ppp <- rep(NA,length(opa$event))
                        ppp[opa$event==1] <- abremPivotals::getPPP(x=which(opa$event==1),#s=which(opa$event==0),
                        ppos=opa$ppos[1])$ppp
                        ret <- data.frame(time=qlnorm(ppp,opa$mulog,opa$sigmalog),event=opa$event)
                    }else{
                        stop("Argument \"ppos\" is missing; no ranking method supplied.")
                    }
                }else{stop("Arguments \"mulog\" and/or \"sigmalog\" not supplied.")}
            }
        }else{
            stop("Currently, only rank regression is supported.")
            ret <- NULL
        }
        ret
    }else{
        stop("Argument \"dist\" is missing.")
        ret <- NULL
    }
}
