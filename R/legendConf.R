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
#
legendConf <- function(fit,conftype,opadata,...){
    if(!is.null(fit)){
        # TODO: what happens when trying to plot superSMITH confidence bounds
        # when no fit data are available?
        if(!is.null(fit$options))
            opafit <- modifyList(opadata,fit$options)
        opafit <- modifyList(opafit,list(...))
        if(identical(tolower(conftype),"blives")){
            if(!is.null(fit$conf$blives)){
                for.each.blicon <- function(blicon){
                    if(!is.null(blicon$options)){
                        opaconf <- modifyList(opafit,blicon$options)
                    }else{opaconf <- opafit}
                    if(opaconf$in.legend){
                            # TODO: correct usage of this logical value?
                        li <- list()
                        li[[1]] <-  bsll(legend=paste0("B-lives, type = ",
                            ifelse(is.null(blicon$type),"NA",
                            paste0("\"",blicon$type,"\""))),
                            col=opaconf$col,lwd=opaconf$lwd,lty=opaconf$lty)
                        li[[2]] <- bsll(legend=paste0("  CL = ",
                            ifelse(is.null(blicon$cl),"NA",
                                paste0(signif(blicon$cl*100,4)," [%]")),
                            ifelse(is.null(blicon$S),"",
                                paste0(", S = ",blicon$S))))
                        if(opaconf$in.legend.blives){
                            params <- unlist(list(beta=fit$beta,eta=fit$eta,t0=fit$t0,
                                mulog=fit$mulog,sigmalog=fit$sigmalog,rate=fit$rate))
                            if(is.null(bl <- blicon$unrel))bl <- opaconf$unrel
                            fu <- function(bl){
                                bsll(legend=Blifestring(bl,blicon,opafit$signif,params))
                            }
                            c(li,lapply(bl,fu))
                        }else(li)
                    }else NULL
                }
                unlist(lapply(fit$conf$blives,for.each.blicon),FALSE)
                    # TODO: replace by do.call ?
            }else{NULL}
        }
    }else{NULL}
}