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

buildSingleFitLegend <- function(fit,opadata,...){
    arg <- list(...)
    if(!is.null(fit$options)){
        opafit <- modifyList(opadata,fit$options)
    }else{opafit <- opadata}
    opafit <- modifyList(opafit,list(...))
    t0 <- NULL
    le <- NULL
    
    if(opafit$is.plot.legend){
        if(is.logical(opafit$threshold))if(opafit$threshold){
            if(is.logical(opadata$threshold)){if(opadata$threshold)
                warning("opafit$threshold and opadata$threshold are logical values but numeric values were expected. Proceeding...")
            }else{
                # reuse the t0 value from the data level
                t0 <- opadata$threshold
            }
        }
        if(is.numeric(opafit$threshold))t0 <- opafit$threshold
        si <- function(number)signif(number,opafit$signif)
            # shorter writing form for signif()
        li <- list()
        if(opadata$in.legend){
    
            li[[10]]    <- bsll(legend=paste0("ppos = ",opafit$ppos[1],ifelse(is.null(t0),"",paste0(" (t0 = ",si(t0),")"))),
                col=opadata$col,pch=opadata$pch,lwd=opadata$lwd.points)
            li[[15]]    <- bsll(legend=paste0("n (fail | susp.) = ",fit$n,
                " (",fit$fail," | ",fit$susp,")"))
        }
        if(opafit$in.legend){
            li[[20]]    <- bsll(legend = paste0(fit$options$dist," (",
                paste0(fit$options$method.fit,collapse=", "),")"),
                col=opafit$col,lwd=opafit$lwd,lty=opafit$lty)
#            li[[30]]    <- bsll(legend=ifelse(is.null(fit$rate),NA,
#                    paste0("rate = ",si(fit$rate))))
            li[[40]]    <- bsll(legend=ifelse(is.null(fit$mulog),NA,
                    paste0("mu(log) = ",si(exp(fit$mulog))," (",
                    si(fit$mulog),")")))
            li[[50]]    <- bsll(legend=ifelse(is.null(fit$sigmalog),NA,
                    paste0("sigma(log) = ",si(exp(fit$sigma))," (",
                    si(fit$sigmalog),")")))
            li[[60]]    <- bsll(legend=ifelse(is.null(fit$beta),NA,
                    paste0("beta = ",si(fit$beta))))
            li[[70]]    <- bsll(legend=ifelse(is.null(fit$eta),NA,
                    paste0("eta = ",si(fit$eta))))
            li[[80]]    <- bsll(legend=ifelse(is.null(fit$t0),NA,
                    paste0("t0 = ",si(fit$t0))))
            if(!is.null(fit$gof) && opafit$in.legend.gof){
                if(!is.null(fit$gof$r2)){
                    if(!is.null(fit$gof$CCC2)){
                        li[[100]]    <- bsll(legend=paste0("r^2 | CCC^2 = ",
                            si(fit$gof$r2)," | ",si(fit$gof$CCC2),
                            ifelse(fit$gof$r2>=fit$gof$CCC2," (good)"," (BAD)")))
                    }else{
                        li[[100]]    <- bsll(legend=paste0("r^2 = ",si(fit$gof$r2)))
                    }
                }
                if(!is.null(fit$gof$loglik)){
                    li[[110]]    <- bsll(legend=paste0("loglik = ",si(fit$gof$loglik)))
                }
                li[[120]]    <- bsll(
                    legend=ifelse(is.null(fit$gof$AbPval),NA,
                        paste0("AbPval = ",si(fit$gof$AbPval))))
                        #," (S=",ifelse(is.null(fit$gof$S),"NA",fit$gof$S),")")))
                        # AbPval is not created by MC, so S is not used and needed here
            }
        }
        #leconfpos <- length(na.omit(unlist(li))) + 1
            # where displaying confidence info begins
        leconf <- legendConf(fit,"blives",opadata=opadata,...)
        if(!is.null(leconf))li[[130]] <- bsll(legend="")
        li <- c(li,leconf)
        removeBadLegendEntries <- function(e){
            if(!is.null(e))!is.na(e$legend) else FALSE
        }
        if(length(li)>0)li <- li[sapply(li,removeBadLegendEntries)]
        else li <- ""
            # remove list items where the legend text = NA
        fu  <- function(x,i){if(i %in% names(x))x[[i]]}
        fu2 <- function(i,x){lapply(x,fu,i=i)}
        items <- c("legend","lty","lwd","pch","col")
        le  <- lapply(items,fu2,li)
        names(le) <- items
        if(identical(label <- opafit$label,""))label <- NULL
        le$rect <- legend(
            "bottomright",
    #                "topright",
            legend=le$legend,
            title=label,
            cex = opafit$legend.text.size,
    #        inset=0.1,
    #        merge = TRUE,
            plot=FALSE)$rect
        le$label <- opafit$label
        le$legend.text.size <- opafit$legend.text.size
    }
    le
}
