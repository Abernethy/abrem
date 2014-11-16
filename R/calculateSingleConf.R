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

calculateSingleConf <- function(fit,opadata,datarange,...){
    # fit is a single fit
    arg <- list(...)
    if(!is.null(list(...)$threshold))
        message("calculateSingleConf: Currently, passing the \'threshold\' argument to abrem.conf  is not supported. Proceeding...")
    if(missing(fit)){
        stop("Argument \"fit\" is missing.")
    }else{
        if(!is.null(fit) && any(sapply(fit,function(fi)!is.null(fi)))){
            if(!is.null(fit$options)) opafit <- modifyList(opadata,fit$options)
            opafit$importfile <- NULL
                # never use the importfile from abrem.fit; only use it when
                # explicitly supplied as a function argument
            opaconf <- modifyList(opafit,arg)
            if(!is.null(fit$options$dist)){
#                if(opaconf$verbosity >= 1)
#                    message("calculateSingleConf: Found weibull 2P distribution.")
                if("blives" %in% tolower(opaconf$conf.what)){
                    if(opaconf$verbosity >= 1)
                        message("calculateSingleConf: Calculating B-lives confidence bounds.")
                    mini <- min(c(opaconf$ylim[1]/10,datarange$yrange[1]/10),0.001)
                    maxi <- max(c((1-(1-opaconf$ylim[2])/10),
                        (1-(1-datarange$yrange[2])/10),0.999))
                    if(opaconf$unrel.n==0){
                        if(length(opaconf$unrel>=1)){
                            if(any(c(opaconf$unrel >=1 ,opaconf$unrel <= 0))){
                            #if(any(c(opaconf$unrel >1 ,opaconf$unrel < 0))){
                            # theoretically, the subsequent code should be able to deal 
                            # with 0 and 1 in the unrel vector, but on another note 
                            # it appears that
                            # unrel = c() is transformed to opaconf$unrel=0. To catch the latter
                            # bad input, the slichtly more restrictive if statement is used.
                                stop("Argument \'unrel\' must contain values from interval ]0,1[.")
                            }
                            unrel <- opaconf$unrel[order(opaconf$unrel)]

                            # assume that the user supplies enough values in the unrel
                            # vector to display confidence bounds
                            # this feature is useful in combination with unit testing
                            # (testthat) of sumerSMITH iported plotting reports
                        }else{
                            stop("When unrel.n=0, argument \'unrel\' must contain at least one element.")
                        }
                    }else{
                        lo <- opaconf$unrel.n -
#                            length(opaconf$unrel+2)))
                                # that's a bug... ?
                            length(opaconf$unrel)-2
                        if (lo < 1){
                            stop("Bad combination of \'unrel\' and \'unrel.n\'.\nIncrease \'unrel.n\' and\\or decrease number of elements in \'unrel\'.")
                        }
                        unrel <- c(F0(seq(F0inv(mini),F0inv(maxi),
                                # TODO: this isn't right...
    #                        unrel <- c(F0(seq(F0inv(1e-3),
    #                            F0inv(0.999),
                            length.out=lo)),
                            opaconf$unrel,0.5,F0(0))
                            # effectively ignoring any ylim
                            # setting per fit.
                        unrel <- unique(signif(unrel[order(unrel)]))
                            # signif() needed for eliminating
                            # identical looking unreliability
                            # levels that differ only at place far
                            # from the decimal point
                            # unrel <- c(F0(seq(par('usr')[3],par('usr')[4],
                    }
                    if(is.null(fit$conf)) fit$conf <- list()
                    atLeastOneBLifeConf <- FALSE
                    if(is.null(fit$conf$blives)){
                        if(opaconf$verbosity >= 2)message(
                            "calculateSingleConf: Creating the first ",
                            "B-life confidence calculation in the fit...")
                        i <- 1
                        fit$conf$blives <- list()
                    }else{
                        if(opaconf$verbosity >= 2)message(
                            "calculateSingleConf: Appending a new ",
                            "B-life confidence calculation to the fit...")
                        i <- length(fit$conf$blives)+1
                    }
                    fit$conf$blives[[i]] <- list()
                    if(!is.null(opaconf$importfile)){
                        #                            ____  __  __ ___ _____ _   _
                        #  ___ _   _ _ __   ___ _ __/ ___||  \/  |_ _|_   _| | | |
                        # / __| | | | '_ \ / _ \ '__\___ \| |\/| || |  | | | |_| |
                        # \__ \ |_| | |_) |  __/ |   ___) | |  | || |  | | |  _  |
                        # |___/\__,_| .__/ \___|_|  |____/|_|  |_|___| |_| |_| |_|
                        #           |_|
                        if(opaconf$verbosity >= 1)message("calculateSingleConf : ",
                            "importing confidence bounds from superSMITH report file:\n",opaconf$importfile)
                        try(fi <- file(opaconf$importfile))
                        if(!is.null(fi)){
                            try(re <- readLines(fi))
                            if(!is.null(re)){
                                atLeastOneBLifeConf <- TRUE
                                fit$conf$blives[[i]]$type   <- "superSMITH"
                                fit$conf$blives[[i]]$source <- re
                                extract <- function(string)
                                    na.omit(as.numeric(unlist(strsplit(gsub(",",".",string)," "))))
                                bounds <- data.frame(do.call("rbind",lapply(re[12:length(re)],extract)))
                                bounds[,1] <- bounds[,1]/100
                                names(bounds) <- c("unrel","lower","datum", "upper")
                                fit$conf$blives[[i]]$bounds <- bounds
                            }
                            close(fi)
                        }
                        op <- unique(c(names(opafit),names(opaconf)))
                        if(length(li <- opaconf[sapply(op,function(y){
                            !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                            fit$conf$blives[[i]]$options <- li
                        }
                        return(fit)
                    }else{
                        if("bbb" %in% tolower(opaconf$method.conf.blives)){
                            #  ____  ____  ____
                            # | __ )| __ )| __ )
                            # |  _ \|  _ \|  _ \
                            # | |_) | |_) | |_) |
                            # |____/|____/|____/

                            # TODO: bbb for mle?
                            # TODO: ppp calculation on the fly or taken from x$fit[[i]]$data?
                            # TODO: what if no ppp are in x$fit[[i]]$data?
                            # TODO: bbb for lognormal?

                            if(opaconf$verbosity >= 1)message(
                                "calculateSingleConf: Calculating bbb confidence bounds...")
                            fit$conf$blives[[i]]$type <- "bbb"
                            fit$conf$blives[[i]]$cl <- opaconf$cl
                            fit$conf$blives[[i]]$sides <- opaconf$conf.blives.sides

                            ### calculate adjusted ranks, just for these BB bounds
                            sx <- fit$data
                            if(is.null(fit$data[['ppp',exact=FALSE]])){
                                # no ranks available, likely because the fit was mle
                                if(opaconf$verbosity >= 1)
                                    message('calculateSingleConf: creating ranks for calculating \"bbb\" bounds...')
                            #    ppp <- .Call(ty,fit$data$event, PACKAGE= "pivotals")
                                ppp <- abremPivotals::getPPP(
                                    x=fit$data$time[fit$data$event==1],
                                    s=fit$data$time[fit$data$event==0],
                                    ppos=opaconf$ppos,na.rm=FALSE)$ppp
                                    # TODO: ppos=opaconf$ppos is not compatible with the intended feature that multiple adjuste rank methods can be supplied!
                                sx$ppp[sx$event] <- ppp
                                    # this assumes fit$data and thus sx is ordered
                                    # TODO: shouldn't I use the ranks that are part of the
                                    # data since they will ALWAYS be part of
                                    # an abrem object?
                                    # The argument can be made that for MLE bbb should also be calculated
                                    # 
                                # TODO: makes sense to have BBB bounds with mle?
                            }
                            sx <- sx[order(sx[['ppp',exact=FALSE]]),]
                                # order data according to rank
                            sx <- cbind(sx,arank=NA)
                            sx$rrank <- (fit$n+1-order(sx[['ppp',exact=FALSE]]))
                                # TODO: does order() completely replace x$ppp? (NA?)
                                # reverse rank order
                                # TODO: keep the rrank and arank in fit$data or discard?
                            parank <- 0
                            for (j in 1:fit$n){
                                if(!sx$event[j] || is.null(sx$event)){
                                    sx$arank[j] <- NA
                                }else{
                                    sx$arank[j] <- (sx$rrank[j]*parank + fit$n +1)/(sx$rrank[j]+1)
                                    parank <- sx$arank[j]
                                }
                                # adjusted_rank =
                                # (reversed rank * previous adj. rank + n + 1)/(reversed rank + 1)
                                # see "The new Weibull handbook, fifth edition" p. 2-7, formula 2-5
                            }
                            da <- data.frame(
                                unrel= sx[['ppp',exact=FALSE]],
                                lower= bbb(sx$arank,fit$n,(1-opaconf$cl)/2,fit$beta,fit$eta),
                                upper= bbb(sx$arank,fit$n,1-(1-opaconf$cl)/2,fit$beta,fit$eta))
                            lo <- approxfun(F0inv(sx[['ppp',exact=FALSE]]),log(da$lower))
                            up <- approxfun(F0inv(sx[['ppp',exact=FALSE]]),log(da$upper))
                            bl <- F0inv(unrel)
                            da <- rbind(da,data.frame(unrel=unrel,lower=exp(lo(bl)),upper=exp(up(bl))))
                            da <- da[order(da$unrel),]
                            da <- da[!duplicated(da$unrel),]
                            fit$conf$blives[[i]]$bounds <- da
                            op <- unique(c(names(opafit),names(opaconf)))
                                # this is needed to add options from opafit into li that
                                # are NULL in opafit
                                # TODO: tolower() not needed?
                            if(length(li <- opaconf[sapply(op,function(y){
                                !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                fit$conf$blives[[i]]$options <- li
                            }
                        }
                        if("lrb" %in% tolower(opaconf$method.conf.blives)){
                            #  _     ____  ____
                            # | |   |  _ \| __ )
                            # | |   | |_) |  _ \
                            # | |___|  _ <| |_) |
                            # |_____|_| \_\____/

                            if(opaconf$verbosity >= 1)message(
                                "calculateSingleConf: Calculating Likelihood Ratio confidence bounds.")
                            fail <- fit$data$time[fit$data$event==1]
                            susp <- fit$data$time[fit$data$event==0]
                                # TODO: this implies only right censoring !
                            fit$conf$blives[[i]]        <- list()
                            fit$conf$blives[[i]]$type   <- "lrb"
                            fit$conf$blives[[i]]$cl     <- opaconf$cl
                            fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
                            fit$conf$blives[[i]]$unrel <- opaconf$unrel
                            is_debias <- ifelse("mle" %in% tolower(fit$options$method.fit),FALSE,TRUE)
                            ret <- NULL
                            con <- NULL
                            try(con <- debias::MLEw2pContour(fail,susp,CL=opaconf$cl,debias=is_debias,show=FALSE))
                            if(!is.null(con)){
                                fit$conf$blives[[i]]$MLEXContour <- list()
                                fit$conf$blives[[i]]$MLEXContour[[1]] <- con
                                
                                retfit <- abrem.fit(Abrem(fail,susp=susp),method.fit=ifelse(is_debias,"mle-rba","mle"))
                                fit$conf$blives[[i]]$MLEXContour$MLEpoint <-
                                    data.frame(Eta=retfit$fit[[1]]$eta,Beta=retfit$fit[[1]]$beta)
#                                MLEpoint <- data.frame(Eta=fit$Eta,Beta=fit$Beta)
#                                fit$conf$blives[[i+1]] <- list()
#                                fit$conf$blives[[i+1]]$MLEXContour <-
#                                    list(upper=MLEpoint,lower=MLEpoint,right=MLEpoint,left=MLEpoint)
#                                    # TODO: this hack should produce a point at the MLE location
                                try(ret <- debias::MLEw2pBounds(fail,susp,Blives=unrel,MLEcontour=con,debias=is_debias,show=FALSE))
                                    # debias is true in all cases except for regulaer MLE
                                if(!is.null(ret)){
                                    atLeastOneBLifeConf <- TRUE
                                    fit$conf$blives[[i]]$bounds <- cbind(unrel,exp(ret[,-1]))
                                    names(fit$conf$blives[[i]]$bounds) <- tolower(names(fit$conf$blives[[i]]$bounds))
                                    # TODO: ask jacob to provide his dataframe in lowercase
                                }
                            }
                            op <- unique(c(names(opafit),names(opaconf)))
                                # this is needed to add options from opafit into li that
                                # are NULL in opafit
                                # TODO:tolower() not needed?
                            if(length(li <- opaconf[sapply(op,function(y){
                                !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                fit$conf$blives[[i]]$options <- li
                            }
                        }
                        if(any(c("mcpivotals","mcpivotal") %in% tolower(opaconf$method.conf.blives))){
                            #                       _            _        _
                            #  _ __ ___   ___ _ __ (_)_   _____ | |_ __ _| |___
                            # | '_ ` _ \ / __| '_ \| \ \ / / _ \| __/ _` | / __|
                            # | | | | | | (__| |_) | |\ V / (_) | || (_| | \__ \
                            # |_| |_| |_|\___| .__/|_| \_/ \___/ \__\__,_|_|___/
                            #                |_|

                            if(opaconf$verbosity >= 1)message(
                                "calculateSingleConf: Calculating Monte Carlo Pivotal confidence bounds.")
#                            i <- length(fit$conf$blives)
#                            if(length(fit$conf$blives[[i]]) > 0){
#                                i <- i+1
#                            }
                            if(any(c("weibull","weibull2p","weibull3p") %in% tolower(fit$options$dist))){
                                dst <- "weibull"
                                dx <- params.to.ob(fit$options$dist,eta=1,beta=1,event=fit$data$event)
                            }
                            if(any(c("lognormal","lognormal2p","lognormal3p") %in% tolower(fit$options$dist))){
                                dst <- "lognormal"
                                dx <- params.to.ob(fit$options$dist,mulog=0,sigmalog=1,event=fit$data$event)
                            }
                            r1 <- abrem.fit(Abrem(dx[dx$event==1,]),dist=fit$options$dist,
                                method.fit=fit$options$method.fit)
                                # TODO: what happens when the above are NULL?
                            fit$conf$blives[[i]]        <- list()
                            fit$conf$blives[[i]]$type   <- "mcpivotals"
                            fit$conf$blives[[i]]$S      <- opaconf$S
                            fit$conf$blives[[i]]$seed   <- opaconf$seed
                            fit$conf$blives[[i]]$rgen   <- opaconf$rgen
                            fit$conf$blives[[i]]$cl     <- opaconf$cl
                            fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
                            fit$conf$blives[[i]]$unrel  <- opaconf$unrel
                            ret <- NULL
#                                    if(fit$susp != 0){
#                                        warning(
#                                        "calculateSingleConf: Currently, MC Pivotal bounds for (heavily) censored data\n",
#                                        "are still experimental.")
#                                    }
                            if(is.null(fit$data[['ppp',exact=FALSE]])){
                                message("calculateSingleConf: Currently, only Rank Regression is supported.")
                            }else{
#                                        try(ret <- .Call("pivotalMC",
                                if(dst=="weibull" && !is.null(r1$fit[[1]]$eta) && !is.null(r1$fit[[1]]$beta)){
                                    Scale <- r1$fit[[1]]$eta
                                    Shape <- r1$fit[[1]]$beta
                                }
                                if(dst=="lognormal" && !is.null(r1$fit[[1]]$mulog) && !is.null(r1$fit[[1]]$sigmalog)){
                                    Scale <- r1$fit[[1]]$mulog
                                    Shape <- r1$fit[[1]]$sigmalog
                                }
						## This is the only call in all of package abrem to abremPivotals::pivotalMC
						## A new dataframe object is created from the data in the fit object so that
						## adjustments can be made here assuring proper arguments arrive at C++ call
						## pivotalMC will simply raise a stop/error if any NA's are present.	

							## check for location of ppp column is done here only because Jacob is 
							## uncertain of column ordering, or potential changes elsewhere in code,
							## This can be simplified if Jurgen is sure of location.
								 ppp_col<- which ( (tolower(names(fit$data)) %in% 
								  c("ppp","ppp.benard","ppp.beta","ppp.mean",
								  "ppp.hazen","ppp.km","ppp.kaplan-meier","ppp.blom")))
							## na.omit can be used here because the source of the fit$data is known.
							## if placed in abremPivotals package alteration of someone else's use of NA
							## would be an improper assumption.	Appearance of NA there must be stopped as an error.							  
								 pivotal_frame<-na.omit(data.frame(time=fit$data$time,ppp=fit$data[ppp_col]))

                                try(ret <- abremPivotals::pivotalMC(
                                    x=pivotal_frame,
                                    dist=dst,
                                    reg_method=ifelse("xony" %in% tolower(opaconf$method.fit),"xony","yonx"),
                                    r2=0.0,CL=opaconf$cl,
                                    # by passing r2=0.0, only the pivotals are returned in a matrix
                                    unrel=unrel,
                                    P1=Scale,
                                    P2=Shape,
                                    S=opaconf$S,
                                    seed=sample.int(.Machine$integer.max,1),
                                    ProgRpt=FALSE
                                    ))
#                                            ,PACKAGE = "abremPivotals"))
                            }
                            if(!is.null(ret)){
                                atLeastOneBLifeConf <- TRUE
                                if(dst=="weibull")fit$conf$blives[[i]]$bounds <- 
                                    cbind(unrel,exp(log(fit$eta)+ ret/fit$beta))
                                if(dst=="lognormal")fit$conf$blives[[i]]$bounds <-
                                    cbind(unrel,exp(fit$mulog+ ret*fit$sigmalog))
                                names(fit$conf$blives[[i]]$bounds) <- c("unrel","lower","datum", "upper")
                                op <- unique(c(names(opafit),names(opaconf)))
                                    # this is needed to add options from opafit into li that
                                    # are NULL in opafit
                                    # TODO:tolower() not needed?
                                if(length(li <- opaconf[sapply(op,function(y){
                                    !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                    fit$conf$blives[[i]]$options <- li
                                }
                            }else{
                                message("calculateSingleConf: Confidence calculation failed.")
                                fit$conf$blives[[i]] <- NULL
                            }
                        }
                        if(any(c("exp1","exp-1") %in% tolower(opaconf$method.conf.blives))){
                            # experimental R based pivotals code
                            #                  _
                            #   _____  ___ __ / |
                            #  / _ \ \/ / '_ \| |
                            # |  __/>  <| |_) | |
                            #  \___/_/\_\ .__/|_|
                            #           |_|
                            if(opaconf$verbosity >= 1)message(
                                "calculateSingleConf: Calculating EXP-1 confidence bounds.")
                            dx <- params.to.ob("weibull",beta=1,eta=1,
                                event=fit$data$event)
                            r1 <- abrem.fit(Abrem(dx[dx$event==1,]),dist=fit$options$dist,
                                method.fit=fit$options$method.fit)
                                # TODO: what happens when the above are NULL?
                                # TODO: no problems with NA?
                            fit$conf$blives[[i]]        <- list()
                            fit$conf$blives[[i]]$type   <- "exp1"
                            fit$conf$blives[[i]]$S      <- opaconf$S
                            fit$conf$blives[[i]]$seed   <- opaconf$seed
                            fit$conf$blives[[i]]$rgen   <- opaconf$rgen
                            fit$conf$blives[[i]]$cl     <- opaconf$cl
                            fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
                            fit$conf$blives[[i]]$unrel  <- opaconf$unrel
                            ret <- NULL
                            daevent <- fit$data$event
                            MCfun <- function(){
                                d2 <- data.frame(time=NA,event=daevent)
                                d2[daevent == 1,'time'] <- sort(
                                    rweibull(length(daevent[daevent==1]),
                                    r1$fit[[1]]$beta,r1$fit[[1]]$eta))
                                if(!is.null(ret))return(
                                    c(u_hat=log(ret[1]),b_hat=1/ret[2]))
                                else stop()
                            }
                            piv <- as.data.frame(t(replicate(opaconf$S,MCfun())))
                            wp  <- log(qweibull(unrel,1,1))
                                #wp  <- F0inv(unrel) # identical as the above line
                            Zp  <- function(wp)((piv$u_hat-wp)/piv$b_hat)
                                # calculate the pivotal quantities for each u_hat and b_hat...
                            piv <- cbind(piv,sapply(wp,Zp))
                                # ... and add them to the dataframe
                            names(piv) <- c("u_hat","b_hat",signif(unrel))

                            fit$conf$blives[[i]]$bounds <- data.frame(unrel=unrel,row.names=unrel)
                            Tp <- function(Zp,cl)exp(log(fit$eta)-quantile(Zp,cl)/fit$beta)
                            fit$conf$blives[[i]]$bounds <-
                                cbind(fit$conf$blives[[i]]$bounds,
                                lower =sapply(piv[,c(-1,-2)],Tp,1-(1-opaconf$cl)/2),
                                datum =sapply(piv[,c(-1,-2)],Tp,0.5),
                                upper =sapply(piv[,c(-1,-2)],Tp,(1-opaconf$cl)/2))
                            op <- unique(c(names(opafit),names(opaconf)))
                                # this is needed to add options from opafit into li that
                                # are NULL in opafit
                                # TODO:tolower() not needed?
                            if(length(li <- opaconf[sapply(op,function(y){
                                !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                fit$conf$blives[[i]]$options <- li
                            }
                        }
                    }
                }
#if("weibull3p" %in% tolower(fit$options$dist)){
#    if(is.null(fit$beta) || is.null(fit$eta) || is.null(fit$t0)){
#        stop("Beta, Eta and/or t0 are not available.")
#    }else{
#        message("calculateSingleConf: Currently, confidence bounds for Weibull 3P are not supported.")
#    }
#}
#if(tolower(fit$options$dist) %in% c("lognormal","lognormal2p")){
#    message("calculateSingleConf: Currently, confidence bounds for lognormal are not supported.")
#}
            }else{
                stop("Distribution type was not provided.")
            }
        }else{
            if(opadata$verbosity >= 1)
                # TODO: using opadata since no other location of $verbosity is available.
                message("calculateSingleConf: The fit argument is empty or contains no fits.")
        }
    }
    fit
}
