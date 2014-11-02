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

calculateSingleFit <- function(x,...){
    # x is a single Abrem object

    #########################
    #  auxiliary functions  #
    #########################
    opadata <- x$options
    opafit <- modifyList(opadata,list(...))
    vm <- function(vlevel,mess)if(opafit$verbosity >= vlevel)message(mess)
    debug1 <- function()vm(2,paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),
            '), ppos = \"',opafit$ppos,'\" fit...'))
    debug2 <- function()vm(2,paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),") fit..."))
    neededcolumns <- function(pppmethod=NULL){
        # This function selects the appropriate ppp columns for fitting
        pppcolumn <- function(colname,pppmethod){
            na <- unlist(strsplit(tolower(colname),".",TRUE))
            identical(na[1],"ppp") && identical(na[2],tolower(pppmethod))
        }
        basis <- x$data[,c("time","event")]
        if(is.null(pppmethod)){
            basis
        }else{
            wh <- which(sapply(names(x$data),pppcolumn,pppmethod,USE.NAMES=FALSE))
            cbind(basis,ppp=x$data[,wh])
            # TODO: explore usefulness of having the ranking method embedded in the
            # column name at the fit level.
            
        }
    }
    
    #####################
    #  goodness of fit  #
    #####################
    goodness_of_fit <- function(npar){
        if(!is.null(x$fit[[i]])){
            if(is.null(x$fit[[i]]$gof)){
                vm(2,"calculateSingleFit: calculating r^2 using cor()...")
                x$fit[[i]]$gof <<- list()
                x$fit[[i]]$gof$r2 <<- cor(times, ranks, use = "complete.obs")^2
            }else vm(2,"calculateSingleFit: r^2 was already set...")
                # if !NULL, then it was already set by the cpp version of the fitting method
    
            if(npar==2){
                if(identical(x$fit[[i]]$gof$r2,1)){
                    vm(2,"calculateSingleFit: r^2 is exactly 1, bypassing AbPval and CCC^2 calculations...")
                    if(is.null(x$fit[[i]]$gof$AbPval))x$fit[[i]]$gof$AbPval <<- 100
                    # TODO: infinity or 100% ?
                }else{
                    vm(2,"calculateSingleFit: r^2 is lower than 1...")
                    vm(2,"calculateSingleFit: Calculating AbPval and CCC^2...")
                    # TODO: prr: see pivotalMC
                    if(!any(c("mle","mle-rba","mle2","mle2-rba","mle3","mle3-rba") 
                        # TODO: does it make sense to have reports of r^2 and ccc^2 while using MLE?
                        # Jurgen: yes, for comparing with the Rank Regression values
                        # what does Jabob think?
                        %in% tolower(opafit$method.fit))){
                        gof <- abremPivotals::AbPval(x$fit[[i]]$fail,x$fit[[i]]$gof$r2)
                        if(is.null(x$fit[[i]]$gof$AbPval))
                            x$fit[[i]]$gof$AbPval <<- gof[['AbPval']]
                        if(is.null(x$fit[[i]]$gof$CCC2))
                            x$fit[[i]]$gof$CCC2 <<- gof[[2]]
                    }
                }
            }else{
                vm(2,paste("calculateSingleFit: bypassing AbPval and CCC^2",
                "calculation for three parameter models..."))
            }
        }else{
            vm(1,"calculateSingleFit: no fit available for goodness-of-fit calculation.")
        }
    }
    
    ########################
    #  main function body  #
    ########################
    i <- 1
    atLeastOneFit <- FALSE
    if(is.null(x$fit)){
        vm(2,"calculateSingleFit: Creating the first fit in the abrem object...")
        i <- 1
        x$fit <- list()
    }else{
        vm(2,"calculateSingleFit: Appending a new fit to the existing abrem object...")
        i <- length(x$fit)+1
    }
    x$fit[[i]] <- list()
    op <- unique(c(names(x$options),names(opafit)))
        # this is needed to add options from opafit into li that
        # are NULL in x$options
        # TODO:tolower() needed?
    if(length(li <- opafit[sapply(op,function(y){
        !identical(x$options[[y]], opafit[[y]])})]) > 0){
        x$fit[[i]]$options <- li
        # the above enlists only options that are different from the abrems
        # 'main' options. This excludes options$dist and options$method.fit
    }
    x$fit[[i]]$n    <- x$n
    x$fit[[i]]$fail <- x$fail
    x$fit[[i]]$susp <- x$susp
        # TODO: this replicates the data from the abrem level -> should not be necessary!

    if(!is.null(opafit$importfile)){
        if(opafit$verbosity >= 1)message("calculateSingleFit : ",
            "importing fit results from superSMITH report file\n",opafit$importfile)
        try(fi <- file(opafit$importfile))
        if(!is.null(fi)){
            try(re <- readLines(fi))
            if(!is.null(re)){
                extract <- function(string)
                    na.omit(as.numeric(unlist(strsplit(gsub(",",".",string),"[^0123456789.]"))))
                he <- data.frame(do.call("rbind",lapply(re[1:10],extract)))
                atLeastOneFit       <- TRUE

                x$fit[[i]]$options$dist       <- "weibull2p"
                x$fit[[i]]$options$method.fit <- "superSMITH"
                x$fit[[i]]$eta      <- he[3,3]
                x$fit[[i]]$beta     <- he[3,4]
                x$fit[[i]]$gof      <- list()
                x$fit[[i]]$gof$r2   <- he[2,3]
                x$fit[[i]]$gof$prr  <- he[2,1]
                x$fit[[i]]$gof$CCC2 <- he[2,5]

                x$fit[[i]]$n    <- he[5,1]
                x$fit[[i]]$fail <- he[5,1]-he[5,2]
                x$fit[[i]]$susp <- he[5,2]
            }
            close(fi)
        }
        return(x)
    }
    if("rr" %in% tolower(opafit$method.fit)){
        #  ____             _                                       _
        # |  _ \ __ _ _ __ | | __  _ __ ___  __ _ _ __ ___  ___ ___(_) ___  _ __
        # | |_) / _` | '_ \| |/ / | '__/ _ \/ _` | '__/ _ \/ __/ __| |/ _ \| '_ \
        # |  _ < (_| | | | |   <  | | |  __/ (_| | | |  __/\__ \__ \ | (_) | | | |
        # |_| \_\__,_|_| |_|_|\_\ |_|  \___|\__, |_|  \___||___/___/_|\___/|_| |_|
        #                                   |___/
        debug1()
        x$fit[[i]]$data <- neededcolumns(opafit$ppos[1])
        times <- log(x$fit[[i]]$data$time)
        npar  <- 2
        to_rr2 <- FALSE
            # used for redirecting from rr to rr2 in case of three parameter distributions
        xy <- ifelse(is_xony <- ("xony" %in% tolower(opafit$method.fit)),"xony","yonx")
        if(tolower(opafit$dist) %in% c("weibull","weibull2p","weibull3p")){
            ranks <- log(qweibull(x$fit[[i]]$data[['ppp',exact=FALSE]],1,1))
            x$fit[[i]]$options$dist <- "weibull2p"
            dst <- "weibull"
            if(tolower(opafit$dist) %in% c("weibull3p")){
                x$fit[[i]]$options$dist <- "weibull3p"
                npar <- 3            
            }
        }
        if(tolower(opafit$dist) %in% c("lognormal","lognormal2p","lognormal3p")){
            ranks <- log(qlnorm(x$fit[[i]]$data[['ppp',exact=FALSE]],0,1))
            x$fit[[i]]$options$dist <- "lognormal2p"
            dst <- "lognormal"
            if(tolower(opafit$dist) %in% c("lognormal3p")){
                x$fit[[i]]$options$dist <- "lognormal3p"
                npar <- 3  
            }
        }

        if(any(c("use.lm","use_lm") %in% tolower(opafit$method.fit))){
            if(sum(!is.na(ranks)) < 2){
                vm(1,"calculateSingleFit: Rank Regression only supports (life-)time observations including at least two failures.")
                x$fit[i]  <- list(NULL)
            }else{
                x$fit[[i]]$options$method.fit <- c("rr",xy,"use.lm")
                if(npar==2){
                    atLeastOneFit <- TRUE
                    if(is_xony) x$fit[[i]]$lm   <- lm(times ~ ranks,x$fit[[i]]$data)
                    else        x$fit[[i]]$lm   <- lm(ranks ~ times,x$fit[[i]]$data)
                        # TODO: add error checking
                    B <- coef(x$fit[[i]]$lm)[[2]]
                    A <- coef(x$fit[[i]]$lm)[[1]]
                    if(dst=="lognormal"){
                        x$fit[[i]]$mulog     <- ifelse(is_xony,A,-A/B)
                        x$fit[[i]]$sigmalog  <- ifelse(is_xony,B,1/B)
                    }
                    if(dst=="weibull"){
                        x$fit[[i]]$eta   <- ifelse(is_xony,exp(A),exp(-A/B))
                        x$fit[[i]]$beta  <- ifelse(is_xony,1/B,B)
                    }
                }else{
                    vm(0,paste0('calculateSingleFit: Currently, three parameter distributions cannnot be fit using lm(), proceeding...'))
                    to_rr2 <- TRUE
                }
            }
        } 

        if(!any(c("use.lm","use_lm") %in% tolower(opafit$method.fit)) || to_rr2){
            if(sum(!is.na(ranks)) < 2){
                vm(1,"calculateSingleFit: Rank Regression only supports (life-)time observations including at least two failures.")
                x$fit[i]  <- list(NULL)
            }else{
                x$fit[[i]]$options$method.fit <- c("rr",xy)
                ret <- NULL
                me <- NULL
                try(ret <- abremPivotals::lslr(
                    x$fit[[i]]$data,
                    dist=dst,
                    npar=npar,
                    reg_method=xy))
                if(!is.null(ret) && !any(is.nan(ret))){
                    atLeastOneFit        <- TRUE
                    if(tolower(opafit$dist) %in% c("weibull","weibull2p","weibull3p")){
                        x$fit[[i]]$eta       <- ret[['Eta']]
                        x$fit[[i]]$beta      <- ret[['Beta']]
                        if(tolower(opafit$dist) %in% c("weibull3p")){
                            x$fit[[i]]$t0    <- ret[['t0']]
                        }
                    }
                    if(tolower(opafit$dist) %in% c("lognormal","lognormal2p","lognormal3p")){
                        x$fit[[i]]$mulog     <- ret[['Mulog']]
                        x$fit[[i]]$sigmalog  <- ret[['Sigmalog']]
                        if(tolower(opafit$dist) %in% c("lognormal3p")){
                            x$fit[[i]]$t0    <- ret[['t0']]
                        }
                    }
                    x$fit[[i]]$gof       <- list()
                    x$fit[[i]]$gof$r2    <- ret[['Rsqr']]
                    if(npar==2) x$fit[[i]]$gof$AbPval    <- ret[['AbPval']]
                        # AbPval is not supported for three parameter distributions
                    if(!is.null(opafit$threshold)){
                        # this overwrites any threshold setting at the data level with a number
                        # TODO: this is not the way to go when trying to implement support for
                        # threshold with plot.abrem()                    
                        if(is.logical(opafit$threshold) && opafit$threshold)
                            x$options$threshold  <- ret[[3]]
                    }
                }else{
                    vm(1,"calculateSingleFit: Fitting failed.")
                    x$fit[i]  <- list(NULL)
                        # note that is.null(x$fit[[i]]) will exit with an error
                        # TODO: replace with x$fit[[i]]  <- list(NULL)
                }
            }
        }
        goodness_of_fit(npar)
    }
   
  
    if(any(c("mle","mle-rba") %in% tolower(opafit$method.fit))){
        #   from email David Silkworth <djsilk@openreliability.org> ma 4/11/2013 18:29
        #   The MLE you want to call in abrem is MLEw2p_cpp  This is the fast one in compiled code.
        #
        #   The other two are simply demonstrations in R.  
        #     - MLEw2p_abrem solves the MLE using the method shown in The Handbook. 
        #       The likelihood function has been differentiated analytically  
        #       so as to separate beta from eta (by others as can be found in 
        #       the literature).  Then a relatively simple root finder algorithm
        #       (Newton method) identifies the  beta_hat, followed by 
        #       calculation of eta_hat from the separated function.  
        #        This method is detailed in Appendix C, section C.4 of the Handbook.
        #
        #    - MLEw2p_optim solves the MLE using the optim function the same 
        #       way that surv package would.  The optim function is being 
        #       called with a default for the Nelder-Meade "simplex" algorithm.
        #
        #   For the Cpp implementation it was simpler for me to port the 
        #   Newton method as I approached the weibull first (before lognormal).
        #   Later, in order to do the Cpp implementation of the lognormal MLE,
        #   I indeed ended up coding a Nelder-Meade simplex algorithm from
        #   scratch.  That algorithm became the basis of the port that is
        #   called by MLEln2p_cpp. I left the development files in the 
        #   package for eventual tutorial value.

        # "mle"  = MLEw2p_abrem
        # "mle2" = MLEw2p_cpp , should be default
        # "mle3" = MLEw2p_optim
        #  __  __ _     _____
        # |  \/  | |   | ____|
        # | |\/| | |   |  _|
        # | |  | | |___| |___
        # |_|  |_|_____|_____|

        debug2()
        x$fit[[i]]$data <- neededcolumns(opafit$ppos[1])

        fa <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==1]
        su <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==0]
        if(length(su)==0) su <- NULL
        ret <- NULL        
        if(length(fa) < 1){
            vm(1,"calculateSingleFit: MLE only supports (life-)time observations including at least one failure.")
        }else{        
            #is_3p <- FALSE
            npar <- 2
            if(tolower(opafit$dist) %in% c("weibull","weibull2p","weibull3p")){
                if(tolower(opafit$dist) %in% c("weibull","weibull2p")){
                    # Nov 2014: dropped support for MLEw2p_abrem and MLEw2p_optim
                    # per request from Jacob
                    x$fit[[i]]$options$dist <- "weibull2p"                    
                    x$fit[[i]]$options$method.fit <- "mle"
                    if(!is.null(fa)) try(ret <- debias::MLEw2p_cpp(fa,s=su))
                }
                if(tolower(opafit$dist) %in% c("weibull3p")){
                    #is_3p <- TRUE
                    npar <- 3
                    x$fit[[i]]$options$dist <- "weibull3p"
                    if(!is.null(fa)) try(ret <- debias::MLEw3p_secant(fa,s=su))
                        # secant: purely  R code ...
                        # TODO: check if secant is using lm() or optim !
                        # TODO: this will be significantly changed in abremDebias
                        # -> check this out
                }
                if(!is.null(ret)){
                    atLeastOneFit <- TRUE
                    x$fit[[i]]$beta <- ret[[2]]
                    x$fit[[i]]$eta  <- ret[[1]]
                    if(npar==3){
                        x$fit[[i]]$t0   <- ret[[3]]
                        x$fit[[i]]$gof  <- list()
                        x$fit[[i]]$gof$loglik <- ret[[4]]
                    }else{
                        x$fit[[i]]$gof <- list()
                        x$fit[[i]]$gof$loglik <- ret[[3]]
                    }
                    if(!is.null(opafit$threshold)){
                        if(is.logical(opafit$threshold) && opafit$threshold)
                             x$options$threshold  <- ret[[3]]
                    }
                        # this overwrites any threshold setting at the data level with a number
                        # TODO: this is not the way to go when trying to implement support for
                        # threshold with plot.abrem()
                    if("mle-rba" %in% tolower(opafit$method.fit)){
                        x$fit[[i]]$options$method.fit <- "mle-rba"
                        vm(2,"calculateSingleFit: Applying Abernethy's Bias Reduction ...")
                        x$fit[[i]]$beta <- ret[[2]]*debias::RBAbeta(length(fa))
                            # TODO: set the option: median or mean bias reduction
                    }
                    goodness_of_fit(npar)
                }else{
                    vm(1,"calculateSingleFit: Fitting failed.")
                    x$fit[i]  <- list(NULL)
                }
            }
    
            if(tolower(opafit$dist) %in% c("lognormal","lognormal2p")){
                x$fit[[i]]$options$dist <- "lognormal2p"
                x$fit[[i]]$options$method.fit <- "mle"
                try(ret <- debias::MLEln2p_cpp(fa,s=su))
                if(!is.null(ret)){
                    atLeastOneFit       <- TRUE
                    x$fit[[i]]$mulog    <- ret[[1]]
                    x$fit[[i]]$sigmalog <- ret[[2]]
                    x$fit[[i]]$gof      <- list()
                    x$fit[[i]]$gof$loglik <- ret[[3]]
                    if("mle-rba" %in% tolower(opafit$method.fit)){
                        x$fit[[i]]$options$method.fit <- "mle-rba"
                        vm(2,"calculateSingleFit: Applying Abernethy's Median Bias Reduction ...")
                        x$fit[[i]]$sigmalog <- ret[[2]]*debias::RBAsigma(length(fa))
                            # with RBAsigma, there are no options...
                    }
                    goodness_of_fit(npar)
                }else{
                    vm(1,"calculateSingleFit: Fitting failed.")
                    x$fit[i]  <- list(NULL)
                }
            }
        }
    }
    if(!atLeastOneFit){
        message("*** calculateSingleFit: Nothing has been fitted. ***\n",
                '*** Does \"method.fit\" include sensible options? ***')
        # x$fit[[i]] <- NULL
    }
    if(is.numeric(opafit$threshold))
        x$options$threshold <- opafit$threshold
        # overwrite any previously set data-level-t0 to the one specified as an argument to abrem.fit()
        # Don't know why - here - you MUST use <- in favor of <<- ...
    x
    # return a single Abrem object
}