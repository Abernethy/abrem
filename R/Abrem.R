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

Abrem <- function(x,...){
    arg <- splitargs(...)
    opa <- modifyList(options.abrem(), arg$opa)
    ret <- list()
    NA.present <- FALSE
        # remember if there were any NA values in the time argument(s)
    class(ret) <- "abrem"
    timeorder <- c()
    if(!missing(x)){
        ret$data <- NULL
        if(is.vector(x) || is.numeric(x)){
            # TODO: the effects of adding is.numeric(x) are not tested thoroughly yet...
            if(opa$verbosity >= 2)message(
                'Abrem: Argument \"x\" is a (numeric) vector of (life-)time observations...')
            if(any(is.na(x))){
                NA.present <- TRUE 
                timeorder <- 1:length(x)
            }
            else timeorder <- order(x)
                # the above is to prevent ordering attempts when NA values are
                # present in the lifetime observation vector.
                # having NA values implies that the data must be ordered.
            if(!is.null(arg$rem$susp)){
                if(is.vector(arg$rem$susp)){
                    if(opa$verbosity >= 2)message(
                        'Abrem: Argument \"susp\" is vector of right-censored (suspended) (life-)time observations...')
                    # x is now consireded complete failures
                    if(!NA.present){
                        timeorder <- order(c(x,arg$rem$susp))
                        ret$data  <- data.frame(time=c(x,arg$rem$susp)[timeorder],
                            event=c(rep(1,length(x)),rep(0,length(arg$rem$susp)))[timeorder])
                    }else{stop('Argument \"x\" cannot contain NA values when \"susp\" is also passed.')}
                        # TODO: a better description of the error is probably needed here...
                }else{stop('Argument \"susp\" must be a vector.')}
            }else{            
                ret$data <- data.frame(time=x[timeorder],event=1)
            }
        }
        if(is.data.frame(x)){
            if(!is.null(x$time) && !is.null(x$event)){
                if(opa$verbosity >= 2)message(
                    'Abrem: Argument \"x\" is a dataframe with $time and $event ',
                        'columns...')
                if(any(is.na(x$time))) timeorder <- 1:length(x$time)
                else timeorder <- order(x$time)
                ret$data  <- as.data.frame(x[timeorder,])
#                ret$data$event <- 1
#                    # temporarily set event vector to 1
            }else{
                stop('Argument \"x\" is missing $time and/or ",
                    "$event columns...')
            }
        }
    }
   
    ### setting the event vector correctly ###
    if(!is.null(arg$rem$event) && !is.null(ret$data)){
        if(is.vector(arg$rem$event)){
            if(opa$verbosity >= 2)message(
                'Abrem: Argument \"event\" is event vector...')
            if((l1 <- length(arg$rem$event)) != (l2 <- length(timeorder))){
                mes <- paste0('Length of argument \"event\" (',l1,
                    ') does not match the size of the time data (',l2,').')
                stop(mes)
                # since time dat is in x or x$time, I use the vector
                # 'timeorder' here to get the length of the time data
            }else{
                ret$data$event <- arg$rem$event[timeorder]
            }
        }
    }
    addpppcolumns <- function(ppos){
        if(opa$verbosity >= 2)message(paste0(
            'Abrem: Adding ',ppos,' ranks to (life-)time observations...'))
        ret$data <<- cbind(ret$data,ppp=NA)
        if(any(is.na(ret$data$time))){
            # experimental code, in combination with support in
            # abremPivotals::gePPP for event vector arguments

            ret$data$ppp <<-
                ###[ret$data$event==1,'ppp'] <<-
                abremPivotals::getPPP(
                    x=ret$data$event,
                    ppos=ppos,na.rm=FALSE)$ppp
        }else{
            ret$data <<- abremPivotals::getPPP(
                    x=ret$data$time[ret$data$event==1],
                    s=ret$data$time[ret$data$event==0],
                    ppos=ppos,na.rm=FALSE)
        }
        whi <- which(colnames(ret$data)=="ppp")
        colnames(ret$data)[whi] <<- paste0("ppp.",ppos)
            # renaming the added column to include the type of ranking 
            # Jurgen October 7, 2014: not sure why I didn't implement this
            # solution earlier
    }

    if(any(
        c("benard","beta","mean","km","hazen","blom") %in% tolower(opa$ppos))){
        do.call(addpppcolumns,as.list(tolower(opa$ppos)))
    }
    if(any(
        c("kaplan-meier","kaplanmeier","kaplan_meier","kaplan.meier") %in% 
        tolower(opa$ppos))){
        do.call(addpppcolumns,list("km"))
    }
   ret$n    <- length(ret$data$time)
        # This assumes that any NA time (in any present
        # in the time column is there for a good reason:
        # accompanied with a censoring indicator (0 or FALSE)
    ret$fail <- sum(ret$data$event)
        # TODO: Warning; this only works when event can take values of 0 and one!
        # This can be solved when events are changed to factors.
    ret$susp <- ret$n-ret$fail
    ret$options <- opa
        # always store a full copy of the options.abrem structure here
    ret
}