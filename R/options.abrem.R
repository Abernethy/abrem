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

options.abrem <- function(...){
    # function to handle the many options of the weibull toolkits functions
    # the option list should only be manipulated through this function!
    
    # TODO: WARNING: partial matching is in effect!
    # options.abrem()$ylim will return options.abrem()$ylim.default if
    # $ylim was set to NULL!

    single <- FALSE
    args <- list(...)

    if(!exists(as.character(substitute(options_abrem))))
        # if the globally accessible variable was not defined yet, then
        # create it here with default values OR reset to default values
        # message ("Resetting Weibulltoolkit options to default values...")
        options_abrem <<- list(
            dist="weibull",
            method.fit=c("rr","xony"),
                # TODO: decide between "rr" and "rr2".
                # "rr2" is not 100%compatible with params.to.ob
            conf.what="blives",
            conf.blives.sides="double",
            unrel.n=25,
            method.conf.blives="mcpivotals",
            ppos="benard",#,"beta"),#,"benard_cpp","hazen","mean"),
                #"benard", "beta", "mean", "hazen", "Kaplan-Meier", "Blom".
                # TODO: make the transition from pp to ppos everywhere in the abrem code!
                # TODO: Code has changed from beta to benard as the default!
            S=1e4,
            pivotals=FALSE,
            cl=0.9,
            # cl or CL ?
            unrel=c(0.1,0.05,0.01),
            verbosity=0,
            mar=c(5.1,4.1,5.1,2.1),
            main="Probability Plot",
            main.contour="Contour Plot",
                # a default title for Contour plots
            sub=NULL,
            sub.contour=NULL,
            xlim=NULL,
            ylim=NULL,
            xlab="Time To Failure",
            ylab="Unreliability [%]",
            log="x", # maybe this should be removed in favor of "canvas"
            #canvas="weibull",
            coordinate.text.size=0.7,
            signif=4,
            pch=1,
            lwd=2,
            lwd.points=2,
            cex.points=1,
            lty=1,
            col="black",
            col.grid="gray",
            threshold=FALSE,
            is.plot.grid=TRUE,
            is.plot.fit=TRUE,
            is.plot.ppp=TRUE,
            is.plot.pppcoordinates=FALSE,
            is.plot.legend=TRUE,
            #         legend.position="bottomright",
            legend.text.size=0.7,
            label="",
            in.legend=TRUE,
            in.legend.blives=TRUE,
            in.legend.gof=TRUE,
            is.plot.cb = TRUE,
            persistent=TRUE)
            
    if (!length(args))
        args <- options_abrem
           # return the current option list
    else {
#        if (all(unlist(lapply(args, is.character))))
            # if all items in the args are characters, then
            # treat them as the names of the options.
#            args <- as.list(unlist(args))
        # TODO; the above abrem causes bug 5596
        if (length(args) == 1) {
            if (is.list(args[[1L]]) | is.null(args[[1L]]))
                args <- args[[1L]]
                # unlist the first (and only) argument to a string
            else if(is.null(names(args)))
                # if there is no name to args then
                # the arg itself is the name (?)
            single <- TRUE
        }
    }
    value <- args
    if(options_abrem$persistent){
        options_abrem <<-modifyList(options_abrem, value)
    }
    if(!is.null(args$persistent)){
        value <- args
        if(args$persistent){
            options_abrem <<-modifyList(options_abrem, value)
        }
    }
    # make the options stick between calls of options.abrem()
    if(is.null(names(args)))
        value <- options_abrem[match(args,names(options_abrem))]
    if(single) value <- value[[1L]]
    value
}
# TODO :options that are NULL are not shown in the printout
