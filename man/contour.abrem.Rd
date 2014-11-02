\name{contour.abrem}
\alias{contour.abrem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    MLE Contour Plotting
}
\description{
    This function adds the \code{.abrem} method to \code{\link[graphics]{contour}}
    from the \pkg{graphics} package.

    Currently, the function plots all MLE contours, found in an \code{abrem}
    object or a list of \code{abrem} objects. 
}
\usage{\method{contour}{abrem}(x, \dots)}
\arguments{
    \item{x}{
        Object of class \code{"abrem"} or a list of \code{abrem} objects.
    }
    \item{\dots}{
        Options for plotting the contours in the \code{abrem} object; see section "Details".
    }
}
\details{
    The \code{\dots} argument can be any graphical parameter that can be
    supplied to \code{\link[graphics]{plot.default}}, and any option that can be
    set by the function \code{\link{options.abrem}}. The options set
    in this way are applied to all graphical elements of the plot, overriding
    any previously supplied options.
    
    One can pass a list of \code{abrem} objects to \code{\link{contour.abrem}}; in
    that case it is mandatory to use the full method name: \code{contour.abrem(\dots)}
    and not \code{contour(\dots)}.
    
    Currently, MLE contours are added to the abrem object by calculating
    Likelihood Ratio confidence bounds for B-lives (see \code{\link{abrem.conf}});
    this function calls the \code{MLEXXXContour} function from package \pkg{debias}, where
    \code{XXX} is an abbreviation for a supported distribution. Currently, only
    two-parameter Weibull is supported, so MLE contours can only
    be calculated for this distribution. The latter function's output is stored
    in the abrem object and its output can be used to plot the contours.
    
    When the distribution fitting type is either MLE or MLE-RBA,
    the MLE  point (by definition always calculated with MLE or MLE-RBA)
    of the contours coincides exactly with the calculated distribution
    parameters. In all other cases, there will be a spatial difference
    between the two. In that case, the correct MLE point is added to the plot as
    a dot.
    
    In the future, calculating contours using a function like \code{abrem.contour()}
    for adding them to the \code{abrem} object as well displaying them will be added
    to package \pkg{abrem}. In the mean time, check out the contour
    functions from package \pkg{debias} for customized contour calculations.
    
    As this function is still in development, no legends are currently plotted.
    However, clever usage ot \code{plot.abrem} can provide all necessary
    information for interpreting the contour plot (see section "Examples").
    
}
\value{
    Currently, the function returns no value. 
}

\author{Jurgen Symynck \email{jusy@openreliability.org}}

\examples{
## some standard options ##
defaults <- options.abrem()
options.abrem(method.conf.blives="lrb",is.plot.cb=FALSE)

## simple example ##
da <- abrem.fit(Abrem(c(round(rweibull(5,3,1000)))))
da <- abrem.conf(da)
par(mfrow=c(1,2))
plot(da)
contour(da)

##      multiple datasets and contours       ##
## while prevention excessive plotting of CB ##
fail1 <- c(round(rweibull(5,1,1000)))
fail2 <- c(round(rweibull(8,2,3000)))

da1 <- abrem.fit(Abrem(fail1,pch=0,col="red",label="First test batch"))
da2 <- abrem.fit(Abrem(fail2,pch=4,col="blue",label="Second test batch"))

da1 <- abrem.conf(da1,cl=0.5 ,in.legend=FALSE)
da1 <- abrem.conf(da1,cl=0.9 ,is.plot.cb=TRUE)
da1 <- abrem.conf(da1,cl=0.95,in.legend=FALSE)
da2 <- abrem.conf(da2,cl=0.5 ,in.legend=FALSE)
da2 <- abrem.conf(da2,cl=0.9 ,is.plot.cb=TRUE)
da2 <- abrem.conf(da2,cl=0.95,in.legend=FALSE)
    # prevent plotting of some confidence bounds to not overload the plot,
    # also prevent inclusion in the legend.
set <- list(da1,da2)
par(mfrow=c(1,2))
plot.abrem(set,xlim=c(5,5e5))
contour.abrem(set)
invisible(options.abrem(defaults))
}
