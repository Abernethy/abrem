\name{plot.abrem}
\alias{plot.abrem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    \code{Abrem} Object Plotting
}
\description{
    This function adds the \code{.abrem} method to \code{\link[graphics]{plot}}
    from the \pkg{graphics} package.

    Currently, the function plots the (life-)time observations, fits (if any)
    and confidence bounds for B-lives (if any) of an
    \code{abrem} object or a list of \code{abrem} objects on Weibull or
    lognormal probability paper. 

    For each fit in the (list of) \code{abrem} object(s), legends are added to
    the plot, displaying the fit parameters and (if available) goodness-of-fit
    indicators and confidence information.
}
\usage{\method{plot}{abrem}(x, \dots)}
\arguments{
    \item{x}{
        Object of class \code{"abrem"} or a list of \code{abrem} objects.
    }
    \item{\dots}{
        Options for plotting the \code{abrem} object; see section "Details".
    }
}
\details{
    The \code{\dots} argument can be any graphical parameter that can be
    supplied to \code{\link[graphics]{plot.default}}, and any option that can be
    set by the function \code{\link{options.abrem}}. The options set
    in this way are applied to all graphical elements of the plot, overriding
    any previously supplied options.
    
    One can pass a list of \code{abrem} objects to \code{\link{plot.abrem}}; in
    that case it is mandatory to use the full method name: \code{plot.abrem(\dots)}
    and not \code{plot(\dots)}.
    
    The calculated Weibull or lognormal distribution fits are plotted
    on Weibull probability paper by default, but by passing the argument
    \code{\link{log} = "xy"} to the function, lognormal paper is used
    (see \code{\link{options.abrem}}).
    
    When a \emph{list} of abrem objects is passed, the plot window is generated
    with the options of the first \code{abrem} object in the list.
    % TODO: list these options
        
%    A thin dashed line appears close to the fitted line (if any) when confidence
%    are present and plotted.  
}
\value{
    Currently, the function returns no value. 
}
%\section{To Do/Known issues/Bugs}{
%    \itemize{
%        \item The legend positions are slightly to the right, overlapping the
%            right Y axis (especially for small horizontal ranges).
%        \item Have the function return the updated \code{abrem} arguments.
%   }
%}
\author{Jurgen Symynck \email{jusy@openreliability.org}}

\seealso{
    \code{\link{options.abrem}}
}
\examples{
da1 <- Abrem(rweibull(5,3,1000))
da1 <- abrem.fit(da1,dist="weibull",  method.fit=c("rr","xony"),pch=3)
da1 <- abrem.fit(da1,dist="weibull",  method.fit="mle",col="red1")
da1 <- abrem.fit(da1,dist="weibull",  method.fit="mle-rba",col="orange",lty=2)
da1 <- abrem.fit(da1,dist="lognormal",method.fit=c("rr","xony"),
    col="steelblue3",pch=8)
da1 <- abrem.conf(da1)
plot(da1,col="black") # plot all in black
plot(da1,main="Weibull distributed observations") # plot with specified colors
    
### usage of lists ###
options.abrem(blives=0.1) # make the legend boxes a bit shorter...
da2 <- abrem.conf(abrem.fit(Abrem(runif(5,10,100),col="red")))
da3 <- abrem.conf(abrem.fit(Abrem(rweibull(5,2,1000),col="green4",pch=3)))
da4 <- abrem.conf(abrem.fit(Abrem(rlnorm(5,log(500),log(2)),col="blue3",pch=8),
    dist="lognormal"))

plot.abrem(list(da2,da3,da4),xlim=c(1,1e6),
    main="Uniformly distributed observations")
}
