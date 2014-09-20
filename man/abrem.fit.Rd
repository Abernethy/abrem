\name{abrem.fit}
\alias{abrem.fit}
\alias{dist}
\alias{method.fit}
\alias{prr}
\alias{P-value}
\alias{AbPval}
\alias{threshold}
\alias{in.legend}

\title{
    Fit Distributions to \code{Abrem} Objects
}
\description{
   This function fits probability distributions to \code{Abrem} objects.
}
\usage{abrem.fit(x,\dots)}
\arguments{
    \item{x}{
        Object of class \code{"abrem"}.
    }
    \item{\dots}{
        Options for fitting the (life-)time observations,
        and for plotting the results.
    }
}
\details{
    This function calculates fits for the (life-)time observations in the
    \code{abrem} object and adds them to the object alongside any
    pre-existing fits.

    Fitting options are passed with the \code{dist} and \code{method.fit}
    arguments:
    
    \describe{
        \item{\code{dist}}{
            A character string with the target distribution for fitting.
            Possible values are \code{"weibull"} or \code{"weibull2p"},
            \code{"weibull3p"} (three-parameter Weibull), \code{"lognormal"}
            or \code{"lognormal2p"} and \code{"lognormal3p"} (three-parameter lognormal).
            
            Defaults to \code{"weibull"}.
        }
        \item{\code{in.legend}}{
            Logical value controlling the inclusion of various elements in 
            the legend. 
            
            If \code{in.legend=FALSE} is passed,
            the resulting fit calculations will be omitted from the legend, 
            leaving only observation summary data.
            
            Defaults to \code{TRUE}.
        }
        \item{\code{method.fit}}{
            A vector of class \code{"character"} with fitting options.
            
            Defaults to \code{c("rr","xony")}.
            
            \itemize{
                \item \code{"rr"}: Rank Regression (RR). The parameters are calculated
                using least square linear regression on the probability plot positions that
                were calculated from the (life-)time observations during the creation of the
                \code{Abrem} object (see option \code{\link{ppos}} and
                function \code{\link{Abrem}}). The \code{"rr"} argument invokes
                the \code{\link[stats]{lm}} function of the \pkg{stats} package,
                and its return object can be retrieved from the \code{abrem}
                object under \code{x$fit[[i]]$lm} where \code{x} is the return
                object of \code{abrem.fit} and \code{i} the number of the fit 
                in the abrem object.
                                 
                By passing \code{"rr2"}, one can explicitly use the fast C++ compiled 
                linear regression code from \pkg{abremPivotals} that is also
                used for the Monte Carlo pivotal confidence bound calculation.
                
                If Rank Regression is used then it 
                is \emph{mandatory} to additionally specify either X-on-Y or
                Y-on-X regression.
                
                \item \code{"xony"},\code{"yonx"}: Differentiate between X-on-Y and Y-on-X
                regression, respectively. For rank regression in lifetime analysis,
                it is best practice to use the X values ((life-)time observations)
                as the response variables whose horizontal distance to the fit line
                must be minimized, and the Y values (unreliabilities) as the
                explanatory variable.

                \item \code{"mle"}: Maximum Likelihood Estimation (MLE), using
                many functions of the \pkg{debias} package.

                \item \code{"mle-rba"}: Maximum Likelihood Estimation with Reduced Bias
                based on the median bias of the distributions MLE.
                This options uses functions \code{\link[debias]{RBAbeta}} and 
                \code{\link[debias]{RBAsigma}}of the
                \pkg{debias} package.
            }

            Similar to Rank Regression, by passing \code{"mle2"} or
            \code{"mle2-rba"} one can explicitly use fast C++ compiled code
            from the \pkg{debias} package. Support for these extra
            implementations (in the case of MLE) might be removed in
            future versions.
        }

        Additionally, one can pass any options available from \code{options.abrem},
        such as \code{col} or \code{is.plot.legend}. The graphical options
        will be used when plotting the (life-)time observations using \code{plot.abrem}.
        Subsequent calls to \code{abrem.conf} will inherit these options.

        The "prr" goodness-of-fit indicator is also calculated here when appropriate.
        See the "Examples" section on how to retrieve it, see the "References"
        section for additional information on the prr, pve and AbPval values.
        % \code{\link[abremout:plot.abrem]{plot.abrem}}.
        
        Passing the \code{threshold} parameter here will result in plotting the
        fit (and its associated probability plotting positions) with a
        threshold value subtracted. 
        
        If three-parameter models are used and
        \code{threshold = TRUE}, the calculated third parameter (\code{t0}) of
        the \emph{last three-parameter fit} in the abrem object will be used for 
        plotting the graphical entities. If a numeric value is passed, then 
        only the value from the \emph{last} call to \code{abrem.fit} is used.
        % what if different t0 values are given with different fits? this should
        % result in only one fit used.
        % test the statement that the t0 of the last three-parameter fit
        % is used
        
        Currently, there is no graceful error recovery after attempting to fit
        lifetime data including negative time observations, for example 
        \code{abrem.fit(Abrem(-5:10)).}
        
    }
}
\value{
    The function returns its argument \code{x}, extended with the
    calculated fit and the optional graphical and calculation arguments as
    provided to the function.
}
\author{Jurgen Symynck \email{jusy@openreliability.org}}
\note{
    The \code{$time} column of the \code{abrem} object can have \code{NA}
    values when accompanied with the 'suspended' indicator in the \code{$event}
    column (= \code{0}). While this poses no problem for \code{abrem.fit} when
    using Rank Regression by means of \code{"rr"}, it results in
    different fits when using the Maximum Likelihood Estimantion method of
    \code{\link[survival]{survreg}}. Also, \code{"rr2"} currently doesn't
    support \code{NA} values fully.
}
%\section{To Do}{
%    \itemize{
%        \item Research the effect of \code{NA} values in the (life-)time
%            observations of the \code{abrem} argument when not using rank regression.
%    }
%}
\seealso{
    \code{\link{Abrem}},
    \code{\link[abremPivotals]{AbPval}}
}
\references{
    \itemize{
        \item
            \emph{Improved Goodness of Fit:
                P-value of the Correlation Coefficient}, Wes Fulton
                
            \url{http://www.barringer1.com/jun05prb.htm}
         \item   
            \url{http://www.openreliability.org/r_and_d.html}
            
            Discussion on prr, pve and AbPval by Jacob Ormerod.
    }
}
\examples{
da1 <- Abrem(runif(5,100,2000))
da1 <- abrem.fit(da1,dist="weibull",method.fit=c("rr","xony"),pch=3)

AbPval <- da1$fit[[1]]$gof$AbPval
message("Abernethy's P-value goodness-of-fit of first fit:")
message(paste0(AbPval,ifelse(AbPval >= 10," -> Good fit."," -> BAD fit!")))

da1 <- abrem.fit(da1,dist="weibull",method.fit="mle",col="red1")
da1 <- abrem.fit(da1,dist="weibull",method.fit="mle-rba",col="orange3",lty=2)
da1 <- abrem.fit(da1,dist="lognormal",method.fit=c("rr","xony"),col="steelblue3",pch=8)
#da1 <- abrem.conf(da1)
me <- "Comparison between RR, MLE, MLE-RBA and lognormal2p"
plot(da1,main=me);message(me)
message(paste0(
    "  RR : beta=",signif(da1$fit[[1]]$beta),
    ", eta=",signif(da1$fit[[1]]$eta)))
message(paste0(
    "  MLE: beta=",signif(da1$fit[[2]]$beta),
    ", eta=",signif(da1$fit[[2]]$eta)))
message(paste0(
    "  MLE: beta=",signif(da1$fit[[3]]$beta),
    ", eta=",signif(da1$fit[[3]]$eta)))
    
### threshold parameter usage demo ###
data(abrem_mix1)
earlyda <-abrem_mix1[1:10]
midda   <-abrem_mix1[11:131]
endda   <-abrem_mix1[132:200]
da       <-Abrem(abrem_mix1,col="gray",pch=1,
            label="Complete, unaltered dataset")
da21     <-Abrem(fail=endda,susp=c(earlyda,midda),col="black",pch=19)
da22     <-Abrem(fail=endda,susp=c(earlyda,midda),col="blue",pch=3)
da23     <-Abrem(fail=endda,susp=c(earlyda,midda),col="green3",pch=4)
da21 <- abrem.fit(da21,
    label="threshold=FALSE",dist="weibull3p",threshold=FALSE)
da22 <- abrem.fit(da22,
    label="threshold=TRUE",dist="weibull3p",threshold=TRUE)
da23 <- abrem.fit(da23,
    label="threshold=5000",dist="weibull3p",threshold=1800)
plot.abrem(list(da,da21,da22,da23),xlim=c(10,1e5),
    main="Threshold parameter usage")
}
