\name{params.to.ob}
\alias{params.to.ob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Create (un-)Censored Testdata from Distribution Parameters}
\description{
    Create a set of (life-)time observations (possibly with censoring),
    perfectly matching a given distribution.
}
\usage{params.to.ob(dist, \ldots)}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{dist}{
        The target distribution for creating the (life-)time observations.
    }
    \item{\ldots}{
        Named arguments for calculating the dataset such as slope, shape,
        number and event vector. See section "Details".
    }
}
\details{
    This function can be used for testing purposes. Internally, it is used for
    the experimental calculation of Monte Carlo Pivotal confidence bounds for
    right censored (life-)time observations.
    
    \describe{
        \item{generating Weibull datasets}{
            \code{params.to.ob("weibull",beta=3,eta=1000,n=5)}
        }
        \item{generating lognormal datasets}{
            \code{params.to.ob("lognormal",mulog=log(1000),sigmalog=log(2),n=5)}
        }
        \item{censoring, event vector}{
            \code{params.to.ob("weibull",beta=3,eta=1000,event=c(1,1,1,0,0))}
        }
    }
    When \code{\link{abrem.fit}} is called on an \code{abrem} object based on
    these (life-)time observations, the same fit parameters will be found as
    those used to generate the dataset.
}
\value{
    A dataframe with two columns:
    \describe{
        \item{\code{$time}}{
            An ordered vector with (life-)time observations.
        }
        \item{\code{$event}}{
            A vector of class \code{"numeric"} with right-censoring indicators.
            See \code{\link{Abrem}} for more details on the indicators.
        }
        
%%        If argument \code{x} was a dataframe with additional columns, these will also be returned.
    }
}
\author{Jurgen Symynck \email{jusy@openreliability.org}}
\note{
    Currently, only distributions fitted with
    \code{method.fit = c("rr","xony")} 
    are supported.
}
\examples{
## generate three synthetic datasets ##
d1 <- params.to.ob("weibull",beta=3,eta=1000,n=10)
d1 <- abrem.fit(Abrem(d1),lwd=1)
print(d1$data)
message(paste0(
    "  beta=",d1$fit[[1]]$beta,
    ", eta=",d1$fit[[1]]$eta))
    
d2 <- params.to.ob("weibull",beta=3,eta=1000,
    event=c(1,1,0,0,0,1,0,1,0,0))
d2 <- abrem.fit(Abrem(d2,pch=3,col="red",cex.points=1.5),lty=3,lwd=3)
print(d2$data)
message(paste0(
    "  beta=",d2$fit[[1]]$beta,
    ", eta=",d2$fit[[1]]$eta))
    
d3 <- params.to.ob("lognormal",mulog=log(1000),sigmalog=log(2),n=10)
d3 <- abrem.fit(Abrem(d3,pch=0),dist="lognormal")
print(d3$data)
message(paste0(
    "  mulog=",d3$fit[[1]]$mulog,
    ", sigmalog=",d3$fit[[1]]$sigmalog))
plot.abrem(list(d1,d2,d3),main="Demo of params.to.ob()")
}