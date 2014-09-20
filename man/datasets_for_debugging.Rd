\name{debugging datasets}
\alias{abrem2}
\alias{abrem3}
\alias{abrem13}
\alias{abrem13c1}
\alias{abrem13c2}
\alias{abrem13c3}
\alias{abrem13c4}
\docType{data}
\title{
    Synthetic Datasets, for Demo's and Debugging \pkg{abrem} .
}
\description{
    A collection of synthetic datasets for debugging the \pkg{abrem} package,
    created using the \code{\link{params.to.ob}} function. When these datasets
    are fitted with a two-parameter Weibull distribution using X-on-Y rank
    regression on exact 'median' probability plotting positions 
    (\code{ppos = "beta"}), the resulting parameters \code{beta} and
     \code{eta} are (almost) exactly 3 and 1000, respectively.
}
\note{
    The observations in this dataset are rounded output values from
    \code{params.to.ob}. Even higher precision can be achived by using the output of
    \code{params.to.ob} directly, as evidenced by comparing the goodness-of-fit
    indicators from both fits.
}
\usage{
    abrem2
    abrem3
    abrem13
    abrem13c1
    abrem13c2
    abrem13c3
    abrem13c4
}
\format{
    An ordered dataframe with columns \code{$time}
    ((life-)time observations) and \code{$event} (censoring indicators).

    All datasets have comments; see \code{\link[base]{comment}} for more info.
    
    \describe{
        \item{\code{abrem2}}{
            Two failures, no censoring.
        }
        \item{\code{abrem3}}{
            Three failures, no censoring.
        }
        \item{\code{abrem13}}{
            Thirteen failures, no censoring.
        }
        \item{\code{abrem13c1}}{
            Type II censored dataset with 13 observations: two failures
            and 11 suspensions (at the same time as the latest failure).
        }
        \item{\code{abrem13c2}}{
            Type II censored dataset with 13 observations: three failures
            and 10 suspensions (at the same time as the latest failure).
        }
        \item{\code{abrem13c3}}{
            Type II censored dataset with 13 observations: ten failures
            and three suspensions (at the same time as the latest failure).
        }
        \item{\code{abrem13c4}}{
            Type II censored dataset with 13 observations: 12 failures
            and one suspension (at the same time as the latest failure).
        }
    }
}
\keyword{datasets}
