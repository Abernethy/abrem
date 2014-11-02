\name{Abrem}
\alias{Abrem}
\alias{ppos}
\alias{ppp}
\alias{event}
\alias{susp}
\alias{beta}
\alias{Benard}
\alias{mean}
\alias{km}
\alias{Kaplan-Meier}
\alias{Hazen}
\alias{Blom}
%% should the above be capitalized?

\title{
    Create an \code{abrem} Object for Lifetime and Reliability Analysis
}
\description{
    This function creates an object of class \code{"abrem"} for further processing
    by the other functions of \pkg{abrem}.
}
\usage{Abrem(x,\dots)}
\arguments{
    \item{x}{
        Either a dataframe containing at least \code{$time} and \code{$event}
        columns, or a vector of class \code{"numeric"} or \code{"integer"} with
        (life-)time observations.
        
        See section "Details" for other data passing arguments.
    }
    \item{\dots}{
        Graphical options for plotting the \code{abrem} object; see section "Details".
    }
}
\details{
    There are several methods to passing arguments for building an \code{abrem}
    object.
    \itemize{
        \item When a single unnamed vector of class \code{"numeric"}
            or \code{"integer"} is supplied, it is treated as a vector
            of (life-)time observations.
        \item If argument \code{event} is provided, it is treated as
            a vector of event indicators detailing the observations in \code{x}. 
            Possible values are \code{0} and \code{1}. 
            See section "Value" for more details on event vectors.
        \item If argument \code{susp} is provided, it is treated as
            a vector of right-censored (life-)time observations (also called
            suspended observations or suspensions).
            In that case, argument \code{x} is treated as a vector of 
            complete-failure times.
        \item If argument \code{x} is of class \code{"data.frame"},
            then it should at least contain \code{$time} and \code{$event}
            columns. Additional columns in the dataframe will be reused in the
            \code{abrem} object, allowing for extra information like
            serial numbers to be included (see section "Examples").
        %%\item If argument \code{interval}  is provided, ...        
    }

    \code{Abrem} \emph{always} generates probability plotting positions for
    graphically displaying the (life-)time observations and for later usage
    by \code{\link{abrem.fit}} when using Rank Regression.
    The type of plotting positions to be calculated (also quite confusingly
    called "ranks") is controlled by argument \code{ppos}, a vector of
    class \code{"character"}. Currently, all ranking calculations are done by
    function \code{\link[abremPivotals]{getPPP}} from
    package \pkg{abremPivotals}. Supported ranking methods include:
    \itemize{
        \item \code{"beta"}: Ranking using the Incomplete Beta function, also called "exact median ranks".
        \item \code{"benard"}: A good approximation to exact median ranks,
        and currently the default method.
        \item \code{"mean"}, also known as Herd-Johnson.
        \item \code{"km"}: Kaplan-Meier ranking with modification for final complete failure.
        \item \code{"hazen"} or \emph{modified} Kaplan-Meier.
        \item \code{"blom"}.
    }

    All methods can be passed in the \code{ppos} argument vector
    at the same time but currently, only the first element in the \code{ppos}
    vector will be used for further fitting by \code{\link{abrem.fit}}.

    Note that is is currently allowed to have \code{NA} values in \code{x}.
    In that case, the vector is expected to be ordered and no ordering will
    be applied by \code{Abrem}. This feature is
    useful in combination with the output of \code{\link{params.to.ob}}.

    Additionally, one can supply any options available from \code{options.abrem},
    such as \code{col} or \code{is.plot.legend}. Some of these options
    will be used when plotting the (life-)time observations using \code{plot.abrem}.
    Subsequent calls to \code{abrem.fit} and \code{abrem.conf} will inherit these options.

    % \code{\link[abremout:plot.abrem]{plot.abrem}}.
}
\value{
    A named list of class \code{"abrem"}. The first list
    item (\code{$data}) is a dataframe with at least three columns:
    \describe{
        \item{\code{$time}}{
            An ordered vector with (life-)time observations.
            %% TODO:what class can this vector be?
        }
        \item{\code{$event}}{
            A vector of class \code{"numeric"} with suspension (right-censoring)
            indicators.
            Values of \code{1} mean "dead" or "failed" while \code{0}
            mean "alive" or "suspended"/"right-censored" observations.
            This censoring indicator scheme is modeled after the
            \code{Surv} function of the \pkg{survival} package.
            %% add support for factors
        }
        \item{\code{$ppp. \dots }}{
            Depending on the argument \code{\link{ppos}} (either passed as
            an argument or taken from \code{options.abrem}), a vector
            of class \code{"numeric"} with the probability plot positions' y-coordinates. 
            The exact column name depends on the selected ranking method.
            
            Defaults to \code{$ppp.benard}.
        }
    }
}
\author{Jurgen Symynck \email{jusy@openreliability.org}}
\examples{
## These code lines all generate the same object ##
Abrem(x=c(500,1200,900,1300,510))
Abrem(c(500,1200,900,1300,510),event=c(1,1,1,1,1))
Abrem(c(500,1200,900,1300,510),susp=c())
da1 <- data.frame(
    serial=c("S12","S16","S17","S3","S5"),
    time=c(500,1200,900,1300,510),
    event=c(1,1,1,1,1))
Abrem(da1,label="complete dataset",pch=1)
da1 <- Abrem(da1,label="complete dataset",pch=3,col="orange3")

## Generate a similar dataset, but with suspensions ##
Abrem(c(500,1200,900,1300,510),event=c(1,1,1,0,0))
Abrem(data.frame(time=c(500,1200,900,1300,510),event=c(1,1,1,0,0)))
Abrem(c(500,1200,900),susp=c(1300,510))
da2 <- Abrem(c(500,1200,900,1300,510),
    event=c(1,1,1,0,0),label="censored dataset",pch=1,col="blue")

## plot datasets ##
plot.abrem(list(da1,da2))

## different ranking methods ##
## note that ppos is implemented case insensitive ##
lto <- runif(8,100,1000)

da3 <- list(Abrem(lto,ppos="Benard",col="black",pch=1),
           Abrem(lto,ppos="beta",col="blue",pch=2),
           Abrem(lto,ppos="mean",col="green",pch=3),
           Abrem(lto,ppos="KM",col="yellow3",pch=4),
           Abrem(lto,ppos="Hazen",col="orange",pch=5),
           Abrem(lto,ppos="Blom",col="red",pch=6))
da3 <- abrem.fit(da3)
plot.abrem(da3,main='Comparing different ranking methods.',in.legend.gof=FALSE)
}
