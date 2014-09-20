\name{print.abrem}
\alias{print.abrem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    \code{Abrem} Object Printing
}
\description{
    This function prints the \code{abrem} object while - for clarity - omitting
    the long \code{$options} named list.
}
\usage{\method{print}{abrem}(x, \dots)}
\arguments{
    \item{x}{
        Object of class \code{"abrem"}.
    }
    \item{\dots}{
        Further arguments passed to or from other methods.
    }
}

\author{Jurgen Symynck \email{jusy@openreliability.org}}

\examples{
da <- abrem.fit(Abrem(rweibull(13,3,1000)))
print(da)
}
