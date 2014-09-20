\name{datasets}
\alias{abrem_mix1}
\docType{data}
\title{
    Synthetic Datasets.
}
\description{
    A collection of synthetic datasets. Currently, only one dataset is provided
}

\usage{
    abrem_mix1
}
\format{
%    Either a numeric vector, or an ordered dataframe with columns \code{$time}
%    ((life-)time observation s) and \code{$event} (censoring indicators).

    \describe{
        \item{\code{abrem_mix1}}{
            "Diaphragm life data of an acid gas compressor".  A numeric vector
            with complete, mixed data (200 failures) with multiple failure modes,
            modeled on a real (proprietary) dataset.
        }
    }
    
    All datasets have comments; see \code{\link[base]{comment}} for more info.
    
}
\keyword{datasets}