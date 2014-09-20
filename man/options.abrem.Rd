\name{options.abrem}
\alias{options.abrem}
\alias{cex.points}
\alias{cl}
\alias{col}
\alias{col.grid}
\alias{conf.blives.sides}
\alias{conf.what}
\alias{coordinate.text.size}
\alias{dist}
\alias{in.legend}
\alias{in.legend.blives}
\alias{in.legend.gof}
\alias{is.plot.cb}
\alias{is.plot.fit}
\alias{is.plot.grid}
\alias{is.plot.legend}
\alias{is.plot.ppp}
\alias{is.plot.pppcoordinates}
\alias{label}
\alias{legend.text.size}
\alias{log}
\alias{lty}
\alias{lwd}
\alias{lwd.points}
\alias{main}
\alias{main.contour}
\alias{mar}
\alias{method.conf.blives}
\alias{method.fit}
\alias{pch}
\alias{persistent}
\alias{pivotals}
\alias{ppos}
\alias{S}
\alias{signif}
\alias{sub}
\alias{threshold}
\alias{unrel}
\alias{unrel.n}
\alias{verbosity}
\alias{xlab}
\alias{xlim}
\alias{ylab}
\alias{ylim}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    \pkg{abrem} Options
}
\description{
    This function handles the various calculation, printing and plotting options
    of the \pkg{abrem} package.
}
\usage{options.abrem(\dots)}
\arguments{
    \item{\dots}{
        Options for calculating, printing and plotting with the \pkg{abrem}
        package.
    }
}

\details{
    This function borrows its internal structure from
    the \code{\link[graphics]{par}} function of package \pkg{graphics}.
    It can be used in different ways:

        \code{options.abrem()}

        Returns the currently used options and there values.


        \code{options.abrem()$dist \cr
              options.abrem("dist")}

        Returns the current value of an option.

        \code{options.abrem(cl=0.95) \cr
              options.abrem(list(cl=0.95,S=5e4))}

        Sets the specified options.

    Currently, there is no way to reset the options to the default values using
    this function.  One might, before changing any options, store the option
    list in a temporary variable like

    \code{abrem.defaults <- options.abrem()}

    for restoring it later by running \code{options.abrem(abrem.defaults)}.

    The function creates a globally accessible list named  \code{options_abrem},
    holding the options.  One should always use the  \code{options.abrem}
    function to access the option list, do not access this list directly.

}

\section{Abrem options}{

    \describe{
        \item{\code{cex.points}}{
            A number describing the relative size of the datapoint glyphs.

            Defaults to \code{1}.
        }
        \item{\code{cl}}{
            Confidence level: A single number from the interval \code{[0,[1}
            specifying the confidence level for various confidence calculations.

            Defaults to \code{0.9}.
        }
        \item{\code{col}}{
            An integer or character string describing the color of a
            graphical entity.
            See function \code{\link[grDevices]{colors}} for available colors.

            Defaults to \code{"black"}.
        }
        \item{\code{col.grid}}{
            An integer or character string describing the color of the grid

            Defaults to \code{"gray"}.
        }
        \item{\code{conf.blives.sides}}{
            Either \code{"lower"},  \code{"upper"} or \code{"double"},
            specifying the type of bound(s) to be calculated.

            Defaults to \code{c("double")}.
        }
        \item{\code{conf.what}}{
            A vector of class \code{"character"} describing for which entities
            that confidence should be calculated.

            Defaults to \code{c("blives")}.
        }
        \item{\code{coordinate.text.size}}{
            A number determining the relative coordinate text label size.

            Defaults to \code{0.7}.
        }
        \item{\code{dist}}{
            A character string with the target distribution for fitting.
            See \code{\link{abrem.fit}} for in-depth discussion of the settings.

            Defaults to \code{"weibull"}.
        }
        \item{\code{in.legend}}{
            Logical value controlling the inclusion of various elements in
            the legend. See \code{\link{abrem.fit}} and \code{\link{abrem.conf}}
            for more details.

            Defaults to \code{TRUE}.
        }
        \item{\code{in.legend.blives}}{
            Logical value controlling the inclusion of B-life confidence bounds
            in the legend.

            Defaults to \code{TRUE}.
        }
        \item{\code{in.legend.gof}}{
            Logical value controlling the inclusion of goodness-of-fit
            indicators in the legend. Note that this does \emph{not} prohibit
            calculation of sometimes time consuming \code{prr} values, it only
            prevents inclusion in the legend.
            %% TODO: check if this is still valid (prr <-> pve <-> AbPval)

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.cb}}{
            Logical value controlling the plotting of various types of
            confidence bounds (if any present in the \code{abrem} object).

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.fit}}{
            Logical value controlling the plotting of the fitted line.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.grid}}{
            Logical value controlling the plotting of the grid.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.legend}}{
            Logical value controlling the plotting of the legend.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.ppp}}{
            Logical value controlling the plotting of the
            (life-)time observations' probability plot positions.

            Defaults to \code{TRUE}.
        }
%%        \item{\code{is.plot.pppvalues}}{
%%            Logical value controlling the plotting of the text labels
%%            (the coordinates on the plot) next to the plot positions.
%%
%%            Defaults to \code{FALSE}.
%%        }
        \item{\code{label}}{
            A character string with the title of the legend box, can be used to
            label a dataset or fit.

            Defaults to \code{NULL}.
        }
        \item{\code{legend.text.size}}{
            A number determining the relative legend text size.

            Defaults to \code{0.7}.
        }
        \item{\code{log}}{
            A character string describing the type of plotting canvas to use.
            Possible values are \code{"x"} (weibull canvas), \code{"xy"}
            (lognormal canvas), \code{""} (weibull Y axis, linear X axis)
            and \code{"y"} (lognormal Y axis, linear X axis).


            Defaults to \code{"x"}, Weibull canvas.
            For contour plots, \code{log} is always \code{""} and cannot be set.
        }
        \item{\code{lty}}{
            An integer describing the line type of both the fitted
            line and the confidence bounds, if any.
            See function \code{\link[graphics]{par}} for available line types.

            Defaults to \code{1}.
        }
        \item{\code{lwd}}{
            An integer describing the line width of both the fitted
            line and the confidence bounds, if any.

            Defaults to \code{2}.
        }
        \item{\code{lwd.points}}{
            An integer describing the thickness of the datapoint glyphs.

            Defaults to \code{2}.
        }
        \item{\code{main}, \code{main.contour}}{
            A character string; the main title of the plot or contour plot.

            Defaults to \code{"Probability Plot"} or \code{"Contour Plot"} .
        }
        \item{\code{mar}}{
            A numerical vector of the form \code{c(bottom, left, top, right)}
            which gives the number of lines of margin to be specified on
            the four sides of the plot.
            See function \code{\link[graphics]{par}} for more info.

            Defaults to \code{c(5.1,4.1,5.1,2.1)}.
        }
        \item{\code{method.conf.blives}}{
            A vector of class \code{"character"} describing the techniques used
            for calculating confidence for B-lives.
            See \code{\link{abrem.conf}} for in-depth discussion of the settings.

            Defaults to \code{c("mcpivotals")}.
        }
        \item{\code{method.fit}}{
            A vector of class \code{"character"} with fitting options.
            See \code{\link{abrem.fit}} for in-depth discussion of the settings.

            Defaults to \code{c("rr","xony")}.
        }
        \item{\code{pch}}{
            An integer or single character describing the plotting symbol, used for
            plotting the datapoints.  For more info, see
            \code{\link[graphics]{points}}.

            Defaults to \code{1}.
        }
        \item{\code{persistent}}{
            Experimental parameter, do not use.

            Defaults to \code{TRUE}.
        }
        \item{\code{pivotals}}{
            Currently unused. Defaults to \code{FALSE}.
        }
        \item{\code{ppos}}{
            Short for "probability plotting positions", it is a vector of
            class \code{"character"} describing the type of plotting positions to be
            calculated.
            See \code{\link{Abrem}} for in-depth discussion of the settings.

            Defaults to \code{c("benard")}.
        }
        \item{\code{S}}{
            An integer describing the number of Monte Carlo simulations on
            which the Monte Carlo pivotal confidence bounds and calculation
            of the "prr" goodness-of-fit indicator are based (See
            \code{\link[abremPivotals]{pivotalMC}} of package \pkg{abremPivotals} for more
            details).
            % TODO: rewrite, since there is no mention anymore of prr in abremPivotals
            % Actually, there is access to prr somewhere...

            Defaults to \code{10000}.
        }
        \item{\code{signif}}{
            An integer describing the significant digits of various numbers
            that are displayed in the legend.

            Defaults to \code{4}.
        }
        \item{\code{sub}, \code{sub.contour}}{
            A character string; the subtitle of the plot or contour plot.

            Defaults to \code{""}.
        }
        \item{\code{threshold}}{
            A logical value specifying whether a threshold time value should be
            substracted from the graphical elements before plotting. This is
            particulary useful when fitting three-parameter models like
            \code{weibull3p}, where the third model parameter is a threshold value.

            If \code{TRUE}, the software will subtract the threshold value
            (that is to be calculated by \code{abrem.fit}) from its
            associated graphical elements like plot poistions, fits and confidence bounds.

            If a numeric value is passed, the software will subtract this value
            from the appropriate graphical elements.

            Note: currently, \code{"threshold"} should only be used in combination with
            \code{Abrem} and \code{abrem.fit} and not with
            \code{plot.abrem}. See \code{abrem.fit} for more info.


            Defaults to \code{FALSE}
        }
        \item{\code{unrel}}{
            An unordered numeric vector with unreliability levels for which
            B-life confidence will be calculated.

            Defaults to \code{c(0.1,0.05,0.01)}.
        }
        \item{\code{unrel.n}}{
            An integer controlling the amount of unreliability levels for
            which confidence bounds are calculated and ultimately plotted.
            See \code{\link{abrem.conf}} for more details.

            Defaults to \code{25}
        }
        \item{\code{verbosity}}{
            An integer from \code{c(1,2,3)} setting the level of verbosity of
            all \pkg{abrem} functions. Use this for debugging.

            Defaults to \code{1}.
        }
        \item{\code{xlab}, \code{ylab}}{
            Character strings with the labels for the X and Y axis.

            Default to \code{"Time To Failure"} and
            \code{"Unreliability [\%]"} respectively.
            For contour plots, Default to \code{"Eta"} and \code{"Beta"} respectively.
        }
        \item{\code{xlim}}{
            A numeric vector with two values determining the plotting range
            of the horizontal axis of the plot.
            \code{plot.abrem} calculates horizontal limits automatically
            from its data argument.

            Defaults to \code{NULL}.
        }
        \item{\code{ylim}}{
            A numeric vector with two values determining the plotting range
            of the vertical axis of the plot.  Allowed values come
            from the interval \code{]0,1[}. \code{plot.abrem} calculates
            vertical limits automatically from its data argument; if vertical
            plotting positions do not exceed the standard range of
            \code{c(0.01,0.99)}, then the latter is used.
            Otherwise, the standard range is expanded to include the
            extreme plot positions.

            The standard vertical range is currently hardcoded to \code{c(0.01,0.99)}.

            Defaults to \code{NULL}.
        }
    }
}

\value{
   Executing \code{options.abrem} without arguments returns a named list
   containing the currently active global options of the \pkg{abrem} package.

   When arguments were supplied, these are returned in a named list.
}
\author{Jurgen Symynck \email{jusy@openreliability.org}}
\note{
    Typical usage of \pkg{abrem} involves calling a sequence of functions like:

    \code{da <- Abrem(c(10,11,27))}\cr
    \code{da <- abrem.fit(da)}\cr
    \code{da <- abrem.conf(da)}\cr
    \code{plot(da)}

    Do not call \code{options.abrem} in between these functions because some
    options are locked and cannot be altered further in this chain. This is an
    implication of the way the \code{abrem} object is structured.
    
    The correct time to specify an option is when it is needed for the first
    time. For example, when the color setting option \code{col = "red"} is
    passed as an argument of function \code{Abrem}, it will be used for
    datapoints, fits and confidence bounds. If supplied to \code{\link{abrem.conf}},
    only the confidence bounds will have the specified color, hereby overriding
    any previously inherited color settings from \code{\link{abrem.fit}}
    or \code{\link{Abrem}}.
}
%\section{To Do}{
%    \itemize{
%        \item Add the possibility to reset to the default values.
%        \item Implement returning of the pivotals dataframe in the
%            \code{abrem} object.
%        \item Add detection of unsupported arguments.
%    }
%}
\examples{
## backup options ##
abrem.defaults <- options.abrem()

## setting new options ##
options.abrem(S=5e5,cl=0.99)
%%options.abrem(sub="Testing options.abrem()")

## listing options ##
options.abrem()
options.abrem()$main

## restore options ##
options.abrem(abrem.defaults)
}
