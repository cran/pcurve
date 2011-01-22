\name{pcdiags.plt}
\alias{pcdiags.plt}
\alias{pcres1.plt}
\alias{pcres2.plt}
\alias{pcqqnorm.plt}
\alias{pcchisq.plt}
\alias{pcresid.plt}
\alias{pcfinder.plt}
\alias{pcresp.plt}
\alias{pcflip.plt}
\alias{finder}
\alias{pcdists.plt}
\alias{pchist.plt}
\alias{pcenv.plt}
\title{
  Diagnostic Plots for Principal Curve Analysis
}
\description{
A menu of 12 plots for diagnosis of results from principal curve
analysis, \code{pcurve}
}
\usage{
pcdiags.plt(zz, xx, pch = 1, graphics = TRUE)
}
\arguments{
\item{zz}{an object of class principal curve, being the value of the
  funtion \code{pcurve}.
}
\item{xx}{data.frame or matrix of explanatory (environmental) variables
  to be used in constrained pcs.
}
\item{pch}{symbol to be used in plots.
  }
\item{graphics}{a logical argument of \code{menu} indicating whether a graphics menu should be used. Currently unused.
}
}
\details{
Produces a menu of 12 (or thirteen if xx is not missing) options. Once a
selection is made, return to the menu by left-mouse clicking on the
plot.

0. Exit

1. Residuals plots for each variable on the PC (by the internal function
\code{pcres1.plt})

2. Absolute residuals plot for each variable on the PC (by the internal
function \code{pcres2.plt})

3. QQ normal residuals plot for each variable (by the internal function
\code{pcqqnorm.plt})

4. QQ chi-squared quantile residuals plot (by the internal function
\code{pcchisq.plt})

5. Response plot and residual plot for each variable (by the internal
function \code{pcresid.plt})

6. Differenced locations: Plot of distances between consecutive
locations on the PC (by the internal function \code{pcfinder.plt})

7. Response plots for each variable along the PC (by the internal
function \code{pcresp.plt})

8. Flip plots: Plot of the PC projected onto a bi-plot of the first two
principal coordinates, showing fitted locations of the variables on the
PC.  Left-mouse click to scroll through biplots of other principal
coordinate combinations. (Right-mouse-click to return to the menu)
(Using the internal function \code{pcflip.plt})

9. Fix curve: a utility to break the curve in up to two places (by left
mouse-clicks), re-order the segments and rerun the PC analysis with a
new start. (using the internal function \code{finder})

10. Scatterplots of Eclidean and Bray-Curtis distances against the
PC. (using the internal function \code{pcdists.plt})

11. Histograms of Eclidean and Bray-Curtis distances against the
PC. (using the internal function \code{pchist.plt})

12. A toggle to use Case numbers or symbols in plots

13. Env. vars. vs Gradient: if xx is not missing, plots of distance
along the PC and explanatory variables (using the internal function \code{pcenv.plt})
}
\value{Produces plots
}
\references{ 
De'ath, G. 1999 Principal Curves: a new technique for indirect and direct gradient analysis. \emph{Ecology} \bold{80}, 2237--2253.
}
\author{R port by Chris Walsh \email{cwalsh@unimelb.edu.au} from S+
library by Glenn De'ath \email{g.death@aims.gov.au}.
}
\keyword{hplot}
\keyword{iplot}
\keyword{multivariate}
\keyword{smooth}




