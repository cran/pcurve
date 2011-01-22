\name{pcurve}
\alias{pcurve}
\title{
  Principal Curve Analysis
}  
\description{
Fits a principal curve to a numeric multivariate dataset in arbitrary dimensions. Produces diagnostic plots.  
}
\usage{
pcurve(x, xcan = NULL, start = "ca", rank = FALSE, cv.fit = FALSE,
penalty= 1, cv.all = FALSE, df = "vary", fit.meth = "spline",
canfit = "lm",candf = FALSE, vary.adj = FALSE, subset,
robust = FALSE, lowf = 0.5, min.df, max.df, max.df.cv.fit,
ext.dist = TRUE, ext.dc = 0.9, metric = "bray", latent = FALSE,
plot.pca = TRUE, thresh = 0.001, plot.true = TRUE,
plot.init = FALSE, plot.segs = TRUE, plot.resp = TRUE,
plot.cov = TRUE, maxit = 10, stretch = 2, fits = FALSE,
prnt.fits = TRUE, trace = TRUE, trace.all = FALSE, pch = 1,
row.chk0 = FALSE, col.chk0 = TRUE, use.loc = FALSE)
 }
\arguments{
  \item{x}{numeric data matrix or data.frame.}
  
  \item{xcan}{data.frame or matrix of explanatory variables to be used in constrained PCs.}

  \item{start}{specifies how to determine the starting configuration (location of points on initial curve): "ca" = correspondence analysis; "pca" = principal components analysis with Euclidan metric;  "pca.bc" = principal components analysis with Bray-Curtis metric; "mds" = non-metric multidimensional scaling with Euclidean metric; "mds.bc" = non-metric multidimensional scaling with Bray-Curtis metric; "cs.bc" = classical scaling (metric multidimensional scaling) with Bray-Curtis metric; "ran" = random start.  Or if start is numeric and of length dim(x)[1] a user supplied configuration will be used.}
  
  \item{rank}{if TRUE starting configuration is transformed to rank}

  \item{cv.fit}{if TRUE a final iteration using cross-validation is done.}

  \item{penalty}{penalty for smoothing spline. A value of 1 corresponds
    to no penalty with values > 1 giving a less-smoothed fit. Increasing
    the penalty for small data sets can reduce over-fitting.  If penalty =
    "np", penalty = 1 for  N > 1000, penalty = 2 for N <=100, and
    penalty = 4-log(N, 10) for N  > 100 and N <= 1000.}

  \item{cv.all}{if TRUE a cross-validated smoothing spline fit at each iteration.}

  \item{df}{if numeric specifies the df for the smoothing spline.}
  
  \item{fit.meth}{specifies smoother. "spline" = smooth.spline, "poisson" = poisson general additive model, "binomial" = binomial general additive model, "lowess" = lowess smoother (this argument overridden by robust = TRUE).}

  \item{canfit}{"lm" or "gam", model used to relate pc to xcan.}

  \item{candf}{if canfit = "gam", df for model. May be a single value or
  a vector of FALSE or positive integers indicating dfs for each
  explanatory  variable in xcan.  If FALSE, this is equivalent to
  fx=FALSE in \code{gam}, and d.f. is selected by GCV.UBRE}

  \item{vary.adj}{if FALSE the same df are used for the smooth of each variable, otherwise each variable  has its own df.}

  \item{subset}{used to take a subset of x and start (if numeric).}

  \item{robust}{if TRUE uses lowess smooths, if FALSE uses smoothing spline.}

  \item{lowf}{specifies the span of the lowess smooth.}

  \item{min.df}{specifies the min df for the smoothing.}

  \item{max.df}{specifies the max df for smoothing during cross-validation.}

  \item{max.df.cv.fit}{specifies the max df for the smoothing.}
  
  \item{ext.dist}{if TRUE extended dissimilarities in calculation of
    initial configuration using the flexible shortest path. If FALSE
    standard dissimilarites are used (see De'ath, 1999b and
    \code{stepacross} in package vegan).}

  \item{ext.dc}{critical distance, the toolong argument in \code{stepacross}.}

  \item{metric}{similarity metric, the method argument in \code{vegdist}
    in package vegan.}

  \item{latent}{if FALSE locations are rescaled after each iteration to give distance along the curve; if TRUE no rescaling is done.}

  \item{plot.pca}{if TRUE the fitting is plotted (assuming plot.true = TRUE) in the first 2  dimensions of PCA space.}
  
  \item{thresh}{threshold value of difference in cross-validation for ceasing iteration}
  
  \item{plot.true}{if TRUE the fitting process is plotted.}

  \item{plot.init}{ if TRUE the initial fits to each variable are plotted.}

  \item{plot.segs}{if TRUE segments linking the fitted points on the curves to their corresponding data points are plotted.}

  \item{plot.resp}{if TRUE the final response curves are plotted.}

  \item{plot.cov}{if TRUE covariate partial effects are plotted (only if xcan is not null).}

  \item{maxit}{specifies the maximin number of iterations.}

  \item{stretch}{end segments of the curve are stretched by this factor at each iteration.}

  \item{fits}{if TRUE value of pcurve includes diagnostics for each variable.}

  \item{prnt.fits}{statistics on model fits printed.}

  \item{trace}{prints out useful fitting diagnostics at each iteration.}

  \item{trace.all}{if TRUE prints out all curve details at each iteration.}

  \item{pch}{symbol for plots}

  \item{row.chk0}{if TRUE checks for and removes rows of x identically 0.}

  \item{col.chk0}{if TRUE checks for and removes columns of x identically 0.}

  \item{use.loc}{if TRUE pauses during the fitting displays (left mouse-click to progress to next plot).}
}
\details{
See De'ath (1999a) for a full discussion of the functions and their application.
}
\value{
  An object of class principal curve containing a list comprising

  \item{s}{fitted values}
  
  \item{tag}{order of points along the curve}

  \item{lambda}{locations along the curve}

  \item{dist}{sum of squared distances of points from the curve}

  \item{c}{call to pcurve}

  \item{x}{data to which the curve was fitted}

  \item{df}{degrees of freedom for the smoothers used in the fit}

  \item{fit.list}{diagnostics for each variable, only included if fits = TRUE.}
}
\references{
  
De'ath, G. 1999a Principal Curves: a new technique for indirect and direct gradient analysis. \emph{Ecology} \bold{80}, 2237--2253.

De'ath, G. 1999b Extended dissimilarity: method of robust estimation of ecological distances with high beta diversity. \emph{Plant Ecology} \bold{144}, 191--199.

Gittins, R. 1985 \emph{Canonical Analysis.  A review with applications in ecology.}  Berlin: Springer-Verlag.

Hastie, T.J and Tibshirani, R.J. 1990 \emph{Generalized additive models.} London: Chapman and Hall.

Hastie, T.J. and Stuetzle, W. 1989 Principal Curves. \emph{Journal of the American Statistical Association} \bold{84}, 502--516.
}
\author{
R port by Chris Walsh \email{cwalsh@unimelb.edu.au} from S+ library by Glenn De'ath \email{g.death@aims.gov.au}. Original S code for principal curve analysis by Trevor Hastie \email{hastie@stat.stanford.edu}.
}
\seealso{\code{\link{pcdiags.plt}}, \code{\link[vegan]{vegdist}},  \code{\link[vegan]{stepacross}}}
\examples{
#a simulated dataset with 4 response variables (taxa 1-4),
#n=100.  The response curve is Gaussian and noise is Poisson.
    data(sim4var)
    sim4fit <-  pcurve(sim4var, plot.init = FALSE, use.loc = TRUE)

#Limestone grassland community example worked by De'ath (1999a),
#from data in Gittins (1985)
    data(soilspec)
    species <- sqrt(soilspec[,2:9])
    envvar <- soilspec[,10:12]
#indirect gradient analysis
    spec.fit <- pcurve(species, start = "mds.bc", plot.init = FALSE,
                       use.loc = TRUE)
#direct gradient analysis
    soilspec.fit <- pcurve(species, xcan = envvar, 
                           start = "mds.bc", plot.init = FALSE,  
                           fits = TRUE, prnt.fits = TRUE,
                           use.loc = TRUE)
}
\keyword{multivariate}
\keyword{smooth}
\keyword{loess}
\keyword{hplot}
