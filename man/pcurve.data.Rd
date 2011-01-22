\name{pcurve.data}
\alias{sim4var}
\alias{sim10var}
\alias{soilspec}
\alias{fish}
\title{
  Example data for pcurve
}
\description{
  Example data sets for pcurve package.
  }
\usage{
  data(sim4var)
  data(sim10var)
  data(fish)
  data(soilspec)
}
\details{
    sim4var.txt

    Simulated data. Comprises 4 response variables (Taxa1 - Taxa4) and
    the generating locations (Location).  Number of cases = 100.  This
    example was worked by De'ath (1999).  The response curves are
    Gaussian and noise is Poisson.  Most starting configurations are
    adequate, square root transformation helps.

  
    sim10var.txt

    Simulated data. Comprises 10 response variables
    (Taxa1 - Taxa10) and the generating locations (Location).  Number
    of cases = 100. The response curves are Gaussian and noise is
    Poisson.  The beta-diversity is high and recovering the generating
    locations is difficult.
    A more difficult exercise.  Transformation is a must (square-root
    is ok).  Many starting configurations fail.  CA or MDS-BC succeed
    with appropriate smoothness.


    fish.txt

    Comprises counts on 10 families of reef fish
    (n = 33) and a factor variable IMO denoting the position of the
    sites across the reef.  Log-transformation is effective and a
    final cross-validation helps improve the fit.  The locations
    vary systematically with cross shelf position (IMO).


    soilspec.txt

    Comprises data on 8 species of plants and 3
    soil characteristics and their interactions.
    Source Gittins (1985), where a relatively complex canonical
    analysis was used to model the data.
    This example was worked by De'ath (1999).
}
\references{
  
De'ath, G. 1999 Principal Curves: a new technique for indirect and direct gradient analysis. \emph{Ecology} \bold{80}, 2237--2253.

Gittins, R. 1985 \emph{Canonical Analysis.  A review with applications in ecology.}  Berlin: Springer-Verlag.
}
\author{
  R port by Chris Walsh \email{cwalsh@unimelb.edu.au} from S+
  library by Glenn De'ath \email{g.death@aims.gov.au}.
}
\keyword{datasets}







