\name{pca}
\alias{pca}
\title{
  Principal Component Analysis
}
\description{
  Calculates principal components from a matrix
}
\usage{
pca(mat, cent = TRUE, scle = FALSE)
}
\arguments{
  \item{mat}{a numeric matrix.}
  
  \item{cent}{a logical value referring to center argument in \code{scale}.}
  
  \item{scle}{a logical value referring to scale argument in \code{scale}.}
}
\value{
  a list containing
  \item{pcs}{a matrix of principal component loadings}

  \item{d}{a matrix containing the singular value (eigenvalue) of each
    principal component on its diagonal}

  \item{v}{a matrix of eigenvectors}
}
\author{
  R port by Chris Walsh \email{cwalsh@unimelb.edu.au} from S+
  library by Glenn De'ath \email{g.death@aims.gov.au}.
}
\examples{
         data(soilspec)
         species <- sqrt(soilspec[,2:9])
         specpca <- pca(species)
         eqscplot(specpca$pcs[,1], specpca$pcs[,2], type = "n",
              xlab = "Principal component 1",
              ylab = "Principal component 2") 
         text(specpca$pcs[,1], specpca$pcs[,2], 
              soilspec$site) 
         mtext(paste("Grassland communities in 45 sites"))
}
\keyword{multivariate}



