\name{TARF}
\alias{TARF}
\title{A treeless absolutely random forest}
\description{
The probabilistic structure corresponding to an ARF is called a Treeless Absolute Random Forest (TARF). With high probability, the algorithm will split units whose feature vectors are far apart and keep together units whose feature vectors are similar. Thus, the underlying structure of the data drives the growth of the tree. The expected value of pairwise proximities is obtained for three pathway functions. One, a completely common pathway function, is an indicator of whether a pair of units follow the same path from the root to the leaf node.
}
\usage{
TARF(data,depth,clusters=2,method="TARFcp")
}
\arguments{
  \item{data}{ an input dataframe without label}
  \item{depth}{the depth of tree}
  \item{clusters}{number of clusters based on the proximity matrix}
  \item{method}{method to calculate proximity matrix. method include "TARFcp", "TARFdiv", and "TARFnu"}

}
\value{
  An object of class \code{TARF}, which is a list with the following components:
  \item{proximity}{output the proximity}
  \item{clusters}{output clusters}
}
\examples{
data(iris)
##Using TARF method
rs=TARFdiv(iris[,1:4],depth=3,clusters=2,method="TARFcp")
##output proximity matrix
print(rs$proximity)
print(rs$clusters)
}


