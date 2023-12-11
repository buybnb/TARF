\name{TARFnu}
\alias{TARFnu}
\title{Proximity based on the number of units in the node where u and v diverge}
\description{
The pathway function is the number of units in the node at divergence.
}
\usage{
TARFnu(data,depth)
}
\arguments{
  \item{data}{ an input dataframe without label}
  \item{depth}{the depth of tree}
}

\value{
  An object of class \code{TARFnu}, which is a list with the following components:
  \item{proximity}{output the proximity}

}

\examples{

data(iris)
##Using TARFnu method
rs=TARFnu(iris[,1:4],depth=3)
##output proximity matrix
print(rs)

}
