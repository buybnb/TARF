\name{TARFdiv}
\alias{TARFdiv}
\title{Proximity based on the expected number of nodes in the common path to divergence of u and v}
\description{
The node count to divergence pathway function for two units is a count s uv of the number of nodes that u and v share until either they split, or they jointly reach their common terminal node.
}
\usage{
TARFdiv(data,depth)
}
\arguments{
  \item{data}{ an input dataframe without label}
  \item{depth}{the depth of tree}

}
\value{
  An object of class \code{TARFdiv}, which is a list with the following components:
  \item{proximity}{output the proximity}
}

\examples{

data(iris)
##Using TARFdiv method
rs=TARFdiv(iris[,1:4],depth=3)
##output proximity matrix
print(rs)

}


