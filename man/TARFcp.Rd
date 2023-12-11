\name{TARFcp}
\alias{TARFcp}
\title{Proximity based on the completely common pathway to a terminal node}
\description{
The completely common pathway (cp) function is a binary indicator that takes the value 1 if units u and v share a common path from the root to the terminal node and zero otherwise. Because bootstrap sampling is not used in an ARF ensemble, every pair of units is present in the root node of every tree. For depth, D, the expected value of the indicator of the common path proximity of units u and v is the probability that they will not be split at any node as they travel down the path of a tree to the terminal node.
}
\usage{
TARFcp(data,depth)
}
\arguments{
  \item{data}{ an input dataframe without label}
  \item{depth}{the depth of tree}
}

\value{
  An object of class \code{TARFcp}, which is a list with the following components:
  \item{proximity}{output the proximity}

}
\examples{

data(iris)
##Using TARFcp method
rs=TARFcp(iris[,1:4],depth=3)
##output proximity matrix
print(rs)
}
