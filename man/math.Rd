\name{math}
\alias{math}
\title{THE PROBABILITY THAT TWO UNITS SPLIT AT A NODE IN AN ARF}
\usage{
math(data)
}
\description{
the probability of a fundamental event that takes place in the process of growing an ARF, a split between units u and v with corresponding feature vectors x_u and x_v
}
\arguments{
  \item{data}{an input dataframe without label}
}
\value{
  An object of class \code{math}, which is a list with the following components:
  \item{prox}{output the proximity}
}
\examples{
data(iris)
###get proximity
rs=math(iris[,1:4])
print(rs$prox)
}
