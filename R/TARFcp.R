TARFcp <- function(data,depth){
  pp <- math(data)$prox
  proximity <- pp^depth
  proximity
}
