TARF <- function(data,depth,clusters=2,method="TARFcp"){

  if(method=="TARFcp"){
    proximity <- TARFcp(data,depth)
  }else if(method=="TARFdiv"){
    proximity <- TARFdiv(data,depth)
  }else{
    proximity <- TARFnu(data,depth)
  }
  D <- sqrt(1-proximity)
  pams <- pam(D, k = clusters)
  result <- list("proximity" = proximity, "clusters" = pams$clustering)
  result
}



