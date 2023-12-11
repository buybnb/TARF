TARFnu=function(data,depth){
  pp=math(data)$prox
  ppp=pp
  for(i in 1:ncol(pp)){
    ppp[,i]=apply(pp,2,mean)
  }
  proximity=pp
  for(j in 2:depth){
    proximity=proximity+pp*((1-pp)^(j-1))*(ppp^(j-1))
  }
  proximity
}
