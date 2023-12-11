TARFdiv=function(data,depth){
  proximity=math(data)$prox
  proximity=pp*(1-pp^depth)/(1-pp)
  proximity[is.nan(proximity)]=1
  proximity=proximity/max(proximity)
  diag(proximity)=1
  proximity
}
