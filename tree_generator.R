library("phytools", lib.loc="~/R/win-library/3.4")
for(n in 6:20) {
  d<-c()
  t=unroot(rtree(n))
  s=paste('./Test Trees/','d',n,'.m',sep='')
  cat(paste('n=',n,';\n\n',"d = [ ",sep=''),file=s,append=FALSE)
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      d<-c(d,length(nodepath(t,i,j))-2)
      cat(length(nodepath(t,i,j))-1,' ',sep='',file=s,append=T)
    }
  }
  cat("];\n",file=s,append=T)
  cat('xtrue = [',2^(n-2-d),sep=' ',file=s,append=T)
  cat(' ];\n\n',file=s,append=T)
}