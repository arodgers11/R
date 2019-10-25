w<-gwindow("",visible=TRUE,width=200,anchor=c(-1,-1))
t<-glayout(container=w,expand=TRUE)
stats<-c("G","ERA","W","L","IP","SO","BB","R","BAOpp","TEAM")
statline<-data.frame(era=0,w=0,l=0,g=0,ip=0,k=0,bb=0,r=0,baopp=0,team="")
STAT=length(stats)
for(i in 1:STAT) {
  if(i<STAT){
    t[i,1]<-stats[i]
    t[i,2]<-gedit(container=t,coerce.with = as.numeric,width=15)
  }
  else{
    t[i,1]<-stats[i]
    t[i,2]<-gedit(container=t,coerce.with = as.character,width=15)
  }
}
focus(w)<-TRUE
visible(w)<-TRUE
button<-function(h,...) {
  cat("\014")
  tmp<-t[STAT,2]
  if(!is.na(svalue(tmp))) svalue(tmp)<-toupper(svalue(tmp))
  if(nchar(svalue(tmp))>3) svalue(tmp)<-substr(as.character(svalue(tmp)),1,3)
  for(i in 1:(STAT-1)) {
    tmp<-t[i,2]
    if(is.na(svalue(tmp)))
      statline[,i]<<-0
    else statline[,i]<<-svalue(tmp)
  }
  tmp<-t[STAT,2]
  if(is.na(svalue(tmp))) svalue(tmp)<-""
  statline[,STAT]<<-svalue(tmp)
  populateresults()
}
t[STAT+1,1] <- gbutton("Search",container=t,handler=button)
addHandlerKeystroke(w, function(h, ...){
  if(h$key=="\r") button()
})
addHandlerKeystroke(w, function(h, ...){
  if(h$key=="\033") dispose(w)
})
f <- function(x,y) {
  if(y<0)
    return(x<=(-y))
  else
    return(x>=y)
}
results <<- Pitching
populateresults<<-function(h,...) {
  allzero <- TRUE
  for(i in 1:(STAT-1)){
    if(floor(abs(statline[,i]))!=0)
    {allzero <- FALSE
    return}
    if(is.na(statline[,STAT]))
    {allzero <- FALSE
    return}
  }
  if(allzero) return
  for(i in 1:(STAT-1)){
    if(statline[,i]>max(results[i+4])) stop("NO RESULTS")
    results=results[which(f(results[i+4],statline[,i])),]
  }
  if(!is.na(statline[,STAT])) results=results[which(results$teamID==statline[,STAT]),]
  cat(length(results$playerID),"RESULTS")
  results[14]=results[1]
  results[1]=Master[match(results$playerID,Master$playerID),"Name"]
  x=c(1,14)
  colnames(results)[x]=c("Name","playerID")
  View(results)
  results<<-results
}