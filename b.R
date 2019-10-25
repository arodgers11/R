##window

w<-gwindow("",visible=TRUE,width=200,anchor=c(-1,-1))
t<-glayout(container=w,expand=TRUE)
stats<-c("AVG","H","HR","2B","3B","RBI","AB","TEAM")
statline<-data.frame(avg=0,hits=0,hr=0,doubles=0,triples=0,rbi=0,ab=0,team="")
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

##search button
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

t[STAT+1,1:2] <-  gbutton("Search",container=t,handler=button)

##hotkeys
addHandlerKeystroke(w, function(h, ...){
  if(h$key=="\r") button()
})
addHandlerKeystroke(w, function(h, ...){
  #if(h$key=="\033") dispose(w)
  cat(h$key)
})


f <- function(x,y) {
  if(y<0)
    return(x<=(-y))
  else
    return(x>=y)
}
results <<- Batting

##populate results
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
  if(statline[,1]>1000) stop("CAN'T BAT OVER 1.000")
  results=Batting[which(f(Batting$AVG,floor(statline[,1])/1000)),]
  for(i in 2:(STAT-1)){
    if(statline[,i]>max(results[i+5])) stop("NO RESULTS")
    results=results[which(f(results[i+5],statline[,i])),]
  }
  if(statline[,STAT]!="") results=results[which(results$teamID==statline[,STAT]),]
  cat(length(results$playerID),"RESULTS")
  results[17]=results[1]
  results[1]=Master[match(results$playerID,Master$playerID),"Name"]
  x=c(1,17)
  colnames(results)[x]=c("Name","playerID")
  View(results)
  results<<-results
}