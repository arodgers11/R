setwd("C:/Users/ajr115/OneDrive/Documents/R")
library("gWidgets2", lib.loc="~/R/win-library/3.4")
library("gWidgets2RGtk2", lib.loc="~/R/win-library/3.4")
library("digest", lib.loc="~/R/win-library/3.4")
library("memoise", lib.loc="~/R/win-library/3.4")
library("RGtk2", lib.loc="~/R/win-library/3.4")
Master <- read.csv("C:/Users/ajr115/OneDrive/DOcuments/R/Lahman2016/Master.csv",stringsAsFactors=FALSE)
 NULL -> Master$birthDay
 NULL -> Master$birthMonth
 NULL -> Master$birthYear
 NULL -> Master$birthCountry
 NULL -> Master$birthState
 NULL -> Master$birthCity
 NULL -> Master$deathYear
 NULL -> Master$deathMonth
 NULL -> Master$deathDay
 NULL -> Master$deathCountry
 NULL -> Master$deathState
 NULL -> Master$deathCity
 NULL -> Master$weight
 NULL -> Master$height
 NULL -> Master$debut
 NULL -> Master$finalGame
 NULL -> Master$retroID
 NULL -> Master$bbrefID
Master$Name=paste(Master$nameFirst,Master$nameLast,spe=" ")
Master$Name = substr(Master$Name,1,nchar(Master$Name)-2)
Batting <- read.csv("C:/Users/ajr115/OneDrive/DOcuments/R/Lahman2016/Batting.csv",stringsAsFactors=FALSE)
 NULL -> Batting$stint
 NULL -> Batting$CS
 NULL -> Batting$IBB
 NULL -> Batting$SH
 NULL -> Batting$SF
 NULL -> Batting$GIDP
Batting$AVG=round(Batting$H/Batting$AB,3)
Batting=Batting[Batting$lgID=="AL" | Batting$lgID=="NL",]
Batting=Batting[Batting$yearID>=1900,]
Batting=Batting[Batting$playerID %in% Master$playerID,]
Batting=Batting[c(1,2,3,4,5,17,8,11,9,10,12,6,13,14,15,16)]
Pitching <- read.csv("C:/Users/ajr115/OneDrive/DOcuments/R/Lahman2016/Pitching.csv")
 NULL -> Pitching$stint
 NULL -> Pitching$IBB
 NULL -> Pitching$WP
 NULL -> Pitching$HBP
 NULL -> Pitching$BK
 NULL -> Pitching$BFP
 NULL -> Pitching$GF
 NULL -> Pitching$SH
 NULL -> Pitching$SF
 NULL -> Pitching$GIDP
Pitching=Pitching[Pitching$lgID=="AL" | Pitching$lgID=="NL",]
Pitching=Pitching[Pitching$year>=1900,]
Pitching=Pitching[Pitching$playerID %in% Master$playerID,]
Pitching=Pitching[c(1,2,3,4,7,19,5,6,12,17,16,20,18)]