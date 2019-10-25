dev.off()
OWL <<- read.csv("~/OWL.csv", na.strings="#N/A")
par(mfrow=c(1,1))
min=20
tank <- subset(OWL,((OWL$Role=="Tank") & (OWL$Average>=min)),select=c(Player,Average))
dps <- subset(OWL,((OWL$Role=="Offense") & (OWL$Average>=min)),select=c(Player,Average))
support <- subset(OWL,((OWL$Role=="Support") & (OWL$Average>=min)),select=c(Player,Average))
boxplot(tank$Average,dps$Average,support$Average,names=c("Tank","DPS","Support"),
        xlab = "Points",horizontal = TRUE,col="grey")