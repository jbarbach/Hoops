## Function determines the COUNT of the players with positive Z scores in Points Scored
Pos_Z_PTS.fun <-function()
{
	PtsMean<-mean(Hoops$PTS.)
PtsSd<-sd(Hoops$PTS.)
PtsZ<-(Hoops$PTS.-PtsMean)/PtsSd
PtsZCount<-sum(PtsZ>0)
return(PtsZCount)
}

#Assigns the Count to Variable Name HowMany
HowMany<-Pos_Z_PTS.fun()

#Plots Only those Data Points for Rebounds where Player has Positive Z scores in Points Scored
#Adds Regression line for this subset
plot(Hoops[1:HowMany,12], Hoops[1:HowMany,13], xlab="Points", ylab="Rebounds", asp=2)
abline(lm(Hoops$REB[1:HowMany] ~ Hoops$PTS.[1:HowMany] ), col = "red")