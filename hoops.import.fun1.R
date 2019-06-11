hoops.import.fun1 <- function(GPX)
{ 
	training_matrix <-read.csv("/Users/joshbarbach/Dropbox/SFL/Hoops-R-Training1.csv", header=TRUE)
	AveGP<-mean(training_matrix[,2])
	rowcount<-length(training_matrix[,1])
	GPcount <- sum(training_matrix[,2] > AveGP+5)
	return(GPcount)
}

Hoops <-read.csv(file.choose(), sep=",",header=TRUE)
attach (Hoops)
plot(Hoops[,12], Hoops[,13], xlab="Points", ylab="Rebounds")



plot(Hoops[,12], Hoops[,13], xlab="Points", ylab="Rebounds", asp=2)
abline(lm(Hoops$REB ~ Hoops$PTS. ), col = "green")
PtsMean<-mean(Hoops$PTS.)
PtsSd<-sd(Hoops$PTS.)
PtsZ<-(Hoops$PTS.-PtsMean)/PtsSd
PtsZCount<-sum(PtsZ>0)
cor(Hoops[,12:13])
cor(Hoops[1:141,12], Hoops[1:141,13])


plot(Hoops[1:141,12], Hoops[1:141,13], xlab="Points", ylab="Rebounds", asp=2)
abline(lm(Hoops$REB[1:141] ~ Hoops$PTS.[1:141] ), col = "red")


plot(Hoops[142:350,12], Hoops[142:350,13], xlab="Points", ylab="Rebounds", color="orange", asp=2)
abline(lm(Hoops$REB[142:350] ~ Hoops$PTS.[142:350] ), col = "purple")

plot(Hoops[,12],Cars[,13], xlab="Points", ylab="Rebounds", asp=2)
abline(lm(Hoops$REB ~ Hoops$PTS. ), col = "green")
abline(lm(Hoops$REB[1:141] ~ Hoops$PTS.[1:141] ), col = "red")
abline(lm(Hoops$REB[142:350] ~ Hoops$PTS.[142:350] ), col = "purple")
