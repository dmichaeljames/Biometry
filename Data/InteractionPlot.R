#create data file
cage <- rep(c("Cage1","Cage2","Cage3"), c(8,8,8))
individual <- factor(rep(rep(1:4, c(2,2,2,2)), 3))
wing <- c(58.5, 59.5, 77.8, 80.9, 84.0, 83.6, 70.1, 68.3, 69.8, 69.8, 56.0, 54.5, 50.7, 49.3, 63.8, 65.8, 56.6, 57.5, 77.8, 79.2, 69.9, 69.2, 62.1, 64.5)
mosquito <- data.frame(cage, individual, wing)
rm(cage,individual,wing)

#calculate group means
mos.c1<-mosquito[mosquito$cage=="Cage1", ]
mos.c2<-mosquito[mosquito$cage=="Cage2", ]
mos.c3<-mosquito[mosquito$cage=="Cage3", ]
c1.means<-tapply(mos.c1$wing, mos.c1$individual, mean)
c2.means<-tapply(mos.c2$wing, mos.c1$individual, mean)
c3.means<-tapply(mos.c3$wing, mos.c1$individual, mean)
group.means<-rbind(c1.means, c2.means, c3.means)
group.means
group.means.t<-t(group.means)

#calculate group standard errors of the means
sem<-function(x){
    s<-sd(x)
    n<-length(x)
    s/sqrt(n)}
c1.sems<-tapply(mos.c1$wing, mos.c1$individual, sem)
c2.sems<-tapply(mos.c2$wing, mos.c1$individual, sem)
c3.sems<-tapply(mos.c3$wing, mos.c1$individual, sem)
group.sems<-rbind(c1.sems, c2.sems, c3.sems)
group.sems
group.sems.t<-t(group.sems)

#plot
lower95<-group.means.t+group.sems.t*qnorm(0.025)
upper95<-group.means.t+group.sems.t*qnorm(0.975)
xvals<-barplot(group.means.t, ylim=c(0,90), beside=T, legend.text=rownames(group.means.t))
arrows(xvals,lower95, xvals, upper95, angle=90, code=3, length=0.1)
box()
title(xlab="Cages", ylab="Wing Length")