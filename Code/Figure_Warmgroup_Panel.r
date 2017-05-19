####catchments
# Create new catch data 
new.catch <- seq(min(catch),max(catch),length=50)


# Standardize new catch variable
new.catch.stdz <- (new.catch - mean(new.catch))/sd(new.catch)

T <- length(unique(groups))

# container for predicted value
probs1 <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.catch.stdz) ) )
dim(probs1)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.catch.stdz)){ # species
			probs1[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.group[i,t] + out$BUGSoutput$sims.list$beta.group7[i,t]*mean(loglwd) + out$BUGSoutput$sims.list$beta.group3[i,t] * new.catch.stdz[n]) 
		}	
	}
}

mean.probs1 <- matrix(NA, nrow=T, ncol=length(new.catch.stdz))
for(t in 1:T){
	for(n in 1:length(new.catch.stdz)){
		mean.probs1[t,n] <- mean(probs1[,t,n])
	}
}



### CIs
probsLCI1 <- matrix(NA, nrow=T, ncol=length(new.catch.stdz))
for(t in 1:T){
	for(n in 1:length(new.catch.stdz)){
		probsLCI1[t,n] <- quantile(probs1[,t,n],0.05)
	}
}	


probsUCI1 <- matrix(NA, nrow=T, ncol=length(new.catch.stdz))
for(t in 1:T){
	for(n in 1:length(new.catch.stdz)){
		probsUCI1[t,n] <- quantile(probs1[,t,n],0.95)
	}
}	

####slope
# Create new slope data 
new.slope <- seq(min(slope),max(slope),length=50)


# Standardize new slope variable
new.slope.stdz <- (new.slope - mean(new.slope))/sd(new.slope)

T <- length(unique(groups))

# container for predicted value
probs2 <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.slope.stdz) ) )
dim(probs2)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.slope.stdz)){ # species
			probs2[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.group[i,t] + out$BUGSoutput$sims.list$beta.group7[i,t]*mean(loglwd) + out$BUGSoutput$sims.list$beta.group6[i,t] * new.slope.stdz[n]) 
		}	
	}
}

mean.probs2 <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		mean.probs2[t,n] <- mean(probs2[,t,n])
	}
}



### CIs
probsLCI2 <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsLCI2[t,n] <- quantile(probs2[,t,n],0.05)
	}
}	


probsUCI2 <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsUCI2[t,n] <- quantile(probs2[,t,n],0.95)
	}
}	

####forest
# Create new forest data 
new.forest <- seq(min(forest),max(forest),length=50)


# Standardize new slope variable
new.forest.stdz <- (new.forest - mean(new.forest))/sd(new.forest)

T <- length(unique(groups))

# container for predicted value
probs3 <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.forest.stdz) ) )
dim(probs3)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.slope.stdz)){ # species
			probs3[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.group[i,t] + out$BUGSoutput$sims.list$beta.group7[i,t]*mean(loglwd) + out$BUGSoutput$sims.list$beta.group5[i,t] * new.forest.stdz[n]) 
		}	
	}
}

mean.probs3 <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		mean.probs3[t,n] <- mean(probs3[,t,n])
	}
}



### CIs
probsLCI3 <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsLCI3[t,n] <- quantile(probs3[,t,n],0.05)
	}
}	


probsUCI3 <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsUCI3[t,n] <- quantile(probs3[,t,n],0.95)
	}
}	



warm.mean.probs <-  matrix(NA, nrow=3, ncol=length(new.catch.stdz))
warm.probsLCI <-  matrix(NA, nrow=3, ncol=length(new.catch.stdz))
warm.probsUCI <-  matrix(NA, nrow=3, ncol=length(new.catch.stdz))

for(i in 1:length(new.catch.stdz)){
warm.mean.probs[1,i] <- mean.probs1[3,i]
warm.mean.probs[2,i] <- mean.probs2[3,i]
warm.mean.probs[3,i] <- mean.probs3[3,i]}

for(i in 1:length(new.catch.stdz)){
warm.probsLCI[1,i] <- probsLCI1[3,i]
warm.probsLCI[2,i] <- probsLCI2[3,i]
warm.probsLCI[3,i] <- probsLCI3[3,i]}

for(i in 1:length(new.catch.stdz)){
warm.probsUCI[1,i] <- probsUCI1[3,i]
warm.probsUCI[2,i] <- probsUCI2[3,i]
warm.probsUCI[3,i] <- probsUCI3[3,i]}

##########################################
###########################################
res<-6
name_figure <- "Figure_Warmgroup_Panel.png"
png(filename = name_figure, height = 500*res, width = 920*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.0
x.label1 = expression('                   Upstream Catchment Area (km'^2*')')
x.label2 = 'Slope'
x.label3 = 'Percent Forest'
y.label = 'Occurrence Probability'

range(new.catch)

xlab1 <- matrix(NA, nrow=3, ncol=4)
xlab2 <- matrix(NA, nrow=3, ncol=4)


catch.label1 <- c(
(20000000-mean(new.catch))/sd(new.catch),
(40000000-mean(new.catch))/sd(new.catch), 
(60000000-mean(new.catch))/sd(new.catch),
(80000000-mean(new.catch))/sd(new.catch))

slope.label1 <- c(
(0-mean(new.slope))/sd(new.slope),
(0.1-mean(new.slope))/sd(new.slope),
(0.2-mean(new.slope))/sd(new.slope), 
(0.3-mean(new.slope))/sd(new.slope)
)

forest.label1 <- c((60-mean(new.forest))/sd(new.forest),
(70-mean(new.forest))/sd(new.forest), 
(80-mean(new.forest))/sd(new.forest),
(90-mean(new.forest))/sd(new.forest))


for(i in c(1:4)){
xlab1[1,i] <- catch.label1[i]
xlab1[2,i] <- slope.label1[i]
xlab1[3,i] <-forest.label1[i]}

catch.label2 <- c(20,40,60,80)
slope.label2 <- xlab1[2,] * sd(new.slope) + mean(new.slope)
forest.label2 <- xlab1[3,] * sd(new.forest) + mean(new.forest)

for(i in c(1:4)){
xlab2[1,i] <- catch.label2[i]
xlab2[2,i] <- slope.label2[i]
xlab2[3,i] <-forest.label2[i]}


nf <- layout(matrix( c(1:3),nrow=1,ncol=3,byrow=T),  TRUE) 
layout.show(nf)
par(mar=c(0.5,0.5,0.5,0.5),oma=c(3,4,2,1),mai=c(0.2,0.1,0.2,0) )	
#par(mar=c(0.1,0.1,0.1,0.1),oma=c(3,3,0,1),mai=c(0.05,0.05,0.05,0) )


for(i in c(1:3)){

plot(new.catch.stdz, warm.mean.probs[i, ], axes=F, ylim=c(min(warm.probsLCI),max(warm.probsUCI) ), ylab='', xlab='', type='n')
	
i.for <- order(new.catch.stdz )
i.back <- order(new.catch.stdz , decreasing = TRUE )

x.polygon <- c(new.catch.stdz[i.for] , new.catch.stdz[i.back] )
y.polygon <- c(warm.probsLCI[i, ][i.for] , warm.probsUCI[i, ][i.back] )

polygon( x.polygon , y.polygon , col = "gray" , border = NA )

points(new.catch.stdz, warm.mean.probs[i, ], cex=0.8, pch=16,type='l',lty=1)


axis(side=1,cex.axis=1.2, mgp=c(1,0.6,0),tck= -0.01,at=xlab1[i,], labels=xlab2[i,])
	
#y-axis
if( i == 1 ){
	axis(side=2,cex.axis=1.2, mgp=c(0,0.6,0),tck= -0.01, las=1)
	} else {
	axis(side=2,cex.axis=1.2, mgp=c(0,0.2,0),tck= -0.01, labels=F)

  }

mtext(x.label1, line = 1.3, side = 1, adj = -0.001,cex = size.text, outer=T)
mtext(x.label3, line = 1.2, side = 1, adj = 0.89, cex = size.text, outer=T)
mtext(x.label2, line = 1.2, side = 1,  cex = size.text, outer=T)
mtext(y.label, line = 1.6, side = 2, cex = size.text, outer=T)
title <- 'Warmwater Species'
mtext(title, side=3, cex=1.6, outer=T)
box()
}

par(def.par)
dev.off()





