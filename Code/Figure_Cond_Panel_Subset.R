# Create new discharge data 
cond <- apply(cond3, 1, mean)

new.cond <- seq(min(cond),max(cond),length=50)

# Standardize new discharge variable
new.cond.stdz <- (new.cond - mean(new.cond))/sd(new.cond)

T <- length(unique(df4$species))

# container for predicted value
probs <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.cond.stdz) ) )
dim(probs)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.cond.stdz)){ # species
			probs[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.p[i,t] + out$BUGSoutput$sims.list$beta.p2[i,t]*mean(loglwd) + 
								out$BUGSoutput$sims.list$beta.p1[i,t] * new.cond.stdz[n])
		}	
	}
}

mean.probs <- matrix(NA, nrow=T, ncol=length(new.cond.stdz))
for(t in 1:T){
	for(n in 1:length(new.cond.stdz)){
		mean.probs[t,n] <- mean(probs[,t,n])
	}
}

head(mean.probs)
dim(mean.probs)

### CIs
probsLCI <- matrix(NA, nrow=T, ncol=length(new.cond.stdz))
for(t in 1:T){
	for(n in 1:length(new.cond.stdz)){
		probsLCI[t,n] <- quantile(probs[,t,n],0.05)
	}
}	

head(probsLCI)

probsUCI <- matrix(NA, nrow=T, ncol=length(new.cond.stdz))
for(t in 1:T){
	for(n in 1:length(new.cond.stdz)){
		probsUCI[t,n] <- quantile(probs[,t,n],0.95)
	}
}	

head(probsUCI)

spp <- c(6,13)
spp.names <- c('Brook Trout','Cutlips Minnow')

mean.probs.sub <- mean.probs[spp,]
probsLCI.sub <- probsLCI[spp,]
probsUCI.sub <- probsUCI[spp,]


##########################################
###########################################
res<-6
name_figure <- "Figure_Cond_Panel_Subset.png"
png(filename = name_figure, height = 600*res, width = 920*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.0
x.label = 'Conductivity (mS/cm)'
y.label = 'Detection probability'

range(new.cond)

xlab1 <- c(
(20-mean(new.cond))/sd(new.cond),
(100-mean(new.cond))/sd(new.cond),
(200-mean(new.cond))/sd(new.cond), 
(300-mean(new.cond))/sd(new.cond))



xlab2 <- xlab1 * sd(new.cond) + mean(new.cond)

nf <- layout(matrix( c(1:2),nrow=1,ncol=2,byrow=T),  TRUE) 
layout.show(nf)
par(mar=c(0,0.5,0.5,0.5),oma=c(3,4,2,1),mai=c(0.1,0.1,0.3,0) )	
#par(mar=c(0.1,0.1,0.1,0.1),oma=c(3,3,0,1),mai=c(0.05,0.05,0.05,0) )


for(i in c(1:2)){

plot(new.cond.stdz, mean.probs.sub[i, ], axes=F, ylim=c(min(probsLCI.sub),max(probsUCI.sub) ), ylab='', xlab='', type='n')
	
i.for <- order(new.cond.stdz )
i.back <- order(new.cond.stdz , decreasing = TRUE )

x.polygon <- c( new.cond.stdz[i.for] , new.cond.stdz[i.back] )
y.polygon <- c( probsLCI.sub[i, ][i.for] , probsUCI.sub[i, ][i.back] )

polygon( x.polygon , y.polygon , col = "gray" , border = NA )

points(new.cond.stdz, mean.probs.sub[i, ], cex=0.8, pch=16,type='l',lty=1)
 

title(main=spp.names[i])

#x-axis

axis(side=1,cex.axis=1.2, mgp=c(1,0.6,0),tck= -0.01,at=xlab1, labels=xlab2)
	
#y-axis
if( i == 1 ){
	axis(side=2,cex.axis=1.2, mgp=c(0,0.6,0),tck= -0.01, las=1)
	} else {
	axis(side=2,cex.axis=1.2, mgp=c(0,0.2,0),tck= -0.01, labels=F)

  }

mtext(x.label, line = 2, side = 1, cex = size.text, outer=T)
mtext(y.label, line = 2, side = 2, cex = size.text, outer=T)
box()
}

par(def.par)
dev.off()
























