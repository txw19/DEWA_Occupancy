####slope
# Create new slope data 
new.slope <- seq(min(slope),max(slope),length=50)


# Standardize new catch variable
new.slope.stdz <- (new.slope - mean(new.slope))/sd(new.slope)

T <- length(unique(groups))

# container for predicted value
probs <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.slope.stdz) ) )
dim(probs)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.slope.stdz)){ # species
			probs[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.group[i,t] + out$BUGSoutput$sims.list$beta.group7[i,t]*mean(loglwd) + out$BUGSoutput$sims.list$beta.group6[i,t] * new.slope.stdz[n]) 
		}	
	}
}

mean.probs <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		mean.probs[t,n] <- mean(probs[,t,n])
	}
}



### CIs
probsLCI <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsLCI[t,n] <- quantile(probs[,t,n],0.05)
	}
}	


probsUCI <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsUCI[t,n] <- quantile(probs[,t,n],0.95)
	}
}	

group.names <- c('Cold', 'Cool', 'Warm')
##########################################
###########################################
res<-6
name_figure <- "Figure_Group_Slope_Panel_All.png"
png(filename = name_figure, height = 400*res, width = 900*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.0
x.label = 'Slope'									#expression('Upstream Catchment Area (km'^2*')')
y.label = 'Occurrence Probability'

range(elev)

xlab1 <- c(
(0-mean(new.slope))/sd(new.slope),
(0.1-mean(new.slope))/sd(new.slope),
(0.2-mean(new.slope))/sd(new.slope), 
(0.3-mean(new.slope))/sd(new.slope)
)

xlab2 <- xlab1 * sd(new.slope) + mean(new.slope)

nf <- layout(matrix( c(1:3),nrow=1,ncol=3,byrow=T),  TRUE) 
layout.show(nf)
par(mar=c(0.5,0.5,0.5,0.5),oma=c(3,4,2,1),mai=c(0.2,0.1,0.2,0) )	
#par(mar=c(0.1,0.1,0.1,0.1),oma=c(3,3,0,1),mai=c(0.05,0.05,0.05,0) )


for(i in c(1:3)){

plot(new.slope.stdz, mean.probs[i, ], axes=F, ylim=c(min(probsLCI),max(probsUCI) ), ylab='', xlab='', type='n')
	
i.for <- order(new.slope.stdz )
i.back <- order(new.slope.stdz , decreasing = TRUE )

x.polygon <- c(new.slope.stdz[i.for] , new.slope.stdz[i.back] )
y.polygon <- c(probsLCI[i, ][i.for] , probsUCI[i, ][i.back] )

polygon( x.polygon , y.polygon , col = "gray" , border = NA )

points(new.slope.stdz, mean.probs[i, ], cex=0.8, pch=16,type='l',lty=1)


axis(side=1,cex.axis=1.2, mgp=c(1,0.6,0),tck= -0.01,at=xlab1, labels=xlab2)
	
#y-axis
if( i == 1 ){
	axis(side=2,cex.axis=1.2, mgp=c(0,0.6,0),tck= -0.01, las=1)
	} else {
	axis(side=2,cex.axis=1.2, mgp=c(0,0.2,0),tck= -0.01, labels=F)

  }

mtext(x.label, line = 1.3, side = 1,cex = size.text, outer=T)
mtext(y.label, line = 1.6, side = 2, cex = size.text, outer=T)

title(main=group.names[i])

box()
}

par(def.par)
dev.off()






















