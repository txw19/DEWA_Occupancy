# Create new lwd data 
new.lwd <- seq(min(loglwd),max(loglwd),length=50)

T <- length(unique(df4$species))

# container for predicted value
probs <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.lwd) ) )
dim(probs)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.lwd)){ # species
			probs[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.occ[i,t] + out$BUGSoutput$sims.list$beta.occ[i,t] * new.lwd[n])
		}	
	}
}

mean.probs <- matrix(NA, nrow=T, ncol=length(new.lwd))
for(t in 1:T){
	for(n in 1:length(new.lwd)){
		mean.probs[t,n] <- mean(probs[,t,n])
	}
}

head(mean.probs)
dim(mean.probs)

### CIs
probsLCI <- matrix(NA, nrow=T, ncol=length(new.lwd))
for(t in 1:T){
	for(n in 1:length(new.lwd)){
		probsLCI[t,n] <- quantile(probs[,t,n],0.05)
	}
}	

head(probsLCI)

probsUCI <- matrix(NA, nrow=T, ncol=length(new.lwd))
for(t in 1:T){
	for(n in 1:length(new.lwd)){
		probsUCI[t,n] <- quantile(probs[,t,n],0.95)
	}
}	

head(probsUCI)

spp <- c(3,6)
spp.names <- c('Blacknose Dace','Brook Trout')

mean.probs <- mean.probs[spp,]
probsLCI <- probsLCI[spp,]
probsUCI <- probsUCI[spp,]


##########################################
###########################################
res<-6
name_figure <- "Figure_LWDdetection_Panel_Subset.png"
png(filename = name_figure, height = 600*res, width = 920*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.5
x.label = expression('log'[e]*'LWD density')
y.label = 'Occurrence probability'

range(loglwd)

xlab1 <- c(-7:-1)
xlab2 <- c(-7:-1)

nf <- layout(matrix( c(1:2),nrow=1,ncol=2,byrow=T),  TRUE) 
layout.show(nf)
par(mar=c(0,0.5,0.5,0.5),oma=c(3,4,2,1),mai=c(0.1,0.1,0.3,0) )	
#par(mar=c(0.1,0.1,0.1,0.1),oma=c(3,3,0,1),mai=c(0.05,0.05,0.05,0) )

for(i in c(1:2)){

plot(new.lwd, mean.probs[i, ], axes=F, ylim=c(min(probsLCI),max(probsUCI) ), ylab='', xlab='', type='n')
	
i.for <- order(new.lwd)
i.back <- order(new.lwd, decreasing = TRUE )

x.polygon <- c( new.lwd[i.for] , new.lwd[i.back] )
y.polygon <- c( probsLCI[i, ][i.for] , probsUCI[i, ][i.back] )

polygon( x.polygon , y.polygon , col = "gray" , border = NA )

points(new.lwd, mean.probs[i, ], cex=0.8, pch=16,type='l',lty=1)

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



