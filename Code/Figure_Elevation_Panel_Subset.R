# Create new elevation data 
new.elev <- seq(min(elev),max(elev),length=50)

# Standardize new elevation variable
new.elev.stdz <- (new.elev - mean(new.elev))/sd(new.elev)

T <- length(unique(df4$species))

# container for predicted value
probs <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.elev.stdz) ) )
dim(probs)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.elev.stdz)){ # species
			probs[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.occ[i,t] + out$BUGSoutput$sims.list$beta7[i,t]*mean(loglwd) + out$BUGSoutput$sims.list$beta4[i,t] * new.elev.stdz[n])
		}	
	}
}

mean.probs <- matrix(NA, nrow=T, ncol=length(new.elev.stdz))
for(t in 1:T){
	for(n in 1:length(new.elev.stdz)){
		mean.probs[t,n] <- mean(probs[,t,n])
	}
}

head(mean.probs)
dim(mean.probs)

### CIs
probsLCI <- matrix(NA, nrow=T, ncol=length(new.elev.stdz))
for(t in 1:T){
	for(n in 1:length(new.elev.stdz)){
		probsLCI[t,n] <- quantile(probs[,t,n],0.05)
	}
}	

head(probsLCI)

probsUCI <- matrix(NA, nrow=T, ncol=length(new.elev.stdz))
for(t in 1:T){
	for(n in 1:length(new.elev.stdz)){
		probsUCI[t,n] <- quantile(probs[,t,n],0.95)
	}
}	

head(probsUCI)

spp <- c(1,8,13,22)
# spp.names <- c('American Eel','Brown Trout','Cutlips Minnow','Rainbow Trout')
spp.names <- c('(A)','(B)','(C)','(D)')

mean.probs.sub <- mean.probs[spp,]
probsLCI.sub <- probsLCI[spp,]
probsUCI.sub <- probsUCI[spp,]

##########################################
###########################################
res<-6
name_figure <- "Figure_Elevation_Panel_Subset.png"
png(filename = name_figure, height = 600*res, width = 800*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.2
x.label = 'Elevation (m)'
y.label = 'Occurrence probability'


range(new.elev)

xlab1 <- c(
(100-mean(new.elev))/sd(new.elev),
(200-mean(new.elev))/sd(new.elev), 
(300-mean(new.elev))/sd(new.elev),
(400-mean(new.elev))/sd(new.elev)
)

xlab2 <- xlab1 * sd(new.elev) + mean(new.elev)



nf <- layout(matrix( c(1:4),nrow=2,ncol=2,byrow=T),  TRUE) 
layout.show(nf)

par(mar=c(0,0.5,0.5,0.5),oma=c(2,2.8,0.5,0.5),mai=c(0.1,0.1,0.0,0) )	
#par(mar=c(0.1,0.1,0.1,0.1),oma=c(3,3,0,1),mai=c(0.05,0.05,0.05,0) )

for(i in c(1:4)){

plot(new.elev.stdz, mean.probs.sub[i, ], axes=F, ylim=c(min(probsLCI.sub),max(probsUCI.sub) ), ylab='', xlab='', type='n')
	
i.for <- order(new.elev.stdz )
i.back <- order(new.elev.stdz , decreasing = TRUE )

x.polygon <- c( new.elev.stdz[i.for] , new.elev.stdz[i.back] )
y.polygon <- c( probsLCI.sub[i, ][i.for] , probsUCI.sub[i, ][i.back] )

polygon( x.polygon , y.polygon , col = "gray" , border = NA )

points(new.catch.stdz, mean.probs.sub[i, ], cex=0.8, pch=16,type='l',lty=1, lwd=2)

text(-1.5, 0.95, spp.names[i], cex=1.2)

if( i <= 2 ){
  axis(side=1,cex.axis=1.2, mgp=c(1,0.15,0),tck= -0.01, at=xlab1, labels=F ) 
} else {
  axis(side=1,cex.axis=1.2, mgp=c(1,0.25,0),tck= -0.01,at=xlab1, labels=xlab2)
}	

if( i == 1 | i == 3){
  axis(side=2,cex.axis=1.2, mgp=c(0,0.25,0),tck= -0.01, las=1)
} else {
  axis(side=2,cex.axis=1.2, mgp=c(0,0.25,0),tck= -0.01, labels=F)
  
}

mtext(x.label, line = 0.9, side = 1, cex = size.text, outer=T)
mtext(y.label, line = 1.4, side = 2, cex = size.text, outer=T)
box()
}

par(def.par)
dev.off()























