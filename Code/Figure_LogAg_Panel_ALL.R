# Create new forest data 
new.ag <- seq(min(logag),max(logag),length=50)

# Standardize new forest variable
new.ag.stdz <- (new.ag -mean(new.ag))/sd(new.ag)

T <- length(unique(df4$species))

# container for predicted value
probs <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.ag.stdz) ) )
dim(probs)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.ag.stdz)){ # species
			probs[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.occ[i,t] + out$BUGSoutput$sims.list$beta.occ[i,t] * new.ag.stdz[n])
		}	
	}
}

mean.probs <- matrix(NA, nrow=T, ncol=length(new.ag.stdz))
for(t in 1:T){
	for(n in 1:length(new.ag.stdz)){
		mean.probs[t,n] <- mean(probs[,t,n])
	}
}

head(mean.probs)
dim(mean.probs)

### CIs
probsLCI <- matrix(NA, nrow=T, ncol=length(new.ag.stdz))
for(t in 1:T){
	for(n in 1:length(new.ag.stdz)){
		probsLCI[t,n] <- quantile(probs[,t,n],0.05)
	}
}	

head(probsLCI)

probsUCI <- matrix(NA, nrow=T, ncol=length(new.ag.stdz))
for(t in 1:T){
	for(n in 1:length(new.ag.stdz)){
		probsUCI[t,n] <- quantile(probs[,t,n],0.95)
	}
}	

head(probsUCI)

##########################################
###########################################
res<-6
name_figure <- "Figure_LogAg_Panel_ALL.png"
png(filename = name_figure, height = 500*res, width = 900*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.0
x.label = 'log(Percent Agriculture)'
y.label = 'Occurrence probability'


range(logag)

xlab1 <- c(
(-7-mean(new.ag))/sd(new.ag),
(-5-mean(new.ag))/sd(new.ag),
(-3-mean(new.ag))/sd(new.ag), 
(-1-mean(new.ag))/sd(new.ag),
(1-mean(new.ag))/sd(new.ag),
(2-mean(new.ag))/sd(new.ag),
(3-mean(new.ag))/sd(new.ag)
)


xlab2 <- xlab1 * sd(new.ag) + mean(new.ag)

nf <- layout(matrix( c(1:32),nrow=4,ncol=8,byrow=T),  TRUE) 
layout.show(nf)
#par(mar=c(0.5,0.5,0.5,0.5),oma=c(3,3,0,1),mai=c(0.1,0.1,0.1,0) )	
par(mar=c(0.1,0.1,0.1,0.1),oma=c(3,3,0,1),mai=c(0.05,0.05,0.05,0) )

for(i in c(1:32) ){

plot(new.ag.stdz, mean.probs[i, ], axes=F, ylim=c(min(probsLCI),max(probsUCI) ), ylab='', xlab='', type='n')
	
i.for <- order(new.ag.stdz )
i.back <- order(new.ag.stdz , decreasing = TRUE )

x.polygon <- c( new.ag.stdz[i.for] , new.ag.stdz[i.back] )
y.polygon <- c( probsLCI[i, ][i.for] , probsUCI[i, ][i.back] )

polygon( x.polygon , y.polygon , col = "gray" , border = NA )

points(new.ag.stdz, mean.probs[i, ], cex=0.8, pch=16,type='l',lty=1)

text(0.1, 0.8, i ,font=3,col='black' )


if( i <= 24 ){
	axis(side=1,cex.axis=1, mgp=c(1,0,0),tck= -0.01, at=xlab1, labels=F ) 
	} else {
	axis(side=1,cex.axis=1, mgp=c(1,0,0),tck= -0.01,at=xlab1, labels=xlab2)
  }	

if( i == 1 | i== 9 | i== 17 | i== 25){
	axis(side=2,cex.axis=1, mgp=c(0,0.5,0),tck= -0.01, las=1)
	} else {
	axis(side=2,cex.axis=1, mgp=c(0,0.5,0),tck= -0.01, labels=F)

  }

mtext(x.label, line = 1, side = 1, cex = size.text, outer=T)
mtext(y.label, line = 1.2, side = 2, cex = size.text, outer=T)
box()
}

par(def.par)
dev.off()















axis(side=1,cex.axis=size.text, at=xlab1, labels=xlab2 )
axis(side=2,cex.axis=size.text, las=1)

for(i in 1:T){
	points(new.forest.stdz, mean.probs[i,], type='l', lwd=0.5, col='black')
}

points(new.forest.stdz, mean.probs[i,], type='l', lwd=4, lty=1, col='blue')
points(new.forest.stdz, probsLCI[i,], pch='*', col='blue')
points(new.forest.stdz, probsUCI[i,], pch='*', col='blue')


mtext(x.label, line = 3, side = 1, cex = size.text)
mtext(y.label, line = 3, side = 2, cex = size.text)
box()
par(def.par)
dev.off()

###########








