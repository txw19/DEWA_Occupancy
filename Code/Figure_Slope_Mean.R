# Create new forest data 
new.slope <- seq(min(slope),max(slope),length=50)

# Standardize new forest variable
new.slope.stdz <- (new.slope - mean(new.slope))/sd(new.slope)

T <- length(unique(df4$species))

# container for predicted value
probs <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.slope.stdz) ) )
dim(probs)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.slope.stdz)){ # species
			probs[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.occ[i,t] + out$BUGSoutput$sims.list$beta7[i,t]*mean(loglwd) + out$BUGSoutput$sims.list$beta6[i,t] * new.slope.stdz[n])
		}	
	}
}

mean.probs <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		mean.probs[t,n] <- mean(probs[,t,n])
	}
}

head(mean.probs)
dim(mean.probs)

### CIs
probsLCI <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsLCI[t,n] <- quantile(probs[,t,n],0.05)
	}
}	

head(probsLCI)

probsUCI <- matrix(NA, nrow=T, ncol=length(new.slope.stdz))
for(t in 1:T){
	for(n in 1:length(new.slope.stdz)){
		probsUCI[t,n] <- quantile(probs[,t,n],0.95)
	}
}	

head(probsUCI)

##########################################
###########################################
res<-6
name_figure <- "Figure_Slope_Mean.png"
png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.3
x.label = 'Slope'
y.label = 'Occurrence probability'



par(mfrow = c(1,1), mar=c(6,5,1,1))

range(slope)

xlab1 <- c(
(0-mean(new.slope))/sd(new.slope),
(0.1-mean(new.slope))/sd(new.slope),
(0.2-mean(new.slope))/sd(new.slope), 
(0.3-mean(new.slope))/sd(new.slope)
)


xlab2 <- xlab1 * sd(new.slope) + mean(new.slope)


plot(new.slope.stdz, mean.probs[i,], ylim=c(0,1), xlab='', ylab='', type='n', axes=F)

axis(side=1,cex.axis=size.text, at=xlab1, labels=xlab2 )
axis(side=2,cex.axis=size.text, las=1)

for(i in 1:T){
	points(new.slope.stdz, mean.probs[i,], type='l', lwd=0.5, col='black')
}

points(new.slope.stdz, mean.probs[i,], type='l', lwd=4, lty=1, col='blue')
points(new.slope.stdz, probsLCI[i,], pch='*', col='blue')
points(new.slope.stdz, probsUCI[i,], pch='*', col='blue')


mtext(x.label, line = 3, side = 1, cex = size.text)
mtext(y.label, line = 3, side = 2, cex = size.text)
box()
par(def.par)
dev.off()

###########








