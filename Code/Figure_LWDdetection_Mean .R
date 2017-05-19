# Create new lwd data 
new.lwd <- seq(min(loglwd),max(loglwd),length=50)



T <- length(unique(df4$species))

# container for predicted value
probs <- array(NA,c(out$BUGSoutput$n.sim,T,length(new.lwd) ) )
dim(probs)

for(i in 1:out$BUGSoutput$n.sim){ # simulations
	for(t in 1:T){ #species
		for(n in 1:length(new.lwd)){ # species
			probs[i,t,n] <- plogis(out$BUGSoutput$sims.list$alpha.p[i,t] + out$BUGSoutput$sims.list$beta.p2[i,t] * new.lwd[n])
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

##########################################
###########################################
res<-6
name_figure <- "Figure_LWDdetection_mean.png"
png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)

def.par <- par(no.readonly = TRUE) 

size.label = 1
size.text = 1.3
x.label = 'log(LWD density)'
y.label = 'Occurrence probability'



par(mfrow = c(1,1), mar=c(6,5,1,1))



plot(new.lwd, mean.probs[i,], ylim=c(0,1), xlab='', ylab='', type='n', axes=F)

axis(side=1,cex.axis=size.text)
axis(side=2,cex.axis=size.text, las=1)

for(i in 1:T){
	points(new.lwd, mean.probs[i,], type='l', lwd=0.5, col='black')
}

points(new.lwd, mean.probs[i,], type='l', lwd=4, lty=1, col='blue')
points(new.lwd, probsLCI[i,], pch='*', col='blue')
points(new.lwd, probsUCI[i,], pch='*', col='blue')

mtext(x.label, line = 3, side = 1, cex = size.text)
mtext(y.label, line = 3, side = 2, cex = size.text)
box()
par(def.par)
dev.off()

###########









