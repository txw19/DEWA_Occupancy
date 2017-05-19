# Define model
sink("model.txt")
cat("
model {

#occupancy	
	

	#intercepts
	  #means
		for(i in 1:Ngroups){
			alpha.group[i] <- log(alpha0[i]/(1-alpha0[i])) 
			alpha0[i] ~ dunif(0,1)}		
	  #precisions
		for(i in 1:Ngroups){
			tau.alpha[i] <- (1/(sigma.alpha[i]*sigma.alpha[i]))
			sigma.alpha[i] ~ dunif(0,10)}
	
#detection
	
	

	#intercepts
		
		p0 ~ dunif(0,1)
		sigma.p ~ dunif(0,10)

		mu.p <- log(p0/(1-p0))
		tau.p <- (1/(sigma.p*sigma.p))
		
	
# Process model
for(i in 1:nspec){  # loop over species 
	alpha.occ[i] ~ dnorm(alpha.group[group[i]], tau.alpha[group[i]])
	
		for(j in 1:nquadrat){  # loop over sites
			logit(psi[j,i]) <- alpha.occ[i]
			z[j,i] ~ dbern(psi[j,i])
	}
}
# Observation model
for(k in 1:(nspec)){            # loop over species
	alpha.p[k] ~ dnorm(mu.p, tau.p)


	for (i in 1:nquadrat) {          # loop over sites
		for(j in 1:T){ 	         # loop over reps (T = 3)
			logit(mu[i,j,k]) <- alpha.p[k]	
			mu2[i,j,k] <- z[i,k]*mu[i,j,k]
			y[i,j,k] ~ dbern(mu2[i,j,k])
		}
	}
}

}
",fill = TRUE)
sink()


Ngroups <- length(unique(groups))
nsites <- dim(a2)[1]
nspec <- dim(a2)[3]


# Bundle data
y <- a2
rownames(y) <- NULL # Get rid of row names - WinBUGs does not like
dim(y)
win.data <- list(y = y, T = dim(a2)[2], nquadrat=dim(a2)[1], nspec=dim(a2)[3], group=groups, Ngroups=Ngroups)


# Initial values
inits <- function(){list(
    alpha0 = runif(3, .25,1),


    p0 = runif(1, .25,1),

  
    sigma.p = runif(1,0,1.5),


    sigma.alpha =  runif(3,0,1.5),


	   alpha.p=rnorm(nspec),
	   alpha.occ=rnorm(nspec),

	   z=a3)
}


# Define parameters to be monitored
params <- c('alpha.occ','alpha.group','alpha.p')

# Call WinBUGS from R and time the run 
start.time = Sys.time()         # Set timer

out <- jags(win.data, inits, params, "model.txt", n.chains = nc, 
n.thin = nt, n.iter = ni, n.burnin = nb)

end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='')

#occupancy

lower<-plogis(apply(out$BUGSoutput$sims.list$alpha.occ,2,cr5))
upper<-plogis(apply(out$BUGSoutput$sims.list$alpha.occ,2,cr95))
alphas<-plogis(apply(out$BUGSoutput$sims.list$alpha.occ,2,mean))
species<-unique(sort(df4$species))

credibles <- data.frame(species, lower, alphas, upper)
colnames(credibles) <- c('species', '5%', 'mean', '95%')
credibles

write.table(credibles, 'occupancy_table.txt')

#occupancy groups

lower<-plogis(apply(out$BUGSoutput$sims.list$alpha.group,2,cr5))
upper<-plogis(apply(out$BUGSoutput$sims.list$alpha.group,2,cr95))
alphas<-plogis(apply(out$BUGSoutput$sims.list$alpha.group,2,mean))
group<-unique(sort(groups))

credibles <- data.frame(group, lower, alphas, upper)
colnames(credibles) <- c('group', '5%', 'mean', '95%')
credibles

write.table(credibles, 'groups_table.txt')

#detection

lower<-plogis(apply(out$BUGSoutput$sims.list$alpha.p,2,cr5))
upper<-plogis(apply(out$BUGSoutput$sims.list$alpha.p,2,cr95))
alphas<-plogis(apply(out$BUGSoutput$sims.list$alpha.p,2,mean))
species<-unique(sort(df4$species))

credibles <- data.frame(species, lower, alphas, upper)
colnames(credibles) <- c('species', '5%', 'mean', '95%')
credibles

write.table(credibles, 'detection_table.txt')







