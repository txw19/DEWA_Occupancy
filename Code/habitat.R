# rm(list=ls())

library(arm)
library(R2jags)
library(coda)

#fish data
df <- read.table('fish_data_for_r.txt', header=T, na.strings='NA')
summary(df)
head(df)
dim(df)

# create data frame of just spp names 
spp <- data.frame(unique(df[,5]))
head(spp)
colnames(spp) <- 'species'
head(spp)
dim(spp)
levels(spp$species)

#Make every site have every species and if species was not originally found there
#give the number found (total) a zero

library(plyr)

df2 <- ddply(df, c("site_id","visit"), function(df3) {
  df3 <- merge(spp, df3[,c("species","total")], all.x=TRUE) #adds extra rows that have no matching y
  df3[is.na(df3$total),"total"] <- 0 				    
  df3
})


head(df2)
length(unique(df2$site_id))
length(unique(df2$species))

# Remove any species with NA
df3 <- df2[!is.na(df2$species),]
length(unique(df3$species))

# Remove sites with NA
df3 <- df3[!is.na(df3$site_id),]
length(unique(df3$site_id))


######################################

# Create a binary response variable
df3$y <- ifelse(df3$total >0, 1,0)
head(df3)

library(reshape2)

df4 <- reshape(df3, idvar = c("site_id",'species'), timevar = "visit", 
	direction = "wide")
head(df4)

# Remove crap columns
df4 <- df4[,-c(3,5,7)]
head(df4)
dim(df4)

##########################

library(reshape2)

melt2 <- melt(df4, id.vars=c('species','site_id'))
head(melt2)

# Arrange data into proper 3-D array
a2 <- acast(melt2, site_id~variable~species ) # site, rep, taxa
a2
# Replace NA's with zero
a2[is.na(a2)] <- 0

dim(a2) 

##for initial z-values
a3 <- acast(melt2, site_id~species, mean)
a3[a3 > 0] <- 1

rownames(a3) <- NULL
colnames(a3) <- NULL

dim(a3) #detection/non-detection for every species at every site
a3

sum(a3[,3])

###############
#species groups
###############
species <- sort(unique(df4$species))
#write.table(species, file="species.txt")
groups <- read.table('species_groups.txt', header=T)
groups <- as.numeric(groups$Water.temp)
length(groups)
# 1 = cold	
# 2 = cool
# 3 = warm

############
#covariates
#############
#read in site and visit data
site <- read.table('site_data_for_R.txt', header=T, na.strings='NA')
site <- site[order(site$site_id),]
dim(site)

visit <- read.table('site_visit_data_for_R.txt', header=T, na.strings='NA')
visit <- visit[order(visit$site_id),]
dim(visit)
head(visit)

#ag
ag <- site$agriculture
logag <- log(ag+0.001)

#canopy
head(site)
canopy <- site$X25m_ripar_cover
canopyz <- (canopy - mean(canopy))/sd(canopy) #standardize
length(canopyz)

#catchment size
catch <- site$catch_area
catchz <- (catch - mean(catch))/sd(catch) #standardize
length(catchz)

#elevation
elev <- site$elevation
elevz <- (elev - mean(elev))/sd(elev) #standardize
length(elevz)

#forest cover
forest <- site$forest
forestz <- (forest - mean(forest))/sd(forest) #standardize
length(forestz)

#impoundment (0 or 1)
impound <- site$impoundment
length(impound)

#slope
slope <- site$slope
slopez <- (slope - mean(slope))/sd(slope) #standardize
length(slopez)

#barrier (make score 0 or 1)
barrier <- site$score.per.km
barrier[barrier > 0] <- 1
length(barrier)
barrier[is.na(barrier)] <- 1

#conductivity (by site and visit)
cond1 <- visit[c("site_id","visit_number","cond")]
cond2 <- reshape(cond1, idvar = c("site_id"), timevar = "visit_number", 
	direction = "wide")

cond3 <- cond2[,c(2,3,4)]

condz <- (cond3-sapply(cond3,mean))/sapply(cond3, sd)
dim(condz)

rownames(condz) <- NULL
condz <- as.matrix(condz)
 
#reach area
reach <- site$reach.area
reachz <- (reach - mean(reach))/sd(reach) #standardize
length(reachz)

#LWD density
lwd <- site$LWD.density
loglwd <- log(lwd + 0.001)

#discharge
discharge1 <- visit[c("site_id","visit_number","discharge")]
discharge2 <- reshape(discharge1, idvar = c("site_id"), timevar = "visit_number", 
	direction = "wide")
discharge3 <- discharge2[,c(2,3,4)]
dischargez <- (discharge3-sapply(discharge3,mean))/sapply(discharge3, sd)
dim(dischargez)

rownames(dischargez) <- NULL
dischargez <- as.matrix(dischargez)


###########################
##########################

# Define model
sink("model.txt")
cat("
model {

#occupancy	
	
	#slopes	
	  #means
		

		for(i in 1:Ngroups){
			beta.group3[i] <- log(beta.3[i]/(1-beta.3[i]))
			beta.3[i] ~ dunif(0,1)}

		for(i in 1:Ngroups){
			beta.group4[i] <- log(beta.4[i]/(1-beta.4[i]))
			beta.4[i] ~ dunif(0,1)}

		for(i in 1:Ngroups){
			beta.group5[i] <- log(beta.5[i]/(1-beta.5[i]))
			beta.5[i] ~ dunif(0,1)}

		for(i in 1:Ngroups){
			beta.group6[i] <- log(beta.6[i]/(1-beta.6[i]))
			beta.6[i] ~ dunif(0,1)}

		for(i in 1:Ngroups){
			beta.group7[i] <- log(beta.7[i]/(1-beta.7[i]))
			beta.7[i] ~ dunif(0,1)}
	
	  #precisions
		

		for(i in 1:Ngroups){
			tau.beta3[i] <- (1/(sigma.beta3[i]*sigma.beta3[i]))
			sigma.beta3[i] ~ dunif(0,10)}

		for(i in 1:Ngroups){
			tau.beta4[i] <- (1/(sigma.beta4[i]*sigma.beta4[i]))
			sigma.beta4[i] ~ dunif(0,10)}

		for(i in 1:Ngroups){
			tau.beta5[i] <- (1/(sigma.beta5[i]*sigma.beta5[i]))
			sigma.beta5[i] ~ dunif(0,10)}
		
		for(i in 1:Ngroups){
			tau.beta6[i] <- (1/(sigma.beta6[i]*sigma.beta6[i]))
			sigma.beta6[i] ~ dunif(0,10)}

		for(i in 1:Ngroups){
			tau.beta7[i] <- (1/(sigma.beta7[i]*sigma.beta7[i]))
			sigma.beta7[i] ~ dunif(0,10)}


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
	
	#slopes

		p.1 ~ dunif(0,1)
		p.2 ~ dunif(0,1)
		p.3 ~ dunif(0,1)
		
		sigma.p1 ~ dunif(0,10)
		sigma.p2 ~ dunif(0,10)
		sigma.p3 ~ dunif(0,10)
		
		mubeta.p1 <- log(p.1/(1-p.1))
		mubeta.p2 <- log(p.2/(1-p.2))
		mubeta.p3 <- log(p.3/(1-p.3))
		
		taubeta.p1 <- (1/(sigma.p1*sigma.p1))
		taubeta.p2 <- (1/(sigma.p2*sigma.p2))
		taubeta.p3 <- (1/(sigma.p3*sigma.p3))

	#intercepts
		
		p0 ~ dunif(0,1)
		sigma.p ~ dunif(0,10)

		mu.p <- log(p0/(1-p0))
		tau.p <- (1/(sigma.p*sigma.p))
		
	
# Process model
for(i in 1:nspec){  # loop over species 
	alpha.occ[i] ~ dnorm(alpha.group[group[i]], tau.alpha[group[i]])
	beta3[i] ~ dnorm(beta.group3[group[i]], tau.beta3[group[i]])
	beta4[i] ~ dnorm(beta.group4[group[i]], tau.beta4[group[i]])
	beta5[i] ~ dnorm(beta.group5[group[i]], tau.beta5[group[i]])
	beta6[i] ~ dnorm(beta.group6[group[i]], tau.beta6[group[i]])
	beta7[i] ~ dnorm(beta.group7[group[i]], tau.beta7[group[i]])

		for(j in 1:nquadrat){  # loop over sites
			logit(psi[j,i]) <- alpha.occ[i] + beta3[i]*catchz[j] + beta4[i]*elevz[j] + beta5[i]*forestz[j]      #species-specific covariates that influence the prob of species i occurring at site j
								+ beta6[i]*slopez[j] + beta7[i]*loglwd[j]
			z[j,i] ~ dbern(psi[j,i])
	}
}
# Observation model
for(k in 1:(nspec)){            # loop over species
	alpha.p[k] ~ dnorm(mu.p, tau.p)
	beta.p1[k] ~ dnorm(mubeta.p1,taubeta.p1)
	beta.p2[k] ~ dnorm(mubeta.p2,taubeta.p2)
	beta.p3[k] ~ dnorm(mubeta.p3,taubeta.p3)

	for (i in 1:nquadrat) {          # loop over sites
		for(j in 1:T){ 	         # loop over reps (T = 3)
			logit(mu[i,j,k]) <- alpha.p[k] + beta.p1[k]*condz[i,j] + beta.p3[k]*dischargez[i,j]
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
win.data <- list(y = y, T = dim(a2)[2], nquadrat=dim(a2)[1], nspec=dim(a2)[3], group=groups, Ngroups=Ngroups, catchz=catchz,
				elevz=elevz, forestz=forestz, slopez=slopez, loglwd=loglwd, condz=condz, dischargez=dischargez)


# Initial values
inits <- function(){list(
    alpha0 = runif(3, .25,1),
    beta.3 = runif(3, .25, 1),
    beta.4 = runif(3, .25, 1),
    beta.5 = runif(3, .25, 1),
    beta.6 = runif(3, .25, 1),
    beta.7 = runif(3, .25, 1),

    p0 = runif(1, .25,1),
    p.1 = runif(1, .25,1),
    p.2 = runif(1, .25,1),
    p.3 = runif(1, .25,1),
  
    sigma.p = runif(1,0,1.5),
    sigma.p1 =  runif(1,0,1.5),
    sigma.p2 =  runif(1,0,1.5),
    sigma.p3 =  runif(1,0,1.5),

    sigma.alpha =  runif(3,0,1.5),
    sigma.beta3=runif(3,0,1.5),
    sigma.beta4=runif(3,0,1.5),
    sigma.beta5=runif(3,0,1.5),
    sigma.beta6=runif(3,0,1.5),
    sigma.beta7=runif(3,0,1.5),

	   alpha.p=rnorm(nspec),
	   alpha.occ=rnorm(nspec),
	   beta3 = rnorm(nspec),
	   beta4 = rnorm(nspec),
	   beta5 = rnorm(nspec),
	   beta6 = rnorm(nspec),
	   beta7 = rnorm(nspec),
	   beta.p1 = rnorm(nspec),
	   beta.p2 = rnorm(nspec),
	   beta.p3 = rnorm(nspec),
	   z=a3)
}



# Define parameters to be monitored
params <- c('alpha.p','beta.p1','beta.p2','beta.p3','alpha.occ','beta3','beta4','beta5','beta6','beta7',
            'beta.group3','beta.group4','beta.group5','beta.group6','beta.group7')
			

# MCMC settings: need quite long chains
ni <- 200000
nt <- 2
nb <- 150000
nc <- 3


# Call WinBUGS from R and time the run 
start.time = Sys.time()         # Set timer

out <- jags(win.data, inits, params, "model.txt", n.chains = nc, 
n.thin = nt, n.iter = ni, n.burnin = nb)

end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='')


summary <- out$BUGSoutput$summary
write.csv(summary,'final_model_summary.csv',row.names = T)


mcmcOut <- out$BUGSoutput$sims.list
saveRDS(mcmcOut, file="MCMC_out.rds")

# str(out)

# $ sims.matrix    : num [1:75000, 1:336]

# Inspect output
# print(out, dig=3)

species<-unique(sort(df4$species))



# Grab posterior means for each parameter/species
# ests <- matrix(out$BUGSoutput$summary[1:320,1], c(32,10), byrow=F)

# Esclude thermal group pop-aves and deviance
ests <- matrix(out$BUGSoutput$summary[-c(65:79,336),1], c(32,10), byrow=F)



matrixout <- out$BUGSoutput$sims.matrix
dim(matrixout)

# Grab lower and upper 90% CIs
lowercis <- apply(matrixout[,-c(65:79,336)],2,quantile, probs=c(0.05))
low <- matrix(lowercis, c(32,10), byrow=F)

uppercis <- apply(matrixout[,-c(65:79,336)],2,quantile, probs=c(0.95))
up <- matrix(uppercis, c(32,10), byrow=F)

# "Significance"
signif <- as.numeric(lowercis) * as.numeric(uppercis) > 0
signif2 <- matrix(signif, c(32,10), byrow=F)

FinalOutput1 <- cbind(ests[,1],low[,1],up[,1],signif2[,1],
                      ests[,2],low[,2],up[,2],signif2[,2],
                      ests[,3],low[,3],up[,3],signif2[,3],
                      ests[,4],low[,4],up[,4],signif2[,4],
                      ests[,5],low[,5],up[,5],signif2[,5],
                      ests[,6],low[,6],up[,6],signif2[,6],
                      ests[,7],low[,7],up[,7],signif2[,7],
                      ests[,8],low[,8],up[,8],signif2[,8],
                      ests[,9],low[,9],up[,9],signif2[,9],
                      ests[,10],low[,10],up[,10],signif2[,10])

FinalOutput2 <- data.frame(species,FinalOutput1)

colnames(FinalOutput2) <- c("Species","alpha.occ", "lower", "upper", "overlap 0",
                            "alpha.p", "lower", "upper", "overlap 0",
                            "beta.p1", "lower", "upper", "overlap 0",
                            "beta.p2", "lower", "upper", "overlap 0",
                            "beta.p3", "lower", "upper", "overlap 0",
                            "beta.occ1", "lower", "upper", "overlap 0",
                            "beta.occ2", "lower", "upper", "overlap 0",
                            "beta.occ3", "lower", "upper", "overlap 0",
                            "beta.occ4", "lower", "upper", "overlap 0",
                            "beta.occ5", "lower", "upper", "overlap 0")

write.csv(FinalOutput2,'FINAL_parameter_ests.csv',row.names = F)

dim(FinalOutput2)


str(out)

# Group pop averages
# Catchment size
apply(out$BUGSoutput$sims.list$beta.group3,2,mean)
apply(out$BUGSoutput$sims.list$beta.group3,2,quantile, c(0.05, 0.95))

# elevation
apply(out$BUGSoutput$sims.list$beta.group4,2,mean)
apply(out$BUGSoutput$sims.list$beta.group4,2,quantile, c(0.05, 0.95))

# Forest
apply(out$BUGSoutput$sims.list$beta.group5,2,mean)
apply(out$BUGSoutput$sims.list$beta.group5,2,quantile, c(0.05, 0.95))

# Slope
apply(out$BUGSoutput$sims.list$beta.group6,2,mean)
apply(out$BUGSoutput$sims.list$beta.group6,2,quantile, c(0.05, 0.95))

# Slope
apply(out$BUGSoutput$sims.list$beta.group7,2,mean)
apply(out$BUGSoutput$sims.list$beta.group7,2,quantile, c(0.05, 0.95))





