############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#define global variables
J <- length(unique(d$newID))		  #number people
K <- length(unique(d$question))		#number questions
T <- length(unique(d$target.num))	#number targets (ego, in-group, out-group)
Mach <- 2							            #number ethnicities
jj <- d$newID                     #vector of person IDs
kk <- d$question                  #vector of question IDs
y <- d$response                   #vector of responses



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


data_list <- list(
  J = length(unique(d$newID)),    			#number of people
  K = length(unique(d$question)),   		#number of questions
  T = length(unique(d$target.num)), 		#number of targets
  N = nrow(d),                          #total number of responses
  jj = d$newID,                         #N vector of person IDs
  kk = d$question,                      #N vector of question IDs
  tt = d$target.num,                    #N vector of target numbers
  y = d$response,                       #N vector of responses

  Machi = 2 - d[,"Machi"],              #ethnicity converted to index for each response

  MachiIndiv = 2 - unique(d[which(d$target.num==1),c("newID","Machi")])[,2], #ethnicity for each individual

  EdMes = d$EdMes,                      #vector of education experience with mestizos for each guess
  LabMes = d$LabMes,                    #vector of labor experience with mestizos for each guess
  ComMes = d$ComMes,                    #vector of commerce experience with mestizos for each guess

  FamMat = d$FamMat,                    #vector of family experience with machis for each guess
  EmpMat = d$EmpMat,                    #vector of employment experience with machis for each guess
  CtyMat = d$CtyMat,                    #vector of community experience with machis for each guess

  Adol = d$adol.less20,                 #vector of 1 if adolescent, else 0
  Old = d$old.over50,                   #vector of 1 if old, else 0
  Male = d$Sex.1male                    #vector of 1 if male, 0 if female
)


start_list <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)), 

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aEdMes = array(data=0, dim=c(Mach,T)),
  aLabMes = array(data=0, dim=c(Mach,T)),
  aComMes = array(data=0, dim=c(Mach,T)),

  aFamMat = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T)),

  aAdol = array(data=0, dim=c(Mach,T)),
  aOld = array(data=0, dim=c(Mach,T)),
  aMale = array(data=0, dim=c(Mach,T))
)


#special start lists for first three models

start_1 <- list(
  a0=c(0,0,0),
  aIndiv = as.array( rbind( rep(0, times=J), rep(0, times=J), rep(0, times=J) ) ), 
  beta = as.array( rbind( rep(0, times=K), rep(0, times=K), rep(0, times=K) ) ),
  gamma = as.array( rbind( rep(1, times=K), rep(1, times=K), rep(1, times=K) ) ),
  sigma_beta=c(1,1,1),
  sigma_gamma=c(1,1,1)
)


start_2 <- list(
  aInt = matrix(0, nrow=J, ncol=T),
  muInt = as.array(rep(0, times=T)),
  sigmaInt = as.array(rep(1, times=T)),
  R_a = diag(x=1, nrow=T, ncol=T),

  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),

  muGamma = as.array(rep(1, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),

  quest = matrix(c(0,0,0,1,1,1), nrow=K, ncol=T*2, byrow=TRUE),
  R_q = diag(x=1, nrow=T*2, ncol=T*2)
)


start_3 <- list(
  zInt = matrix(0, nrow=T, ncol=J),
  muInt = as.array(rep(0, times=T)),
  sigmaInt = as.array(rep(1, times=T)),
  L_R_a = diag(x=0, nrow=T, ncol=T),  

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2)
)



############################### Run important Stan models: m1, m4, m11, and m19

####### m1

set.seed(1)
m1 <- stan( file=model_file[1],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_1), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post1 <- extract.samples( m1 ) 


print(m1, pars=c(
         "a0",
         "aIndiv",
         "beta",
         "gamma",
         "sigma_beta",
         "sigma_gamma",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m1.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z1 in 1:T ) {
  print(traceplot(m1, pars=paste("a0[", z1, "]", sep=""), inc_warmup=T))
}
for ( z1 in 1:T ) {
  for ( z2 in 1:J ) {
      print(traceplot(m1, pars=paste("aIndiv[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
    }
}
for ( z1 in 1:T ) {
  for ( z2 in 1:K ) {
      print(traceplot(m1, pars=paste("beta[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
    }
}
for ( z1 in 1:T ) {
  for ( z2 in 1:K ) {
      print(traceplot(m1, pars=paste("gamma[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
    }
}
for ( z1 in 1:T ) {
  print(traceplot(m1, pars=paste("sigma_beta[", z1, "]", sep=""), inc_warmup=T))
}
for ( z1 in 1:T ) {
  print(traceplot(m1, pars=paste("sigma_gamma[", z1, "]", sep=""), inc_warmup=T))
}
print(traceplot(m1, pars="lp__", inc_warmup=T))
graphics.off()






####### m4

set.seed(1)
m4 <- stan(	file=model_file[4],
            	data=data_list,
            	iter=samps,
            	chains=num_chains, 
            	init=rep(list(start_list), num_chains),
            	control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post4 <- extract.samples( m4 ) 

print(m4, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",

				 "rBeta",
				 "muBeta",
				 "sigmaBeta",

				 "rGamma",
         "muGamma",
         "sigmaGamma",
         "L_R_q",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)


#look at all traces
pdf(file="./Plots/traces_m4.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m4)

graphics.off()





####### m11

set.seed(1)
m11 <- stan(	file=model_file[11],
            	data=data_list,
            	iter=samps,
            	chains=num_chains, 
            	init=rep(list(start_list), num_chains),
            	control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post11 <- extract.samples( m11 ) 


print(m11, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
				 "aEdMes",
				 "aLabMes",
				 "aComMes",				 

				 "rBeta",
				 "muBeta",
				 "sigmaBeta",

				 "rGamma",
         "muGamma",
         "sigmaGamma",
         "L_R_q",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m11.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m11)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m11, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m11, pars=paste("aLabMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m11, pars=paste("aComMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()





####### m19

set.seed(1)
m19 <- stan(	file=model_file[19],
            	data=data_list,
            	iter=samps,
            	chains=num_chains, 
            	init=rep(list(start_list), num_chains),
            	control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post19 <- extract.samples( m19 ) 


print(m19, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
				 "aFamMat",
				 "aEmpMat",
				 "aCtyMat",

				 "rBeta",
				 "muBeta",
				 "sigmaBeta",

				 "rGamma",
         "muGamma",
         "sigmaGamma",
         "L_R_q",
         "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m19.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m19)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m19, pars=paste("aFamMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m19, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m19, pars=paste("aCtyMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()






