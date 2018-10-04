############################### Run all Stan models other than m1, m4, m11, and m19

############################################## m2

set.seed(1)
m2 <- stan( file=model_file[2],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_2), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post2 <- extract.samples( m2 ) 


print(m2, pars=c("aInt",
				 "muInt",
				 "sigmaInt",
				 "R_a",

				 "muBeta",
				 "sigmaBeta",

				 "muGamma",
				 "sigmaGamma",

                 "quest",
                 "R_q",
                 "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m2.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m2, pars=paste("aInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ) {
	print(traceplot(m2, pars=paste("muInt[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ) {
	print(traceplot(m2, pars=paste("sigmaInt[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ) {
	for ( z3 in 1:T ) {
		print(traceplot(m2, pars=paste("R_a[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m2, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m2, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m2, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m2, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:K ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m2, pars=paste("quest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m2, pars=paste("R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()






############################################### m3

set.seed(1)
m3 <- stan( file=model_file[3],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_3), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post3 <- extract.samples( m3 )



print(m3, pars=c(
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
                 "lp__"), 		#log probability (will be large negative number)),  
      probs = c(0.025,0.975), digits_summary=2)



#look at all traces
pdf(file="./Plots/traces_m3.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:T ) {
	for ( z3 in 1:J ) {
		print(traceplot(m3, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ) {
	print(traceplot(m3, pars=paste("muInt[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ) {
	print(traceplot(m3, pars=paste("sigmaInt[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ) {
	for ( z3 in 1:T ) {
		print(traceplot(m3, pars=paste("L_R_a[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m3, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m3, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m3, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m3, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m3, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m3, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m3, pars="lp__", inc_warmup=T)) 			#this is the target, and should not wander around
graphics.off()




############################################### m5

set.seed(1)
m5 <- stan( file=model_file[5],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post5 <- extract.samples( m5 )


print(m5, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
				 "aEdMes",			 

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
pdf(file="./Plots/traces_m5.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m5)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m5, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()



############################################### m6

set.seed(1)
m6 <- stan( file=model_file[6],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post6 <- extract.samples( m6 )


print(m6, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
				 "aLabMes",			 

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
pdf(file="./Plots/traces_m6.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m6)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m6, pars=paste("aLabMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()



############################################### m7

set.seed(1)
m7 <- stan( file=model_file[7],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post7 <- extract.samples( m7 )


print(m7, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
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
pdf(file="./Plots/traces_m7.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m7)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m7, pars=paste("aComMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()



############################################### m8

set.seed(1)
m8 <- stan( file=model_file[8],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post8 <- extract.samples( m8 )


print(m8, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
				 "aEdMes",
				 "aLabMes",				 

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
pdf(file="./Plots/traces_m8.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m8)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m8, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m8, pars=paste("aLabMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()



############################################### m9

set.seed(1)
m9 <- stan( file=model_file[9],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post9 <- extract.samples( m9 )


print(m9, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
				 "aEdMes",
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
pdf(file="./Plots/traces_m9.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m9)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m9, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m9, pars=paste("aComMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()



############################################### m10

set.seed(1)
m10 <- stan( file=model_file[10],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post10 <- extract.samples( m10 )


print(m10, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",

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
pdf(file="./Plots/traces_m10.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m10)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m10, pars=paste("aLabMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m10, pars=paste("aComMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()



############################################### m12

set.seed(1)
m12 <- stan( file=model_file[12],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post12 <- extract.samples( m12 )


print(m12, pars=c(
				 "aInt",
				 #"zInt",
				 "muInt",
				 "sigmaInt",
				 "L_R_a",
				 
				 "aEdMes",
				 "aLabMes",
				 "aComMes",

				 "aAdol",
				 "aOld",
				 "aMale",				 

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
pdf(file="./Plots/traces_m12.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m12)

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("aLabMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("aComMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("aAdol[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("aOld[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("aMale[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

graphics.off()



############################################### m13

set.seed(1)
m13 <- stan( file=model_file[13],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post13 <- extract.samples( m13 )


print(m13, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aFamMat",

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
pdf(file="./Plots/traces_m13.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m13)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m13, pars=paste("aFamMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()




############################################### m14

set.seed(1)
m14 <- stan( file=model_file[14],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post14 <- extract.samples( m14 )


print(m14, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aEmpMat",

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
pdf(file="./Plots/traces_m14.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m14)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m14, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()



############################################### m15

set.seed(1)
m15 <- stan( file=model_file[15],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post15 <- extract.samples( m15 )


print(m15, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
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
pdf(file="./Plots/traces_m15.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m15)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m15, pars=paste("aCtyMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()



############################################### m16

set.seed(1)
m16 <- stan( file=model_file[16],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post16 <- extract.samples( m16 )


print(m16, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aFamMat",
         "aEmpMat",

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
pdf(file="./Plots/traces_m16.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m16)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m16, pars=paste("aFamMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m16, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()



############################################### m17

set.seed(1)
m17 <- stan( file=model_file[17],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post17 <- extract.samples( m17 )


print(m17, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
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
pdf(file="./Plots/traces_m17.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m17)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m17, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m17, pars=paste("aCtyMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()



############################################### m18

set.seed(1)
m18 <- stan( file=model_file[18],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post18 <- extract.samples( m18 )


print(m18, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aFamMat",
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
pdf(file="./Plots/traces_m18.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m18)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m18, pars=paste("aFamMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m18, pars=paste("aCtyMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()




############################################### m20

set.seed(1)
m20 <- stan( file=model_file[20],
              data=data_list,
              iter=samps,
              chains=num_chains, 
              init=rep(list(start_list), num_chains),
              control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
            )

post20 <- extract.samples( m20 )


print(m20, pars=c(
         "aInt",
         #"zInt",
         "muInt",
         "sigmaInt",
         "L_R_a",
         
         "aFamMat",
         "aEmpMat",
         "aCtyMat",

         "aAdol",
         "aOld",
         "aMale",

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
pdf(file="./Plots/traces_m20.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

Stan_traces(m20)

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("aFamMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("aCtyMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("aAdol[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("aOld[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("aMale[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

graphics.off()

