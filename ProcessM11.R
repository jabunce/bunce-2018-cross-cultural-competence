#### m11 results


################### Contrasts of probabilities of answer=positive

#str(post11)
post0 <- post11
num_samp <- length(post0$lp__)	


#initialize matrices
probs.e.ma.no11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.no11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.ma.no11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.ma.com11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.com11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.ma.com11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.ma.lab11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.lab11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.ma.lab11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.ma.ed11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.ed11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.ma.ed11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.me.typ11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) ) #typical mestizo

probs.e.ma.ed.lab11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.e.ma.ed.com11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.e.ma.lab.com11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.e.ma.ed.lab.com11 <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )


#mean posterior estimates of probability of each person answering each question as positive
for (k in 1:K) {

	#no inter-ethnic experience
	probs.e.ma.no11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] - post0$rBeta[,k,1] ) ) #muInt[sample, Mach, T]
	probs.i.ma.no11[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] - post0$rBeta[,k,2] ) )
	probs.o.ma.no11[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] - post0$rBeta[,k,3] ) )

	#commerce experience
	probs.e.ma.com11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] + post0$aComMes[,1,1] - post0$rBeta[,k,1] ) )
	probs.i.ma.com11[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] + post0$aComMes[,1,2] - post0$rBeta[,k,2] ) )
	probs.o.ma.com11[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] + post0$aComMes[,1,3] - post0$rBeta[,k,3] ) )

	#labor
	probs.e.ma.lab11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] + post0$aLabMes[,1,1] - post0$rBeta[,k,1] ) )
	probs.i.ma.lab11[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] + post0$aLabMes[,1,2] - post0$rBeta[,k,2] ) )	
	probs.o.ma.lab11[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] + post0$aLabMes[,1,3] - post0$rBeta[,k,3] ) )

	#education
	probs.e.ma.ed11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] + post0$aEdMes[,1,1] - post0$rBeta[,k,1] ) )
	probs.i.ma.ed11[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] + post0$aEdMes[,1,2] - post0$rBeta[,k,2] ) )	
	probs.o.ma.ed11[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] + post0$aEdMes[,1,3] - post0$rBeta[,k,3] ) )

	#typical average mestizo
	probs.e.me.typ11[,k] <- inv.logit( post0$rGamma[,k,1]*
							( post0$muInt[,2,1] + post0$aEdMes[,2,1] + post0$aLabMes[,2,1] + post0$aComMes[,2,1] - 
								post0$rBeta[,k,1] ) )

  	#other machi ego experience combinations
  	probs.e.ma.ed.lab11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] +							#edu and labor
                                        post0$aEdMes[,1,1] + post0$aLabMes[,1,1] - post0$rBeta[,k,1] ) )
  	probs.e.ma.ed.com11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] + 							#edu and commerce
                                        post0$aEdMes[,1,1] + post0$aComMes[,1,1] - post0$rBeta[,k,1] ) )
  	probs.e.ma.lab.com11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] +
                                        post0$aLabMes[,1,1] + post0$aComMes[,1,1] - post0$rBeta[,k,1] ) )

  	probs.e.ma.ed.lab.com11[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] +
                                            post0$aEdMes[,1,1] + post0$aLabMes[,1,1] + post0$aComMes[,1,1] - post0$rBeta[,k,1] ) )


	if (k %in% c(12) ) { #differences in coding between ego and ingroup axes
		probs.i.ma.no11[,k] <- 1 - probs.i.ma.no11[,k]
		probs.i.ma.com11[,k] <- 1 - probs.i.ma.com11[,k]
		probs.i.ma.lab11[,k] <- 1 - probs.i.ma.lab11[,k]
		probs.i.ma.ed11[,k] <- 1 - probs.i.ma.ed11[,k]
	} #if

	if (k %in% c(8,12,13) ) { #differences in coding between ego and outgroup axes
		probs.o.ma.no11[,k] <- 1 - probs.o.ma.no11[,k]
		probs.o.ma.com11[,k] <- 1 - probs.o.ma.com11[,k]
		probs.o.ma.lab11[,k] <- 1 - probs.o.ma.lab11[,k]
		probs.o.ma.ed11[,k] <- 1 - probs.o.ma.ed11[,k]
	} #if

} # for k

p_ma.no.e_me.typ.e11 <- probs.e.ma.no11 - probs.e.me.typ11 	#posterior estimates of differences between ego means: machi no experience - average mestizo
p_ma.no.e_ma.com.e11 <- probs.e.ma.no11 - probs.e.ma.com11 	#machi no experience - machi commerce experience
p_ma.no.e_ma.lab.e11 <- probs.e.ma.no11 - probs.e.ma.lab11
p_ma.no.e_ma.ed.e11 <- probs.e.ma.no11 - probs.e.ma.ed11

p_ma.com.e_me.typ.e11 <- probs.e.ma.com11 - probs.e.me.typ11
p_ma.com.e_ma.lab.e11 <- probs.e.ma.com11 - probs.e.ma.lab11
p_ma.com.e_ma.ed.e11 <- probs.e.ma.com11 - probs.e.ma.ed11

p_ma.lab.e_me.typ.e11 <- probs.e.ma.lab11 - probs.e.me.typ11
p_ma.lab.e_ma.ed.e11 <- probs.e.ma.lab11 - probs.e.ma.ed11

p_ma.ed.e_me.typ.e11 <- probs.e.ma.ed11 - probs.e.me.typ11


p_ma.acc.no_typ11 <- probs.o.ma.no11 - probs.e.me.typ11 	#inaccuracy (difference) of machi outgroup guesses: machi no exp outgroup guess - average mestizo ego response
p_ma.acc.com_typ11 <- probs.o.ma.com11 - probs.e.me.typ11
p_ma.acc.lab_typ11 <- probs.o.ma.lab11 - probs.e.me.typ11
p_ma.acc.ed_typ11 <- probs.o.ma.ed11 - probs.e.me.typ11


probs.e.me.typ11[probs.e.me.typ11 == 0] <- 0.0001  #to keep kl divergence from exploding, statisitcal rethinking pg 179
probs.e.me.typ11[probs.e.me.typ11 == 1] <- 0.9999

probs.o.ma.no11[probs.o.ma.no11 == 0] <- 0.0001
probs.o.ma.no11[probs.o.ma.no11 == 1] <- 0.9999

probs.o.ma.com11[probs.o.ma.com11 == 0] <- 0.0001
probs.o.ma.com11[probs.o.ma.com11 == 1] <- 0.9999

probs.o.ma.lab11[probs.o.ma.lab11 == 0] <- 0.0001
probs.o.ma.lab11[probs.o.ma.lab11 == 1] <- 0.9999

probs.o.ma.ed11[probs.o.ma.ed11 == 0] <- 0.0001
probs.o.ma.ed11[probs.o.ma.ed11 == 1] <- 0.9999

#kl divergence of machi outgroup guesses
kl_ma.no_typ11 <-  probs.e.me.typ11*log(probs.e.me.typ11/probs.o.ma.no11) + 				#machi no exp outgroup guess -> average mestizo ego response
					(1-probs.e.me.typ11)*log( (1-probs.e.me.typ11)/(1-probs.o.ma.no11) )
kl_ma.com_typ11 <-  probs.e.me.typ11*log(probs.e.me.typ11/probs.o.ma.com11) + 			
					(1-probs.e.me.typ11)*log( (1-probs.e.me.typ11)/(1-probs.o.ma.com11) )
kl_ma.lab_typ11 <-  probs.e.me.typ11*log(probs.e.me.typ11/probs.o.ma.lab11) + 			
					(1-probs.e.me.typ11)*log( (1-probs.e.me.typ11)/(1-probs.o.ma.lab11) )
kl_ma.ed_typ11 <-  probs.e.me.typ11*log(probs.e.me.typ11/probs.o.ma.ed11) + 			
					(1-probs.e.me.typ11)*log( (1-probs.e.me.typ11)/(1-probs.o.ma.ed11) )

#Jeffreys divergence (symmetric) of ego responses
jef_ma.no_ed <- probs.e.ma.no11*log(probs.e.ma.no11/probs.e.ma.ed11) + 	
					(1-probs.e.ma.no11)*log( (1-probs.e.ma.no11)/(1-probs.e.ma.ed11) ) +
				probs.e.ma.ed11*log(probs.e.ma.ed11/probs.e.ma.no11) + 				
					(1-probs.e.ma.ed11)*log( (1-probs.e.ma.ed11)/(1-probs.e.ma.no11) )
jef_ma.no_lab <- probs.e.ma.no11*log(probs.e.ma.no11/probs.e.ma.lab11) + 	
					(1-probs.e.ma.no11)*log( (1-probs.e.ma.no11)/(1-probs.e.ma.lab11) ) +
				probs.e.ma.lab11*log(probs.e.ma.lab11/probs.e.ma.no11) + 				
					(1-probs.e.ma.lab11)*log( (1-probs.e.ma.lab11)/(1-probs.e.ma.no11) )
jef_ma.no_com <- probs.e.ma.no11*log(probs.e.ma.no11/probs.e.ma.com11) + 	
					(1-probs.e.ma.no11)*log( (1-probs.e.ma.no11)/(1-probs.e.ma.com11) ) +
				probs.e.ma.com11*log(probs.e.ma.com11/probs.e.ma.no11) + 				
					(1-probs.e.ma.com11)*log( (1-probs.e.ma.com11)/(1-probs.e.ma.no11) )

jef_ma.ed_lab <- probs.e.ma.ed11*log(probs.e.ma.ed11/probs.e.ma.lab11) + 	
					(1-probs.e.ma.ed11)*log( (1-probs.e.ma.ed11)/(1-probs.e.ma.lab11) ) +
				probs.e.ma.lab11*log(probs.e.ma.lab11/probs.e.ma.ed11) + 				
					(1-probs.e.ma.lab11)*log( (1-probs.e.ma.lab11)/(1-probs.e.ma.ed11) )
jef_ma.ed_com <- probs.e.ma.ed11*log(probs.e.ma.ed11/probs.e.ma.com11) + 	
					(1-probs.e.ma.ed11)*log( (1-probs.e.ma.ed11)/(1-probs.e.ma.com11) ) +
				probs.e.ma.com11*log(probs.e.ma.com11/probs.e.ma.ed11) + 				
					(1-probs.e.ma.com11)*log( (1-probs.e.ma.com11)/(1-probs.e.ma.ed11) )

jef_ma.lab_com <- probs.e.ma.lab11*log(probs.e.ma.lab11/probs.e.ma.com11) + 	
					(1-probs.e.ma.lab11)*log( (1-probs.e.ma.lab11)/(1-probs.e.ma.com11) ) +
				probs.e.ma.com11*log(probs.e.ma.com11/probs.e.ma.lab11) + 				
					(1-probs.e.ma.com11)*log( (1-probs.e.ma.com11)/(1-probs.e.ma.lab11) )




p_ma.acc.no_ed.o11 <- abs(p_ma.acc.no_typ11) - abs(p_ma.acc.ed_typ11)		#contrast in (difference) inaccuracy of outgroup guesses: machi no exp inaccuracy - machi edu exp inaccuracy
#mean(colMeans(p_ma.acc.no_ed.o11))
#mean(rowMeans(p_ma.acc.no_ed.o11))

p_ma.acc.no_lab.o11 <- abs(p_ma.acc.no_typ11) - abs(p_ma.acc.lab_typ11)
p_ma.acc.no_com.o11 <- abs(p_ma.acc.no_typ11) - abs(p_ma.acc.com_typ11)

p_ma.acc.ed_lab.o11 <- abs(p_ma.acc.ed_typ11) - abs(p_ma.acc.lab_typ11)
p_ma.acc.ed_com.o11 <- abs(p_ma.acc.ed_typ11) - abs(p_ma.acc.com_typ11)

p_ma.acc.lab_com.o11 <- abs(p_ma.acc.lab_typ11) - abs(p_ma.acc.com_typ11)




####### mean ego response probabilities across questions

#means across questions
mean_probs.e.ma.no <- rowMeans(probs.e.ma.no11)
mean_probs.i.ma.no <- rowMeans(probs.i.ma.no11)
mean_probs.o.ma.no <- rowMeans(probs.o.ma.no11)

mean_probs.e.ma.ed <- rowMeans(probs.e.ma.ed11)
mean_probs.i.ma.ed <- rowMeans(probs.i.ma.ed11)
mean_probs.o.ma.ed <- rowMeans(probs.o.ma.ed11)

mean_probs.e.ma.lab <- rowMeans(probs.e.ma.lab11)
mean_probs.i.ma.lab <- rowMeans(probs.i.ma.lab11)
mean_probs.o.ma.lab <- rowMeans(probs.o.ma.lab11)

mean_probs.e.ma.com <- rowMeans(probs.e.ma.com11)
mean_probs.i.ma.com <- rowMeans(probs.i.ma.com11)
mean_probs.o.ma.com <- rowMeans(probs.o.ma.com11)

#contrasts of mean responses
ma.resp.no_ed <- mean_probs.e.ma.no - mean_probs.e.ma.ed
ma.resp.no_lab <- mean_probs.e.ma.no - mean_probs.e.ma.lab
ma.resp.no_com <- mean_probs.e.ma.no - mean_probs.e.ma.com
                          
ma.resp.ed_lab <- mean_probs.e.ma.ed - mean_probs.e.ma.lab
ma.resp.ed_com <- mean_probs.e.ma.ed - mean_probs.e.ma.com

ma.resp.lab_com <- mean_probs.e.ma.lab - mean_probs.e.ma.com


#Mean jeffreys divergence of ego responses
mean.ma.jef.no_ed <- rowMeans(jef_ma.no_ed)
mean.ma.jef.no_lab <- rowMeans(jef_ma.no_lab)
mean.ma.jef.no_com <- rowMeans(jef_ma.no_com)
                          
mean.ma.jef.ed_lab <- rowMeans(jef_ma.ed_lab)
mean.ma.jef.ed_com <- rowMeans(jef_ma.ed_com)

mean.ma.jef.lab_com <- rowMeans(jef_ma.lab_com)



###### cross-cultural competence


#first calculate weighted average probability of machi ego responses in order to compare against machi in-group guesses

#proportion of machis giving ego responses, with each experience combination
prop_mach_ed <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==1 & d$LabMes==0 & d$ComMes==0),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))
prop_mach_lab <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==0 & d$LabMes==1 & d$ComMes==0),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))
prop_mach_com <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==0 & d$LabMes==0 & d$ComMes==1),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))

prop_mach_ed_lab <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==1 & d$LabMes==1 & d$ComMes==0),"ID"])) /
                    length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))
prop_mach_ed_com <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==1 & d$LabMes==0 & d$ComMes==1),"ID"])) /
                    length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))
prop_mach_lab_com <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==0 & d$LabMes==1 & d$ComMes==1),"ID"])) /
                      length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))

prop_mach_no <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==0 & d$LabMes==0 & d$ComMes==0),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))
prop_mach_ed_lab_com <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==1 & d$LabMes==1 & d$ComMes==1),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))


# sum(prop_mach_ed, prop_mach_lab, prop_mach_com,
#     prop_mach_ed_lab, prop_mach_ed_com, prop_mach_lab_com,
#     prop_mach_no, prop_mach_ed_lab_com)


# dim(probs.e.ma.no11)


probs.e.ma.weighted.mean <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )

for (k in 1:K) { 
  probs.e.ma.weighted.mean[,k] <- probs.e.ma.ed11[,k]*prop_mach_ed +
                                  probs.e.ma.lab11[,k]*prop_mach_lab +
                                  probs.e.ma.com11[,k]*prop_mach_com +
                                  probs.e.ma.ed.lab11[,k]*prop_mach_ed_lab +
                                  probs.e.ma.ed.com11[,k]*prop_mach_ed_com +
                                  probs.e.ma.lab.com11[,k]*prop_mach_lab_com +
                                  probs.e.ma.no11[,k]*prop_mach_no +
                                  probs.e.ma.ed.lab.com11[,k]*prop_mach_ed_lab_com
} #for k



probs.e.ma.weighted.mean[probs.e.ma.weighted.mean == 0] <- 0.0001  #to keep kl divergence from exploding, statisitcal rethinking pg 179
probs.e.ma.weighted.mean[probs.e.ma.weighted.mean == 1] <- 0.9999

probs.i.ma.no11[probs.i.ma.no11 == 0] <- 0.0001
probs.i.ma.no11[probs.i.ma.no11 == 1] <- 0.9999

probs.i.ma.com11[probs.i.ma.com11 == 0] <- 0.0001
probs.i.ma.com11[probs.i.ma.com11 == 1] <- 0.9999

probs.i.ma.lab11[probs.i.ma.lab11 == 0] <- 0.0001
probs.i.ma.lab11[probs.i.ma.lab11 == 1] <- 0.9999

probs.i.ma.ed11[probs.i.ma.ed11 == 0] <- 0.0001
probs.i.ma.ed11[probs.i.ma.ed11 == 1] <- 0.9999


kl_ma.no_ma11 <-  probs.e.ma.weighted.mean*log(probs.e.ma.weighted.mean/probs.i.ma.no11) + 				#kl divergence of machi ingroup guesses: machi no exp ingroup guess -> average machi ego response
					(1-probs.e.ma.weighted.mean)*log( (1-probs.e.ma.weighted.mean)/(1-probs.i.ma.no11) )
kl_ma.com_ma11 <-  probs.e.ma.weighted.mean*log(probs.e.ma.weighted.mean/probs.i.ma.com11) + 			
					(1-probs.e.ma.weighted.mean)*log( (1-probs.e.ma.weighted.mean)/(1-probs.i.ma.com11) )
kl_ma.lab_ma11 <-  probs.e.ma.weighted.mean*log(probs.e.ma.weighted.mean/probs.i.ma.lab11) + 			
					(1-probs.e.ma.weighted.mean)*log( (1-probs.e.ma.weighted.mean)/(1-probs.i.ma.lab11) )
kl_ma.ed_ma11 <-  probs.e.ma.weighted.mean*log(probs.e.ma.weighted.mean/probs.i.ma.ed11) + 			
					(1-probs.e.ma.weighted.mean)*log( (1-probs.e.ma.weighted.mean)/(1-probs.i.ma.ed11) )





#mean (difference) inaccuracy of machi outgroup and ingroup guesses, by experience type
mean_inacc_mach_ed_out <- rowMeans(abs(p_ma.acc.ed_typ11))
mean_inacc_mach_ed_in <- rowMeans(abs(probs.i.ma.ed11 - probs.e.ma.weighted.mean))

mean_inacc_mach_lab_out <- rowMeans(abs(p_ma.acc.lab_typ11))
mean_inacc_mach_lab_in <- rowMeans(abs(probs.i.ma.lab11 - probs.e.ma.weighted.mean))

mean_inacc_mach_com_out <- rowMeans(abs(p_ma.acc.com_typ11))
mean_inacc_mach_com_in <- rowMeans(abs(probs.i.ma.com11 - probs.e.ma.weighted.mean))

mean_inacc_mach_no_out <- rowMeans(abs(p_ma.acc.no_typ11))
mean_inacc_mach_no_in <- rowMeans(abs(probs.i.ma.no11 - probs.e.ma.weighted.mean))

mean_inacc_out <- cbind(mean_inacc_mach_ed_out,
                        mean_inacc_mach_lab_out,
                        mean_inacc_mach_com_out,
                        mean_inacc_mach_no_out)
mean_inacc_in <- cbind(mean_inacc_mach_ed_in,
                       mean_inacc_mach_lab_in,
                       mean_inacc_mach_com_in,
                       mean_inacc_mach_no_in)


#mean kl divergence of machi outgroup and ingroup guesses, by experience type
mean_kl_mach_ed_out <- rowMeans(kl_ma.ed_typ11)
mean_kl_mach_ed_in <- rowMeans(kl_ma.ed_ma11)

mean_kl_mach_lab_out <- rowMeans(kl_ma.lab_typ11)
mean_kl_mach_lab_in <- rowMeans(kl_ma.lab_ma11)

mean_kl_mach_com_out <- rowMeans(kl_ma.com_typ11)
mean_kl_mach_com_in <- rowMeans(kl_ma.com_ma11)

mean_kl_mach_no_out <- rowMeans(kl_ma.no_typ11)
mean_kl_mach_no_in <- rowMeans(kl_ma.no_ma11)

mean_kl_out <- cbind(mean_kl_mach_ed_out,
                        mean_kl_mach_lab_out,
                        mean_kl_mach_com_out,
                        mean_kl_mach_no_out)
mean_kl_in <- cbind(mean_kl_mach_ed_in,
                       mean_kl_mach_lab_in,
                       mean_kl_mach_com_in,
                       mean_kl_mach_no_in)



######################## contrasts of kl divergence of outgroup and ingroup guesses by experience type for machis

##out
ma.kl.o.no_ed <- mean_kl_mach_no_out - mean_kl_mach_ed_out
ma.kl.o.no_lab <- mean_kl_mach_no_out - mean_kl_mach_lab_out
ma.kl.o.no_com <- mean_kl_mach_no_out - mean_kl_mach_com_out

ma.kl.o.ed_lab <- mean_kl_mach_ed_out - mean_kl_mach_lab_out
ma.kl.o.ed_com <- mean_kl_mach_ed_out - mean_kl_mach_com_out

ma.kl.o.lab_com <- mean_kl_mach_lab_out - mean_kl_mach_com_out

##in
ma.kl.i.ed_no <- mean_kl_mach_ed_in - mean_kl_mach_no_in
ma.kl.i.lab_no <- mean_kl_mach_lab_in - mean_kl_mach_no_in
ma.kl.i.com_no <- mean_kl_mach_com_in - mean_kl_mach_no_in

ma.kl.i.lab_ed <- mean_kl_mach_lab_in - mean_kl_mach_ed_in
ma.kl.i.com_ed <- mean_kl_mach_com_in - mean_kl_mach_ed_in

ma.kl.i.com_lab <- mean_kl_mach_com_in - mean_kl_mach_lab_in


#make list for plotting
kl_list.abils <- list(
                  ma.kl.o.no_ed,   #1
                  ma.kl.o.no_lab,  #2
                  ma.kl.o.no_com,  #3
                          
                  ma.kl.o.ed_lab,  #4
                  ma.kl.o.ed_com,  #5

                  ma.kl.o.lab_com, #6

                  ma.kl.i.ed_no,  #7
                  ma.kl.i.lab_no,  #8
                  ma.kl.i.com_no,  #9
                          
                  ma.kl.i.lab_ed, #10
                  ma.kl.i.com_ed, #11

                  ma.kl.i.com_lab  #12
        )


names(kl_list.abils) <- c("kl.o none - edu",
                            "kl.o none - lab",
                            "kl.o none - com",
                            "kl.o edu - lab",
                            "kl.o edu - com",
                            "kl.o lab - com",

                            "kl.i edu - none",
                            "kl.i lab - none",
                            "kl.i com - none",
                            "kl.i lab - edu",
                            "kl.i com - edu",
                            "kl.i com - lab")

#str(kl_list.abils)


cont_list_col1 <- list( kl_list.abils[[1]], #ma.acc.o.no_ed
            kl_list.abils[[7]], #me.acc.i.ed_no
            kl_list.abils[[8]], #me.acc.i.lab_no
            kl_list.abils[[9]]  #me.acc.i.com_no
             )
names(cont_list_col1) <- names(kl_list.abils)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
#str(cont_list_col1)


cont_list_col2 <- list( kl_list.abils[[2]], #ma.acc.o.no_lab
            kl_list.abils[[4]], #ma.acc.o.ed_lab
            kl_list.abils[[10]], #me.acc.i.lab_ed
            kl_list.abils[[11]] #me.acc.i.com_ed
             )
names(cont_list_col2) <- names(kl_list.abils)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
#str(cont_list_col2)


cont_list_col3 <- list( kl_list.abils[[3]], #ma.acc.o.no_com
            kl_list.abils[[5]], #ma.acc.o.ed_com
            kl_list.abils[[6]], #ma.acc.o.lab_com
            kl_list.abils[[12]] #me.acc.i.com_lab
             )
names(cont_list_col3) <- names(kl_list.abils)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
#str(cont_list_col3)




xmin <- -1
xmax <- 1
xrange <- abs(xmin-xmax)

col1.xmax <- xmax + 3*xrange
col1.xmin <- xmin - 0*xrange

col2.xmax <- xmax + 2*xrange
col2.xmin <- xmin - 1*xrange

col3.xmax <- xmax + 1*xrange
col3.xmin <- xmin - 2*xrange



#### density plot of contrasts
pdf(file="./Plots/m11_kl_exp_contrasts.pdf", 
height=5, width=5)
par(xpd=TRUE) #clip at figure region, not plot region
par(oma = c(4, 4, 4, 4)) # oma for outer margins: bottom, left, top, right



denschart3( cont_list_col1 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          #xlim=c(-0.25, 0.25 + 1.5)
          xlim=c(col1.xmin, col1.xmax)
 )
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


lines( list( x= c(xmin, xmax),
  y=c(1.25,1.25) ), lwd=0.5 )
lines( list( x=  c(xmin, xmax), 
  y=c(2.25,2.25) ), lwd=0.5 )
lines( list( x= c(xmin, xmax), 
  y=c(3.25,3.25) ), lwd=4 )

rect(xleft=xmin,
   ybottom=0.2,
   xright=xmax, 
   ytop=4.25, 
   lwd = 0.5)

lines( list( x=c(0,0), y=c(0.2,4.25) ), lty=2, lwd=1 ) #vertical line at 0


########## column 2

par(new = TRUE)     #draw on top of previous plot

denschart3( cont_list_col2 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          xlim=c(col2.xmin, col2.xmax)
 )
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)

#abline(v=0)
lines( list( x= c(xmin, xmax),
  y=c(1.25,1.25) ), lwd=0.5 )
lines( list( x= c(xmin, xmax),  
  y=c(2.25,2.25) ), lwd=4 )
lines( list( x= c(xmin, xmax),  c(-0.25,0.25),
  y=c(3.25,3.25) ), lwd=0.5 )
lines( list( x= c(xmin, xmin), 
  y=c(2.25, 3.25) ), lwd=4 )

rect(xleft=xmin,  
   ybottom=0.2,
   xright=xmax,   
   ytop=4.25, 
   lwd = 0.5)

lines( list( x=c(0,0), y=c(0.2,4.25) ), lty=2, lwd=1 ) #vertical line at 0



########## column 3

par(new = TRUE)     #draw on top of previous plot

denschart3( cont_list_col3 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          xlim=c(col3.xmin, col3.xmax)
 )
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)

#abline(v=0)
lines( list( x= c(xmin, xmax), 
  y=c(1.25,1.25) ), lwd=4 )
lines( list( x= c(xmin, xmax), 
  y=c(2.25,2.25) ), lwd=0.5 )
lines( list( x= c(xmin, xmax),  
  y=c(3.25,3.25) ), lwd=0.5 )
lines( list( x= c(xmin, xmin), 
  y=c(1.25, 2.25) ), lwd=4 )

rect(xleft=xmin,  
   ybottom=0.2,
   xright=xmax,  
   ytop=4.25, 
   lwd = 0.5)

lines( list( x=c(0,0), y=c(0.2,4.25) ), lty=2, lwd=1 ) #vertical line at 0


text(labels="Lab", x=0,
  y=-0.4, las=1, cex=0.75)  #bottom
text(labels="Edu", x=-1*xrange,  
  y=-0.4, las=1, cex=0.75)
text(labels="no exp", x=-2*xrange,   
  y=-0.4, las=1, cex=0.75)

text(labels="Com", x=0,
  y=4.5, las=1, cex=0.75)   #top
text(labels="Lab", x=-1*xrange, 
  y=4.5, las=1, cex=0.75)
text(labels="Edu", x=-2*xrange, 
  y=4.5, las=1, cex=0.75)

text(labels="Lab", x=xrange*0.8,  
  y=1.75, las=1, cex=0.75)  #right
text(labels="Edu", x=xrange*0.8, 
 y=2.75, las=1, cex=0.75)
text(labels="no exp", x=xrange*0.8, 
  y=3.75, las=1, cex=0.75, adj=0.3)

text(labels="Com", x=-2*xrange-xrange*0.8,  
  y=0.75, las=1, cex=0.75)  #left
text(labels="Lab", x=-2*xrange-xrange*0.8,   
  y=1.75, las=1, cex=0.75)
text(labels="Edu", x=-2*xrange-xrange*0.8,   
  y=2.75, las=1, cex=0.75)

 #left
mtext("In-Group Guesses", side=2, line=3.5, cex=1, adj=0.15)
text(labels="Out-Group Guesses", x=1.5*xrange, 
  y=2.5, cex=1, adj=0.6, srt=270) #right  
text(labels="Out-Group Guesses", x=-1*xrange,  
  y=5, cex=1)  #top
text(labels="In-Group Guesses", x=-1*xrange,  
  y=-0.9, cex=1) #bottom

graphics.off()




########## Contrasts


#### density plot of (difference) inaccuracy
pdf(file="./Plots/m11_inaccuracy_dens.pdf", 
height=5, width=10)
par(mfcol=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(p_ma.acc.no_typ11, col(p_ma.acc.no_typ11))),
		  yextra=1.5,
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1) 
 )
text(x=0, y=15, labels="MatsOut no-Mest", cex=1)
axis(side=2,
	col="white",
	at=c(1:14), 
	labels=rev(quest_names.e), las=1, cex.axis=1)	#left
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=1)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_ma.acc.com_typ11, col(p_ma.acc.com_typ11))),
		  yextra=1.5,
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="MatsOut com-Mest", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_ma.acc.lab_typ11, col(p_ma.acc.lab_typ11))),
		  yextra=1.5,
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="MatsOut lab-Mest", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_ma.acc.ed_typ11, col(p_ma.acc.ed_typ11))),
		  yextra=1.5,
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="MatsOut ed-Mest", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("Difference in Probability Response=Positive", side = 1, outer = TRUE, cex = 0.8, line = 2.2)

graphics.off()




#### density plot of kl inaccuracy
pdf(file="./Plots/m11_kl_dens.pdf", 
height=5, width=14)
par(mfcol=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

denschart3( rev(split(kl_ma.no_typ11, col(kl_ma.no_typ11))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 2),
          clip(0.035,1,0,16) #x1, x2, y1, y2	clip at x=0
 )
text(x=1, y=15, labels="MatsOut no-Mest", cex=1)
axis(side=2,
	col="white",
	at=c(1:14), 
	labels=rev(quest_names.e), las=1, cex.axis=1)	#left
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(kl_ma.com_typ11, col(kl_ma.com_typ11))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 2),
          clip(0.035,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=1, y=15, labels="MatsOut com-Mest", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_ma.lab_typ11, col(kl_ma.lab_typ11))),
		  #labels=rev(quest_names)
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 2),
          clip(0.035,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=1, y=15, labels="MatsOut lab-Mest", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_ma.ed_typ11, col(kl_ma.ed_typ11))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 2),
          clip(0.035,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=1, y=15, labels="MatsOut ed-Mest", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("KL Divergence", side = 1, outer = TRUE, cex = 1, line = 2.2)

graphics.off()

