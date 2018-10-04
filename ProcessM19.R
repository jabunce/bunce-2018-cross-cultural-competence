#### m19 results


################### Contrasts of probabilities of answer=positive

#str(post19)
post0 <- post19
num_samp <- length(post0$lp__)	

#initialize matrices
probs.e.me.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.me.no <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.me.fam <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.fam <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.me.fam <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.me.emp <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.emp <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.me.emp <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.me.cty <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.cty <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.i) )
probs.o.me.cty <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.o) )

probs.e.ma.typ <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) ) #typical machi

probs.e.me.fam.emp <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.e.me.fam.cty <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.e.me.emp.cty <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.e.me.fam.emp.cty <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )


#mean posterior estimates of probability of each person answering each question as positive
for (k in 1:K) {

	probs.e.me.no[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] - post0$rBeta[,k,1] ) ) #muInt[sample, Mach, T]
	probs.i.me.no[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] - post0$rBeta[,k,2] ) )
  probs.o.me.no[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] - post0$rBeta[,k,3] ) )  

	probs.e.me.fam[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] + post0$aFamMat[,2,1] - post0$rBeta[,k,1] ) )
	probs.i.me.fam[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] + post0$aFamMat[,2,2] - post0$rBeta[,k,2] ) )
  probs.o.me.fam[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] + post0$aFamMat[,2,3] - post0$rBeta[,k,3] ) )  

	probs.e.me.emp[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] + post0$aEmpMat[,2,1] - post0$rBeta[,k,1] ) )
	probs.i.me.emp[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] + post0$aEmpMat[,2,2] - post0$rBeta[,k,2] ) )
  probs.o.me.emp[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] + post0$aEmpMat[,2,3] - post0$rBeta[,k,3] ) )

	probs.e.me.cty[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] + post0$aCtyMat[,2,1] - post0$rBeta[,k,1] ) )
	probs.i.me.cty[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] + post0$aCtyMat[,2,2] - post0$rBeta[,k,2] ) )
  probs.o.me.cty[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] + post0$aCtyMat[,2,3] - post0$rBeta[,k,3] ) ) 

	probs.e.ma.typ[,k] <- inv.logit( post0$rGamma[,k,1]*
							( post0$muInt[,1,1] + post0$aFamMat[,1,1] + post0$aCtyMat[,1,1] - 
								post0$rBeta[,k,1] ) )

  	#other ego experience combinations
  	probs.e.me.fam.emp[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] +
                                        post0$aFamMat[,2,1] + post0$aEmpMat[,2,1] - post0$rBeta[,k,1] ) )
  	probs.e.me.fam.cty[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] +
                                        post0$aFamMat[,2,1] + post0$aCtyMat[,2,1] - post0$rBeta[,k,1] ) )
  	probs.e.me.emp.cty[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] +
                                        post0$aEmpMat[,2,1] + post0$aCtyMat[,2,1] - post0$rBeta[,k,1] ) )

  	probs.e.me.fam.emp.cty[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] +
                                            post0$aFamMat[,2,1] + post0$aEmpMat[,2,1] + post0$aCtyMat[,2,1] - post0$rBeta[,k,1] ) )


  	if (k %in% c(12) ) { #differences in coding between ego and ingroup axes
    	probs.i.me.no[,k] <- 1 - probs.i.me.no[,k]
    	probs.i.me.fam[,k] <- 1 - probs.i.me.fam[,k]
    	probs.i.me.emp[,k] <- 1 - probs.i.me.emp[,k]
    	probs.i.me.cty[,k] <- 1 - probs.i.me.cty[,k]
  	} #if


	if (k %in% c(8,12,13) ) { #differences in coding between ego and outgroup axes
		probs.o.me.no[,k] <- 1 - probs.o.me.no[,k]
		probs.o.me.fam[,k] <- 1 - probs.o.me.fam[,k]
		probs.o.me.emp[,k] <- 1 - probs.o.me.emp[,k]
		probs.o.me.cty[,k] <- 1 - probs.o.me.cty[,k]
	} #if


} # for k

p_me.no.e_ma.typ.e <- probs.e.me.no - probs.e.ma.typ 	#posterior estimates of differences between means
p_me.no.e_me.fam.e <- probs.e.me.no - probs.e.me.fam
p_me.no.e_me.emp.e <- probs.e.me.no - probs.e.me.emp
p_me.no.e_me.cty.e <- probs.e.me.no - probs.e.me.cty

p_me.fam.e_ma.typ.e <- probs.e.me.fam - probs.e.ma.typ
p_me.fam.e_me.emp.e <- probs.e.me.fam - probs.e.me.emp
p_me.fam.e_me.cty.e <- probs.e.me.fam - probs.e.me.cty

p_me.emp.e_ma.typ.e <- probs.e.me.emp - probs.e.ma.typ
p_me.emp.e_me.cty.e <- probs.e.me.emp - probs.e.me.cty

p_me.cty.e_ma.typ.e <- probs.e.me.cty - probs.e.ma.typ

p_me.acc.no_typ <- probs.o.me.no - probs.e.ma.typ 	# (difference) inaccuracy of mestizo outgroup guesses
p_me.acc.fam_typ <- probs.o.me.fam - probs.e.ma.typ
p_me.acc.emp_typ <- probs.o.me.emp - probs.e.ma.typ
p_me.acc.cty_typ <- probs.o.me.cty - probs.e.ma.typ



probs.e.ma.typ[probs.e.ma.typ == 0] <- 0.0001  #to keep kl divergence from exploding, statistical rethinking pg 179
probs.e.ma.typ[probs.e.ma.typ == 1] <- 0.9999

probs.o.me.no[probs.o.me.no == 0] <- 0.0001
probs.o.me.no[probs.o.me.no == 1] <- 0.9999

probs.o.me.fam[probs.o.me.fam == 0] <- 0.0001
probs.o.me.fam[probs.o.me.fam == 1] <- 0.9999

probs.o.me.emp[probs.o.me.emp == 0] <- 0.0001
probs.o.me.emp[probs.o.me.emp == 1] <- 0.9999

probs.o.me.cty[probs.o.me.cty == 0] <- 0.0001
probs.o.me.cty[probs.o.me.cty == 1] <- 0.9999

#kl divergence of mestizo outgroup guesses
kl_me.no_typ <-  probs.e.ma.typ*log(probs.e.ma.typ/probs.o.me.no) + 				# mest no exp outgroup guess -> average machi ego response
					(1-probs.e.ma.typ)*log( (1-probs.e.ma.typ)/(1-probs.o.me.no) )
kl_me.fam_typ <-  probs.e.ma.typ*log(probs.e.ma.typ/probs.o.me.fam) + 			
					(1-probs.e.ma.typ)*log( (1-probs.e.ma.typ)/(1-probs.o.me.fam) )
kl_me.emp_typ <-  probs.e.ma.typ*log(probs.e.ma.typ/probs.o.me.emp) + 			
					(1-probs.e.ma.typ)*log( (1-probs.e.ma.typ)/(1-probs.o.me.emp) )
kl_me.cty_typ <-  probs.e.ma.typ*log(probs.e.ma.typ/probs.o.me.cty) + 			
					(1-probs.e.ma.typ)*log( (1-probs.e.ma.typ)/(1-probs.o.me.cty) )



#Jeffreys divergence (symmetric) of ego responses
jef_me.no_fam <- probs.e.me.no*log(probs.e.me.no/probs.e.me.fam) + 	
					(1-probs.e.me.no)*log( (1-probs.e.me.no)/(1-probs.e.me.fam) ) +
				probs.e.me.fam*log(probs.e.me.fam/probs.e.me.no) + 				
					(1-probs.e.me.fam)*log( (1-probs.e.me.fam)/(1-probs.e.me.no) )
jef_me.no_emp <- probs.e.me.no*log(probs.e.me.no/probs.e.me.emp) + 	
					(1-probs.e.me.no)*log( (1-probs.e.me.no)/(1-probs.e.me.emp) ) +
				probs.e.me.emp*log(probs.e.me.emp/probs.e.me.no) + 				
					(1-probs.e.me.emp)*log( (1-probs.e.me.emp)/(1-probs.e.me.no) )
jef_me.no_cty <- probs.e.me.no*log(probs.e.me.no/probs.e.me.cty) + 	
					(1-probs.e.me.no)*log( (1-probs.e.me.no)/(1-probs.e.me.cty) ) +
				probs.e.me.cty*log(probs.e.me.cty/probs.e.me.no) + 				
					(1-probs.e.me.cty)*log( (1-probs.e.me.cty)/(1-probs.e.me.no) )

jef_me.fam_emp <- probs.e.me.fam*log(probs.e.me.fam/probs.e.me.emp) + 	
					(1-probs.e.me.fam)*log( (1-probs.e.me.fam)/(1-probs.e.me.emp) ) +
				probs.e.me.emp*log(probs.e.me.emp/probs.e.me.fam) + 				
					(1-probs.e.me.emp)*log( (1-probs.e.me.emp)/(1-probs.e.me.fam) )
jef_me.fam_cty <- probs.e.me.fam*log(probs.e.me.fam/probs.e.me.cty) + 	
					(1-probs.e.me.fam)*log( (1-probs.e.me.fam)/(1-probs.e.me.cty) ) +
				probs.e.me.cty*log(probs.e.me.cty/probs.e.me.fam) + 				
					(1-probs.e.me.cty)*log( (1-probs.e.me.cty)/(1-probs.e.me.fam) )

jef_me.emp_cty <- probs.e.me.emp*log(probs.e.me.emp/probs.e.me.cty) + 	
					(1-probs.e.me.emp)*log( (1-probs.e.me.emp)/(1-probs.e.me.cty) ) +
				probs.e.me.cty*log(probs.e.me.cty/probs.e.me.emp) + 				
					(1-probs.e.me.cty)*log( (1-probs.e.me.cty)/(1-probs.e.me.emp) )



p_me.acc.no_fam <- abs(p_me.acc.no_typ) - abs(p_me.acc.fam_typ)   #contrast in (difference) inaccuracy of outgroup guesses
p_me.acc.no_emp <- abs(p_me.acc.no_typ) - abs(p_me.acc.emp_typ)
p_me.acc.no_cty <- abs(p_me.acc.no_typ) - abs(p_me.acc.cty_typ)

p_me.acc.fam_emp <- abs(p_me.acc.fam_typ) - abs(p_me.acc.emp_typ)
p_me.acc.fam_cty <- abs(p_me.acc.fam_typ) - abs(p_me.acc.cty_typ)

p_me.acc.emp_cty <- abs(p_me.acc.emp_typ) - abs(p_me.acc.cty_typ)




####### mean ego response probabilities

#means across questions
mean_probs.e.me.no <- rowMeans(probs.e.me.no)
mean_probs.i.me.no <- rowMeans(probs.i.me.no)
mean_probs.o.me.no <- rowMeans(probs.o.me.no)

mean_probs.e.me.fam <- rowMeans(probs.e.me.fam)
mean_probs.i.me.fam <- rowMeans(probs.i.me.fam)
mean_probs.o.me.fam <- rowMeans(probs.o.me.fam)

mean_probs.e.me.emp <- rowMeans(probs.e.me.emp)
mean_probs.i.me.emp <- rowMeans(probs.i.me.emp)
mean_probs.o.me.emp <- rowMeans(probs.o.me.emp)

mean_probs.e.me.cty <- rowMeans(probs.e.me.cty)
mean_probs.i.me.cty <- rowMeans(probs.i.me.cty)
mean_probs.o.me.cty <- rowMeans(probs.o.me.cty)

#contrasts of mean responses
me.resp.fam_no <- mean_probs.e.me.fam - mean_probs.e.me.no
me.resp.emp_no <- mean_probs.e.me.emp - mean_probs.e.me.no
me.resp.cty_no <- mean_probs.e.me.cty - mean_probs.e.me.no
                          
me.resp.emp_fam <- mean_probs.e.me.emp - mean_probs.e.me.fam
me.resp.cty_fam <- mean_probs.e.me.cty - mean_probs.e.me.fam

me.resp.cty_emp <- mean_probs.e.me.cty - mean_probs.e.me.emp



#Mean jeffreys divergence of ego responses
mean.me.jef.no_fam <- rowMeans(jef_me.no_fam)
mean.me.jef.no_emp <- rowMeans(jef_me.no_emp)
mean.me.jef.no_cty <- rowMeans(jef_me.no_cty)
                          
mean.me.jef.fam_emp <- rowMeans(jef_me.fam_emp)
mean.me.jef.fam_cty <- rowMeans(jef_me.fam_cty)

mean.me.jef.emp_cty <- rowMeans(jef_me.emp_cty)




###### cross-cultural competence

#propotion of mestizos giving ego responses, with each experience combination
prop_mest_fam <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==1 & d$EmpMat==0 & d$CtyMat==0),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))
prop_mest_emp <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==0 & d$EmpMat==1 & d$CtyMat==0),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))
prop_mest_cty <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==0 & d$EmpMat==0 & d$CtyMat==1),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))

prop_mest_fam_emp <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==1 & d$EmpMat==1 & d$CtyMat==0),"ID"])) /
                    length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))
prop_mest_fam_cty <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==1 & d$EmpMat==0 & d$CtyMat==1),"ID"])) /
                    length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))
prop_mest_emp_cty <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==0 & d$EmpMat==1 & d$CtyMat==1),"ID"])) /
                      length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))

prop_mest_no <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==0 & d$EmpMat==0 & d$CtyMat==0),"ID"])) /
                length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))
prop_mest_fam_emp_cty <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==1 & d$EmpMat==1 & d$CtyMat==1),"ID"])) /
                          length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"]))


# sum(prop_mest_fam, prop_mest_emp, prop_mest_cty,
#     prop_mest_fam_emp, prop_mest_fam_cty, prop_mest_emp_cty,
#     prop_mest_no, prop_mest_fam_emp_cty)


# dim(probs.e.me.no)



probs.e.me.weighted.mean <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )

for (k in 1:K) { 
  probs.e.me.weighted.mean[,k] <- probs.e.me.fam[,k]*prop_mest_fam +
                                  probs.e.me.emp[,k]*prop_mest_emp +
                                  probs.e.me.cty[,k]*prop_mest_cty +
                                  probs.e.me.fam.emp[,k]*prop_mest_fam_emp +
                                  probs.e.me.fam.cty[,k]*prop_mest_fam_cty +
                                  probs.e.me.emp.cty[,k]*prop_mest_emp_cty +
                                  probs.e.me.no[,k]*prop_mest_no +
                                  probs.e.me.fam.emp.cty[,k]*prop_mest_fam_emp_cty
} #for k




probs.e.me.weighted.mean[probs.e.me.weighted.mean == 0] <- 0.0001  #to keep kl divergence from exploding, statistical rethinking pg 179
probs.e.me.weighted.mean[probs.e.me.weighted.mean == 1] <- 0.9999

probs.i.me.no[probs.i.me.no == 0] <- 0.0001
probs.i.me.no[probs.i.me.no == 1] <- 0.9999

probs.i.me.fam[probs.i.me.fam == 0] <- 0.0001
probs.i.me.fam[probs.i.me.fam == 1] <- 0.9999

probs.i.me.emp[probs.i.me.emp == 0] <- 0.0001
probs.i.me.emp[probs.i.me.emp == 1] <- 0.9999

probs.i.me.cty[probs.i.me.cty == 0] <- 0.0001
probs.i.me.cty[probs.i.me.cty == 1] <- 0.9999


kl_me.no_me <-  probs.e.me.weighted.mean*log(probs.e.me.weighted.mean/probs.i.me.no) + 				#kl divergence of mest ingroup guesses: mest no exp ingroup guess -> average mest ego response
					(1-probs.e.me.weighted.mean)*log( (1-probs.e.me.weighted.mean)/(1-probs.i.me.no) )
kl_me.fam_me <-  probs.e.me.weighted.mean*log(probs.e.me.weighted.mean/probs.i.me.fam) + 			
					(1-probs.e.me.weighted.mean)*log( (1-probs.e.me.weighted.mean)/(1-probs.i.me.fam) )
kl_me.emp_me <-  probs.e.me.weighted.mean*log(probs.e.me.weighted.mean/probs.i.me.emp) + 			
					(1-probs.e.me.weighted.mean)*log( (1-probs.e.me.weighted.mean)/(1-probs.i.me.emp) )
kl_me.cty_me <-  probs.e.me.weighted.mean*log(probs.e.me.weighted.mean/probs.i.me.cty) + 			
					(1-probs.e.me.weighted.mean)*log( (1-probs.e.me.weighted.mean)/(1-probs.i.me.cty) )





#mean (difference) inaccuracy of mestizo outgroup and ingroup guesses, by experience type
mean_inacc_mest_fam_out <- rowMeans(abs(p_me.acc.fam_typ))
mean_inacc_mest_fam_in <- rowMeans(abs(probs.i.me.fam - probs.e.me.weighted.mean))

mean_inacc_mest_emp_out <- rowMeans(abs(p_me.acc.emp_typ))
mean_inacc_mest_emp_in <- rowMeans(abs(probs.i.me.emp - probs.e.me.weighted.mean))

mean_inacc_mest_cty_out <- rowMeans(abs(p_me.acc.cty_typ))
mean_inacc_mest_cty_in <- rowMeans(abs(probs.i.me.cty - probs.e.me.weighted.mean))

mean_inacc_mest_no_out <- rowMeans(abs(p_me.acc.no_typ))
mean_inacc_mest_no_in <- rowMeans(abs(probs.i.me.no - probs.e.me.weighted.mean))

mean_inacc_out.me <- cbind(mean_inacc_mest_fam_out,
                        mean_inacc_mest_emp_out,
                        mean_inacc_mest_cty_out,
                        mean_inacc_mest_no_out)
mean_inacc_in.me <- cbind(mean_inacc_mest_fam_in,
                       mean_inacc_mest_emp_in,
                       mean_inacc_mest_cty_in,
                       mean_inacc_mest_no_in)



#mean kl divergence of mestizo outgroup and ingroup guesses, by experience type
mean_kl_mest_fam_out <- rowMeans(kl_me.fam_typ)
mean_kl_mest_fam_in <- rowMeans(kl_me.fam_me)

mean_kl_mest_emp_out <- rowMeans(kl_me.emp_typ)
mean_kl_mest_emp_in <- rowMeans(kl_me.emp_me)

mean_kl_mest_cty_out <- rowMeans(kl_me.cty_typ)
mean_kl_mest_cty_in <- rowMeans(kl_me.cty_me)

mean_kl_mest_no_out <- rowMeans(kl_me.no_typ)
mean_kl_mest_no_in <- rowMeans(kl_me.no_me)

mean_kl_out.me <- cbind(mean_kl_mest_fam_out,
                        mean_kl_mest_emp_out,
                        mean_kl_mest_cty_out,
                        mean_kl_mest_no_out)
mean_kl_in.me <- cbind(mean_kl_mest_fam_in,
                       mean_kl_mest_emp_in,
                       mean_kl_mest_cty_in,
                       mean_kl_mest_no_in)




######################## contrasts of kl divergence of outgroup and ingroup guesses by experience type for mestizos

##out
me.kl.o.no_fam <- mean_kl_mest_no_out - mean_kl_mest_fam_out
me.kl.o.no_emp <- mean_kl_mest_no_out - mean_kl_mest_emp_out
me.kl.o.no_cty <- mean_kl_mest_no_out - mean_kl_mest_cty_out

me.kl.o.fam_emp <- mean_kl_mest_fam_out - mean_kl_mest_emp_out
me.kl.o.fam_cty <- mean_kl_mest_fam_out - mean_kl_mest_cty_out

me.kl.o.emp_cty <- mean_kl_mest_emp_out - mean_kl_mest_cty_out

##in
me.kl.i.fam_no <- mean_kl_mest_fam_in - mean_kl_mest_no_in
me.kl.i.emp_no <- mean_kl_mest_emp_in - mean_kl_mest_no_in
me.kl.i.cty_no <- mean_kl_mest_cty_in - mean_kl_mest_no_in

me.kl.i.emp_fam <- mean_kl_mest_emp_in - mean_kl_mest_fam_in
me.kl.i.cty_fam <- mean_kl_mest_cty_in - mean_kl_mest_fam_in

me.kl.i.cty_emp <- mean_kl_mest_cty_in - mean_kl_mest_emp_in


#make list for plotting
kl_list.abils.me <- list(
                  me.kl.o.no_fam,   #1
                  me.kl.o.no_emp,  #2
                  me.kl.o.no_cty,  #3
                          
                  me.kl.o.fam_emp,  #4
                  me.kl.o.fam_cty,  #5

                  me.kl.o.emp_cty, #6

                  me.kl.i.fam_no,  #7
                  me.kl.i.emp_no,  #8
                  me.kl.i.cty_no,  #9
                          
                  me.kl.i.emp_fam, #10
                  me.kl.i.cty_fam, #11

                  me.kl.i.cty_emp  #12
        )


names(kl_list.abils.me) <- c("kl.o none - fam",
                            "kl.o none - emp",
                            "kl.o none - cty",
                            "kl.o fam - emp",
                            "kl.o fam - cty",
                            "kl.o emp - cty",

                            "kl.i fam - none",
                            "kl.i emp - none",
                            "kl.i cty - none",
                            "kl.i emp - fam",
                            "kl.i cty - fam",
                            "kl.i cty - emp")

#str(kl_list.abils.me)


cont_list_col1 <- list( kl_list.abils.me[[1]], #me.acc.o.no_fam
            kl_list.abils.me[[7]], #me.acc.i.fam_no
            kl_list.abils.me[[8]], #me.acc.i.emp_no
            kl_list.abils.me[[9]]  #me.acc.i.cty_no
             )
names(cont_list_col1) <- names(kl_list.abils.me)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
#str(cont_list_col1)


cont_list_col2 <- list( kl_list.abils.me[[2]], #ma.acc.o.no_emp
            kl_list.abils.me[[4]], #ma.acc.o.fam_emp
            kl_list.abils.me[[10]], #me.acc.i.emp_fam
            kl_list.abils.me[[11]] #me.acc.i.cty_fam
             )
names(cont_list_col2) <- rownames(kl_list.abils.me)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
#str(cont_list_col2)


cont_list_col3 <- list( kl_list.abils.me[[3]], #ma.acc.o.no_cty
            kl_list.abils.me[[5]], #ma.acc.o.fam_cty
            kl_list.abils.me[[6]], #ma.acc.o.emp_cty
            kl_list.abils.me[[12]] #me.acc.i.cty_emp
             )
names(cont_list_col3) <- rownames(kl_list.abils)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
#str(cont_list_col3)




xmin <- -0.5
xmax <- 0.5
xrange <- abs(xmin-xmax)

col1.xmax <- xmax + 3*xrange
col1.xmin <- xmin - 0*xrange

col2.xmax <- xmax + 2*xrange
col2.xmin <- xmin - 1*xrange

col3.xmax <- xmax + 1*xrange
col3.xmin <- xmin - 2*xrange



#### density plot of contrasts
pdf(file="./Plots/m19_kl_exp_contrasts.pdf", 
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
#
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
          xlim=c(col3.xmin, col3.xmax),
          clip(xmin,xmax+0.226,0,16) #x1, x2, y1, y2	clip at x=0
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


text(labels="Emp", x=0,
  y=-0.4, las=1, cex=0.75)  #bottom
text(labels="Fam", x=-1*xrange,  
  y=-0.4, las=1, cex=0.75)
text(labels="no exp", x=-2*xrange,   
  y=-0.4, las=1, cex=0.75)

text(labels="Cty", x=0,
  y=4.5, las=1, cex=0.75)   #top
text(labels="Emp", x=-1*xrange, 
  y=4.5, las=1, cex=0.75)
text(labels="Fam", x=-2*xrange,  
  y=4.5, las=1, cex=0.75)

text(labels="Emp", x=xrange*0.8,  
  y=1.75, las=1, cex=0.75)  #right
text(labels="Fam", x=xrange*0.8,  
 y=2.75, las=1, cex=0.75)
text(labels="no exp", x=xrange*0.8,  
  y=3.75, las=1, cex=0.75, adj=0.3)

text(labels="Cty", x=-2*xrange-xrange*0.8,   
  y=0.75, las=1, cex=0.75)  #left
text(labels="Emp", x=-2*xrange-xrange*0.8,  
  y=1.75, las=1, cex=0.75)
text(labels="Fam", x=-2*xrange-xrange*0.8,   
  y=2.75, las=1, cex=0.75)

#text(labels="Mestizos", x=-37, y=2.25, las=3, cex=1, srt=90) #left
mtext("In-Group Guesses", side=2, line=3.5, cex=1, adj=0.15) #adj=0.015
text(labels="Out-Group Guesses", x=1.5*xrange,  #0.75,
  y=2.5, cex=1, adj=0.6, srt=270) #right  y=2.25
text(labels="Out-Group Guesses", x=-1*xrange,  #-0.5,
  y=5, cex=1)  #top
text(labels="In-Group Guesses", x=-1*xrange,  #-0.5,
  y=-0.9, cex=1) #bottom

graphics.off()




########## Contrasts

#### density plot of out-group inaccuracy
pdf(file="./Plots/m19_inaccuracy_dens.pdf", 
height=5, width=10)
par(mfcol=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(p_me.acc.no_typ, col(p_me.acc.no_typ))),
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
text(x=0, y=15, labels="MestOut no-Mats", cex=1)
axis(side=2,
	col="white",
	at=c(1:14), 
	labels=rev(quest_names), las=1, cex.axis=1)	#left
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=1)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_me.acc.fam_typ, col(p_me.acc.fam_typ))),
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
text(x=0, y=15, labels="MestOut fam-Mats", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_me.acc.emp_typ, col(p_me.acc.emp_typ))),
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
text(x=0, y=15, labels="MestOut emp-Mats", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_me.acc.cty_typ, col(p_me.acc.cty_typ))),
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
text(x=0, y=15, labels="MestOut cty-Mats", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("Difference in Probability Response=Positive", side = 1, outer = TRUE, cex = 0.8, line = 2.2)

graphics.off()




#### density plot of in-group inaccuracy
pdf(file="./Plots/m19_inaccuracy_in_dens.pdf", 
height=5, width=10)
par(mfcol=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(probs.i.me.no - probs.e.me.weighted.mean, col(probs.i.me.no - probs.e.me.weighted.mean))),
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
text(x=0, y=15, labels="MestIn no-Mest", cex=1)
axis(side=2,
  col="white",
  at=c(1:14), 
  labels=rev(quest_names), las=1, cex.axis=1) #left
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(probs.i.me.fam - probs.e.me.weighted.mean, col(probs.i.me.fam - probs.e.me.weighted.mean))),
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
text(x=0, y=15, labels="MestIn fam-Mest", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(probs.i.me.emp - probs.e.me.weighted.mean, col(probs.i.me.emp - probs.e.me.weighted.mean))),
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
text(x=0, y=15, labels="MestIn emp-Mest", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(probs.i.me.cty - probs.e.me.weighted.mean, col(probs.i.me.cty - probs.e.me.weighted.mean))),
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
text(x=0, y=15, labels="MestIn cty-Mest", cex=1)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75)
lines(x=list( x=c(-0.5,-0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("Difference in Probability Response=Positive", side = 1, outer = TRUE, cex = 0.8, line = 2.2)

graphics.off()





#### density plot of out-group kl inaccuracy
pdf(file="./Plots/m19_kl_dens.pdf", 
height=5, width=14)
par(mfcol=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(kl_me.no_typ, col(kl_me.no_typ))),
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
text(x=1, y=15, labels="MestOut no-Mats", cex=1)
axis(side=2,
	col="white",
	at=c(1:14), 
	labels=rev(quest_names), las=1, cex.axis=1)	#left
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_me.fam_typ, col(kl_me.fam_typ))),
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
text(x=1, y=15, labels="MestOut fam-Mats", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_me.emp_typ, col(kl_me.emp_typ))),
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
text(x=1, y=15, labels="MestOut emp-Mats", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_me.cty_typ, col(kl_me.cty_typ))),
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
text(x=1, y=15, labels="MestOut cty-Mats", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("KL Divergence", side = 1, outer = TRUE, cex = 1, line = 2.2)

graphics.off()




#### density plot of in-group kl inaccuracy
pdf(file="./Plots/m19_kl_in_dens.pdf", 
height=5, width=14)
par(mfcol=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(kl_me.no_me, col(kl_me.no_me))),
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
text(x=1, y=15, labels="MestIn no-Mest", cex=1)
axis(side=2,
  col="white",
  at=c(1:14), 
  labels=rev(quest_names), las=1, cex.axis=1) #left
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_me.fam_me, col(kl_me.fam_me))),
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
text(x=1, y=15, labels="MestIn fam-Mest", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_me.emp_me, col(kl_me.emp_me))),
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
text(x=1, y=15, labels="MestIn emp-Mest", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(kl_me.cty_me, col(kl_me.cty_me))),
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
text(x=1, y=15, labels="MestIn cty-Mest", cex=1)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=1)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("KL Divergence", side = 1, outer = TRUE, cex = 1, line = 2.2)

graphics.off()

