####### m4 results

################### Contrasts of abilities

#str(post4)
post0 <- post4
num_samp <- length(post0$lp__)	

machiID <- 	unique(d[,c("newID","Machi")])
Machi <- machiID[,2]

#ego
probs.e.ma <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )        #mean prob for machis
probs.e.ma.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )  #prob with indiv-level variation
probs.e.me <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )		    #mean prob for mestizos
probs.e.me.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )

#ingroup
probs.i.ma <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.ma.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.i.me.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )

#outgroup
probs.o.ma <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.o.ma.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.o.me <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )
probs.o.me.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )


#mean posterior estimates of probability of each person answering each question as positive
for (k in 1:K) {

	#ego
	probs.e.ma[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,1,1] - post0$rBeta[,k,1] ) )	#mean prob machi
	probs.e.me[,k] <- inv.logit( post0$rGamma[,k,1]*( post0$muInt[,2,1] - post0$rBeta[,k,1] ) ) #mean prob mestizo

	#ingroup
	probs.i.ma[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,1,2] - post0$rBeta[,k,2] ) )
	probs.i.me[,k] <- inv.logit( post0$rGamma[,k,2]*( post0$muInt[,2,2] - post0$rBeta[,k,2] ) )

	#outgroup
	probs.o.ma[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,1,3] - post0$rBeta[,k,3] ) )
	probs.o.me[,k] <- inv.logit( post0$rGamma[,k,3]*( post0$muInt[,2,3] - post0$rBeta[,k,3] ) )


	if (k %in% c(12) ) { #differences in coding between ego and ingroup axes
		probs.i.ma[,k] <- 1 - probs.i.ma[,k]
    	probs.i.ma.indiv[,k] <- 1 - probs.i.ma.indiv[,k]   
		probs.i.me[,k] <- 1 - probs.i.me[,k]
    	probs.i.me.indiv[,k] <- 1 - probs.i.me.indiv[,k]    
	} #if

	if (k %in% c(8,12,13) ) { #differences in coding between ego and outgroup axes
		probs.o.ma[,k] <- 1 - probs.o.ma[,k]
    	probs.o.ma.indiv[,k] <- 1 - probs.o.ma.indiv[,k]    
		probs.o.me[,k] <- 1 - probs.o.me[,k]
    	probs.o.me.indiv[,k] <- 1 - probs.o.me.indiv[,k]
	} #if

} # for k


p_ma.e_me.e <- probs.e.ma - probs.e.me 	#posterior estimates of differences between means: machi ego - mestizo ego
p_ma.i_me.i <- probs.i.ma - probs.i.me 	#machi ingroup - mestizo ingroup
p_ma.o_me.o <- probs.o.ma - probs.o.me

p_ma.e_ma.i <- probs.e.ma - probs.i.ma  #inaccuracy (difference) in
p_ma.e_me.o <- probs.e.ma - probs.o.me  #inaccuracy (difference) out

p_me.e_me.i <- probs.e.me - probs.i.me  #inaccuracy (difference) in
p_me.e_ma.o <- probs.e.me - probs.o.ma  #inaccuracy (difference) out

p_ma.i_ma.o <- probs.i.ma - probs.o.ma
p_me.i_me.o <- probs.i.me - probs.o.me

p_ma.e_ma.o <- probs.e.ma - probs.o.ma  
p_me.e_me.o <- probs.e.me - probs.o.me

p_accu_diff_out <- abs(p_ma.e_me.o) - abs(p_me.e_ma.o) #contrast of outgroup guess (difference) inaccuracies: mestizo - machi 
p_accu_diff_in <- abs(p_ma.e_ma.i) - abs(p_me.e_me.i)




probs.e.ma[probs.e.ma == 0] <- 0.0001 		#to keep kl and jeffreys divergence from exploding, statistical rethinking pg 179
probs.e.ma[probs.e.ma == 1] <- 0.9999

probs.i.ma[probs.i.ma == 0] <- 0.0001
probs.i.ma[probs.i.ma == 1] <- 0.9999

probs.o.me[probs.o.me == 0] <- 0.0001
probs.o.me[probs.o.me == 1] <- 0.9999

probs.e.me[probs.e.me == 0] <- 0.0001
probs.e.me[probs.e.me == 1] <- 0.9999

probs.i.me[probs.i.me == 0] <- 0.0001
probs.i.me[probs.i.me == 1] <- 0.9999

probs.o.ma[probs.o.ma == 0] <- 0.0001
probs.o.ma[probs.o.ma == 1] <- 0.9999


# kl divergence
kl_ma.e_ma.i <- probs.e.ma*log(probs.e.ma/probs.i.ma) + 				#machi ingroup guess ->(approximates)-> machi ego response
					(1-probs.e.ma)*log( (1-probs.e.ma)/(1-probs.i.ma) )

kl_ma.e_me.o <- probs.e.ma*log(probs.e.ma/probs.o.me) + 				#mest divergence out	
					(1-probs.e.ma)*log( (1-probs.e.ma)/(1-probs.o.me) )

kl_me.e_me.i <- probs.e.me*log(probs.e.me/probs.i.me) + 				#mest divergence in	
					(1-probs.e.me)*log( (1-probs.e.me)/(1-probs.i.me) )

kl_me.e_ma.o <- probs.e.me*log(probs.e.me/probs.o.ma) + 				#mach divergence out	
					(1-probs.e.me)*log( (1-probs.e.me)/(1-probs.o.ma) )

#Jeffreys divergence (symmetric)
jef_ma.e_me.e <- probs.e.ma*log(probs.e.ma/probs.e.me) + 				#mach-mest ego divergence	
					(1-probs.e.ma)*log( (1-probs.e.ma)/(1-probs.e.me) ) +
				probs.e.me*log(probs.e.me/probs.e.ma) + 				
					(1-probs.e.me)*log( (1-probs.e.me)/(1-probs.e.ma) )

jef_ma.i_ma.o <- probs.i.ma*log(probs.i.ma/probs.o.ma) + 				#mach in-out divergence	
					(1-probs.i.ma)*log( (1-probs.i.ma)/(1-probs.o.ma) ) +
				probs.o.ma*log(probs.o.ma/probs.i.ma) + 				
					(1-probs.o.ma)*log( (1-probs.o.ma)/(1-probs.i.ma) )

jef_me.i_me.o <- probs.i.me*log(probs.i.me/probs.o.me) + 				#mest in-out divergence	
					(1-probs.i.me)*log( (1-probs.i.me)/(1-probs.o.me) ) +
				 probs.o.me*log(probs.o.me/probs.i.me) + 				
					(1-probs.o.me)*log( (1-probs.o.me)/(1-probs.i.me) )


####### mean ego response probabilities across questions

#means across questions
mean_probs.e.ma <- rowMeans(probs.e.ma) #machi ego
mean_probs.i.ma <- rowMeans(probs.i.ma) #machi ingroup
mean_probs.o.ma <- rowMeans(probs.o.ma)

mean_probs.e.me <- rowMeans(probs.e.me)
mean_probs.i.me <- rowMeans(probs.i.me)
mean_probs.o.me <- rowMeans(probs.o.me)



###### cross-cultural competence

#means across questions
#dim(p_me.e_ma.o)
mean_inacc_mach_out <- rowMeans(abs(p_me.e_ma.o)) #mean (difference) inaccuracy of machi out-group guesses
mean_inacc_mach_in <- rowMeans(abs(p_ma.e_ma.i))

mean_inacc_mest_out <- rowMeans(abs(p_ma.e_me.o))
mean_inacc_mest_in <- rowMeans(abs(p_me.e_me.i))

mean_inacc_out <- cbind(mean_inacc_mach_out, mean_inacc_mest_out)
mean_inacc_in <- cbind(mean_inacc_mach_in, mean_inacc_mest_in)

#mean(mean_inacc_mach_out)
#mean(mean_inacc_mest_out)

#mean(rowMeans(abs(p_me.e_ma.o)))
#mean(rowMeans(abs(p_ma.e_me.o)))

#mean(rowMeans(p_accu_diff_out))


#mean kl divergence of outgroup and ingroup guesses
mean_kl_mach_out <- rowMeans(kl_me.e_ma.o)
mean_kl_mach_in <- rowMeans(kl_ma.e_ma.i)

mean_kl_mest_out <- rowMeans(kl_ma.e_me.o)
mean_kl_mest_in <- rowMeans(kl_me.e_me.i)



#contrasts
mean_inacc_contr_out <- mean_inacc_mach_out - mean_inacc_mest_out  #contrast of mean outgroup guess (difference) inaccuracies: machi - mestizo 
mean_inacc_contr_in <- mean_inacc_mach_in - mean_inacc_mest_in

mean_inacc_contr <- cbind(mean_inacc_contr_out, mean_inacc_contr_in)


mean_jef_ma_me <- rowMeans(jef_ma.e_me.e)				  	        #mean jeffreys divergence of mach and mest ego responses
mean_kl_contr_out <- mean_kl_mach_out - mean_kl_mest_out 	  #contrast of mean outgroup guess (divergence) inaccuracies: machi - mestizo
mean_kl_contr_out2 <- rowMeans(kl_me.e_ma.o - kl_ma.e_ma.i) #mean contrast of outgroup guess (divergence) inaccuracies: machi - mestizo 
mean_kl_contr_in <- mean_kl_mach_in - mean_kl_mest_in
mean_kl_contr_in2 <- rowMeans(kl_ma.e_me.o - kl_me.e_me.i)

mean_kl_contr <- cbind(mean_jef_ma_me, mean_kl_contr_out, mean_kl_contr_in)
mean_kl_contr2 <- cbind(mean_jef_ma_me, mean_kl_contr_out2, mean_kl_contr_in2)



#### density plot of responses
pdf(file="./Plots/m4_response_dens.pdf", 
height=5, width=8)
par(mfcol=c(1,6))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(probs.e.ma, col(probs.e.ma))),
		  #labels=rev(quest_names)
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1) 
 )
text(x=0.5, y=15, labels="Mats Ego", cex=0.75)
axis(side=2,
	col="white",
	at=c(1:K), 
	labels=rev(quest_names.e), las=1, cex.axis=1)	#left
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(probs.e.me, col(probs.e.me))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1) 
 )
text(x=0.5, y=15, labels="Mest Ego", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(probs.i.ma, col(probs.i.ma))),
		  #labels=rev(quest_names)
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1) 
 )
text(x=0.5, y=15, labels="Mats In", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(probs.i.me, col(probs.i.me))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1) 
 )
text(x=0.5, y=15, labels="Mest In", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(probs.o.ma, col(probs.o.ma))),
		  #labels=rev(quest_names)
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1) 
 )
text(x=0.5, y=15, labels="Mats Out", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(probs.o.me, col(probs.o.me))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( 0, 1) 
 )
text(x=0.5, y=15, labels="Mest Out", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("Probability Response=Positive", side = 1, outer = TRUE, cex = 0.7, line = 2.2)

graphics.off()




#### density plot of contrasts + divergences
pdf(file="./Plots/m4_kl_dens.pdf", 
height=5, width=10)
par(mfcol=c(1,7))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

denschart3( rev(split(jef_ma.e_me.e, col(jef_ma.e_me.e))),
		  #labels=rev(quest_names)
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1),
          clip(0.1,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=0.5, y=15, labels="Mats-Mest Ego", cex=0.75)
axis(side=2,
	col="white",
	at=c(1:K), 
	labels=rev(quest_names.e), las=1, cex.axis=1)	#left
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

clip(-1,2,-5,16) #expand drawing region
lines(x=c(0,1), y=c(-1.6,-1.6), lwd=1.5)
lines(x=c(0,0), y=c(-1.6,-1.3), lwd=1.5)


denschart3( rev(split(jef_ma.i_ma.o, col(jef_ma.i_ma.o))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1),
          clip(0.1,1,0,16) #x1, x2, y1, y2	clip at x=0   
 )
text(x=0.5, y=15, labels="Mats In-Out", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(jef_me.i_me.o, col(jef_me.i_me.o))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1),
          clip(0.1,1,0,16) #x1, x2, y1, y2	clip at x=0   
 )
text(x=0.5, y=15, labels="Mest In-Out", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("Jeffreys Divergence", side=1, outer=TRUE, cex=0.7, line=2.2, adj=0.178)
clip(0,2,-5,16) #expand drawing region
lines(x=c(0,1), y=c(-1.6,-1.6), lwd=1.5)
lines(x=c(1,1), y=c(-1.6,-1.3), lwd=1.5)



denschart3( rev(split(kl_me.e_ma.o, col(kl_me.e_ma.o))),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 2),
          clip(0.1,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=1, y=15, labels="Mest Ego-Mats Out", cex=0.75)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=0.75)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

clip(-1,2,-5,16) #expand drawing region
lines(x=c(0,2), y=c(-1.6,-1.6), lwd=1.5)
lines(x=c(0,0), y=c(-1.6,-1.3), lwd=1.5)


denschart3( rev(split(kl_ma.e_me.o, col(kl_ma.e_me.o))),
      #labels=rev(quest_names)
      labels="",
      adjust=1,
      color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 2),
          clip(0.1,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=1, y=15, labels="Mats Ego-Mest Out", cex=0.75)
axis(side=1, at=c(0,1,2), labels=c(0,1,2), cex.axis=0.75)
lines(x=list( x=c(1,1), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

clip(-1,2,-5,16) #expand drawing region
lines(x=c(-1,1), y=c(-1.6,-1.6), lwd=1.5)


denschart3( rev(split(kl_ma.e_ma.i, col(kl_ma.e_ma.i))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1),
          clip(0.1,1,0,16) #x1, x2, y1, y2	clip at x=0
 )
text(x=0.5, y=15, labels="Mats Ego-In", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

clip(-1,2,-5,16) #expand drawing region
lines(x=c(0.5,2), y=c(-1.6,-1.6), lwd=1.5)


denschart3( rev(split(kl_me.e_me.i, col(kl_me.e_me.i))),
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1),
          clip(0.1,1,0,16) #x1, x2, y1, y2	clip at x=0 
 )
text(x=0.5, y=15, labels="Mest Ego-In", cex=0.75)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75)
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

clip(0,2,-5,16) #expand drawing region
lines(x=c(0,1), y=c(-1.6,-1.6), lwd=1.5)
lines(x=c(1,1), y=c(-1.6,-1.3), lwd=1.5)

mtext("KL Divergence", side = 1, outer = TRUE, cex = 0.7, line = 2.2, adj=0.745)

graphics.off()




#### density plot of contrasts
pdf(file="./Plots/m4_contrast_dens.pdf", 
height=5, width=9)
par(mfcol=c(1,7))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(p_ma.e_me.e, col(p_ma.e_me.e))),
		  #labels=rev(quest_names)
		  labels="",
		  yextra=1.7,
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1) 
 )
text(x=0, y=15, labels="Mats-Mest Ego", cex=0.75)
axis(side=2,
	col="white",
	at=c(1:K), 
	labels=rev(quest_names.e), las=1, cex.axis=1)	#left
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(p_ma.i_ma.o, col(p_ma.i_ma.o))),
		  labels="",
		  yextra=1.7,
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="Mats In-Out", cex=0.75)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(p_me.i_me.o, col(p_me.i_me.o))),
		  labels="",
		  yextra=1.7,
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="Mest In-Out", cex=0.75)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(p_me.e_ma.o, col(p_me.e_ma.o))),
      		labels="",
		  	yextra=1.7,
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="Mest Ego-Mats Out", cex=0.75)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_ma.e_me.o, col(p_ma.e_me.o))),
      		#labels=rev(quest_names)
      		labels="",
		  	yextra=1.7,
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="Mats Ego-Mest Out", cex=0.75)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

denschart3( rev(split(p_ma.e_ma.i, col(p_ma.e_ma.i))),
		  labels="",
		  yextra=1.7,
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="Mats Ego-In", cex=0.75)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(p_me.e_me.i, col(p_me.e_me.i))),
		  labels="",
		  yextra=1.7,
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-1, 1)  
 )
text(x=0, y=15, labels="Mest Ego-In", cex=0.75)
axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


mtext("Difference in Probability Response=Positive", side = 1, outer = TRUE, cex = 0.7, line = 2.2)

graphics.off()






#### density plot of contrasts of mean kl divergence
pdf(file="./Plots/m4_mean_kl_dens.pdf", 
height=5, width=5)
par(mfcol=c(1,1))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(mean_kl_contr, col(mean_kl_contr))),
      labels="",
      adjust=1,
      yextra=1.7,
      color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(-0.25, 0.76)  
 )
text(x=0, y=3.7, labels="Matsi - Mest", cex=0.75)
text(x=0.5, y=2.6, labels="Mean Jeffreys Divergence", cex=0.65)
axis(side=2,
	col="white",
	at=c(1:3), 
	labels=c("In-group","Out-Group","Ego"), las=1, cex.axis=1)	#left
axis(side=1, at=c(-0.25, 0, 0.25, 0.5, 0.75), labels=c(-0.25, 0, 0.25, 0.5, 0.75), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,3.5) ), lty=2, lwd=0.75)

mtext("Difference in Mean KL Divergence", side = 1, outer = TRUE, cex = 0.7, line = 2.2)

graphics.off()



