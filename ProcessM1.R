
############# m1 results


#plot logistic response functions for a given question and latent trait estimates of indivs

questions <- quest_names.e #from above

# ego responses
pdf(file="./Plots/m1_ego_logistic_responses.pdf", 
  height=10, width=12)
par(mfrow=c(4,4), oma=c(5,7,5,5), mar=c(3,2,2,2))

gamma <- post1$gamma
beta <- post1$beta

for (Quest in 1:K) {
  plot( t( d[which(d$question == Quest & d$target.num==1), "response"] ) ~
        t(  mean(post1$a0[,1]) + colMeans(post1$aIndiv[,1,d[which(d$question == Quest & d$target.num==1), "newID"]]) ),
      col="red", ylim=c(0,1), xlim=c(-2.5,2.5), 
      ylab="",
      xlab="",
      main=c(questions[Quest]),
      cex.main=1.2,
      cex.axis=1.2,
      xaxp=c(-2, 2, 2),
      yaxp=c(0, 1, 2)
      )

  InverseLogit <- function(x) 1/(1+exp(-1*x)) # logit^-1 = logistic, undoing the logit function in the model
  curve( InverseLogit( mean(gamma[,1,Quest])*
    (x - mean(beta[,1,Quest])) ),
    from=-2.5, to=2.5, add=TRUE)
  abline( v= ( mean(beta[,1,Quest]) ) , #Bafumi et al. 2005, pg 174-175
          col=col.alpha("black",0.2) )
} #for
mtext(text="Prob(Response = Positive)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
mtext(text="Latent Ego Axis", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
#mtext(text="Ego Responses", side=3, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
graphics.off()


# ingroup guesses
questions <- quest_names.i

pdf(file="./Plots/m1_in_logistic_responses.pdf", 
  height=10, width=12)
par(mfrow=c(4,4), oma=c(5,7,5,5), mar=c(3,2,2,2))

gamma <- post1$gamma
beta <- post1$beta

for (Quest in 1:K) {
  plot( t( d[which(d$question == Quest & d$target.num==2), "response"] ) ~
        t(  mean(post1$a0[,2]) + colMeans(post1$aIndiv[,2,d[which(d$question == Quest & d$target.num==2), "newID"]]) ),
      col="red", ylim=c(0,1), xlim=c(-2.5,2.5), 
      ylab="",
      xlab="",
      main=c(questions[Quest]),
      cex.main=1.2,
      cex.axis=1.2,
      xaxp=c(-2, 2, 2),
      yaxp=c(0, 1, 2)
      )

  InverseLogit <- function(x) 1/(1+exp(-1*x)) # logit^-1 = logistic, undoing the logit function in the model
  curve( InverseLogit( mean(gamma[,2,Quest])*
    (x - mean(beta[,2,Quest])) ),
    from=-2.5, to=2.5, add=TRUE)
  abline( v= ( mean(beta[,2,Quest]) ) , #Bafumi et al. 2005, pg 174-175
          col=col.alpha("black",0.2) )
} #for
mtext(text="Prob(Response = Positive)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
mtext(text="Latent In-group Axis", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
#mtext(text="Ingroup Responses", side=3, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
graphics.off()


# outgroup guesses
questions <- quest_names.o

pdf(file="./Plots/m1_out_logistic_responses.pdf", 
  height=10, width=12)
par(mfrow=c(4,4), oma=c(5,7,5,5), mar=c(3,2,2,2))

gamma <- post1$gamma
beta <- post1$beta

for (Quest in 1:K) {
  plot( t( d[which(d$question == Quest & d$target.num==3), "response"] ) ~
        t(  mean(post1$a0[,3]) + colMeans(post1$aIndiv[,3,d[which(d$question == Quest & d$target.num==3), "newID"]]) ),
      col="red", ylim=c(0,1), xlim=c(-2.5,2.5), 
      ylab="",
      xlab="",
      main=c(questions[Quest]),
      cex.main=1.2,
      cex.axis=1.2,
      xaxp=c(-2, 2, 2),
      yaxp=c(0, 1, 2)
      )

  InverseLogit <- function(x) 1/(1+exp(-1*x)) # logit^-1 = logistic, undoing the logit function in the model
  curve( InverseLogit( mean(gamma[,3,Quest])*
    (x - mean(beta[,3,Quest])) ),
    from=-2.5, to=2.5, add=TRUE)
  abline( v= ( mean(beta[,3,Quest]) ) , #Bafumi et al. 2005, pg 174-175
          col=col.alpha("black",0.2) )
} #for
mtext(text="Prob(Response = Positive)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
mtext(text="Latent Out-group Axis", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
#mtext(text="Outgroup Responses", side=3, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
graphics.off()




########### discrimination posterior estimates for m1

post0 <- post1

disc_list.e <- list(
                    post0$gamma[,1,1],
                    post0$gamma[,1,2],
                    post0$gamma[,1,3],
                    post0$gamma[,1,4],
                    post0$gamma[,1,5],
                    post0$gamma[,1,6],
                    post0$gamma[,1,7],
                    post0$gamma[,1,8],
                    post0$gamma[,1,9],
                    post0$gamma[,1,10],
                    post0$gamma[,1,11],
                    post0$gamma[,1,12],
                    post0$gamma[,1,13],
                    post0$gamma[,1,14]
                  )
names(disc_list.e) <- quest_names.e

#str(disc_list.e)
disc_list.e <- rev(disc_list.e) #reverse order for plotting
#str(disc_list.e)


disc_list.i <- list(
                    post0$gamma[,2,1],
                    post0$gamma[,2,2],
                    post0$gamma[,2,3],
                    post0$gamma[,2,4],
                    post0$gamma[,2,5],
                    post0$gamma[,2,6],
                    post0$gamma[,2,7],
                    post0$gamma[,2,8],
                    post0$gamma[,2,9],
                    post0$gamma[,2,10],
                    post0$gamma[,2,11],
                    post0$gamma[,2,12],
                    post0$gamma[,2,13],
                    post0$gamma[,2,14]
                  )
names(disc_list.i) <- quest_names.i

#str(disc_list.i)
disc_list.i <- rev(disc_list.i) #reverse order for plotting
#str(disc_list.i)


disc_list.o <- list(
                    post0$gamma[,3,1],
                    post0$gamma[,3,2],
                    post0$gamma[,3,3],
                    post0$gamma[,3,4],
                    post0$gamma[,3,5],
                    post0$gamma[,3,6],
                    post0$gamma[,3,7],
                    post0$gamma[,3,8],
                    post0$gamma[,3,9],
                    post0$gamma[,3,10],
                    post0$gamma[,3,11],
                    post0$gamma[,3,12],
                    post0$gamma[,3,13],
                    post0$gamma[,3,14]
                  )
names(disc_list.o) <- quest_names.o

#str(disc_list.o)
disc_list.o <- rev(disc_list.o) #reverse order for plotting
#str(disc_list.o)



#### density plot of responses
pdf(file="./Plots/m1_disc_dens.pdf", 
height=5, width=8)
par(mfcol=c(1,3))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( disc_list.e,
      #labels=rev(quest_names)
      labels="",
      adjust=1,
      color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( -0.1, 3),
          clip(0.087,1,0,16) #x1, x2, y1, y2	clip at x=0 
 )
text(x=1, y=15, labels="Ego", cex=1.25)
axis(side=2,
  col="white",
  at=c(1:K), 
  labels=rev(quest_names.e), las=1, cex.axis=1) #left
axis(side=1, at=c(0,1,2,3), labels=c(0,1,2,3), cex.axis=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( disc_list.i,
      labels="",
      adjust=1,
      color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( -0.1, 3),
          clip(0.087,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=1, y=15, labels="In-group", cex=1.25)
axis(side=1, at=c(0,1,2,3), labels=c(0,1,2,3), cex.axis=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( disc_list.o,
      #labels=rev(quest_names)
      labels="",
      adjust=1,
      color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range( -0.1, 3),
          clip(0.087,1,0,16) #x1, x2, y1, y2	clip at x=0  
 )
text(x=1, y=15, labels="Out-group", cex=1.25)
axis(side=1, at=c(0,1,2,3), labels=c(0,1,2,3), cex.axis=0.75)
lines(x=list( x=c(0,0), y=c(0,14.5) ), lty=2, lwd=0.75)

mtext("Discrimination (Slope) on Latent axes", side = 1, outer = TRUE, cex = 0.8, line = 2.2)

graphics.off()





############################ Indices of people with different inter-ethnic experience types

machiID.e <- unique(d[which(d$target.num==1),c("newID","Machi")]) #newIDs and ethnicity of people anwering questions for each domain (different numbers of people for each target)
Machi.e <- machiID.e[,2]
machiID.i <- unique(d[which(d$target.num==2),c("newID","Machi")])
Machi.i <- machiID.i[,2]
machiID.o <- unique(d[which(d$target.num==3),c("newID","Machi")])
Machi.o <- machiID.i[,2]

EdMesID.e <- unique(d[which(d$target.num==1),c("newID","EdMes")])
EdMes.e <- EdMesID.e[,2]
EdMesID.i <- unique(d[which(d$target.num==2),c("newID","EdMes")])
EdMes.i <- EdMesID.i[,2]
EdMesID.o <- unique(d[which(d$target.num==3),c("newID","EdMes")])
EdMes.o <- EdMesID.o[,2]

LabMesID.e <- unique(d[which(d$target.num==1),c("newID","LabMes")])
LabMes.e <- LabMesID.e[,2]
LabMesID.i <- unique(d[which(d$target.num==2),c("newID","LabMes")])
LabMes.i <- LabMesID.i[,2]
LabMesID.o <- unique(d[which(d$target.num==3),c("newID","LabMes")])
LabMes.o <- LabMesID.o[,2]

ComMesID.e <- unique(d[which(d$target.num==1),c("newID","ComMes")])
ComMes.e <- ComMesID.e[,2]
ComMesID.i <- unique(d[which(d$target.num==2),c("newID","ComMes")])
ComMes.i <- ComMesID.i[,2]
ComMesID.o <- unique(d[which(d$target.num==3),c("newID","ComMes")])
ComMes.o <- ComMesID.o[,2]

FamMatID.e <- unique(d[which(d$target.num==1),c("newID","FamMat")])
FamMat.e <- FamMatID.e[,2]
FamMatID.i <- unique(d[which(d$target.num==2),c("newID","FamMat")])
FamMat.i <- FamMatID.i[,2]
FamMatID.o <- unique(d[which(d$target.num==3),c("newID","FamMat")])
FamMat.o <- FamMatID.o[,2]

EmpMatID.e <- unique(d[which(d$target.num==1),c("newID","EmpMat")])
EmpMat.e <- EmpMatID.e[,2]
EmpMatID.i <- unique(d[which(d$target.num==2),c("newID","EmpMat")])
EmpMat.i <- EmpMatID.i[,2]
EmpMatID.o <- unique(d[which(d$target.num==3),c("newID","EmpMat")])
EmpMat.o <- EmpMatID.o[,2]

CtyMatID.e <- unique(d[which(d$target.num==1),c("newID","CtyMat")]) 
CtyMat.e <- CtyMatID.e[,2]
CtyMatID.i <- unique(d[which(d$target.num==2),c("newID","CtyMat")]) 
CtyMat.i <- CtyMatID.i[,2]
CtyMatID.o <- unique(d[which(d$target.num==3),c("newID","CtyMat")]) 
CtyMat.o <- CtyMatID.o[,2]


abils.e <- mean(post1$a0[,1]) +  colMeans(post1$aIndiv[,1,machiID.e[,1]])
abils.i <- mean(post1$a0[,2]) +  colMeans(post1$aIndiv[,2,machiID.i[,1]])
abils.o <- mean(post1$a0[,3]) +  colMeans(post1$aIndiv[,3,machiID.o[,1]])

#HPDI
lowers.e <- rep(0, length(machiID.e[,1]))
lowers.i <- rep(0, length(machiID.i[,1]))
lowers.o <- rep(0, length(machiID.o[,1]))
uppers.e <- rep(0, length(machiID.e[,1]))
uppers.i <- rep(0, length(machiID.i[,1]))
uppers.o <- rep(0, length(machiID.o[,1]))
index.e <- 0
index.i <- 0
index.o <- 0
for ( z5 in 1:J ) {
	if ( z5 %in% machiID.e[,1] ) {
		index.e <- index.e + 1
		lowers.e[index.e] <- HPDI(post1$a0[,1] + post1$aIndiv[,1,z5] ,prob=0.9)[1]
  		uppers.e[index.e] <- HPDI(post1$a0[,1] + post1$aIndiv[,1,z5] ,prob=0.9)[2]
  	} #if

	if ( z5 %in% machiID.i[,1] ) {
		index.i <- index.i + 1
  		lowers.i[index.i] <- HPDI(post1$a0[,2] + post1$aIndiv[,2,z5] ,prob=0.9)[1]
  		uppers.i[index.i] <- HPDI(post1$a0[,2] + post1$aIndiv[,2,z5] ,prob=0.9)[2]
  	} #if

  	if ( z5 %in% machiID.o[,1] ) {	
		index.o <- index.o + 1	
  		lowers.o[index.o] <- HPDI(post1$a0[,3] + post1$aIndiv[,3,z5] ,prob=0.9)[1]
		uppers.o[index.o] <- HPDI(post1$a0[,3] + post1$aIndiv[,3,z5] ,prob=0.9)[2]
	} #if
} #for z5


#ability matrices for plotting, different numbers of people for each target
all.e <- as.data.frame( cbind(machiID.e, EdMes.e, LabMes.e, ComMes.e,
							       		 FamMat.e, EmpMat.e, CtyMat.e,
							  		 	 abils.e, lowers.e, uppers.e) )
all.i <- as.data.frame( cbind(machiID.i, EdMes.i, LabMes.i, ComMes.i,
							  			 FamMat.i, EmpMat.i, CtyMat.i,
							  		 	 abils.i, lowers.i, uppers.i,
							  			 abils.e[machiID.i[,1]],
							  			 lowers.e[machiID.i[,1]],
							  			 uppers.e[machiID.i[,1]]) )
names(all.i)[12:14] <- c("abils.e","lowers.e","uppers.e")
all.o <- as.data.frame( cbind(machiID.o, EdMes.o, LabMes.o, ComMes.o,
							  			 FamMat.o, EmpMat.o, CtyMat.o,
							  		 	 abils.o, lowers.o, uppers.o,
							  			 abils.e[machiID.o[,1]],
							  			 lowers.e[machiID.o[,1]],
							  			 uppers.e[machiID.o[,1]],
                       					 abils.i,
                       					 lowers.i,
                       					 uppers.i) )
names(all.o)[12:17] <- c("abils.e","lowers.e","uppers.e","abils.i","lowers.i","uppers.i")




####################### individual ego location highlights 

#sort by response abils
all_ego <- all.e[order(all.e$abils.e),]
all_ego <- cbind(all_ego, c(1:J))
#names(all_ego)
colnames(all_ego)[12] <- c("ord")

pdf(file="./Plots/m1_ego_location_highlights.pdf",
    height=2, width=6)
dotplot( all_ego$abils.e ~ all_ego$ord , data=all_ego,
         horizontal=F ,
         ylim=c(-3, 2),
         xlim=c(-0.5, nrow(all_ego)+1),
         main=NULL, 
         cex=0.4, xlab=NULL, ylab=list(label="Latent Ego Axis", cex=0.75),
         scales=list(x=list(draw=F),
                     tck=c(1,0), cex=0.6,
                     alternating=1),
         panel = function (x, y) {
           

           panel.segments( x0=-0.5, y0=mean(all_ego[which(all_ego$Machi==1), "abils.e"]), #machi mean line
                     x1=nrow(all_ego)+1, y1=mean(all_ego[which(all_ego$Machi==1), "abils.e"]),
                     lty=1, lwd=0.5, col="blue" )

           panel.segments( x0=-0.5, y0=mean(all_ego[which(all_ego$Machi==0), "abils.e"]), #mestizo mean line
                     x1=nrow(all_ego)+1, y1=mean(all_ego[which(all_ego$Machi==0), "abils.e"]),
                     lty=1, lwd=0.5, col="red" )

           panel.segments( x0=-0.5, y0=mean(all_ego[which(all_ego$newID %in% c(83:88,92,104)), "abils.e"]), #familiar machis mean line
                     x1=nrow(all_ego)+1, y1=mean(all_ego[which(all_ego$newID %in% c(83:88,92,104)), "abils.e"]),
                     lty=2, lwd=0.5, col="blue" )

           panel.segments( x0=-0.5, y0=mean(all_ego[which(all_ego$newID %in% c(82,67,56,51,42,76,59,48,9,3)), "abils.e"]), #familiar mestizos mean line
                     x1=nrow(all_ego)+1, y1=mean(all_ego[which(all_ego$newID %in% c(82,67,56,51,42,76,59,48,9,3)), "abils.e"]),
                     lty=2, lwd=0.5, col="red" )                     


           panel.xyplot(x, y, pch = 1, cex=0.3, col = "black") #grey(0.75))
           panel.xyplot(x, y, pch = 16, cex=0.25, col = "white") 
           for (p in 1:J) {
             if (all_ego$Machi[p] == 1) {
               panel.points(all_ego$ord[p],
                        all_ego$abils.e[p],
                        pch=16, cex=0.35, col="black") #grey(0.75))
             } #if
           } #for



           for ( p1 in 1:J ) { #machis familiar to mestizos
             if ( any(all_ego$newID[p1] == c(83:88,92,104)) ) {
               panel.points(all_ego$ord[p1],
                            all_ego$abils.e[p1],
                            pch=16, cex=0.45, lwd=0.5, col="blue")
             } #if
           } #for


           for ( p1 in 1:J ) { #mestizos familiar to machis
             if ( any(all_ego$newID[p1] == c(82,67,56,51,42,76,59,48,9,3)) ) {
               panel.points(all_ego$ord[p1],
                            all_ego$abils.e[p1],
                            pch=1, cex=0.45, lwd=0.5, col="red")
             } #if
           } #for

           #ltext(x=all_ego$ord, #ID labels
           #				y=all_ego$abils.e,
           #				labels=all_ego$newID, pos=1, offset=-0.2, cex=0.1, col=grey(0.6)) 

         } #panel
) #dotplot
graphics.off()

#ID_key2 #match newID to original ID



############################### plot ego vs. ingroup guess individual positions on latent axes, highlighting experience types

pdf(file="./Plots/m1_ego_vs_in.pdf", 
    height=7, width=5)
par(mfcol=c(3,2))
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)) #margins for indiv plot, oma for outer margins

########### machi commerce
plot(x=all.i$abils.e, y=all.i$abils.i,
		type="n", #don't plot points
  		xlim=c( min(all.i$abils.e)-0.1, max(all.i$abils.e)+0.1 ),
  		ylim=c( min(all.i$abils.i)-0.1, max(all.i$abils.i)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		pch=16,
  		col="black"
	)
mtext("Matsi Com", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=2, at=c(-2,-1,0,1))	#left
axis(side=3, at=c(-2,-1,0,1), labels=FALSE) #top
points(x=all.i$abils.e[which(all.i$Machi==1)],	#all machis
		y=all.i$abils.i[which(all.i$Machi==1)],
		pch=16,
		col="black"
		)
points(x=all.i$abils.e[which(all.i$Machi==1 & all.i$ComMes==1)],	#machis with commerce
		y=all.i$abils.i[which(all.i$Machi==1 & all.i$ComMes==1)],
		pch=16,
		col="green"
		)
lines(x=c( min(all.i$abils.e)-0.3, max(all.i$abils.e)+0.3 ),
	  y=c( mean(all.i$abils.i[which(all.i$Machi==1 & all.i$ComMes==1)]), mean(all.i$abils.i[which(all.i$Machi==1 & all.i$ComMes==1)]) ),
	  lwd=1, col="green")
box()

########### machi employment
plot(x=all.i$abils.e, y=all.i$abils.i,
		type="n", #don't plot points
  		xlim=c( min(all.i$abils.e)-0.1, max(all.i$abils.e)+0.1 ),
  		ylim=c( min(all.i$abils.i)-0.1, max(all.i$abils.i)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		pch=16,
  		col="black"
	)
mtext("Matsi Lab", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=2, at=c(-2,-1,0,1))
points(x=all.i$abils.e[which(all.i$Machi==1)],	#all machis
		y=all.i$abils.i[which(all.i$Machi==1)],
		pch=16,
		col="black"
		)
points(x=all.i$abils.e[which(all.i$Machi==1 & all.i$LabMes==1)],	#machis with employment
		y=all.i$abils.i[which(all.i$Machi==1 & all.i$LabMes==1)],
		pch=16,
		col="blue"
		)
lines(x=c( min(all.i$abils.e)-0.3, max(all.i$abils.e)+0.3 ),
	  y=c( mean(all.i$abils.i[which(all.i$Machi==1 & all.i$LabMes==1)]), mean(all.i$abils.i[which(all.i$Machi==1 & all.i$LabMes==1)]) ),
	  lwd=1, col="blue")
box()


######## machi education
plot(x=all.i$abils.e, y=all.i$abils.i,
		type="n", #don't plot points
  		xlim=c( min(all.i$abils.e)-0.1, max(all.i$abils.e)+0.1 ),
  		ylim=c( min(all.i$abils.i)-0.1, max(all.i$abils.i)+0.1 ),
  		cex=1,
  		axes=FALSE,,
  		pch=16,
  		col="black"
	)
mtext("Matsi Edu", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=1, at=c(-2,-1,0,1))
axis(side=2, at=c(-2,-1,0,1))
points(x=all.i$abils.e[which(all.i$Machi==1)],	#all machis
		y=all.i$abils.i[which(all.i$Machi==1)],
		pch=16,
		col="black"
		)
points(x=all.i$abils.e[which(all.i$Machi==1 & all.i$EdMes==1)],	#machis with education
		y=all.i$abils.i[which(all.i$Machi==1 & all.i$EdMes==1)],
		pch=16,
		col="red"
		)
lines(x=c( min(all.i$abils.e)-0.3, max(all.i$abils.e)+0.3 ),
	  y=c( mean(all.i$abils.i[which(all.i$Machi==1 & all.i$EdMes==1)]), mean(all.i$abils.i[which(all.i$Machi==1 & all.i$EdMes==1)]) ),
	  lwd=1, col="red")
box()


########### mestizo family
plot(x=all.i$abils.e, y=all.i$abils.i,
		type="n", #don't plot points
  		xlim=c( min(all.i$abils.e)-0.1, max(all.i$abils.e)+0.1 ),
  		ylim=c( min(all.i$abils.i)-0.1, max(all.i$abils.i)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		pch=16,
  		col="black"
	)
mtext("Mest Fam", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=3, at=c(-2,-1,0,1), labels=FALSE)
axis(side=4, at=c(-2,-1,0,1), labels=FALSE)
points(x=all.i$abils.e[which(all.i$Machi==0)],	#all mestizos
		y=all.i$abils.i[which(all.i$Machi==0)],
		pch=1,
		col="black"
		)
points(x=all.i$abils.e[which(all.i$Machi==0 & all.i$FamMat==1)],	#mestizos with family
		y=all.i$abils.i[which(all.i$Machi==0 & all.i$FamMat==1)],
		pch=1,
		col="purple"
		)
lines(x=c( min(all.i$abils.e)-0.3, max(all.i$abils.e)+0.3 ),
	  y=c( mean(all.i$abils.i[which(all.i$Machi==0 & all.i$FamMat==1)]), mean(all.i$abils.i[which(all.i$Machi==0 & all.i$FamMat==1)]) ),
	  lwd=1, col="purple")
box()

########### mestizo employment
plot(x=all.i$abils.e, y=all.i$abils.i,
		type="n", #don't plot points
  		xlim=c( min(all.i$abils.e)-0.1, max(all.i$abils.e)+0.1 ),
  		ylim=c( min(all.i$abils.i)-0.1, max(all.i$abils.i)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		pch=16,
  		col="black"
	)
mtext("Mest Emp", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=4, at=c(-2,-1,0,1), labels=FALSE)
points(x=all.i$abils.e[which(all.i$Machi==0)],	#all mestizos
		y=all.i$abils.i[which(all.i$Machi==0)],
		pch=1,
		col="black"
		)
points(x=all.i$abils.e[which(all.i$Machi==0 & all.i$EmpMat==1)],	#mestizos with employment
		y=all.i$abils.i[which(all.i$Machi==0 & all.i$EmpMat==1)],
		pch=1,
		col="orange"
		)
lines(x=c( min(all.i$abils.e)-0.3, max(all.i$abils.e)+0.3 ),
	  y=c( mean(all.i$abils.i[which(all.i$Machi==0 & all.i$EmpMat==1)]), mean(all.i$abils.i[which(all.i$Machi==0 & all.i$EmpMat==1)]) ),
	  lwd=1, col="orange")
box()

######## mestizo community
plot(x=all.i$abils.e, y=all.i$abils.i,
		type="n", #don't plot points
  		xlim=c( min(all.i$abils.e)-0.1, max(all.i$abils.e)+0.1 ),
  		ylim=c( min(all.i$abils.i)-0.1, max(all.i$abils.i)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		pch=16,
  		col="black"
	)
mtext("Mest Cty", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=4, at=c(-2,-1,0,1), labels=FALSE)
axis(side=1, at=c(-2,-1,0,1))
points(x=all.i$abils.e[which(all.i$Machi==0)],	#all mestizos
		y=all.i$abils.i[which(all.i$Machi==0)],
		pch=1,
		col="black"
		)
points(x=all.i$abils.e[which(all.i$Machi==0 & all.i$CtyMat==1)],	#machis with education
		y=all.i$abils.i[which(all.i$Machi==0 & all.i$CtyMat==1)],
		pch=1,
		col="turquoise"
		)
lines(x=c( min(all.i$abils.e)-0.3, max(all.i$abils.e)+0.3 ),
	  y=c( mean(all.i$abils.i[which(all.i$Machi==0 & all.i$CtyMat==1)]), mean(all.i$abils.i[which(all.i$Machi==0 & all.i$CtyMat==1)]) ),
	  lwd=1, col="turquoise")
box()

mtext("Ego Latent Axis", side = 1, outer = TRUE, cex = 0.8, line = 2.2)
mtext("In-Group Guess Latent Axis", side = 2, outer = TRUE, cex = 0.8, line = 2.2)

graphics.off()




############################### plot ego vs. outgroup guess, highlighting experience types

pdf(file="./Plots/m1_ego_vs_out.pdf", 
    height=7, width=5)
par(mfcol=c(3,2))
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)) #margins for indiv plot, oma for outer margins

########### machi commerce
plot(x=all.o$abils.e, y=all.o$abils.o,
		#main="Machi Com",
		type="n", #don't plot points
  		xlim=c( min(all.o$abils.e)-0.1, max(all.o$abils.e)+0.1 ),
  		ylim=c( min(all.o$abils.o)-0.1, max(all.o$abils.o)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		#xlab="Interdependence of Personal Responses",
  		#ylab="Accuracy of Perceptions of Out-group",
  		pch=16,
  		col="black"
	)
mtext("Matsi Com", side = 3, line = -1, adj = 0.05, cex = 0.7)
#axis(side=1, at=c(-1,0,1))
axis(side=2, at=c(-1,0,1,2))	#left
axis(side=3, at=c(-2,-1,0,1), labels=FALSE) #top
points(x=all.o$abils.e[which(all.o$Machi==1)],	#all machis
		y=all.o$abils.o[which(all.o$Machi==1)],
		pch=16,
		col="black"
		)
# points(x=all.o$abils.e[which(all.o$Machi==0)],	#all mestizos
# 		y=all.o$abils.o[which(all.o$Machi==0)],
# 		pch=1,
# 		col="black"
# 		)
points(x=all.o$abils.e[which(all.o$Machi==1 & all.o$ComMes==1)],	#machis with commerce
		y=all.o$abils.o[which(all.o$Machi==1 & all.o$ComMes==1)],
		pch=16,
		col="green"
		)
lines(x=c( min(all.o$abils.e)-0.3, max(all.o$abils.e)+0.3 ),
	  y=c( mean(all.o$abils.o[which(all.o$Machi==1 & all.o$ComMes==1)]), mean(all.o$abils.o[which(all.o$Machi==1 & all.o$ComMes==1)]) ),
	  lwd=1, col="green")
box()

########### machi employment
plot(x=all.o$abils.e, y=all.o$abils.o,
		#main="Machi Emp",
		type="n", #don't plot points
  		xlim=c( min(all.o$abils.e)-0.1, max(all.o$abils.e)+0.1 ),
  		ylim=c( min(all.o$abils.o)-0.1, max(all.o$abils.o)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		#xlab="Interdependence of Personal Responses",
  		#ylab="Accuracy of Perceptions of Out-group",
  		pch=16,
  		col="black"
	)
mtext("Matsi Lab", side = 3, line = -1, adj = 0.05, cex = 0.7)
#axis(side=1, at=c(-1,0,1))
axis(side=2, at=c(-1,0,1,2))
points(x=all.o$abils.e[which(all.o$Machi==1)],	#all machis
		y=all.o$abils.o[which(all.o$Machi==1)],
		pch=16,
		col="black"
		)
# points(x=all.o$abils.e[which(all.o$Machi==0)],	#all mestizos
# 		y=all.o$abils.o[which(all.o$Machi==0)],
# 		pch=1,
# 		col="black"
# 		)
points(x=all.o$abils.e[which(all.o$Machi==1 & all.o$LabMes==1)],	#machis with employment
		y=all.o$abils.o[which(all.o$Machi==1 & all.o$LabMes==1)],
		pch=16,
		col="blue"
		)
lines(x=c( min(all.o$abils.e)-0.3, max(all.o$abils.e)+0.3 ),
	  y=c( mean(all.o$abils.o[which(all.o$Machi==1 & all.o$LabMes==1)]), mean(all.o$abils.o[which(all.o$Machi==1 & all.o$LabMes==1)]) ),
	  lwd=1, col="blue")
box()


######## machi education
plot(x=all.o$abils.e, y=all.o$abils.o,
		#main="Machi Edu",
		type="n", #don't plot points
  		xlim=c( min(all.o$abils.e)-0.1, max(all.o$abils.e)+0.1 ),
  		ylim=c( min(all.o$abils.o)-0.1, max(all.o$abils.o)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		#xlab="Interdependence of Personal Responses",
  		#ylab="Accuracy of Perceptions of Out-group",
  		pch=16,
  		col="black"
	)
mtext("Matsi Edu", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=1, at=c(-2,-1,0,1))
axis(side=2, at=c(-1,0,1,2))
points(x=all.o$abils.e[which(all.o$Machi==1)],	#all machis
		y=all.o$abils.o[which(all.o$Machi==1)],
		pch=16,
		col="black"
		)
# points(x=all.o$abils.e[which(all.o$Machi==0)],	#all mestizos
# 		y=all.o$abils.o[which(all.o$Machi==0)],
# 		pch=1,
# 		col="black"
# 		)
points(x=all.o$abils.e[which(all.o$Machi==1 & all.o$EdMes==1)],	#machis with education
		y=all.o$abils.o[which(all.o$Machi==1 & all.o$EdMes==1)],
		pch=16,
		col="red"
		)
lines(x=c( min(all.o$abils.e)-0.3, max(all.o$abils.e)+0.3 ),
	  y=c( mean(all.o$abils.o[which(all.o$Machi==1 & all.o$EdMes==1)]), mean(all.o$abils.o[which(all.o$Machi==1 & all.o$EdMes==1)]) ),
	  lwd=1, col="red")
box()



########### mestizo family
plot(x=all.o$abils.e, y=all.o$abils.o,
		#main="Mest Fam",
		type="n", #don't plot points
  		xlim=c( min(all.o$abils.e)-0.1, max(all.o$abils.e)+0.1 ),
  		ylim=c( min(all.o$abils.o)-0.1, max(all.o$abils.o)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		#xlab="Interdependence of Personal Responses",
  		#ylab="Accuracy of Perceptions of Out-group",
  		pch=16,
  		col="black"
	)
mtext("Mest Fam", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=3, at=c(-2,-1,0,1), labels=FALSE)
axis(side=4, at=c(-1,0,1,2), labels=FALSE)
# points(x=all.o$abils.e[which(all.o$Machi==1)],	#all machis
# 		y=all.o$abils.o[which(all.o$Machi==1)],
# 		pch=16,
# 		col="black"
# 		)
points(x=all.o$abils.e[which(all.o$Machi==0)],	#all mestizos
		y=all.o$abils.o[which(all.o$Machi==0)],
		pch=1,
		col="black"
		)
points(x=all.o$abils.e[which(all.o$Machi==0 & all.o$FamMat==1)],	#mestizos with family
		y=all.o$abils.o[which(all.o$Machi==0 & all.o$FamMat==1)],
		pch=1,
		col="purple"
		)
lines(x=c( min(all.o$abils.e)-0.3, max(all.o$abils.e)+0.3 ),
	  y=c( mean(all.o$abils.o[which(all.o$Machi==0 & all.o$FamMat==1)]), mean(all.o$abils.o[which(all.o$Machi==0 & all.o$FamMat==1)]) ),
	  lwd=1, col="purple")
box()

########### mestizo employment
plot(x=all.o$abils.e, y=all.o$abils.o,
		#main="Mest Emp",
		type="n", #don't plot points
  		xlim=c( min(all.o$abils.e)-0.1, max(all.o$abils.e)+0.1 ),
  		ylim=c( min(all.o$abils.o)-0.1, max(all.o$abils.o)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		#xlab="Interdependence of Personal Responses",
  		#ylab="Accuracy of Perceptions of Out-group",
  		pch=16,
  		col="black"
	)
mtext("Mest Emp", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=4, at=c(-1,0,1,2), labels=FALSE)
#axis(side=2, at=c(-1,0,1))
# points(x=all.o$abils.e[which(all.o$Machi==1)],	#all machis
# 		y=all.o$abils.o[which(all.o$Machi==1)],
# 		pch=16,
# 		col="black"
# 		)
points(x=all.o$abils.e[which(all.o$Machi==0)],	#all mestizos
		y=all.o$abils.o[which(all.o$Machi==0)],
		pch=1,
		col="black"
		)
points(x=all.o$abils.e[which(all.o$Machi==0 & all.o$EmpMat==1)],	#mestizos with employment
		y=all.o$abils.o[which(all.o$Machi==0 & all.o$EmpMat==1)],
		pch=1,
		col="orange"
		)
lines(x=c( min(all.o$abils.e)-0.3, max(all.o$abils.e)+0.3 ),
	  y=c( mean(all.o$abils.o[which(all.o$Machi==0 & all.o$EmpMat==1)]), mean(all.o$abils.o[which(all.o$Machi==0 & all.o$EmpMat==1)]) ),
	  lwd=1, col="orange")
box()

######## mestizo community
plot(x=all.o$abils.e, y=all.o$abils.o,
		#main="Mest Cty",
		type="n", #don't plot points
  		xlim=c( min(all.o$abils.e)-0.1, max(all.o$abils.e)+0.1 ),
  		ylim=c( min(all.o$abils.o)-0.1, max(all.o$abils.o)+0.1 ),
  		cex=1,
  		axes=FALSE,
  		#xlab="Interdependence of Personal Responses",
  		#ylab="Accuracy of Perceptions of Out-group",
  		pch=16,
  		col="black"
	)
mtext("Mest Cty", side = 3, line = -1, adj = 0.05, cex = 0.7)
axis(side=4, at=c(-1,0,1,2), labels=FALSE)
axis(side=1, at=c(-2,-1,0,1))
# points(x=all.o$abils.e[which(all.o$Machi==1)],	#all machis
# 		y=all.o$abils.o[which(all.o$Machi==1)],
# 		pch=16,
# 		col="black"
# 		)
points(x=all.o$abils.e[which(all.o$Machi==0)],	#all mestizos
		y=all.o$abils.o[which(all.o$Machi==0)],
		pch=1,
		col="black"
		)
points(x=all.o$abils.e[which(all.o$Machi==0 & all.o$CtyMat==1)],	#machis with education
		y=all.o$abils.o[which(all.o$Machi==0 & all.o$CtyMat==1)],
		pch=1,
		col="turquoise"
		)
lines(x=c( min(all.o$abils.e)-0.3, max(all.o$abils.e)+0.3 ),
	  y=c( mean(all.o$abils.o[which(all.o$Machi==0 & all.o$CtyMat==1)]), mean(all.o$abils.o[which(all.o$Machi==0 & all.o$CtyMat==1)]) ),
	  lwd=1, col="turquoise")
box()

mtext("Ego Latent Axis", side = 1, outer = TRUE, cex = 0.8, line = 2.2)
mtext("Out-Group Guess Latent Axis", side = 2, outer = TRUE, cex = 0.8, line = 2.2)

graphics.off()


