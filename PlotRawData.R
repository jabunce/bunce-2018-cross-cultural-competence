#################################################raw proportions Figures

###### ego response raw proportions

props_e <- data.frame(
  Quest=c(1:K) ,
  Matsi=table( d$question[which(d$Machi==1 & d$target.num==1)], d$response.e[which(d$Machi==1 & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$Machi==1 & d$target.num==1)], d$response.e[which(d$Machi==1 & d$target.num==1)]) ),

  Mest=table( d$question[which(d$Machi==0 & d$target.num==1)], d$response.e[which(d$Machi==0 & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$Machi==0 & d$target.num==1)], d$response.e[which(d$Machi==0 & d$target.num==1)]) )
)

colnames(props_e)[c(1:3)] <- c("Quest", "mach_yes", "mest_yes")

props_e <- cbind(props_e, quest_names)
#print(props_e, digits=2)

props.e <- props_e[order(props_e$mach_yes),] #reorder by machi percentage


###### outgroup guess raw proportions

props_o <- data.frame(
  Quest=c(1:K) ,
  Matsi=table( d$question[which(d$Machi==1 & d$target.num==3)], d$response.e[which(d$Machi==1 & d$target.num==3)] )[,2] /
    rowSums( table( d$question[which(d$Machi==1 & d$target.num==3)], d$response.e[which(d$Machi==1 & d$target.num==3)]) ),

  Mest=table( d$question[which(d$Machi==0 & d$target.num==3)], d$response[which(d$Machi==0 & d$target.num==3)] )[,2] /
    rowSums( table( d$question[which(d$Machi==0 & d$target.num==3)], d$response[which(d$Machi==0 & d$target.num==3)]) )
)

colnames(props_o)[c(1:3)] <- c("Quest", "mach_yes", "mest_yes")

props_o <- cbind(props_o, quest_names)
#print(props_o, digits=2)



props.o <- props_o[order(props_o$mach_yes),] #reorder by machi percentage



###### ingroup guess raw proportions

props_i <- data.frame(
  Quest=c(1:K) ,
  Matsi=table( d$question[which(d$Machi==1 & d$target.num==2)], d$response.e[which(d$Machi==1 & d$target.num==2)] )[,2] /
    rowSums( table( d$question[which(d$Machi==1 & d$target.num==2)], d$response.e[which(d$Machi==1 & d$target.num==2)]) ),

  Mest=table( d$question[which(d$Machi==0 & d$target.num==2)], d$response.e[which(d$Machi==0 & d$target.num==2)] )[,2] /
    rowSums( table( d$question[which(d$Machi==0 & d$target.num==2)], d$response.e[which(d$Machi==0 & d$target.num==2)]) )
)

colnames(props_i)[c(1:3)] <- c("Quest", "mach_yes", "mest_yes")

props_i <- cbind(props_i, quest_names)
#print(props_i, digits=2)

props.i <- props_i[order(props_i$mach_yes),] #reorder by machi percentage



# ego plot

egoprop_plot <- xyplot( props.e$mach_yes ~ props.e$mest_yes,
  main="",
  xlim=c(-0.05, 1.05),
  ylim=c(-0.05, 1.05),
  xlab=list("Mestizo Proportion Positive", cex=0.8),
  ylab=list("Matsigenka Proportion Positive", cex=0.8),
  scales=list( tck=c(1,1), cex=0.75, alternating=1 ),
  panel = function (x, y) {

          panel.xyplot( x=props.e$mest_yes, y=props.e$mach_yes,
                         pch = 16, cex=1.8, col = "white" )
          panel.xyplot( x=props.e$mest_yes, y=props.e$mach_yes,
                         pch = 1, cex=1.8, col = "black" )
          panel.segments( x0=-0.05, y0=-0.05, #diag line y=x
                     x1=1.05, y1=1.05, col="black", lwd=0.5 )
          ltext(x=props.e$mest_yes,
                 y=props.e$mach_yes,
                 labels=props.e$Quest, pos=1, offset=-0.35, cex=0.7, col="black")

  } #panel

) #xyplot



#out-group plot

outprop_plot <- xyplot( props.o$mach_yes ~ props.o$mest_yes,
  main="",
  xlim=c(-0.05, 1.05),
  ylim=c(-0.05, 1.05),
  xlab=list("Mestizo Guess Matsi Positive", cex=0.8),
  ylab=list("Matsi Guess Mestizos Positive", cex=0.8),
  scales=list( tck=c(1,1), cex=0.75, alternating=1 ),
  panel = function (x, y) {

          panel.xyplot( x=props.o$mest_yes, y=props.o$mach_yes,
                         pch = 16, cex=1.8, col = "white" )
          panel.xyplot( x=props.o$mest_yes, y=props.o$mach_yes,
                         pch = 1, cex=1.8, col = "black" )
          panel.segments( x0=-0.05, y0=-0.05, #diag line y=x
                     x1=1.05, y1=1.05, col="black", lwd=0.5 )
          ltext(x=props.o$mest_yes,
                 y=props.o$mach_yes,
                 labels=props.o$Quest, pos=1, offset=-0.35, cex=0.7, col="black")

  } #panel

) #xyplot



#in-group plot

inprop_plot <- xyplot( props.i$mach_yes ~ props.i$mest_yes,
  main="",
  xlim=c(-0.05, 1.05),
  ylim=c(-0.05, 1.05),
  xlab=list("Mestizo Guess Mestizos Positive", cex=0.8),
  ylab=list("Matsi Guess Matsi Positive", cex=0.8),
  scales=list( tck=c(1,1), cex=0.75, alternating=1 ),
  panel = function (x, y) {

          panel.xyplot( x=props.i$mest_yes, y=props.i$mach_yes,
                         pch = 16, cex=1.8, col = "white" )
          panel.xyplot( x=props.i$mest_yes, y=props.i$mach_yes,
                         pch = 1, cex=1.8, col = "black" )
          panel.segments( x0=-0.05, y0=-0.05, #diag line y=x
                     x1=1.05, y1=1.05, col="black", lwd=0.5 )
          ltext(x=props.i$mest_yes,
                 y=props.i$mach_yes,
                 labels=props.i$Quest, pos=1, offset=-0.35, cex=0.7, col="black")

  } #panel

) #xyplot



#combined figure

pdf(file="./Plots/Fig_raw_props.pdf", 
height=12, width=4)

print(egoprop_plot, split=c(1,1, #position (col, row)
                          	1,3), #in grid of cols, rows
                    more=TRUE)
print(inprop_plot, split=c(1,2,
                           1,3),
                    more=TRUE)
print(outprop_plot, split=c(1,3,
                          	1,3),
                    more=TRUE)
print( grid.text(label="A", x = unit(0.21, "npc"), y = unit(0.94, "npc"),
             gp=gpar(fontsize=20, col="black")) )
print( grid.text(label="B", x = unit(0.21, "npc"), y = unit(0.605, "npc"),
             gp=gpar(fontsize=20, col="black")) )
print( grid.text(label="C", x = unit(0.21, "npc"), y = unit(0.272, "npc"),
             gp=gpar(fontsize=20, col="black")) )

graphics.off()





###### ego response Boca Manu vs. Atalaya raw proportions

props_BA <- data.frame(
  Quest=c(1:K) ,
  BM=table( d$question[which(d$Machi==0 & d$Community=="B" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="B" & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$Machi==0 & d$Community=="B" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="B" & d$target.num==1)]) ),

  At=table( d$question[which(d$Machi==0 & d$Community=="A" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="A" & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$Machi==0 & d$Community=="A" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="A" & d$target.num==1)]) )
)

colnames(props_BA)[c(1:3)] <- c("Quest", "BM_yes", "At_yes")

props1 <- cbind(props_BA, quest_names)
#print(props_BA, digits=2)

props.BA <- props_BA[order(props_BA$BM_yes),] #reorder by BM percentage



###### ego response Boca Manu vs. cards raw proportions

cardIDs.B <- c(246,228,257,260,240,234,217,182,176,247,196,178,198,183,241,236,177,181,189,202,262,233,216,255) #IDs of people on mestizo community cards

props_Bc <- data.frame(
  Quest=c(1:K) ,
  BM=table( d$question[which(d$Machi==0 & d$Community=="B" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="B" & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$Machi==0 & d$Community=="B" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="B" & d$target.num==1)]) ),

  card=table( d$question[which(d$ID %in% cardIDs.B & d$target.num==1)], d$response.e[which(d$ID %in% cardIDs.B & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$ID %in% cardIDs.B & d$target.num==1)], d$response.e[which(d$ID %in% cardIDs.B & d$target.num==1)]) )
)

colnames(props_Bc)[c(1:3)] <- c("Quest", "BM_yes", "card_yes")

props_Bc <- cbind(props_Bc, quest_names)
#print(props_Bc, digits=2)

props.Bc <- props_Bc[order(props_Bc$BM_yes),] #reorder by BM percentage


###### ego response Atalaya vs. cards raw proportions

props_Ac <- data.frame(
  Quest=c(1:K) ,
  At=table( d$question[which(d$Machi==0 & d$Community=="A" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="A" & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$Machi==0 & d$Community=="A" & d$target.num==1)], d$response.e[which(d$Machi==0 & d$Community=="A" & d$target.num==1)]) ),

  card=table( d$question[which(d$ID %in% cardIDs.B & d$target.num==1)], d$response.e[which(d$ID %in% cardIDs.B & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$ID %in% cardIDs.B & d$target.num==1)], d$response.e[which(d$ID %in% cardIDs.B & d$target.num==1)]) )
)

colnames(props_Ac)[c(1:3)] <- c("Quest", "At_yes", "card_yes")

props_Ac <- cbind(props_Ac, quest_names)
#print(props_Ac, digits=2)

props.Ac <- props_Ac[order(props_Ac$At_yes),] #reorder by atalaya percentage



######ego response Tayakome vs. cards raw proportions

cardIDs.T <- c(79,37,142,2,53,107,61,171,56,7,4,137,27,18,44,3,21,141,120,26,67,108,101,16,45) #IDs of people on matsigenka community cards 

props_Tc <- data.frame(
  Quest=c(1:K) ,
  Matsi=table( d$question[which(d$Machi==1 & d$Community=="T" & d$target.num==1)], d$response.e[which(d$Machi==1 & d$Community=="T" & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$Machi==1 & d$Community=="T" & d$target.num==1)], d$response.e[which(d$Machi==1 & d$Community=="T" & d$target.num==1)]) ),

  card=table( d$question[which(d$ID %in% cardIDs.T & d$target.num==1)], d$response.e[which(d$ID %in% cardIDs.T & d$target.num==1)] )[,2] /
    rowSums( table( d$question[which(d$ID %in% cardIDs.T & d$target.num==1)], d$response.e[which(d$ID %in% cardIDs.T & d$target.num==1)]) )
)

colnames(props_Tc)[c(1:3)] <- c("Quest", "Mats_yes", "card_yes")

props_Tc <- cbind(props_Tc, quest_names)
#print(props_Tc, digits=2)

props.Tc <- props_Tc[order(props_Tc$Mats_yes),] #reorder by Tayakome percentage



#Boca Manu vs. Atlaya plot

BocAt_plot <- xyplot( props.BA$BM_yes ~ props.BA$At_yes,
  main="",
  xlim=c(-0.05, 1.05),
  ylim=c(-0.05, 1.05),
  xlab=list("Atalaya Proportion Positive", cex=0.8),
  ylab=list("Boca Manu Proportion Positive", cex=0.8),
  scales=list( tck=c(1,1), cex=0.75, alternating=1 ),
  panel = function (x, y) {

          panel.xyplot( x=props.BA$At_yes, y=props.BA$BM_yes,
                         pch = 16, cex=1.8, col = "white" )
          panel.xyplot( x=props.BA$At_yes, y=props.BA$BM_yes,
                         pch = 1, cex=1.8, col = "black" )
          panel.segments( x0=-0.05, y0=-0.05, #diag line y=x
                     x1=1.05, y1=1.05, col="black", lwd=0.5 )
          ltext(x=props.BA$At_yes,
                 y=props.BA$BM_yes,
                 labels=props.BA$Quest, pos=1, offset=-0.35, cex=0.7, col="black")

  } #panel

) #xyplot



#Boca Manu vs. card plot

BocCard_plot <- xyplot( props.Bc$BM_yes ~ props.Bc$card_yes,
  main="",
  xlim=c(-0.05, 1.05),
  ylim=c(-0.05, 1.05),
  xlab=list("Mestizo Card Proportion Positive", cex=0.8),
  ylab=list("Boca Manu Proportion Positive", cex=0.8),
  scales=list( tck=c(1,1), cex=0.75, alternating=1 ),
  panel = function (x, y) {

          panel.xyplot( x=props.Bc$card_yes, y=props.Bc$BM_yes,
                         pch = 16, cex=1.8, col = "white" )
          panel.xyplot( x=props.Bc$card_yes, y=props.Bc$BM_yes,
                         pch = 1, cex=1.8, col = "black" )
          panel.segments( x0=-0.05, y0=-0.05, #diag line y=x
                     x1=1.05, y1=1.05, col="black", lwd=0.5 )
          ltext(x=props.Bc$card_yes,
                 y=props.Bc$BM_yes,
                 labels=props.Bc$Quest, pos=1, offset=-0.35, cex=0.7, col="black")

  } #panel

) #xyplot



#atalaya vs. card plot

AtCard_plot <- xyplot( props.Ac$At_yes ~ props.Ac$card_yes,
  main="",
  xlim=c(-0.05, 1.05),
  ylim=c(-0.05, 1.05),
  xlab=list("Mestizo Card Proportion Positive", cex=0.8),
  ylab=list("Atalaya Proportion Positive", cex=0.8),
  scales=list( tck=c(1,1), cex=0.75, alternating=1 ),
  panel = function (x, y) {

          panel.xyplot( x=props.Ac$card_yes, y=props.Ac$At_yes,
                         pch = 16, cex=1.8, col = "white" )
          panel.xyplot( x=props.Ac$card_yes, y=props.Ac$At_yes,
                         pch = 1, cex=1.8, col = "black" )
          panel.segments( x0=-0.05, y0=-0.05, #diag line y=x
                     x1=1.05, y1=1.05, col="black", lwd=0.5 )
          ltext(x=props.Ac$card_yes,
                 y=props.Ac$At_yes,
                 labels=props.Ac$Quest, pos=1, offset=-0.35, cex=0.7, col="black")

  } #panel

) #xyplot



#Tayakome vs. card plot

TayCard_plot <- xyplot( props.Tc$Mats_yes ~ props.Tc$card_yes,
  main="",
  xlim=c(-0.05, 1.05),
  ylim=c(-0.05, 1.05),
  xlab=list("Matsigenka Card Proportion Positive", cex=0.8),
  ylab=list("Tayakome Proportion Positive", cex=0.8),
  scales=list( tck=c(1,1), cex=0.75, alternating=1 ),
  panel = function (x, y) {

          panel.xyplot( x=props.Tc$card_yes, y=props.Tc$Mats_yes,
                         pch = 16, cex=1.8, col = "white" )
          panel.xyplot( x=props.Tc$card_yes, y=props.Tc$Mats_yes,
                         pch = 1, cex=1.8, col = "black" )
          panel.segments( x0=-0.05, y0=-0.05, #diag line y=x
                     x1=1.05, y1=1.05, col="black", lwd=0.5 )
          ltext(x=props.Tc$card_yes,
                 y=props.Tc$Mats_yes,
                 labels=props.Tc$Quest, pos=1, offset=-0.35, cex=0.7, col="black")

  } #panel        

) #xyplot



#combined figure

pdf(file="./Plots/Fig_ego_raw_props_cards.pdf", 
height=8, width=8)

print(BocAt_plot, split=c(1,1, #position (column, row)
                          2,2), #in grid of cols, rows
                    more=TRUE)
print(BocCard_plot, split=c(1,2,
                          2,2),
                    more=TRUE)
print(AtCard_plot, split=c(2,1,
                          2,2),
                    more=TRUE)
print(TayCard_plot, split=c(2,2,
                          2,2),
                    more=TRUE)
print( grid.text(label="A", x = unit(0.12, "npc"), y = unit(0.9, "npc"),
             gp=gpar(fontsize=20, col="black")) )
print( grid.text(label="B", x = unit(0.62, "npc"), y = unit(0.9, "npc"),
             gp=gpar(fontsize=20, col="black")) )
print( grid.text(label="C", x = unit(0.12, "npc"), y = unit(0.4, "npc"),
             gp=gpar(fontsize=20, col="black")) )
print( grid.text(label="D", x = unit(0.62, "npc"), y = unit(0.4, "npc"),
             gp=gpar(fontsize=20, col="black")) )

graphics.off()

