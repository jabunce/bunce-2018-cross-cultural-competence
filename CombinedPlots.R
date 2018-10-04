

######### Combined plots using results of m4, m11, and m19


################### mean KL divergence for in-group and out-group guesses

#mean ego responses

mean_ego_list <- list(mean_probs.e.ma,  	#m4
                      mean_probs.e.me,

                      mean_probs.e.ma.ed, 	#m11
                      mean_probs.e.ma.lab,
                      mean_probs.e.ma.com,
                      mean_probs.e.ma.no,

                      mean_probs.e.me.cty, 	#m19
                      mean_probs.e.me.emp,
                      mean_probs.e.me.fam,
                      mean_probs.e.me.no)


names(mean_ego_list) <- c("Matsigenka",
                          "Mestizos",
                          "Matsi w/ edu",
                          "Matsi w/ lab",
                          "Matsi w/ com",
                          "Matsi w/ none",
                          "Mest w/ cty",
                          "Mest w/ emp",
                          "Mest w/ fam",
                          "Mest w/ none")

#str(mean_ego_list)


mean_kl_out_list <- list(mean_kl_mach_out,
                            mean_kl_mest_out,

                            mean_kl_mach_ed_out,
                            mean_kl_mach_lab_out,
                            mean_kl_mach_com_out,
                            mean_kl_mach_no_out,
                            
                            mean_kl_mest_cty_out,
                            mean_kl_mest_emp_out,
                            mean_kl_mest_fam_out,
                            mean_kl_mest_no_out)

names(mean_kl_out_list) <- c("Matsi",
                                "Mest",
                                "Matsi w/ edu",
                                "Matsi w/ lab",
                                "Matsi w/ com",
                                "Matsi w/ none",
                                "Mest w/ cty",
                                "Mest w/ emp",
                                "Mest w/ fam",
                                "Mest w/ none")

mean_kl_in_list <- list(mean_kl_mach_in,
                            mean_kl_mest_in,

                            mean_kl_mach_ed_in,
                            mean_kl_mach_lab_in,
                            mean_kl_mach_com_in,
                            mean_kl_mach_no_in,
                            
                            mean_kl_mest_cty_in,
                            mean_kl_mest_emp_in,
                            mean_kl_mest_fam_in,
                            mean_kl_mest_no_in)

names(mean_kl_in_list) <- c("Matsi",
                                "Mest",
                                "Matsi w/ edu",
                                "Matsi w/ lab",
                                "Matsi w/ com",
                                "Matsi w/ none",
                                "Mest w/ cty",
                                "Mest w/ emp",
                                "Mest w/ fam",
                                "Mest w/ none")

#str(mean_kl_out_list)
#str(mean_kl_in_list)




#### density plot of innaccuracy contrasts, just m11 and m19
pdf(file="./Plots/mean_guess_kl_m11m19_dens.pdf", 
height=7, width=7)
layout( t(c(1,2,3)), widths=c(1.9,1,1) )
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(mean_ego_list)[1:8],
		yvals=c(1,2,3,4, 6,7,8,9), 
      	labels="",
      	adjust=1,
      	color="black",
        colorHPDI=grey(0.45),
        HPDI=0.9,
        border=NA, yaxt="n",
        cex=0.8, height=0.7,
        xlim=range(-0.4, 0.8),
        clip(0.07,1,0,16), #x1, x2, y1, y2	clip at x=0 
        vline=c(0.35,0.5,0.65) 
 )
text(x=0.5, y=9.85, labels="Ego Responses", cex=1.25)

text( x=rep(-0.3,8), y=c(1,2,3,4, 6,7,8,9), cex=1.5, adj=0, labels=rev(c("Education", "Labor", "Commerce", "None",
																		"Community", "Employer", "Family", "None")) )
axis(side=1, at=c(0.2,0.5,0.8), labels=c(0.2,0.5,0.8), cex.axis=1) #bottom

par(xpd=NA) #set clipping region to entire device
lines(x=list( x=c(-0.75,1.5), y=c(5.25,5.25) ), lty=1, lwd=0.75) #horizontal line
par(xpd=TRUE) #set clipping region to figure region

mtext("Probability", side = 1, outer = TRUE, cex = 0.9, line = 2.2, adj=0.345)



denschart3( rev(mean_kl_out_list)[1:8],
			yvals=c(1,2,3,4, 6,7,8,9), 
      		labels="",
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(0, 1),
          	clip(0.07,1,0,16), #x1, x2, y1, y2	clip at x=0  
          	vline=c(0.25,0.5,0.75)
 )
text(x=0.5, y=9.85, labels="Out-Group Guesses", cex=1.25)

axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=1) #bottom

lines(x=list( x=c(-0.5,1.5), y=c(5.25,5.25) ), lty=1, lwd=0.75) #horizontal line




denschart3( rev(mean_kl_in_list)[1:8],
			yvals=c(1,2,3,4, 6,7,8,9), 
      		labels="",
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(0, 0.3),
          	clip(0.05,1,0,16), #x1, x2, y1, y2	clip at x=0 
          	vline=c(0.0725,0.15,0.2225) 
 )
text(x=0.15, y=9.85, labels="In-Group Guesses", cex=1.25)

axis(side=1, at=c(0,0.15,0.3), labels=c(0,0.15,0.3), cex.axis=1) #bottom


lines(x=list( x=c(-0.5,0.5), y=c(5.25,5.25) ), lty=1, lwd=0.75) #horizontal line

mtext("Inaccuracy", side = 1, outer = TRUE, cex = 0.9, line = 2.2, adj=0.79)
mtext("A. Matsigenka", side = 2, outer = TRUE, cex = 1.25, line = 0, adj=0.3, padj=-19.35, las=1)
mtext("B. Mestizos", side = 2, outer = TRUE, cex = 1.25, line = 0, adj=0.4, padj=0.75, las=1)

graphics.off()




#### density plot of innaccuracy contrasts, just m4
pdf(file="./Plots/mean_guess_kl_m4_dens.pdf", 
height=3, width=7)
layout( t(c(1,2,3)), widths=c(1.9,1,1) )
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(mean_ego_list)[9:10],
		yvals=c(1,2), 
      	labels="",
      	adjust=1,
      	color="black",
        colorHPDI=grey(0.45),
        HPDI=0.9,
        border=NA, yaxt="n",
        cex=0.8, height=0.7,
        xlim=range(-0.4, 0.8),
        clip(0.07,1,0,16), #x1, x2, y1, y2	clip at x=0 
        vline=c(0.35,0.5,0.65) 
 )
text(x=0.5, y=2.75, labels="Ego Responses", cex=1.25)

text( x=rep(-0.3,8), y=c(1,2), cex=1.5, adj=0, labels=rev(c("Matsigenka", "Mestizos")) )
axis(side=1, at=c(0.2,0.5,0.8), labels=c(0.2,0.5,0.8), cex.axis=1) #bottom

mtext("Probability", side = 1, outer = TRUE, cex = 0.9, line = 2.2, adj=0.345)



denschart3( rev(mean_kl_out_list)[9:10],
			yvals=c(1,2), 
      		labels="",
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(0, 1),
          	clip(0.07,1,0,16), #x1, x2, y1, y2	clip at x=0  
          	vline=c(0.25,0.5,0.75)
 )
text(x=0.5, y=2.75, labels="Out-Group Guesses", cex=1.25)

axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=1) #bottom

lines(x=list( x=c(-0.5,1.5), y=c(5.25,5.25) ), lty=1, lwd=0.75) #horizontal line



denschart3( rev(mean_kl_in_list)[9:10],
			yvals=c(1,2), 
      		labels="",
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(0, 0.3),
          	clip(0.05,1,0,16), #x1, x2, y1, y2	clip at x=0 
          	vline=c(0.0725,0.15,0.2225) 
 )
text(x=0.15, y=2.75, labels="In-Group Guesses", cex=1.25)

axis(side=1, at=c(0,0.15,0.3), labels=c(0,0.15,0.3), cex.axis=1) #bottom

mtext("Inaccuracy", side = 1, outer = TRUE, cex = 0.9, line = 2.2, adj=0.79)

graphics.off()






################## Contrasts plots of mean ego responses

#make list for plotting
cont_mean_resp_list <- list(
                  ma.resp.no_ed,   #1
                  ma.resp.no_lab,  #2
                  ma.resp.no_com,  #3
                          
                  ma.resp.ed_lab,  #4
                  ma.resp.ed_com,  #5

                  ma.resp.lab_com, #6

                  me.resp.fam_no,  #7
                  me.resp.emp_no,  #8
                  me.resp.cty_no,  #9
                          
                  me.resp.emp_fam, #10
                  me.resp.cty_fam, #11

                  me.resp.cty_emp  #12
        )

names(cont_mean_resp_list) <- c( "machi no-ed",
                                  "machi no-lab",
                                  "machi no-com",

                                  "machi ed-lab",
                                  "machi ed-com",

                                  "machi lab-com",

                                  "mest fam-no",
                                  "mest emp-no",
                                  "mest cty-no",

                                  "mest emp-fam",
                                  "machi cty-fam",

                                  "mest cty-emp")
#str(cont_mean_resp_list)


cont_list_col1 <- list( cont_mean_resp_list[[1]], #ma.resp.no_ed
            cont_mean_resp_list[[7]], #me.resp.fam_no
            cont_mean_resp_list[[8]], #me.resp.emp_no
            cont_mean_resp_list[[9]]  #me.resp.cty_no
             )
names(cont_list_col1) <- names(cont_mean_resp_list)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
#str(cont_list_col1)


cont_list_col2 <- list( cont_mean_resp_list[[2]], #ma.acc.no_lab
            cont_mean_resp_list[[4]], #ma.resp.ed_lab
            cont_mean_resp_list[[10]], #me.resp.emp_fam
            cont_mean_resp_list[[11]] #me.resp.cty_fam
             )
names(cont_list_col2) <- rownames(cont_mean_resp_list)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
#str(cont_list_col2)


cont_list_col3 <- list( cont_mean_resp_list[[3]], #ma.acc.no_com
            cont_mean_resp_list[[5]], #ma.resp.ed_com
            cont_mean_resp_list[[6]], #ma.resp.lab_com
            cont_mean_resp_list[[12]] #me.resp.cty_emp
             )
names(cont_list_col3) <- rownames(cont_mean_resp_list)[c(3,5,6,12)]
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
pdf(file="./Plots/mean_ego_contrasts.pdf", 
height=5, width=5)
par(xpd=TRUE) #clip at figure region, not plot region
par(oma = c(4, 4, 4, 4)) # oma for outer margins: bottom, left, top, right


########## column 1

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

par(new = TRUE)     #draw on top of previous plot

denschart3( cont_list_col2 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          #xlim=c(-0.25 - 0.5, 0.25 + 1)
          xlim=c(col2.xmin, col2.xmax)
 )
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


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
text(labels="None", x=-2*xrange,   
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
text(labels="None", x=xrange*0.8,  
  y=3.75, las=1, cex=0.75, adj=0.3)

text(labels="Cty", x=-2*xrange-xrange*0.8,   
  y=0.75, las=1, cex=0.75)  #left
text(labels="Emp", x=-2*xrange-xrange*0.8,   
  y=1.75, las=1, cex=0.75)
text(labels="Fam", x=-2*xrange-xrange*0.8,   
  y=2.75, las=1, cex=0.75)

#text(labels="Mestizos", x=-37, y=2.25, las=3, cex=1, srt=90) #left
mtext("Mestizos", side=2, line=3.5, cex=1, adj=0.3)
text(labels="Matsigenka", x=1.5*xrange,  
  y=2.5, cex=1, adj=0.65, srt=270) #right
text(labels="Matsigenka", x=-1*xrange,  
  y=5, cex=1)  #top
text(labels="Mestizos", x=-1*xrange,  
  y=-0.9, cex=1) #bottom

graphics.off()





################## Jeffreys divergence plots of mean ego responses

#make list for plotting
jef_mean_resp_list <- list(
                  mean.ma.jef.no_ed,   #1
                  mean.ma.jef.no_lab,  #2
                  mean.ma.jef.no_com,  #3
                          
                  mean.ma.jef.ed_lab,  #4
                  mean.ma.jef.ed_com,  #5

                  mean.ma.jef.lab_com, #6

                  mean.me.jef.no_fam,  #7
                  mean.me.jef.no_emp,  #8
                  mean.me.jef.no_cty,  #9
                          
                  mean.me.jef.fam_emp, #10
                  mean.me.jef.fam_cty, #11

                  mean.me.jef.emp_cty  #12
        )

names(jef_mean_resp_list) <- c( "machi no-ed",
                                  "machi no-lab",
                                  "machi no-com",

                                  "machi ed-lab",
                                  "machi ed-com",

                                  "machi lab-com",

                                  "mest fam-no",
                                  "mest emp-no",
                                  "mest cty-no",

                                  "mest emp-fam",
                                  "machi cty-fam",

                                  "mest cty-emp")
#str(jef_mean_resp_list)


cont_list_col1 <- list( jef_mean_resp_list[[1]], #ma.resp.no_ed
            jef_mean_resp_list[[7]], #me.resp.fam_no
            jef_mean_resp_list[[8]], #me.resp.emp_no
            jef_mean_resp_list[[9]]  #me.resp.cty_no
             )
names(cont_list_col1) <- names(jef_mean_resp_list)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
#str(cont_list_col1)


cont_list_col2 <- list( jef_mean_resp_list[[2]], #ma.acc.no_lab
            jef_mean_resp_list[[4]], #ma.resp.ed_lab
            jef_mean_resp_list[[10]], #me.resp.emp_fam
            jef_mean_resp_list[[11]] #me.resp.cty_fam
             )
names(cont_list_col2) <- rownames(jef_mean_resp_list)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
#str(cont_list_col2)


cont_list_col3 <- list( jef_mean_resp_list[[3]], #ma.acc.no_com
            jef_mean_resp_list[[5]], #ma.resp.ed_com
            jef_mean_resp_list[[6]], #ma.resp.lab_com
            jef_mean_resp_list[[12]] #me.resp.cty_emp
             )
names(cont_list_col3) <- rownames(jef_mean_resp_list)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
#str(cont_list_col3)



xmin <- -0.05
xmax <- 0.25
xrange <- abs(xmin-xmax)

col1.xmax <- xmax + 3*xrange
col1.xmin <- xmin - 0*xrange

col2.xmax <- xmax + 2*xrange
col2.xmin <- xmin - 1*xrange

col3.xmax <- xmax + 1*xrange
col3.xmin <- xmin - 2*xrange



#### density plot of contrasts
pdf(file="./Plots/mean_ego_jef_contrasts.pdf", 
height=5, width=5)
par(xpd=TRUE) #clip at figure region, not plot region
par(oma = c(4, 4, 4, 4)) # oma for outer margins: bottom, left, top, right


########## column 1

denschart3( cont_list_col1 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          xlim=c(col1.xmin, col1.xmax),
          clip(xmin+0.01,xmax-0.065,0,16) #x1, x2, y1, y2	clip at x=0 
 )
axis(side=1, at=c(0,0.5*xmax), labels=c(0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


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
          xlim=c(col2.xmin, col2.xmax),
          clip(xmax-0.02,(2*xmax)-0.045,0,16) #x1, x2, y1, y2	clip at x=0 
 )
axis(side=1, at=c(0,0.5*xmax), labels=c(0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


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
          clip(2*xmax,3*xmax-0.026,0,16) #x1, x2, y1, y2	clip at x=0 
 )
axis(side=1, at=c(0,0.5*xmax), labels=c(0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)

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

#bottom
text(labels="Emp", x=xrange/2-abs(xmin),
  y=-0.4, las=1, cex=0.75)
text(labels="Fam", x=-1*xrange+xrange/2-abs(xmin),   
  y=-0.4, las=1, cex=0.75)
text(labels="None", x=-2*xrange+xrange/2-abs(xmin),  
  y=-0.4, las=1, cex=0.75)

#top
text(labels="Com", x=xrange/2-abs(xmin),
  y=4.5, las=1, cex=0.75)   
text(labels="Lab", x=-1*xrange+xrange/2-abs(xmin), 
  y=4.5, las=1, cex=0.75)
text(labels="Edu", x=-2*xrange+xrange/2-abs(xmin),  
  y=4.5, las=1, cex=0.75)

#right
text(labels="Lab", x=xrange*0.8+xrange/2-abs(xmin),  
  y=1.75, las=1, cex=0.75)  
text(labels="Edu", x=xrange*0.8+xrange/2-abs(xmin), 
 y=2.75, las=1, cex=0.75)
text(labels="None", x=xrange*0.8+xrange/2-abs(xmin), 
  y=3.75, las=1, cex=0.75, adj=0.3)

#left
text(labels="Cty", x=-2*xrange-xrange*0.8+xrange/2-abs(xmin),   
  y=0.75, las=1, cex=0.75)  
text(labels="Emp", x=-2*xrange-xrange*0.8+xrange/2-abs(xmin), 
  y=1.75, las=1, cex=0.75)
text(labels="Fam", x=-2*xrange-xrange*0.8+xrange/2-abs(xmin),  
  y=2.75, las=1, cex=0.75)


mtext("Mestizos", side=2, line=3.5, cex=1, adj=0.3)
text(labels="Matsigenka", x=1.5*xrange+xrange/2-abs(xmin), 
  y=2.5, cex=1, adj=0.65, srt=270) #right
text(labels="Matsigenka", x=-1*xrange+xrange/2-abs(xmin), 
  y=5, cex=1)  #top
text(labels="Mestizos", x=-1*xrange+xrange/2-abs(xmin), 
  y=-0.9, cex=1) #bottom

graphics.off()






################### plot of residual covariance of abilities across targets after removing effects of indiv-level predictors

######### machi predictors

post0 <- post11 
#str(post0)

#make list for plotting
cov_list11 <- list(
                   post0$Cov_a[,1,1,1], #machi ego variance. indices: Machi, T-row, T-column
                   post0$Cov_a[,1,2,2], #machi in variance
                   post0$Cov_a[,1,3,3], #machi out variance
                   post0$Cov_a[,1,1,2], #machi ego-in covariance
                   post0$Cov_a[,1,1,3], #machi ego-out covariance
                   post0$Cov_a[,1,2,3] #machi in-out covariance
        )

names(cov_list11) <- c(
            "Mats ego var",
            "Mats in var",
            "Mats out var",
            "Mats ego-in cov",
            "Mats ego-out cov",
            "Mats in-out cov"
          )


#str(cov_list11)
cov_list11 <- rev(cov_list11) #reverse order for plotting
#str(cov_list11)





######### mestizo predictors

post0 <- post19 
#str(post0)

#make list for plotting
cov_list19 <- list(
                   post0$Cov_a[,2,1,1], #mest ego variance. indices: Machi, T-row, T-column
                   post0$Cov_a[,2,2,2], #mest in variance
                   post0$Cov_a[,2,3,3], #mest out variance
                   post0$Cov_a[,2,1,2], #mest ego-in covariance
                   post0$Cov_a[,2,1,3], #mest ego-out covariance
                   post0$Cov_a[,2,2,3] #mest in-out covariance
        )

names(cov_list19) <- c(
            "Mest ego var",
            "Mest in var",
            "Mest out var",
            "Mest ego-in cov",
            "Mest ego-out cov",
            "Mest in-out cov"
          )


#str(cov_list19)
cov_list19 <- rev(cov_list19) #reverse order for plotting
#str(cov_list19)




cov_names <- c("Ego var", "In-group var", "Out-group var",
              "Ego-In cov", "Ego-Out cov", "In-Out cov")
cov_names <- rev(cov_names)


#### density plot of mean guess inaccuracy
pdf(file="./Plots/var_cov_dens.pdf", 
height=7, width=7)
#par(mfrow=c(1,2))
layout( t(c(1,2)), widths=c(1.7,1) )
par(mar = c(0, 0, 0, 0), oma = c(6, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( cov_list11,
	  		yextra=1.5,
      		labels="",
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(-2, 2) 
 )
text(x=0, y=7, labels="Matsi", cex=1)

text( x=rep(-0.75,6), y=c(1:6), labels=cov_names, cex=1, adj=1 )
axis(side=1, at=c(-0.5,0,1,2), labels=c(-0.5,0,1,2), cex.axis=0.75) #bottom

lines(x=list( x=c(0,0), y=c(0,6.7) ), lty=2, lwd=1.5) #vertical line

rect(xleft=-0.7, ybottom=3.5, xright=-0.011, ytop=6.5, col="white", border=NA) #variance can't be negative


denschart3( cov_list19,
			yextra=1.5,
      		labels="",
      		adjust=1,
      		color="black",
          	colorHPDI=grey(0.45),
          	HPDI=0.9,
          	border=NA, yaxt="n",
          	cex=0.8, height=0.7,
          	xlim=range(-0.05, 0.2) 
 )
text(x=0, y=7, labels="Mest", cex=1)

#text( x=rep(-0.5,10), y=c(1:6), labels=cov_names, cex=1, adj=0 )
axis(side=1, at=c(-0.05,0,0.1,0.2), labels=c(-0.05,0,0.1,0.2), cex.axis=0.75) #bottom

lines(x=list( x=c(0,0), y=c(0,6.7) ), lty=2, lwd=1.5) #vertical line

mtext("Variance or covariance", side = 1, outer = TRUE, cex = 1, line = 2.8, adj=0.7)

rect(xleft=-0.07, ybottom=3.5, xright=-0.0011, ytop=6.5, col="white", border=NA) #variance can't be negative

graphics.off()





################# combine kl contrast matrices

#m11

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
pdf(file="./Plots/combined_kl_exp_contrasts.pdf", 
height=10, width=5)
par(xpd=TRUE) #clip at figure region, not plot region
layout( matrix(c(1,0,2)), heights=c(1,lcm(1),1) ) #pg 80 Murrell
par(oma = c(3, 4, 2.5, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( cont_list_col1 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          xlim=c(col1.xmin, col1.xmax)
 )
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.75, tcl=-0.3, padj=-2)

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
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.75, tcl=-0.3, padj=-2)


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
          clip(xmin,0.72,0,16) #x1, x2, y1, y2	clip at x=0
 )
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.75, tcl=-0.3, padj=-2)

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
  y=-0.4, las=1, cex=1)  #bottom
text(labels="Edu", x=-1*xrange,   
  y=-0.4, las=1, cex=1)
text(labels="None", x=-2*xrange,   
  y=-0.4, las=1, cex=1)

text(labels="Com", x=0,
  y=4.5, las=1, cex=1)   #top
text(labels="Lab", x=-1*xrange, 
  y=4.5, las=1, cex=1)
text(labels="Edu", x=-2*xrange, 
  y=4.5, las=1, cex=1)

text(labels="Lab", x=xrange*0.8,  
  y=1.75, las=1, cex=1)  #right
text(labels="Edu", x=xrange*0.8,  
 y=2.75, las=1, cex=1)
text(labels="None", x=xrange*0.8, 
  y=3.75, las=1, cex=1, adj=0.3)

text(labels="Com", x=-2*xrange-xrange*0.8,   
  y=0.75, las=1, cex=1)  #left
text(labels="Lab", x=-2*xrange-xrange*0.8,   
  y=1.75, las=1, cex=1)
text(labels="Edu", x=-2*xrange-xrange*0.8,   
  y=2.75, las=1, cex=1)


mtext("In-Group Guesses", side=2, line=3.5, cex=1, adj=0.25) #adj=0.015
 #left  
text(labels="Out-Group Guesses", x=1.5*xrange-0.2,  
  y=2.5, cex=1.5, adj=0.6, srt=270) #right  
text(labels="Out-Group Guesses", x=-1*xrange,  
  y=4.9, cex=1.5)  #top
text(labels="In-Group Guesses", x=-1*xrange,  
  y=-0.8, cex=1.5) #bottom


mtext("A. Matsigenka", side=3, line=4, cex=1.5, adj=-0.45) #side: 1=bottom, 2=left, 3=top, 4=right

#graphics.off()


#########################


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

par(new = FALSE)     #new plot

denschart3( cont_list_col1 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          xlim=c(col1.xmin, col1.xmax)
 )
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.75, tcl=-0.3, padj=-2)

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
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.75, tcl=-0.3, padj=-2)


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
axis(side=1, at=c(0.5*xmin,0,0.5*xmax), col="white", col.ticks="black", cex.axis=0.75, tcl=-0.3, padj=-2)


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
  y=-0.4, las=1, cex=1)  #bottom
text(labels="Fam", x=-1*xrange,   
  y=-0.4, las=1, cex=1)
text(labels="None", x=-2*xrange,   
  y=-0.4, las=1, cex=1)

text(labels="Cty", x=0,
  y=4.5, las=1, cex=1)   #top
text(labels="Emp", x=-1*xrange, 
  y=4.5, las=1, cex=1)
text(labels="Fam", x=-2*xrange,  
  y=4.5, las=1, cex=1)

text(labels="Emp", x=xrange*0.8,  
  y=1.75, las=1, cex=1)  #right
text(labels="Fam", x=xrange*0.8,  
 y=2.75, las=1, cex=1)
text(labels="None", x=xrange*0.8,  
  y=3.75, las=1, cex=1, adj=0.3)

text(labels="Cty", x=-2*xrange-xrange*0.8,   
  y=0.75, las=1, cex=1)  #left
text(labels="Emp", x=-2*xrange-xrange*0.8,   
  y=1.75, las=1, cex=1)
text(labels="Fam", x=-2*xrange-xrange*0.8,   
  y=2.75, las=1, cex=1)

#text(labels="Mestizos", x=-37, y=2.25, las=3, cex=1, srt=90) #left
mtext("In-Group Guesses", side=2, line=3.5, cex=1, adj=0.25) 
text(labels="Out-Group Guesses", x=1.5*xrange-0.1,  
  y=2.5, cex=1.5, adj=0.6, srt=270) #right  
text(labels="Out-Group Guesses", x=-1*xrange,  
  y=4.9, cex=1.5)  #top
text(labels="In-Group Guesses", x=-1*xrange,  
  y=-0.8, cex=1.5) #bottom


mtext("B. Mestizos", side=3, line=4, cex=1.5, adj=-0.4) #side: 1=bottom, 2=left, 3=top, 4=right

graphics.off()

