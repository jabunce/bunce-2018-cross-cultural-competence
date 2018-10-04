
####################### WAIC model comparison

model_waics_mach <- c(waic(m1)$waic,    
                      #waic(m2)$waic, 	#exclude m2 because it doesn't fit well
                      waic(m3)$waic,
                      waic(m4)$waic,
                      waic(m5)$waic,
                      waic(m6)$waic,
                      waic(m7)$waic,
                      waic(m8)$waic,
                      waic(m9)$waic,
                      waic(m10)$waic,
                      waic(m11)$waic,
                      waic(m12)$waic)

model_waics_mest <- c(waic(m1)$waic,
                      #waic(m2)$waic,
                      waic(m3)$waic,
                      waic(m4)$waic,
                      waic(m13)$waic,
                      waic(m14)$waic,
                      waic(m15)$waic,
                      waic(m16)$waic,
                      waic(m17)$waic,
                      waic(m18)$waic,
                      waic(m19)$waic,
                      waic(m20)$waic)

#effective number of parameters
model_pwaics_mach <- c(waic(m1)$p_waic, 
                        #waic(m2)$p_waic,
                        waic(m3)$p_waic,
                        waic(m4)$p_waic,
                        waic(m5)$p_waic,
                        waic(m6)$p_waic,
                        waic(m7)$p_waic,
                        waic(m8)$p_waic,
                        waic(m9)$p_waic,
                        waic(m10)$p_waic,
                        waic(m11)$p_waic,
                        waic(m12)$p_waic)

model_pwaics_mest <- c(waic(m1)$p_waic, #effective number of parameters
                        #waic(m2)$p_waic,
                        waic(m3)$p_waic,
                        waic(m4)$p_waic,
                        waic(m13)$p_waic,
                        waic(m14)$p_waic,
                        waic(m15)$p_waic,
                        waic(m16)$p_waic,
                        waic(m17)$p_waic,
                        waic(m18)$p_waic,
                        waic(m19)$p_waic,
                        waic(m20)$p_waic)


#compare(m1, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12) #last column gives standard error of the difference in waic b/t models
#x_mach <- compare(m1, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
#str(x_mach)
#x_mach@dSE #gives matrix of differences in waic between all models
#x_mach #last column gives standard error of differences from the best model


#compare(m1, m3, m4, m13, m14, m15, m16, m17, m18, m19, m20) #last column gives standard error of the difference in waic b/t models
#x_mest <- compare(m1, m3, m4, m13, m14, m15, m16, m17, m18, m19, m20)
#str(x_mest)
#x_mest@dSE #gives matrix of differences in waic between all models
#x_mest




waic_diffs_mach <- rep(0, length(model_waics_mach))
for ( i in 1:length(model_waics_mach) ) {
  waic_diffs_mach[i] <- model_waics_mach[i] - min(model_waics_mach) 
} #for i

waic_diffs_mest <- rep(0, length(model_waics_mest))
for ( i in 1:length(model_waics_mest) ) {
  waic_diffs_mest[i] <- model_waics_mest[i] - min(model_waics_mest) 
} #for i




model_weights_mach <- rep(0, length(model_waics_mach))
for ( j in 1:length(model_waics_mach) ) {
  model_weights_mach[j] <- exp(-0.5*waic_diffs_mach[j])/sum(exp(-0.5*waic_diffs_mach)) #McElreath rethinking pg199
} #for j

model_weights_mest <- rep(0, length(model_waics_mest))
for ( j in 1:length(model_waics_mest) ) {
  model_weights_mest[j] <- exp(-0.5*waic_diffs_mest[j])/sum(exp(-0.5*waic_diffs_mest)) #McElreath rethinking pg199
} #for j



WAIC_sum_mach <- cbind(model_waics_mach, model_pwaics_mach, waic_diffs_mach, model_weights_mach)
colnames(WAIC_sum_mach) <- c("WAIC", "pWAIC", "dWAIC", "weight")
rownames(WAIC_sum_mach) <- c("m1", "m3", "m4", "m5",
                              "m6", "m7", "m8", "m9", "m10",
                              "m11", "m12")

WAIC_sum_mest <- cbind(model_waics_mest, model_pwaics_mest, waic_diffs_mest, model_weights_mest)
colnames(WAIC_sum_mest) <- c("WAIC", "pWAIC", "dWAIC", "weight")
rownames(WAIC_sum_mest) <- c("m1", "m3", "m4", "m13",
                              "m14", "m15", "m16", "m17", "m18",
                              "m19", "m20")


WAIC_sum_mach <- WAIC_sum_mach[order(WAIC_sum_mach[,1]),]
#print(WAIC_sum_mach, digits=2)

WAIC_sum_mest <- WAIC_sum_mest[order(WAIC_sum_mest[,1]),]
#print(WAIC_sum_mest, digits=2)




#### Latex tables of model outputs

digs <- 2 #number of rounding digits
digs2 <- 2 #rounding digits for waic weights
cred <- 0.90 #HPDI
options(digits=10)


#Machi separate axes
cnames_mach <- c("Model", "Intercept", "Sex", "Adol", "Elder",
                                      "Educ", "Labor", "Comm",
                                                             "WAIC weight")
rnames_mach <- c(
                      "m1", " ", " ",
                      "m3", " ", " ",
                      "m4", " ", " ",    " ", " ", " ",
                      "m5", " ", " ",    " ", " ", " ",
                      "m6", " ", " ",    " ", " ", " ",
                      "m7", " ", " ",    " ", " ", " ",
                      "m8", " ", " ",    " ", " ", " ",
                      "m9", " ", " ",    " ", " ", " ",
                      "m10", " ", " ",    " ", " ", " ",
                      "m11", " ", " ",    " ", " ", " ",
                      "m12", " ", " ",    " ", " ", " "
                  )


Int_col <- c(

  #Intercept
  paste( round(mean(post1$a0[,1]), digs), " (",                                 #m1
         round(as.vector(HPDI(post1$a0[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post1$a0[,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post1$a0[,2]), digs), " (",                                 #m1
         round(as.vector(HPDI(post1$a0[,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post1$a0[,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post1$a0[,3]), digs), " (",                                 #m1
         round(as.vector(HPDI(post1$a0[,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post1$a0[,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post3$muInt[,1]), digs), " (",                                 #m3
         round(as.vector(HPDI(post3$muInt[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post3$muInt[,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post3$muInt[,1]), digs), " (",                                 #m3
         round(as.vector(HPDI(post3$muInt[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post3$muInt[,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post3$muInt[,1]), digs), " (",                                 #m3
         round(as.vector(HPDI(post3$muInt[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post3$muInt[,1], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post4$muInt[,1,1]), digs), " (",                                 #m4
         round(as.vector(HPDI(post4$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post4$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post4$muInt[,1,2]), digs), " (",                                 #m4
         round(as.vector(HPDI(post4$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post4$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post4$muInt[,1,3]), digs), " (",                                 #m4
         round(as.vector(HPDI(post4$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post4$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post4$muInt[,2,1]), digs), " (",                                 #m4
         round(as.vector(HPDI(post4$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post4$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post4$muInt[,2,2]), digs), " (",                                 #m4
         round(as.vector(HPDI(post4$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post4$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post4$muInt[,2,3]), digs), " (",                                 #m4
         round(as.vector(HPDI(post4$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post4$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post5$muInt[,1,1]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$muInt[,1,2]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$muInt[,1,3]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$muInt[,2,1]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$muInt[,2,2]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$muInt[,2,3]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post6$muInt[,1,1]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$muInt[,1,2]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$muInt[,1,3]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$muInt[,2,1]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$muInt[,2,2]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$muInt[,2,3]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post7$muInt[,1,1]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$muInt[,1,2]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$muInt[,1,3]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$muInt[,2,1]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$muInt[,2,2]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$muInt[,2,3]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post8$muInt[,1,1]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$muInt[,1,2]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$muInt[,1,3]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$muInt[,2,1]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$muInt[,2,2]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$muInt[,2,3]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post9$muInt[,1,1]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$muInt[,1,2]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$muInt[,1,3]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$muInt[,2,1]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$muInt[,2,2]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$muInt[,2,3]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post10$muInt[,1,1]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$muInt[,1,2]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$muInt[,1,3]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$muInt[,2,1]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$muInt[,2,2]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$muInt[,2,3]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post11$muInt[,1,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$muInt[,1,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$muInt[,1,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$muInt[,2,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$muInt[,2,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$muInt[,2,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$muInt[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post12$muInt[,1,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$muInt[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$muInt[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$muInt[,1,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$muInt[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$muInt[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$muInt[,1,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$muInt[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$muInt[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$muInt[,2,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$muInt[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$muInt[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$muInt[,2,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$muInt[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$muInt[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$muInt[,2,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$muInt[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$muInt[,2,3], prob=cred))[2], digs), ")", sep="")
)

sex_col <- c(

  #Sex
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  " ", " ", " ",    " ", " ", " ",   #m5
  " ", " ", " ",    " ", " ", " ",   #m6
  " ", " ", " ",    " ", " ", " ",   #m7
  " ", " ", " ",    " ", " ", " ",   #m8
  " ", " ", " ",    " ", " ", " ",   #m9
  " ", " ", " ",    " ", " ", " ",  #m10
  " ", " ", " ",    " ", " ", " ",  #m11
  paste( round(mean(post12$aMale[,1,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aMale[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aMale[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aMale[,1,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aMale[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aMale[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aMale[,1,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aMale[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aMale[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aMale[,2,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aMale[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aMale[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aMale[,2,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aMale[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aMale[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aMale[,2,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aMale[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aMale[,2,3], prob=cred))[2], digs), ")", sep="")
)

adol_col <- c(

#adolescent
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  " ", " ", " ",    " ", " ", " ",   #m5
  " ", " ", " ",    " ", " ", " ",   #m6
  " ", " ", " ",    " ", " ", " ",   #m7
  " ", " ", " ",    " ", " ", " ",   #m8
  " ", " ", " ",    " ", " ", " ",   #m9
  " ", " ", " ",    " ", " ", " ",  #m10
  " ", " ", " ",    " ", " ", " ",  #m11
  paste( round(mean(post12$aAdol[,1,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aAdol[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aAdol[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aAdol[,1,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aAdol[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aAdol[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aAdol[,1,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aAdol[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aAdol[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aAdol[,2,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aAdol[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aAdol[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aAdol[,2,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aAdol[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aAdol[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aAdol[,2,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aAdol[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aAdol[,2,3], prob=cred))[2], digs), ")", sep="")
)

eld_col <- c(

#elder
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  " ", " ", " ",    " ", " ", " ",   #m5
  " ", " ", " ",    " ", " ", " ",   #m6
  " ", " ", " ",    " ", " ", " ",   #m7
  " ", " ", " ",    " ", " ", " ",   #m8
  " ", " ", " ",    " ", " ", " ",   #m9
  " ", " ", " ",    " ", " ", " ",  #m10
  " ", " ", " ",    " ", " ", " ",  #m11
  paste( round(mean(post12$aOld[,1,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aOld[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aOld[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aOld[,1,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aOld[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aOld[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aOld[,1,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aOld[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aOld[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aOld[,2,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aOld[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aOld[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aOld[,2,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aOld[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aOld[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aOld[,2,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aOld[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aOld[,2,3], prob=cred))[2], digs), ")", sep="")
)


edu_col <- c(

#education
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  paste( round(mean(post5$aEdMes[,1,1]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$aEdMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$aEdMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$aEdMes[,1,2]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$aEdMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$aEdMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$aEdMes[,1,3]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$aEdMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$aEdMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$aEdMes[,2,1]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$aEdMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$aEdMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$aEdMes[,2,2]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$aEdMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$aEdMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post5$aEdMes[,2,3]), digs), " (",                                 #m5
         round(as.vector(HPDI(post5$aEdMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post5$aEdMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m6
  " ", " ", " ",    " ", " ", " ",   #m7
  paste( round(mean(post8$aEdMes[,1,1]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aEdMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aEdMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aEdMes[,1,2]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aEdMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aEdMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aEdMes[,1,3]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aEdMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aEdMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aEdMes[,2,1]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aEdMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aEdMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aEdMes[,2,2]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aEdMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aEdMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aEdMes[,2,3]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aEdMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aEdMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post9$aEdMes[,1,1]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aEdMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aEdMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aEdMes[,1,2]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aEdMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aEdMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aEdMes[,1,3]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aEdMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aEdMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aEdMes[,2,1]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aEdMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aEdMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aEdMes[,2,2]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aEdMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aEdMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aEdMes[,2,3]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aEdMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aEdMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",  #m10

  paste( round(mean(post11$aEdMes[,1,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aEdMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aEdMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aEdMes[,1,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aEdMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aEdMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aEdMes[,1,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aEdMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aEdMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aEdMes[,2,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aEdMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aEdMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aEdMes[,2,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aEdMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aEdMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aEdMes[,2,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aEdMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aEdMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post12$aEdMes[,1,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aEdMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aEdMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aEdMes[,1,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aEdMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aEdMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aEdMes[,1,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aEdMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aEdMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aEdMes[,2,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aEdMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aEdMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aEdMes[,2,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aEdMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aEdMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aEdMes[,2,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aEdMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aEdMes[,2,3], prob=cred))[2], digs), ")", sep="")
)


lab_col <- c(

#labor
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  " ", " ", " ",    " ", " ", " ",   #m5

  paste( round(mean(post6$aLabMes[,1,1]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$aLabMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$aLabMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$aLabMes[,1,2]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$aLabMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$aLabMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$aLabMes[,1,3]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$aLabMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$aLabMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$aLabMes[,2,1]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$aLabMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$aLabMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$aLabMes[,2,2]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$aLabMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$aLabMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post6$aLabMes[,2,3]), digs), " (",                                 #m6
         round(as.vector(HPDI(post6$aLabMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post6$aLabMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m7

  paste( round(mean(post8$aLabMes[,1,1]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aLabMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aLabMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aLabMes[,1,2]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aLabMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aLabMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aLabMes[,1,3]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aLabMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aLabMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aLabMes[,2,1]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aLabMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aLabMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aLabMes[,2,2]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aLabMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aLabMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post8$aLabMes[,2,3]), digs), " (",                                 #m8
         round(as.vector(HPDI(post8$aLabMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post8$aLabMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m9

  paste( round(mean(post10$aLabMes[,1,1]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aLabMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aLabMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aLabMes[,1,2]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aLabMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aLabMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aLabMes[,1,3]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aLabMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aLabMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aLabMes[,2,1]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aLabMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aLabMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aLabMes[,2,2]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aLabMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aLabMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aLabMes[,2,3]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aLabMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aLabMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post11$aLabMes[,1,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aLabMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aLabMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aLabMes[,1,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aLabMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aLabMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aLabMes[,1,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aLabMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aLabMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aLabMes[,2,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aLabMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aLabMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aLabMes[,2,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aLabMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aLabMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aLabMes[,2,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aLabMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aLabMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post12$aLabMes[,1,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aLabMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aLabMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aLabMes[,1,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aLabMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aLabMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aLabMes[,1,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aLabMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aLabMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aLabMes[,2,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aLabMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aLabMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aLabMes[,2,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aLabMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aLabMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aLabMes[,2,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aLabMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aLabMes[,2,3], prob=cred))[2], digs), ")", sep="")
)


com_col <- c(

  #commerce
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  " ", " ", " ",    " ", " ", " ",   #m5
  " ", " ", " ",    " ", " ", " ",   #m6

  paste( round(mean(post7$aComMes[,1,1]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$aComMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$aComMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$aComMes[,1,2]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$aComMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$aComMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$aComMes[,1,3]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$aComMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$aComMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$aComMes[,2,1]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$aComMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$aComMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$aComMes[,2,2]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$aComMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$aComMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post7$aComMes[,2,3]), digs), " (",                                 #m7
         round(as.vector(HPDI(post7$aComMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post7$aComMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m8

  paste( round(mean(post9$aComMes[,1,1]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aComMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aComMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aComMes[,1,2]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aComMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aComMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aComMes[,1,3]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aComMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aComMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aComMes[,2,1]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aComMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aComMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aComMes[,2,2]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aComMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aComMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post9$aComMes[,2,3]), digs), " (",                                 #m9
         round(as.vector(HPDI(post9$aComMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post9$aComMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post10$aComMes[,1,1]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aComMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aComMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aComMes[,1,2]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aComMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aComMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aComMes[,1,3]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aComMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aComMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aComMes[,2,1]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aComMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aComMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aComMes[,2,2]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aComMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aComMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post10$aComMes[,2,3]), digs), " (",                                 #m10
         round(as.vector(HPDI(post10$aComMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post10$aComMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post11$aComMes[,1,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aComMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aComMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aComMes[,1,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aComMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aComMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aComMes[,1,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aComMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aComMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aComMes[,2,1]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aComMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aComMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aComMes[,2,2]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aComMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aComMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post11$aComMes[,2,3]), digs), " (",                                 #m11
         round(as.vector(HPDI(post11$aComMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post11$aComMes[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(post12$aComMes[,1,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aComMes[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aComMes[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aComMes[,1,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aComMes[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aComMes[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aComMes[,1,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aComMes[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aComMes[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aComMes[,2,1]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aComMes[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aComMes[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aComMes[,2,2]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aComMes[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aComMes[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(post12$aComMes[,2,3]), digs), " (",                                 #m12
         round(as.vector(HPDI(post12$aComMes[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(post12$aComMes[,2,3], prob=cred))[2], digs), ")", sep="")
)


waic_col <- c(

  #WAIC weight
  signif(model_weights_mach[1], digs2),     #m1    scientific digits notation
  " ", " ",
  signif(model_weights_mach[2], digs2),     #m3
  " ", " ",
  signif(model_weights_mach[3], digs2),     #m4
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[4], digs2),     #m5
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[5], digs2),     #m6
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[6], digs2),     #m7
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[7], digs2),     #m8
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[8], digs2),     #m9
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[9], digs2),     #m10
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[10], digs2),     #m11
  " ", " ",   " ", " ", " ",
  signif(model_weights_mach[11], digs2),     #m12
  " ", " ",   " ", " ", " "
)


tab_mat_mach <- cbind(rnames_mach, Int_col,
                      sex_col, adol_col, eld_col,
                      edu_col, lab_col, com_col,
                                                waic_col)

#dim(tab_mat_mach)

colnames(tab_mat_mach) <- cnames_mach

#tab_mat_mach[1:20,1:9]

temp <- capture.output( stargazer(tab_mat_mach, summary=FALSE, rownames=FALSE, type="latex",
                          out="./Plots/table_mach.tex")
                      ) #prevents table from being printed to console



######################################################################################################################

#Mestizo separate axes
cnames_mest <- c("Model", "Intercept", "Sex", "Adol", "Elder",
                                      "Fam", "Emp", "Cty",
                                                             "WAIC weight")
rnames_mest <- c(
                      "m1", " ", " ",
                      "m3", " ", " ",
                      "m4", " ", " ",    " ", " ", " ",
                      "m13", " ", " ",    " ", " ", " ",
                      "m14", " ", " ",    " ", " ", " ",
                      "m15", " ", " ",    " ", " ", " ",
                      "m16", " ", " ",    " ", " ", " ",
                      "m18", " ", " ",    " ", " ", " ",    #note m17 and m18 switched
                      "m17", " ", " ",    " ", " ", " ",
                      "m19", " ", " ",    " ", " ", " ",
                      "m20", " ", " ",    " ", " ", " "
                  )

#Intercept
col.1.1 <- post1$a0
col.1.2 <- post3$muInt
col.1.3 <- post4$muInt
col.1.4 <- post13$muInt
col.1.5 <- post14$muInt
col.1.6 <- post15$muInt
col.1.7 <- post16$muInt
col.1.8 <- post18$muInt       #note m17 and m18 switched
col.1.9 <- post17$muInt
col.1.10 <- post19$muInt
col.1.11 <- post20$muInt

#Sex
col.2.1 <- post20$aMale

#Adolescent
col.3.1 <- post20$aAdol

#Old
col.4.1 <- post20$aOld

#Family
col.5.1 <- post13$aFamMat
col.5.2 <- post16$aFamMat
col.5.3 <- post18$aFamMat   #note m17 and m18 switched    
col.5.4 <- post19$aFamMat
col.5.5 <- post20$aFamMat

#Employment
col.6.1 <- post14$aEmpMat
col.6.2 <- post16$aEmpMat
col.6.3 <- post17$aEmpMat   #note m17 and m18 switched
col.6.4 <- post19$aEmpMat
col.6.5 <- post20$aEmpMat

#Community
col.7.1 <- post15$aCtyMat
col.7.2 <- post18$aCtyMat     #note m17 and m18 switched
col.7.3 <- post17$aCtyMat
col.7.4 <- post19$aCtyMat
col.7.5 <- post20$aCtyMat

#WAIC weights
weights <- model_weights_mest



col1 <- c(

  #Intercept
  paste( round(mean(col.1.1[,1]), digs), " (",                                 #m1
         round(as.vector(HPDI(col.1.1[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.1[,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.1[,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.1[,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.1[,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.1[,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.1[,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.1[,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.2[,1]), digs), " (",                                 #m3
         round(as.vector(HPDI(col.1.2[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.2[,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.2[,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.2[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.2[,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.2[,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.2[,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.2[,1], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.3[,1,1]), digs), " (",                                 #m4
         round(as.vector(HPDI(col.1.3[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.3[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.3[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.3[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.3[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.3[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.3[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.3[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.3[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.3[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.3[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.3[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.3[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.3[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.3[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.3[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.3[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.4[,1,1]), digs), " (",                                 #m13
         round(as.vector(HPDI(col.1.4[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.4[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.4[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.4[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.4[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.4[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.4[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.4[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.4[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.4[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.4[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.4[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.4[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.4[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.4[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.4[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.4[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.5[,1,1]), digs), " (",                                 #m14
         round(as.vector(HPDI(col.1.5[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.5[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.5[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.5[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.5[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.5[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.5[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.5[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.5[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.5[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.5[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.5[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.5[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.5[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.5[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.5[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.5[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.6[,1,1]), digs), " (",                                 #m15
         round(as.vector(HPDI(col.1.6[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.6[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.6[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.6[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.6[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.6[,1,3]), digs), " (",                                
         round(as.vector(HPDI(col.1.6[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.6[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.6[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.6[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.6[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.6[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.6[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.6[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.6[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.6[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.6[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.7[,1,1]), digs), " (",                                 #m16
         round(as.vector(HPDI(col.1.7[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.7[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.7[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.7[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.7[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.7[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.7[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.7[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.7[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.7[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.7[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.7[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.7[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.7[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.7[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.7[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.7[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.8[,1,1]), digs), " (",                                 #m18 note switch
         round(as.vector(HPDI(col.1.8[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.8[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.8[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.8[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.8[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.8[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.8[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.8[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.8[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.8[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.8[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.8[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.8[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.8[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.8[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.8[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.8[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.9[,1,1]), digs), " (",                                 #m17 note switch
         round(as.vector(HPDI(col.1.9[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.9[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.9[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.9[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.9[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.9[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.9[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.9[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.9[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.9[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.9[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.9[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.9[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.9[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.9[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.9[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.9[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.10[,1,1]), digs), " (",                                 #m19
         round(as.vector(HPDI(col.1.10[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.10[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.10[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.10[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.10[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.10[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.10[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.10[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.10[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.10[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.10[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.10[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.10[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.10[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.10[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.10[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.10[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.1.11[,1,1]), digs), " (",                                 #m20
         round(as.vector(HPDI(col.1.11[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.11[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.11[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.11[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.11[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.11[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.11[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.11[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.11[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.1.11[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.11[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.11[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.1.11[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.11[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.1.11[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.1.11[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.1.11[,2,3], prob=cred))[2], digs), ")", sep="")
)

col2 <- c(

  #Sex
  " ", " ", " ",                     
  " ", " ", " ",                     
  " ", " ", " ",    " ", " ", " ",  
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",  
  " ", " ", " ",    " ", " ", " ",  
  paste( round(mean(col.2.1[,1,1]), digs), " (",                                 #m20
         round(as.vector(HPDI(col.2.1[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.2.1[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.2.1[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.2.1[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.2.1[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.2.1[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.2.1[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.2.1[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.2.1[,2,1]), digs), " (",                               
         round(as.vector(HPDI(col.2.1[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.2.1[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.2.1[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.2.1[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.2.1[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.2.1[,2,3]), digs), " (",                                
         round(as.vector(HPDI(col.2.1[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.2.1[,2,3], prob=cred))[2], digs), ")", sep="")
)

col3 <- c(

#adolescent
  " ", " ", " ",                     
  " ", " ", " ",                     
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",  
  " ", " ", " ",    " ", " ", " ",  
  paste( round(mean(col.3.1[,1,1]), digs), " (",                                 #m20
         round(as.vector(HPDI(col.3.1[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.3.1[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.3.1[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.3.1[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.3.1[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.3.1[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.3.1[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.3.1[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.3.1[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.3.1[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.3.1[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.3.1[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.3.1[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.3.1[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.3.1[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.3.1[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.3.1[,2,3], prob=cred))[2], digs), ")", sep="")
)

col4 <- c(

#elder
  " ", " ", " ",                     
  " ", " ", " ",                     
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",   
  " ", " ", " ",    " ", " ", " ",  
  " ", " ", " ",    " ", " ", " ",  
  paste( round(mean(col.4.1[,1,1]), digs), " (",                                 #m20
         round(as.vector(HPDI(col.4.1[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.4.1[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.4.1[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.4.1[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.4.1[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.4.1[,1,3]), digs), " (",                                
         round(as.vector(HPDI(col.4.1[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.4.1[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.4.1[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.4.1[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.4.1[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.4.1[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.4.1[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.4.1[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.4.1[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.4.1[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.4.1[,2,3], prob=cred))[2], digs), ")", sep="")
)


col5 <- c(

#family
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  paste( round(mean(col.5.1[,1,1]), digs), " (",                                 #m13
         round(as.vector(HPDI(col.5.1[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.1[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.1[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.1[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.1[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.1[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.1[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.1[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.1[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.5.1[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.1[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.1[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.1[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.1[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.1[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.1[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.1[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m14
  " ", " ", " ",    " ", " ", " ",   #m15
  paste( round(mean(col.5.2[,1,1]), digs), " (",                                 #m16
         round(as.vector(HPDI(col.5.2[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.2[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.2[,1,2]), digs), " (",                                
         round(as.vector(HPDI(col.5.2[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.2[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.2[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.2[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.2[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.2[,2,1]), digs), " (",                                
         round(as.vector(HPDI(col.5.2[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.2[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.2[,2,2]), digs), " (",                                
         round(as.vector(HPDI(col.5.2[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.2[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.2[,2,3]), digs), " (",                                
         round(as.vector(HPDI(col.5.2[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.2[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.5.3[,1,1]), digs), " (",                                 #m18
         round(as.vector(HPDI(col.5.3[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.3[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.3[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.3[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.3[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.3[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.3[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.3[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.3[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.5.3[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.3[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.3[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.3[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.3[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.3[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.3[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.3[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",  #m17

  paste( round(mean(col.5.4[,1,1]), digs), " (",                                 #m19
         round(as.vector(HPDI(col.5.4[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.4[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.4[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.4[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.4[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.4[,1,3]), digs), " (",                                
         round(as.vector(HPDI(col.5.4[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.4[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.4[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.5.4[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.4[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.4[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.4[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.4[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.4[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.4[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.4[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.5.5[,1,1]), digs), " (",                                 #m20
         round(as.vector(HPDI(col.5.5[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.5[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.5[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.5[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.5[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.5[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.5[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.5[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.5[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.5.5[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.5[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.5[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.5.5[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.5[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.5.5[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.5.5[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.5.5[,2,3], prob=cred))[2], digs), ")", sep="")
)


col6 <- c(

#employment
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  " ", " ", " ",    " ", " ", " ",   #m13

  paste( round(mean(col.6.1[,1,1]), digs), " (",                                 #m14
         round(as.vector(HPDI(col.6.1[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.1[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.1[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.1[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.1[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.1[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.1[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.1[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.1[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.6.1[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.1[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.1[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.1[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.1[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.1[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.1[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.1[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m15

  paste( round(mean(col.6.2[,1,1]), digs), " (",                                 #m16
         round(as.vector(HPDI(col.6.2[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.2[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.2[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.2[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.2[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.2[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.2[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.2[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.2[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.6.2[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.2[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.2[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.2[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.2[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.2[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.2[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.2[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m18

  paste( round(mean(col.6.3[,1,1]), digs), " (",                                 #m17
         round(as.vector(HPDI(col.6.3[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.3[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.3[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.3[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.3[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.3[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.3[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.3[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.3[,2,1]), digs), " (",                                
         round(as.vector(HPDI(col.6.3[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.3[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.3[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.3[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.3[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.3[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.3[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.3[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.6.4[,1,1]), digs), " (",                                 #m19
         round(as.vector(HPDI(col.6.4[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.4[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.4[,1,2]), digs), " (",                                
         round(as.vector(HPDI(col.6.4[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.4[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.4[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.4[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.4[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.4[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.6.4[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.4[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.4[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.4[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.4[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.4[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.4[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.4[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.6.5[,1,1]), digs), " (",                                 #m20
         round(as.vector(HPDI(col.6.5[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.5[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.5[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.5[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.5[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.5[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.5[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.5[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.5[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.6.5[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.5[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.5[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.6.5[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.5[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.6.5[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.6.5[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.6.5[,2,3], prob=cred))[2], digs), ")", sep="")
)


col7 <- c(

  #community
  " ", " ", " ",                     #m1
  " ", " ", " ",                     #m3
  " ", " ", " ",    " ", " ", " ",   #m4
  " ", " ", " ",    " ", " ", " ",   #m13
  " ", " ", " ",    " ", " ", " ",   #m14

  paste( round(mean(col.7.1[,1,1]), digs), " (",                                 #m15
         round(as.vector(HPDI(col.7.1[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.1[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.1[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.1[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.1[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.1[,1,3]), digs), " (",                                
         round(as.vector(HPDI(col.7.1[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.1[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.1[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.7.1[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.1[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.1[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.1[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.1[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.1[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.1[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.1[,2,3], prob=cred))[2], digs), ")", sep=""),

  " ", " ", " ",    " ", " ", " ",   #m16

  paste( round(mean(col.7.2[,1,1]), digs), " (",                                 #m18
         round(as.vector(HPDI(col.7.2[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.2[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.2[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.2[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.2[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.2[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.2[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.2[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.2[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.7.2[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.2[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.2[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.2[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.2[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.2[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.2[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.2[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.7.3[,1,1]), digs), " (",                                 #m17
         round(as.vector(HPDI(col.7.3[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.3[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.3[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.3[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.3[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.3[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.3[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.3[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.3[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.7.3[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.3[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.3[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.3[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.3[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.3[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.3[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.3[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.7.4[,1,1]), digs), " (",                                 #m19
         round(as.vector(HPDI(col.7.4[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.4[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.4[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.4[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.4[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.4[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.4[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.4[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.4[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.7.4[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.4[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.4[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.4[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.4[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.4[,2,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.4[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.4[,2,3], prob=cred))[2], digs), ")", sep=""),

  paste( round(mean(col.7.5[,1,1]), digs), " (",                                 #m20
         round(as.vector(HPDI(col.7.5[,1,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.5[,1,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.5[,1,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.5[,1,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.5[,1,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.5[,1,3]), digs), " (",                                 
         round(as.vector(HPDI(col.7.5[,1,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.5[,1,3], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.5[,2,1]), digs), " (",                                 
         round(as.vector(HPDI(col.7.5[,2,1], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.5[,2,1], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.5[,2,2]), digs), " (",                                 
         round(as.vector(HPDI(col.7.5[,2,2], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.5[,2,2], prob=cred))[2], digs), ")", sep=""),
  paste( round(mean(col.7.5[,2,3]), digs), " (",                                
         round(as.vector(HPDI(col.7.5[,2,3], prob=cred))[1], digs), ", ",
         round(as.vector(HPDI(col.7.5[,2,3], prob=cred))[2], digs), ")", sep="")
)


waic_col <- c(

  #WAIC weight
  signif(weights[1], digs2),     #m1    scientific digits notation
  " ", " ",
  signif(weights[2], digs2),     #m3
  " ", " ",
  signif(weights[3], digs2),     #m4
  " ", " ",   " ", " ", " ",
  signif(weights[4], digs2),     #m13
  " ", " ",   " ", " ", " ",
  signif(weights[5], digs2),     #m14
  " ", " ",   " ", " ", " ",
  signif(weights[6], digs2),     #m15
  " ", " ",   " ", " ", " ",
  signif(weights[7], digs2),     #m16
  " ", " ",   " ", " ", " ",
  signif(weights[9], digs2),     #m18        #note m17 and m18 switch
  " ", " ",   " ", " ", " ",
  signif(weights[8], digs2),     #m17
  " ", " ",   " ", " ", " ",
  signif(weights[10], digs2),     #m19
  " ", " ",   " ", " ", " ",
  signif(weights[11], digs2),     #m20
  " ", " ",   " ", " ", " "
)


tab_mat_mest <- cbind(rnames_mest, col1,
                      col2, col3, col4,
                      col5, col6, col7,
                      waic_col)

#dim(tab_mat_mest)

colnames(tab_mat_mest) <- cnames_mest

#tab_mat_mest[30:60,1:9]

temp <- capture.output( stargazer(tab_mat_mest, summary=FALSE, rownames=FALSE, type="latex",
                                  out="./Plots/table_mest.tex")
                      )


