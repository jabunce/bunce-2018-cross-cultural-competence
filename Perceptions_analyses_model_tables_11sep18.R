#These are all the IRT models used in the analysis.

#Create a folder called Manu_perceptions and put the data file (Manu_perceptions_22aug18.csv) in there.
#The codes (written in Stan) for each model are separate files named m1.stan to m20.stan.
#Create a sub-folder called "models" and put those 20 model files in there.
#Run this R script from the folder Manu_perceptions.

#Script until line 318 is just wrangling the data into the right format.

#This script produces LaTex output for Appendix tables C.2 and C.3 in the manuscript.
#Script starting line on 2910 is optional. It lets you see how each model is coded and run it individually if you want. 

#Script line 1155 to 1314 runs the twenty Stan models. It may take a few days to run them all.
#You can make them run faster by decreasing the number of chains and samples ("chains" and "iter" arguments in stan() function). 


rm (list = ls(all=TRUE))
library(rstan)
library(rethinking)
library(lattice)
library(MASS)
library(graphics)
library(grid)
library(boot) #for inverse logit function
library(stargazer) #for table of model outputs


#Read the data from the csv data file into R:
Interviews.raw <- read.csv(
	file=
		"./Manu_perceptions/Manu_perceptions_22aug18.csv",		
		header=TRUE)

#Check the variable names and dimensions in the data frame Interviews.raw
names(Interviews.raw)
dim(Interviews.raw)


#question names

quest_names <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.post.chest",
						"9.pot.each",
						"10.good.nonbaptized.heaven",
						"11.postpone.work.visit",
						"12.expensive.store",
						"13.daughter.not.marry",
						"14.laborer.not.drunk"
				)


d.wide <- Interviews.raw
names(d.wide)

#sort by Target (1,2,3), then by Machi (0,1), then by Community (A,B,T)
d.wide <- d.wide[order(d.wide$Target, d.wide$Machi, d.wide$Community), ]

d.wide[,c(1,2,4,15,16,17)]

#add consecutive newID
num_indivs <- length( unique(d.wide$ID) )
ID_key <- cbind( unique(d.wide$ID), 1:num_indivs)

d.wide$newID <- as.numeric( factor(d.wide$ID, levels=unique(d.wide$ID)) )
d.wide[,c("ID","newID")]


############################### convert dataframe from wide to long format

names(d.wide)

names(d.wide)[16:29] <- sprintf( "q.%d", 1:14 ) 	##rename question columns with "." separator before number
d.wide$respID <- 1:nrow(d.wide)						##add column with unique ID for each person-target combination
names(d.wide)							
d.wide[,c("ID","newID","respID")]

d.long <- reshape(d.wide, varying=names(d.wide)[16:29], sep=".",	### column indices of the questions
					 idvar="respID",
					 direction="long")

dim(d.long)
names(d.long)
d.long <- d.long[order(d.long$newID),]					#order questions by person ID
d.long[1:100, c("ID","Machi","time","q")]


#re-name columns
names(d.long)[c(length(names(d.long)) - 4,
				length(names(d.long)) - 1,
				length(names(d.long)))] <- c("target.num", "question", "response") #rename

dim(d.long)
names(d.long)
d.long[1:100, c("newID","Machi","target.num","question","response" )]


d.na.resp <- d.long[which(is.na(d.long$response)==TRUE),] 		#NA responses
unique(d.na.resp$ID)

num.na.resp.ego <- length(which(d.na.resp$target.num==1)) 		#number of NA responses by target, conditional on people being asked the questions
num.na.resp.in <- length(which(d.na.resp$target.num==2))
num.na.resp.out <- length(which(d.na.resp$target.num==3))

num.all.resp.ego <- length(which(d.long$target.num==1))			#number of non-NA ego responses
num.na.resp.ego/num.all.resp.ego 								#proportion of NA ego responses
 
d <- d.long[which(is.na(d.long$response)==FALSE),] 				#NA responses removed



############### dataset characteristics

K <- length(unique(d$question)) 						#number of questions
J <- length(unique(d$newID)) 							#number of people
N <- nrow(d) 											#number of responses across all targets
nummach <- length(unique(d[which(d$Machi==1),"ID"]))	#number machis
nummest <- length(unique(d[which(d$Machi==0),"ID"]))	#number mestizos






#######################flip coding of some questions to polarize latent axes

#index of questions to flip
Flip.e <- ifelse( 
				#ego
				(d$target.num==1 & d$question == 3) | 	#wear dead hat ok, changed=0
               	(d$target.num==1 & d$question == 8) | 	#chest pain go to post, changed=0
               	(d$target.num==1 & d$question == 9) |	#each gets a pot, changed=0
               	(d$target.num==1 & d$question == 10) |	#good nonbaptized heaven, changed=0
               	(d$target.num==1 & d$question == 12) |	#expensive store, changed=0
               	(d$target.num==1 & d$question == 13) |	#daughter not marry disobeys parents, changed=0
               	(d$target.num==1 & d$question == 14) |	#laborer not drunk, changed=0

               	#ingroup
                (d$target.num==2 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==2 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==2 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==2 & d$question == 10) |  #good nonbaptized heaven, changed=0
                (d$target.num==2 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==2 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==2 & d$question == 14) |  #laborer not drunk, changed=0

               	#outgroup
                (d$target.num==3 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==3 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==3 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==3 & d$question == 10) |  #good nonbaptized heaven, changed=0
                (d$target.num==3 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==3 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==3 & d$question == 14) ,  #laborer not drunk, changed=0, 	#laborer not drunk, changed=0
               	1 , 0 )



#allow coding of questions to differ by axis
Flip.io <- ifelse( 
				        #ego
                (d$target.num==1 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==1 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==1 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==1 & d$question == 10) |  #good nonbaptized heaven, changed=0
                (d$target.num==1 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==1 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==1 & d$question == 14) |  #laborer not drunk, changed=0

                #ingroup
                (d$target.num==2 & d$question == 3) |   #wear dead hat ok, changed=0
                (d$target.num==2 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==2 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==2 & d$question == 10) |  #good nonbaptized heaven, changed=0
                #(d$target.num==2 & d$question == 12) |  #expensive store, changed=0
                (d$target.num==2 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==2 & d$question == 14) |  #laborer not drunk, changed=0

                #outgroup
                (d$target.num==3 & d$question == 3) |   #wear dead hat ok, changed=0
                #(d$target.num==3 & d$question == 8) |   #chest pain go to post, changed=0
                (d$target.num==3 & d$question == 9) | #each gets a pot, changed=0
                (d$target.num==3 & d$question == 10) |  #good nonbaptized heaven, changed=0
                #(d$target.num==3 & d$question == 12) |  #expensive store, changed=0
                #(d$target.num==3 & d$question == 13) |  #daughter not marry disobeys parents, changed=0
                (d$target.num==3 & d$question == 14) , 	#laborer not drunk, changed=0
               	1 , 0 )


#modify question names for flipped coding
quest_names.e <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.not.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.not.post.chest",
						"9.not.pot.each",
						"10.good.nonbaptized.not.heaven",
						"11.postpone.work.visit",
						"12.cheap.store",
						"13.daughter.must.marry",
						"14.laborer.is.drunk"
				)

quest_names.i <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.not.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.not.post.chest",
						"9.not.pot.each",
						"10.good.nonbaptized.not.heaven",
						"11.postpone.work.visit",
						"12.expensive.store",
						"13.daughter.must.marry",
						"14.laborer.is.drunk"
				)

quest_names.o <- c(		"1.reverse.gender",
						"2.daughter.babysits",
						"3.not.wear.dead.hat",
						"4.wife.drinks.alone",
						"5.teacher.hits",
						"6.no.questions",
						"7.post.flu",
						"8.post.chest",
						"9.not.pot.each",
						"10.good.nonbaptized.not.heaven",
						"11.postpone.work.visit",
						"12.expensive.store",
						"13.daughter.not.marry",
						"14.laborer.is.drunk"
				)

d$ResponseFlipped.e <- ifelse( Flip.e == 1, ifelse( d$response==1, 0, 1), d$response )
d$ResponseFlipped.io <- ifelse( Flip.io == 1, ifelse( d$response==1, 0, 1), d$response )

#check flipping
d[173:220, c("target.num","question", "response", "ResponseFlipped.e", "ResponseFlipped.io")]


pdf(file="./Flip_check.e.pdf", 
height=4, width=4)
par(mfrow=c(1,1))

plot( jitter(d$ResponseFlipped.e) ~ jitter(d$response), 
  col=ifelse(Flip.e==1,"red","black") )

graphics.off()

pdf(file="./Flip_check.io.pdf", 
height=4, width=4)
par(mfrow=c(1,1))

plot( jitter(d$ResponseFlipped.io) ~ jitter(d$response), 
  col=ifelse(Flip.io==1,"red","black") )

graphics.off()


#rename response columns
names(d)
names(d)[c(length(names(d))-2, length(names(d))-1, length(names(d)))] <- c("resp.original", "response.e", "response")
names(d)






#######################################remove people with NA predictors

#get row indices with NAs
na.rows <- 0
for (n in 1:N) {
	for (p in 1:dim(d)[2]) {
		if ( is.na(d[n,p]) ) na.rows <- c(na.rows, n)
	}
}
na.rows <- unique(na.rows[-1])
d[c(112:152, 1265:1280, 1538:1553, 4748:4763), #look at NA rows
	c("ID", "Machi",
		"EdMes", "LabMes", "ComMes",
		"FamMat", "EmpMat", "CtyMat",
		"target.num", "question")]

#delete NA rows
d_all <- d
d <- d[-na.rows,]
#d[d$newID %in% c(1,2),]
#d[d$newID %in% c(5,44,54,157),]

#get new consecutive newIDs
d$newID <- as.numeric( factor(d$ID, levels=unique(d$ID)) )


#key to match ID and newID
ID_key <- cbind( unique(d$ID), unique(d$newID))
colnames(ID_key) <- c("ID", "newID")
ID_key2 <- ID_key[order(ID_key[,"ID"]),]

d[1:20,c("ID","newID")]
ID_key


#number of people answering each question, by ethnicity, by target
table( d$question, d$target.num, d$Machi )


K <- length(unique(d$question)) 						#number of questions
J <- length(unique(d$newID)) 							#number of people
N <- nrow(d) 											#number of responses across all targets
nummach <- length(unique(d[which(d$Machi==1),"ID"]))	#number machis
nummest <- length(unique(d[which(d$Machi==0),"ID"]))	#number mestizos





############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())

jj <- d$newID			#vector of person IDs
kk <- d$question		#vector of question IDs
y <- d$response			#vector of responses


#WAIC function
waic <- function(stanfit){
  log_lik <- extract (stanfit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  S <- nrow(log_lik)
  n <- ncol(log_lik)
  lpd <- log(colMeans(exp(log_lik)))
  p_waic <- colVars(log_lik)
  elpd_waic <- lpd - p_waic
  waic <- -2*elpd_waic
  loo_weights_raw <- 1/exp(log_lik-max(log_lik))
  loo_weights_normalized <- loo_weights_raw/
    matrix(colMeans(loo_weights_raw),nrow=S,ncol=n,byrow=TRUE)
  loo_weights_regularized <- pmin (loo_weights_normalized, sqrt(S))
  elpd_loo <- log(colMeans(exp(log_lik)*loo_weights_regularized)/
    colMeans(loo_weights_regularized))
  p_loo <- lpd - elpd_loo
  pointwise <- cbind(waic,lpd,p_waic,elpd_waic,p_loo,elpd_loo)
  total <- colSums(pointwise)
  se <- sqrt(n*colVars(pointwise))
  return(list(waic=total["waic"], elpd_waic=total["elpd_waic"],
    p_waic=total["p_waic"], elpd_loo=total["elpd_loo"], p_loo=total["p_loo"],
    pointwise=pointwise, total=total, se=se))
}

#colVars function needed by waic function
colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
                     sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}




####################################################
####################################################
##run all stan models first


###model code

model_file_1 <- "./Manu_perceptions/models/m1.stan"  
model_file_2 <- "./Manu_perceptions/models/m2.stan"  
model_file_3 <- "./Manu_perceptions/models/m3.stan" 
model_file_4 <- "./Manu_perceptions/models/m4.stan" 
model_file_5 <- "./Manu_perceptions/models/m5.stan" 
model_file_6 <- "./Manu_perceptions/models/m6.stan" 
model_file_7 <- "./Manu_perceptions/models/m7.stan" 
model_file_8 <- "./Manu_perceptions/models/m8.stan" 
model_file_9 <- "./Manu_perceptions/models/m9.stan" 
model_file_10 <- "./Manu_perceptions/models/m10.stan" 
model_file_11 <- "./Manu_perceptions/models/m11.stan" 
model_file_12 <- "./Manu_perceptions/models/m12.stan" 
model_file_13 <- "./Manu_perceptions/models/m13.stan" 
model_file_14 <- "./Manu_perceptions/models/m14.stan" 
model_file_15 <- "./Manu_perceptions/models/m15.stan" 
model_file_16 <- "./Manu_perceptions/models/m16.stan" 
model_file_17 <- "./Manu_perceptions/models/m17.stan" 
model_file_18 <- "./Manu_perceptions/models/m18.stan" 
model_file_19 <- "./Manu_perceptions/models/m19.stan"  
model_file_20 <- "./Manu_perceptions/models/m20.stan"  



###### data and start lists for all models

##
data_list_1 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response            #N vector of responses
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))

start_1 <- list(
  a0=c(0,0,0),
  aIndiv = as.array( rbind( rep(0, times=J), rep(0, times=J), rep(0, times=J) ) ), 
  beta = as.array( rbind( rep(0, times=K), rep(0, times=K), rep(0, times=K) ) ),
  gamma = as.array( rbind( rep(1, times=K), rep(1, times=K), rep(1, times=K) ) ),
  sigma_beta=c(1,1,1),
  sigma_gamma=c(1,1,1)
)

##
data_list_2 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response            #N vector of responses
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))

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

##
data_list_3 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response            #N vector of responses
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))

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

##
data_list_4 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2)
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_4 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)), 
  
  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2)
)

##
data_list_5 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EdMes = d$EdMes
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_5 <- list(
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

  aEdMes = array(data=0, dim=c(Mach,T))
)

##
data_list_6 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  LabMes = d$LabMes           #vector of labor experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_6 <- list(
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

  aLabMes = array(data=0, dim=c(Mach,T))
)

##
data_list_7 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  ComMes = d$ComMes           #vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_7 <- list(
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

  aComMes = array(data=0, dim=c(Mach,T))
)

##
data_list_8 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EdMes = d$EdMes,            #vector of education experience with mestizos for each guess
  LabMes = d$LabMes           #vector of labor experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_8 <- list(
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
  aLabMes = array(data=0, dim=c(Mach,T))
)

##
data_list_9 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EdMes = d$EdMes,            #vector of education experience with mestizos for each guess
  ComMes = d$ComMes           #vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_9 <- list(
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
  aComMes = array(data=0, dim=c(Mach,T))
)

##
data_list_10 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  LabMes = d$LabMes,            #vector of labor experience with mestizos for each guess
  ComMes = d$ComMes           #vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_10 <- list(
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

  aLabMes = array(data=0, dim=c(Mach,T)),
  aComMes = array(data=0, dim=c(Mach,T))
)

##
data_list_11 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EdMes = d$EdMes,            #vector of education experience with mestizos for each guess
  LabMes = d$LabMes,            #vector of labor experience with mestizos for each guess
  ComMes = d$ComMes           #vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_11 <- list(
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
  aComMes = array(data=0, dim=c(Mach,T))
)

##
data_list_12 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EdMes = d$EdMes,            #vector of education experience with mestizos for each guess
  LabMes = d$LabMes,            #vector of labor experience with mestizos for each guess
  ComMes = d$ComMes,            #vector of commerce experience with mestizos for each guess

  Adol = d$adol.less20,       #vector of 1 if adolescent, else 0
  Old = d$old.over50,         #vector of 1 if old, else 0
  Male = d$Sex.1male          #vector of 1 if male, 0 if female
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_12 <- list(
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

  aAdol = array(data=0, dim=c(Mach,T)),
  aOld = array(data=0, dim=c(Mach,T)),
  aMale = array(data=0, dim=c(Mach,T))
)

##
data_list_13 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_13 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T))
)

##
data_list_14 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EmpMat = d$EmpMat      #vector of employment experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_14 <- list(
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

  aEmpMat = array(data=0, dim=c(Mach,T))
)


##
data_list_15 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  CtyMat = d$CtyMat     #vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_15 <- list(
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

  aCtyMat = array(data=0, dim=c(Mach,T))
)

##
data_list_16 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat,      #vector of family experience with machis for each guess
  EmpMat = d$EmpMat      #vector of employment experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_16 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T))
)

##
data_list_17 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EmpMat = d$EmpMat,      #vector of employment experience with machis for each guess
  CtyMat = d$CtyMat     #vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_17 <- list(
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

  aEmpMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T))
)


##
data_list_18 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat,      #vector of family experience with machis for each guess
  CtyMat = d$CtyMat     #vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_18 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T))
)

##
data_list_19 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat,      #vector of family experience with machis for each guess
  EmpMat = d$EmpMat,      #vector of employment experience with machis for each guess
  CtyMat = d$CtyMat     #vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_19 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T))
)


##
data_list_20 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat,      #vector of family experience with machis for each guess
  EmpMat = d$EmpMat,      #vector of employment experience with machis for each guess
  CtyMat = d$CtyMat,     #vector of community experience with machis for each guess

  Adol = d$adol.less20,       #vector of 1 if adolescent, else 0
  Old = d$old.over50,         #vector of 1 id old, else 0
  Male = d$Sex.1male          #vector of 1 if male, 0 if female
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_20 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T)),

  aAdol = array(data=0, dim=c(Mach,T)),
  aOld = array(data=0, dim=c(Mach,T)),
  aMale = array(data=0, dim=c(Mach,T))
)



##### run stan models

set.seed(1)
m20 <- stan( #model_code=model_code_20,
            file=model_file_20,
            data=data_list_20,
            init=list(start_20, start_20,  start_20, start_20), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m19 <- stan( #model_code=model_code_19,
            file=model_file_19,
            data=data_list_19,
            init=list(start_19, start_19, start_19, start_19), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m18 <- stan( #model_code=model_code_18,
            file=model_file_18,
            data=data_list_18,
            init=list(start_18, start_18, start_18, start_18), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m17 <- stan( #model_code=model_code_17,
            file=model_file_17,
            data=data_list_17,
            init=list(start_17, start_17, start_17, start_17), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m16 <- stan( #model_code=model_code_16,
            file=model_file_16,
            data=data_list_16,
            init=list(start_16, start_16, start_16, start_16), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m15 <- stan( #model_code=model_code_15,
            file=model_file_15,
            data=data_list_15,
            init=list(start_15, start_15, start_15, start_15), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m14 <- stan( #model_code=model_code_14,
            file=model_file_14,
            data=data_list_14,
            init=list(start_14, start_14, start_14, start_14), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m13 <- stan( #model_code=model_code_13,
            file=model_file_13,
            data=data_list_13,
            init=list(start_13, start_13, start_13, start_13), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m12 <- stan( #model_code=model_code_12,
            file=model_file_12,
            data=data_list_12,
            init=list(start_12, start_12, start_12, start_12), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m11 <- stan( #model_code=model_code_11,
            file=model_file_11,
            data=data_list_11,
            init=list(start_11, start_11, start_11, start_11), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m10 <- stan( #model_code=model_code_10,
            file=model_file_10,
            data=data_list_10,
            init=list(start_10, start_10, start_10, start_10), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m9 <- stan( #model_code=model_code_9,
            file=model_file_9,
            data=data_list_9,
            init=list(start_9, start_9, start_9, start_9), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m8 <- stan( #model_code=model_code_8,
            file=model_file_8,
            data=data_list_8,
            init=list(start_8, start_8, start_8, start_8), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m7 <- stan( #model_code=model_code_7,
            file=model_file_7,
            data=data_list_7,
            init=list(start_7, start_7, start_7, start_7), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m6 <- stan( #model_code=model_code_6,
            file=model_file_6,
            data=data_list_6,
            init=list(start_6, start_6, start_6, start_6), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m5 <- stan( #model_code=model_code_5,
            file=model_file_5,
            data=data_list_5,
            init=list(start_5, start_5, start_5, start_5), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m4 <- stan( #model_code=model_code_4,
            file=model_file_4,
            data=data_list_4,
            init=list(start_4, start_4, start_4, start_4), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10

set.seed(1)
m3 <- stan( #model_code=model_code_3,
            file=model_file_3,
            data=data_list_3,
            init=list(start_3, start_3, start_3, start_3), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99, max_treedepth=12) ) #default treedepth is 10

set.seed(1)
m2 <- stan( #model_code=model_code_2,
            file=model_file_2,
            data=data_list_2,
            init=list(start_2, start_2, start_2, start_2), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99) )
set.seed(1)
m1 <- stan( file=model_file_1,
            #model_code= model_code_1,
            data=data_list_1,
            init=list(start_1, start_1, start_1, start_1), 
                        iter=2000 , chains=4, 
                        control=list(adapt_delta=0.99) )



##### extract fitted output

post1 <- extract.samples( m1 ) 
post2 <- extract.samples( m2 ) 
post3 <- extract.samples( m3 ) 
post4 <- extract.samples( m4 ) 
post5 <- extract.samples( m5 ) 
post6 <- extract.samples( m6 ) 
post7 <- extract.samples( m7 )
post8 <- extract.samples( m8 )
post9 <- extract.samples( m9 )
post10 <- extract.samples( m10 )
post11 <- extract.samples( m11 )
post12 <- extract.samples( m12 )
post13 <- extract.samples( m13 )
post14 <- extract.samples( m14 )
post15 <- extract.samples( m15 ) 
post16 <- extract.samples( m16 ) 
post17 <- extract.samples( m17 ) 
post18 <- extract.samples( m18 ) 
post19 <- extract.samples( m19 ) 
post20 <- extract.samples( m20 )

str(post1)
str(post2)
str(post3)
str(post4)
str(post5)
str(post6)
str(post7)
str(post8)
str(post9)
str(post10)
str(post11)
str(post12)
str(post13)
str(post14)
str(post15)
str(post16)
str(post17)
str(post18)
str(post19)
str(post20)





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


compare(m1, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12) #last column gives standard error of the difference in waic b/t models
x_mach <- compare(m1, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)
str(x_mach)
x_mach@dSE #gives matrix of differences in waic between all models
x_mach #last column gives standard error of differences from the best model


compare(m1, m3, m4, m13, m14, m15, m16, m17, m18, m19, m20) #last column gives standard error of the difference in waic b/t models
x_mest <- compare(m1, m3, m4, m13, m14, m15, m16, m17, m18, m19, m20)
str(x_mest)
x_mest@dSE #gives matrix of differences in waic between all models
x_mest




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
print(WAIC_sum_mach, digits=2)

WAIC_sum_mest <- WAIC_sum_mest[order(WAIC_sum_mest[,1]),]
print(WAIC_sum_mest, digits=2)




#### tables of model outputs

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

dim(tab_mat_mach)

colnames(tab_mat_mach) <- cnames_mach

tab_mat_mach[1:20,1:9]

stargazer(tab_mat_mach, summary=FALSE, rownames=FALSE, type="latex",
          out="./table_mach.tex")



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

dim(tab_mat_mest)

colnames(tab_mat_mest) <- cnames_mest

tab_mat_mest[30:60,1:9]

stargazer(tab_mat_mest, summary=FALSE, rownames=FALSE, type="latex",
          out="./table_mest.tex")










####### m1: IRT for responses by target, rand intercept for each person for each target  ############################
#######				separate axes for each target

model_code_1 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, ingroup, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n
}

parameters {
  real beta[T,K];       //separate betas and gammas for each target
  real<lower=0> gamma[T,K];
  real a0[T];             // mean interviewee location in latent dimension (mean ability intercept), one for each target
  real aIndiv[T,J];       // location of people (differing from the mean), i.e., random effect for person. rows=targets, columns=individuals
  real<lower=0> sigma_beta[T];  // scale (stdev) of question difficulty, one for each target
  real<lower=0> sigma_gamma[T]; // scale of question discrimination
}

model {
  vector[N] params;
  real alpha;
  
  a0 ~ normal(0,1);
  aIndiv[1] ~ normal(0,1);    //identifying prior for location and scale
  aIndiv[2] ~ normal(0,1);
  aIndiv[3] ~ normal(0,1);

  beta[1] ~ normal(0,sigma_beta[1]);
  beta[2] ~ normal(0,sigma_beta[2]);
  beta[3] ~ normal(0,sigma_beta[3]);

  gamma[1] ~ normal(0,sigma_gamma[1]);
  gamma[2] ~ normal(0,sigma_gamma[2]);
  gamma[3] ~ normal(0,sigma_gamma[3]);

  sigma_beta ~ exponential(1);  //exponential(beta), where here beta = lambda = 1/mean
  sigma_gamma ~ exponential(1); //or use uniform(0,5), both prevent ceiling effect

  for (n in 1:N) {
    alpha = ( a0[tt[n]] + aIndiv[tt[n],jj[n]] );    //random effect for person

    params[n] = gamma[tt[n],kk[n]]*(alpha - beta[tt[n],kk[n]]);

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {       //for computing waic
  vector[N] log_lik;
  real alpha;

  for (n in 1:N) {
    alpha = ( a0[tt[n]] + aIndiv[tt[n],jj[n]] );

    //needed for waic function, see Stan manual pg498
    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n],kk[n]]*(alpha - beta[tt[n],kk[n]]) );
  } //for
}
"



data_list_1 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response						#N vector of responses
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))

start_1 <- list(
  a0=c(0,0,0),
  aIndiv = as.array( rbind( rep(0, times=J), rep(0, times=J), rep(0, times=J) ) ), 
  beta = as.array( rbind( rep(0, times=K), rep(0, times=K), rep(0, times=K) ) ),
  gamma = as.array( rbind( rep(1, times=K), rep(1, times=K), rep(1, times=K) ) ),
  sigma_beta=c(1,1,1),
  sigma_gamma=c(1,1,1)
)

set.seed(1)
m1 <- stan( #file=model_file_1,
            model_code= model_code_1,
            data=data_list_1,
            init=list(start_1, start_1), # start_1, start_1), 
            	          iter=1000 , chains=2, 
                        control=list(adapt_delta=0.99) )

print(m1, pars=c("a0",
				 "aIndiv",
				 "beta",
				 "gamma",
                 "sigma_beta",
                 "sigma_gamma",
                 "lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

#for dotplots below 
post1 <- extract.samples( m1 )
str(post1)


#look at all traces
pdf(file="./traces_m1.pdf",
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

waic(m1)$waic







####### m2: IRT for responses by target, covarying rand intercepts for each person for each target  ############################
#######				separate axes for each target, covarying disriminations and difficulties


model_code_2 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n
}

parameters {
  
  vector[T] aInt[J]; 				//array of target intercepts for each individual, array of size J containing vectors of T elements. See Stan manual pg 45-46 and Beta on pg 149
  vector[T] muInt;					//vector of intercept means for each target T
  vector<lower=0>[T] sigmaInt;		//vector of intercept stdevs for each target T
  corr_matrix[T] R_a;				//correlation matrix for T intercepts

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector<lower=0>[T] muGamma;		//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  vector[T*2] quest[K];				//vector of question betas and gammas for each target
  corr_matrix[T*2] R_q;				//correlation matrix for question betas and gammas
}

transformed parameters {
	vector[T*2] muQuest;
	vector[T*2] sigmaQuest;

	muQuest[1] = muBeta[1];
	muQuest[2] = muBeta[2];
	muQuest[3] = muBeta[3];
	muQuest[4] = muGamma[1];
	muQuest[5] = muGamma[2];
	muQuest[6] = muGamma[3];

	sigmaQuest[1] = sigmaBeta[1];
	sigmaQuest[2] = sigmaBeta[2];
	sigmaQuest[3] = sigmaBeta[3];
	sigmaQuest[4] = sigmaGamma[1];
	sigmaQuest[5] = sigmaGamma[2];
	sigmaQuest[6] = sigmaGamma[3];
}

model {
  vector[N] params;			//temporary vector container of gamma(alpha-beta) for each N
  real alpha;				//temporary container for linear model
  real beta;
  real gamma;
  

  muInt ~ normal(0,1);			// T-vector of mean intercepts for each target
  sigmaInt ~ exponential(1);	//use cauchy(0,2)?, T-vector of stdevs for each target
  R_a ~ lkj_corr(2);

  muBeta ~ normal(0,1);			// T-vector of mean betas for each target
  sigmaBeta ~ exponential(1);	// T-vector of beta stdevs

  muGamma ~ normal(0,1);
  sigmaGamma ~ exponential(1);

  R_q ~ lkj_corr(4);


  for (n in 1:N) {

    alpha = aInt[ jj[n], tt[n] ];   	//first index is element of J-array, second index is element of T-vector

    beta = quest[ kk[n], tt[n] ];
    gamma = quest[ kk[n], tt[n]+T ];


    params[n] = gamma*(alpha - beta);

  } //for

  quest ~ multi_normal( muQuest, quad_form_diag(R_q, sigmaQuest) );
  aInt ~ multi_normal( muInt, quad_form_diag(R_a, sigmaInt) );			//sample a T-vector for each cell in the array of T-vectors, stan manual pg 149
  y ~ bernoulli_logit(params);
}
"

data_list_2 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response						#N vector of responses
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))

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

set.seed(1)
m2 <- stan( #model_code=model_code_2,
            file=model_file_2,
            data=data_list_2,
            init=list(start_2, start_2), # start_2, start_2), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99) )

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

#for dotplots below 
post2 <- extract.samples( m2 )
str(post2)


#look at all traces
pdf(file="./traces_m2.pdf",
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





####### m3: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_3 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n
}

parameters {
  matrix[T,J] zInt; 				//matrix of intercept z-scores for each individual, rows = intercepts, cols = indivs
  vector[T] muInt;					//vector of intercept means for each target T
  vector<lower=0>[T] sigmaInt;		//vector of intercept stdevs for each target T
  cholesky_factor_corr[T] L_R_a;	//cholesky factor for correlation matrix for intercepts, a TxT matrix

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T (will be exponentiated, so don't constrain positive)
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 			//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts. Note: dimensions are transposed from zInt
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  																//Stan manual pg 150-151, Rethinking pg 409
  off_Int = (diag_pre_multiply(sigmaInt, L_R_a) * zInt)'; 		//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose so dimensions match off_Int.
  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  to_vector(zInt) ~ normal(0,1);	//vectorize matrix of intercept z-scores for each indiv in order to assign sampling distribution
  muInt ~ normal(0,1); 				//T-vector of mean intercepts
  sigmaInt ~ exponential(1);		//use cauchy(0,2)?, T-vector of stdevs from mean of each intercept
  L_R_a ~ lkj_corr_cholesky(4);		//lower the eta to allow more extreme correlations, Rethinking pg 394

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(1);
  sigmaGamma ~ exponential(1);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt + off_Int[jj[n]]'; 						//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector

  	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] ); 		//exponentiate to constrain discriminations positive
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercepts for each person
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a;		//correlation matrix for intercepts
  matrix[T,T] Cov_a;	//variance-covariance matrix for intercepts
  matrix[T*2,T*2] R_q;	//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt' + off_Int[jj[n]]; 										//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.
  
  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt + off_Int[jj[n]]'; 						
  	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for n

  R_a = L_R_a * L_R_a';																	//reconstruct the correlation matrix to look at in output
  R_q = L_R_q * L_R_q';

  Cov_a = diag_pre_multiply(sigmaInt, L_R_a) * diag_pre_multiply(sigmaInt, L_R_a)';		//construct cov matrix from colesky factors of cov matrix to look at in output
																						//sigmaInt contains stdevs, the final cov matrix has variances down the diagonal, but uncertainty in both parameters is carried through in this calculation, so sqrt(diag) is not exactly sigmaInt
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_3 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response						#N vector of responses
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))

start_3 <- list(
  zInt = matrix(0, nrow=T, ncol=J),
  muInt = as.array(rep(0, times=T)),
  sigmaInt = as.array(rep(1, times=T)),
  L_R_a = diag(x=0, nrow=T, ncol=T),	#initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2)
)

set.seed(1)
m3 <- stan( #model_code=model_code_3,
            file=model_file_3,
            data=data_list_3,
            init=list(start_3, start_3), # start_3, start_3), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=12) ) #default treedepth is 10


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

#for dotplots below 
post3 <- extract.samples( m3 )
str(post3)


#look at all traces
pdf(file="./traces_m3.pdf",
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


waic(m3)$waic



####### m4: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									predictor for ethnicity
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_4 <- "
data {
  int<lower=1> J; 				        // number of interviewees
  int<lower=1> K; 				        // number of questions
  int<lower=1> T;				          // number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				        // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	  // interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	  // question for observation n
  int<lower=1,upper=T> tt[N];	    // target for observation n
  int<lower=0,upper=1> y[N]; 	    // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n
}

transformed data {
  vector[3] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[3] ones = [1,1,1]'; 	  //column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				         //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				         //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	   //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	 //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] muBeta;					         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		   //vector of beta stdevs for each target T	

  vector[T] muGamma;		             //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	   //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	 //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 							 //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															     //Stan manual pg 150-151, Rethinking pg 409
															       //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	   //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	 //array of T-vectors of mean intercepts
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				         //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				       //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);     //increase expoential parameter to concentrate probability closer to 0
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]'; 		//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		     //reconstructed intercept for each individual
  matrix[K,T] rBeta;	     //reconstructed betas for each question
  matrix[K,T] rGamma;	     //reconstructed gammas for each question

  matrix[T,T] R_a[2];		   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		 //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		 //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	 //variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]';
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								            //reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	        //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_4 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2)
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_4 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)),	#initialize cholesky factors with 0?
  
  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2)
)

set.seed(1)
m4 <- stan( model_code=model_code_4,
            #file=model_file_4,
            data=data_list_4,
            init=list(start_4, start_4), # start_4, start_4), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post4 <- extract.samples( m4 )
str(post4)


#look at all traces
pdf(file="./traces_m4.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m4, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m4, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m4, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m4, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m4, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m4, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m4, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m4, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m4, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m4, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m4, pars="lp__", inc_warmup=T))
graphics.off()


waic(m4)$waic





####### m5: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									machi EdMes predictor
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_5 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EdMes[N];	//education experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEdMes[2];				//coef for education experience with mestizos

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aEdMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target
  
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aEdMes[Machi[n]] * EdMes[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aEdMes[Machi[n]] * EdMes[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_5 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	EdMes = d$EdMes
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_5 <- list(
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

  aEdMes = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m5 <- stan( #model_code=model_code_5,
            file=model_file_5,
            data=data_list_5,
            init=list(start_5, start_5), # start_5, start_5), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post5 <- extract.samples( m5 )
str(post5)


#look at all traces
pdf(file="./traces_m5.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m5, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m5, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m5, pars=paste("aEdMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m5, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m5, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m5, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m5, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m5, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m5, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m5, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m5, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m5, pars="lp__", inc_warmup=T))
graphics.off()




####### m6: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									machi LabMes predictor
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_6 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> LabMes[N];	//labor experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aLabMes[2];				//coef for labor experience with mestizos

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aLabMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target

  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aLabMes[Machi[n]] * LabMes[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aLabMes[Machi[n]] * LabMes[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_6 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	LabMes = d$LabMes						#vector of labor experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_6 <- list(
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

  aLabMes = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m6 <- stan( #model_code=model_code_6,
            file=model_file_6,
            data=data_list_6,
            init=list(start_6, start_6), # start_6, start_6), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post6 <- extract.samples( m6 )
str(post6)


#look at all traces
pdf(file="./traces_m6.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m6, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m6, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m6, pars=paste("aLabMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m6, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m6, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m6, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m6, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m6, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m6, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m6, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m6, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m6, pars="lp__", inc_warmup=T))
graphics.off()


waic(m6)$waic





####### m7: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									machi ComMes predictor
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_7 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> ComMes[N];	//commerce experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aComMes[2];				//coef for commerce experience with mestizos

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aComMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target

  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aComMes[Machi[n]] * ComMes[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aComMes[Machi[n]] * ComMes[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_7 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	ComMes = d$ComMes						#vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_7 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)),	#initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aComMes = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m7 <- stan( #model_code=model_code_7,
            file=model_file_7,
            data=data_list_7,
            init=list(start_7, start_7), # start_7, start_7), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post7 <- extract.samples( m7 )
str(post7)


#look at all traces
pdf(file="./traces_m7.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m7, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m7, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m7, pars=paste("aComMes[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m7, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m7, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m7, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m7, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m7, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m7, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m7, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m7, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m7, pars="lp__", inc_warmup=T))
graphics.off()


waic(m7)$waic




####### m8: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									machi EdMes and LabMes predictors
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_8 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EdMes[N];	//education experience with mestizos
  int<lower=0,upper=1> LabMes[N];	//labor experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEdMes[2];				//coef for education experience with mestizos
  vector[T] aLabMes[2];				//coef for labor experience with mestizos

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aEdMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target
  aLabMes ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aLabMes[Machi[n]] * LabMes[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aLabMes[Machi[n]] * LabMes[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_8 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	EdMes = d$EdMes,						#vector of education experience with mestizos for each guess
	LabMes = d$LabMes						#vector of labor experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_8 <- list(
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
  aLabMes = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m8 <- stan( #model_code=model_code_8,
            file=model_file_8,
            data=data_list_8,
            init=list(start_8, start_8), # start_8, start_8), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post8 <- extract.samples( m8 )
str(post8)


#look at all traces
pdf(file="./traces_m8.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m8, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m8, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
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

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m8, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m8, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m8, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m8, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m8, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m8, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m8, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m8, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m8, pars="lp__", inc_warmup=T))
graphics.off()


waic(m8)$waic





####### m9: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									machi EdMes and ComMes predictors
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_9 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EdMes[N];	//education experience with mestizos
  int<lower=0,upper=1> ComMes[N];	//commerce experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEdMes[2];				//coef for education experience with mestizos
  vector[T] aComMes[2];				//coef for commerce experience with mestizos

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aEdMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target
  aComMes ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aComMes[Machi[n]] * ComMes[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aComMes[Machi[n]] * ComMes[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_9 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	EdMes = d$EdMes,						#vector of education experience with mestizos for each guess
	ComMes = d$ComMes						#vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_9 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)),	#initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aEdMes = array(data=0, dim=c(Mach,T)),
  aComMes = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m9 <- stan( #model_code=model_code_9,
            file=model_file_9,
            data=data_list_9,
            init=list(start_9, start_9), # start_9, start_9), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post9 <- extract.samples( m9 )
str(post9)


#look at all traces
pdf(file="./traces_m9.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m9, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m9, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
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

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m9, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m9, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m9, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m9, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m9, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m9, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m9, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m9, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m9, pars="lp__", inc_warmup=T))
graphics.off()


waic(m9)$waic





####### m10: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									machi LabMes and ComMes predictors
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_10 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> LabMes[N];	//labor experience with mestizos
  int<lower=0,upper=1> ComMes[N];	//commerce experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aLabMes[2];				//coef for labor experience with mestizos
  vector[T] aComMes[2];				//coef for commerce experience with mestizos

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aLabMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target
  aComMes ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aLabMes[Machi[n]] * LabMes[n] +
  				  aComMes[Machi[n]] * ComMes[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aLabMes[Machi[n]] * LabMes[n] +
  				  aComMes[Machi[n]] * ComMes[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_10 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	LabMes = d$LabMes,						#vector of labor experience with mestizos for each guess
	ComMes = d$ComMes						#vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_10 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)),	#initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aLabMes = array(data=0, dim=c(Mach,T)),
  aComMes = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m10 <- stan( #model_code=model_code_10,
            file=model_file_10,
            data=data_list_10,
            init=list(start_10, start_10), # start_10, start_10), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post10 <- extract.samples( m10 )
str(post10)


#look at all traces
pdf(file="./traces_m10.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m10, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m10, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}

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

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m10, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m10, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m10, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m10, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m10, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m10, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m10, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m10, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m10, pars="lp__", inc_warmup=T))
graphics.off()


waic(m10)$waic




####### m11: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									all machi predictors
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_11 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EdMes[N];	//education experience with mestizos
  int<lower=0,upper=1> LabMes[N];	//labor experience with mestizos
  int<lower=0,upper=1> ComMes[N];	//commerce experience with mestizos
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEdMes[2];				//coef for education experience with mestizos
  vector[T] aLabMes[2];				//coef for labor experience with mestizos
  vector[T] aComMes[2];				//coef for commerce experience with mestizos

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aEdMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target
  aLabMes ~ multi_normal(zeros, diag_matrix(ones));
  aComMes ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aLabMes[Machi[n]] * LabMes[n] +
  				  aComMes[Machi[n]] * ComMes[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aLabMes[Machi[n]] * LabMes[n] +
  				  aComMes[Machi[n]] * ComMes[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_11 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	EdMes = d$EdMes,						#vector of education experience with mestizos for each guess
	LabMes = d$LabMes,						#vector of labor experience with mestizos for each guess
	ComMes = d$ComMes						#vector of commerce experience with mestizos for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_11 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)),	#initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aEdMes = array(data=0, dim=c(Mach,T)),
  aLabMes = array(data=0, dim=c(Mach,T)),
  aComMes = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m11 <- stan( #model_code=model_code_11,
            file=model_file_11,
            data=data_list_11,
            init=list(start_11, start_11), # start_11, start_11), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post11 <- extract.samples( m11 )
str(post11)


#look at all traces
pdf(file="./traces_m11.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m11, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m11, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
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

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m11, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m11, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m11, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m11, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m11, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m11, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m11, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m11, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m11, pars="lp__", inc_warmup=T))
graphics.off()


waic(m11)$waic





####### m12: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									all machi predictors, plus age category and sex
####### 		separate axes for each target, covarying disriminations and difficulties


model_code_12 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EdMes[N];	//education experience with mestizos
  int<lower=0,upper=1> LabMes[N];	//labor experience with mestizos
  int<lower=0,upper=1> ComMes[N];	//commerce experience with mestizos

  int<lower=0,upper=1> Adol[N]; 	// adolescent
  int<lower=0,upper=1> Old[N]; 		// old, not adolescent and not old = mature = default
  int<lower=0,upper=1> Male[N]; 	// predictor for sex
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEdMes[2];				//coef for education experience with mestizos
  vector[T] aLabMes[2];				//coef for labor experience with mestizos
  vector[T] aComMes[2];				//coef for commerce experience with mestizos

  vector[T] aAdol[2];
  vector[T] aOld[2];
  vector[T] aMale[2]; 

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample two at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aEdMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target
  aLabMes ~ multi_normal(zeros, diag_matrix(ones));
  aComMes ~ multi_normal(zeros, diag_matrix(ones));
  
  aAdol ~ multi_normal(zeros, diag_matrix(ones));
  aOld ~ multi_normal(zeros, diag_matrix(ones));
  aMale ~ multi_normal(zeros, diag_matrix(ones));

  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aLabMes[Machi[n]] * LabMes[n] +
  				  aComMes[Machi[n]] * ComMes[n] +

  				  aAdol[Machi[n]] * Adol[n] +
  				  aOld[Machi[n]] * Old[n] +
  				  aMale[Machi[n]] * Male[n]; 		

   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];

  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;	//for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts three at a time for each indiv. Note transpose of means to row vector.

  	rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
  	rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
  	rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

  	rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
  	rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
  	rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );

  	//needed for waic function, see Stan manual pg498
  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
  				  aEdMes[Machi[n]] * EdMes[n] +
  				  aLabMes[Machi[n]] * LabMes[n] +
  				  aComMes[Machi[n]] * ComMes[n] +
  				  aAdol[Machi[n]] * Adol[n] +
  				  aOld[Machi[n]] * Old[n] +
  				  aMale[Machi[n]] * Male[n]; 
   	beta[1] = muBeta[1] + off_quest[kk[n],1];
  	beta[2] = muBeta[2] + off_quest[kk[n],2];
  	beta[3] = muBeta[3] + off_quest[kk[n],3];
  	gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
  	gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
  	gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_12 <- list(
	J = length(unique(d$newID)),		#number of people
	K = length(unique(d$question)),		#number of questions
	T = length(unique(d$target.num)),	#number of targets
	N = nrow(d),						#total number of responses
	jj = d$newID,						#N vector of person IDs
	kk = d$question,					#N vector of question IDs
	tt = d$target.num, 					#N vector of target numbers
	y = d$response,						#N vector of responses

	Machi = ifelse(d[,"Machi"]==1, 1, 2),   	#ethnicity converted to index
	EdMes = d$EdMes,						#vector of education experience with mestizos for each guess
	LabMes = d$LabMes,						#vector of labor experience with mestizos for each guess
	ComMes = d$ComMes,						#vector of commerce experience with mestizos for each guess

	Adol = d$adol.less20,				#vector of 1 if adolescent, else 0
	Old = d$old.over50,					#vector of 1 id old, else 0
	Male = d$Sex.1male					#vector of 1 if male, 0 if female
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_12 <- list(
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

  aAdol = array(data=0, dim=c(Mach,T)),
  aOld = array(data=0, dim=c(Mach,T)),
  aMale = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m12 <- stan( #model_code=model_code_12,
            file=model_file_12,
            data=data_list_12,
            init=list(start_12, start_12), # start_12, start_12), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post12 <- extract.samples( m12 )
str(post12)


#look at all traces
pdf(file="./traces_m12.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m12, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
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

for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m12, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m12, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m12, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m12, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m12, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m12, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m12, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m12, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m12, pars="lp__", inc_warmup=T))
graphics.off()


waic(m12)$waic




####### m13: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######                 mestizo FamMat predictor


model_code_13 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> FamMat[N]; //family experience with machis
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';    //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aFamMat[2];       //coef for family experience with machis

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aFamMat ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target
  
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);  //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4); //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aFamMat[Machi[n]] * FamMat[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aFamMat[Machi[n]] * FamMat[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_13 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_13 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)), #initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aFamMat = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m13 <- stan( model_code=model_code_13,
            #file=model_file_13,
            data=data_list_13,
            init=list(start_13, start_13), # start_13, start_13), 
                        iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post13 <- extract.samples( m13 )
str(post13)


#look at all traces
pdf(file="./traces_m13.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
  for ( z3 in 1:T ) {
    print(traceplot(m13, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m13, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m13, pars=paste("aFamMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m13, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    for ( z3 in 1:T ) {
      print(traceplot(m13, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
    }
  }
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:K ) {
    print(traceplot(m13, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z2 in 1:T ){
  print(traceplot(m13, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m13, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m13, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m13, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:(T*2) ) {
    print(traceplot(m13, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
print(traceplot(m13, pars="lp__", inc_warmup=T))
graphics.off()





####### m14: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######                 mestizo EmpMat predictor


model_code_14 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EmpMat[N]; //employment experience with machis
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';    //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEmpMat[2];       //coef for employment experience with machis

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aEmpMat ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target

  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);  //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4); //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aEmpMat[Machi[n]] * EmpMat[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aEmpMat[Machi[n]] * EmpMat[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_14 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EmpMat = d$EmpMat      #vector of employment experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_14 <- list(
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

  aEmpMat = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m14 <- stan( model_code=model_code_14,
            #file=model_file_14,
            data=data_list_14,
            init=list(start_14, start_14), # start_14, start_14), 
                        iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post14 <- extract.samples( m14 )
str(post14)


#look at all traces
pdf(file="./traces_m14.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
  for ( z3 in 1:T ) {
    print(traceplot(m14, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m14, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m14, pars=paste("aEmpMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m14, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    for ( z3 in 1:T ) {
      print(traceplot(m14, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
    }
  }
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:K ) {
    print(traceplot(m14, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z2 in 1:T ){
  print(traceplot(m14, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m14, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m14, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m14, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:(T*2) ) {
    print(traceplot(m14, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
print(traceplot(m14, pars="lp__", inc_warmup=T))
graphics.off()




####### m15: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######                 mestizo CtyMat predictor


model_code_15 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> CtyMat[N]; //community experience with machis
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';    //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aCtyMat[2];       //coef for community experience with machis

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aCtyMat ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target

  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);  //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4); //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aCtyMat[Machi[n]] * CtyMat[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aCtyMat[Machi[n]] * CtyMat[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_15 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  CtyMat = d$CtyMat     #vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_15 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)), #initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aCtyMat = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m15 <- stan( model_code=model_code_15,
            #file=model_file_15,
            data=data_list_15,
            init=list(start_15, start_15), # start_15, start_15), 
                        iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post15 <- extract.samples( m15 )
str(post15)


#look at all traces
pdf(file="./traces_m15.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
  for ( z3 in 1:T ) {
    print(traceplot(m15, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m15, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m15, pars=paste("aCtyMat[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m15, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    for ( z3 in 1:T ) {
      print(traceplot(m15, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
    }
  }
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:K ) {
    print(traceplot(m15, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z2 in 1:T ){
  print(traceplot(m15, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m15, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m15, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m15, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:(T*2) ) {
    print(traceplot(m15, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
print(traceplot(m15, pars="lp__", inc_warmup=T))
graphics.off()



####### m16: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######                 mestizo FamMat and EmpMat predictors

model_code_16 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> FamMat[N]; //family experience with machis
  int<lower=0,upper=1> EmpMat[N]; //employment experience with machis
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';    //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aFamMat[2];       //coef for family experience with machis
  vector[T] aEmpMat[2];       //coef for employment experience with machis

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aFamMat ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target
  aEmpMat ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);  //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4); //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aFamMat[Machi[n]] * FamMat[n] +
            aEmpMat[Machi[n]] * EmpMat[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aFamMat[Machi[n]] * FamMat[n] +
            aEmpMat[Machi[n]] * EmpMat[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_16 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat,      #vector of family experience with machis for each guess
  EmpMat = d$EmpMat      #vector of employment experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_16 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m16 <- stan( model_code=model_code_16,
            #file=model_file_16,
            data=data_list_16,
            init=list(start_16, start_16), # start_16, start_16), 
                        iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post16 <- extract.samples( m16 )
str(post16)


#look at all traces
pdf(file="./traces_m16.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
  for ( z3 in 1:T ) {
    print(traceplot(m16, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m16, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
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

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m16, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    for ( z3 in 1:T ) {
      print(traceplot(m16, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
    }
  }
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:K ) {
    print(traceplot(m16, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z2 in 1:T ){
  print(traceplot(m16, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m16, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m16, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m16, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:(T*2) ) {
    print(traceplot(m16, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
print(traceplot(m16, pars="lp__", inc_warmup=T))
graphics.off()




####### m17: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######                 mestizo EmpMat and CtyMat predictors

model_code_17 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> EmpMat[N]; //employment experience with machis
  int<lower=0,upper=1> CtyMat[N]; //community experience with machis
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';    //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aEmpMat[2];       //coef for employment experience with machis
  vector[T] aCtyMat[2];       //coef for community experience with machis

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aEmpMat ~ multi_normal(zeros, diag_matrix(ones)); //array of T-vectors of experience offsets to the mean for each target
  aCtyMat ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);  //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4); //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aEmpMat[Machi[n]] * EmpMat[n] +
            aCtyMat[Machi[n]] * CtyMat[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aEmpMat[Machi[n]] * EmpMat[n] +
            aCtyMat[Machi[n]] * CtyMat[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_17 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  EmpMat = d$EmpMat,      #vector of employment experience with machis for each guess
  CtyMat = d$CtyMat     #vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_17 <- list(
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

  aEmpMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m17 <- stan( model_code=model_code_17,
            #file=model_file_17,
            data=data_list_17,
            init=list(start_17, start_17), # start_17, start_17), 
                        iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post17 <- extract.samples( m17 )
str(post17)


#look at all traces
pdf(file="./traces_m17.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
  for ( z3 in 1:T ) {
    print(traceplot(m17, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m17, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}

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
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m17, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    for ( z3 in 1:T ) {
      print(traceplot(m17, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
    }
  }
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:K ) {
    print(traceplot(m17, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z2 in 1:T ){
  print(traceplot(m17, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m17, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m17, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m17, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:(T*2) ) {
    print(traceplot(m17, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
print(traceplot(m17, pars="lp__", inc_warmup=T))
graphics.off()



####### m18: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######                 mestizo FamMat and CtyMat predictors


model_code_18 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> FamMat[N]; //family experience with machis
  int<lower=0,upper=1> CtyMat[N]; //community experience with machis
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';    //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aFamMat[2];       //coef for family experience with machis
  vector[T] aCtyMat[2];       //coef for community experience with machis

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aFamMat ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target
  aCtyMat ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);  //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4); //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aFamMat[Machi[n]] * FamMat[n] +
            aCtyMat[Machi[n]] * CtyMat[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aFamMat[Machi[n]] * FamMat[n] +
            aCtyMat[Machi[n]] * CtyMat[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_18 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat,      #vector of family experience with machis for each guess
  CtyMat = d$CtyMat     #vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_18 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m18 <- stan( model_code=model_code_18,
            #file=model_file_18,
            data=data_list_18,
            init=list(start_18, start_18), # start_18, start_18), 
                        iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post18 <- extract.samples( m18 )
str(post18)


#look at all traces
pdf(file="./traces_m18.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
  for ( z3 in 1:T ) {
    print(traceplot(m18, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m18, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
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
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m18, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    for ( z3 in 1:T ) {
      print(traceplot(m18, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
    }
  }
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:K ) {
    print(traceplot(m18, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z2 in 1:T ){
  print(traceplot(m18, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m18, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m18, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m18, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:(T*2) ) {
    print(traceplot(m18, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
print(traceplot(m18, pars="lp__", inc_warmup=T))
graphics.off()




####### m19: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									all mestizo predictors

model_code_19 <- "
data {
  int<lower=1> J; 				// number of interviewees
  int<lower=1> K; 				// number of questions
  int<lower=1> T;				// number of targets (ego, outgroup)
  int<lower=1> N; 				// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	// question for observation n
  int<lower=1,upper=T> tt[N];	// target for observation n
  int<lower=0,upper=1> y[N]; 	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];	//Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> FamMat[N];	//family experience with machis
  int<lower=0,upper=1> EmpMat[N];	//employment experience with machis
  int<lower=0,upper=1> CtyMat[N];	//community experience with machis
}

transformed data {
  vector[T] zeros = [0,0,0]'; 	//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]'; 		//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				//J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				//intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aFamMat[2];				//coef for family experience with machis
  vector[T] aEmpMat[2];				//coef for employment experience with machis
  vector[T] aCtyMat[2];				//coef for community experience with machis

  vector[T] muBeta;					//vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		//vector of beta stdevs for each target T	

  vector[T] muGamma;				//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 										//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
  															//Stan manual pg 150-151, Rethinking pg 409
															//create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;															

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
  	off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;					//temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;					//temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));	//array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	//array of T-vectors of mean intercepts
  aFamMat ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target
  aEmpMat ~ multi_normal(zeros, diag_matrix(ones));
  aCtyMat ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);	//use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);	//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);		//lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

  	alpha = muInt[Machi[n]] + off_Int[jj[n]]' + 				//linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
  					aFamMat[Machi[n]] * FamMat[n] +
  					aEmpMat[Machi[n]] * EmpMat[n] +
  					aCtyMat[Machi[n]] * CtyMat[n]; 		

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;		//reconstructed intercept for each individual
  matrix[K,T] rBeta;	//reconstructed betas for each question
  matrix[K,T] rGamma;	//reconstructed gammas for each question

  matrix[T,T] R_a[2];		//2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];		//2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;		//correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;	//variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
  	aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]]; 				//reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aFamMat[Machi[n]] * FamMat[n] +
            aEmpMat[Machi[n]] * EmpMat[n] +
            aCtyMat[Machi[n]] * CtyMat[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_19 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
	FamMat = d$FamMat,			#vector of family experience with machis for each guess
	EmpMat = d$EmpMat,			#vector of employment experience with machis for each guess
	CtyMat = d$CtyMat			#vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_19 <- list(
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

  aFamMat = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m19 <- stan( model_code=model_code_19,
            #file=model_file_19,
            data=data_list_19,
            init=list(start_19, start_19), # start_19, start_19), 
            	          iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post19 <- extract.samples( m19 )
str(post19)


#look at all traces
pdf(file="./traces_m19.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
	for ( z3 in 1:T ) {
		print(traceplot(m19, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m19, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
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
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		print(traceplot(m19, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
	}
}
for ( z1 in 1:Mach) {
	for ( z2 in 1:T ) {
		for ( z3 in 1:T ) {
			print(traceplot(m19, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
		}
	}
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:K ) {
		print(traceplot(m19, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
for ( z2 in 1:T ){
  print(traceplot(m19, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m19, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m19, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m19, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
	for ( z3 in 1:(T*2) ) {
		print(traceplot(m19, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
	}
}
print(traceplot(m19, pars="lp__", inc_warmup=T))
graphics.off()





####### m20: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######                 all mestizo predictors, plus age category and sex
#######     separate axes for each target, covarying disriminations and difficulties

model_code_20 <- "
data {
  int<lower=1> J;         // number of interviewees
  int<lower=1> K;         // number of questions
  int<lower=1> T;       // number of targets (ego, outgroup)
  int<lower=1> N;         // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; // target for observation n
  int<lower=0,upper=1> y[N];  // response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];  //Matsigenka ethnicity: 1=yes, 2=no, for each obs n

  int<lower=0,upper=1> FamMat[N]; //family experience with machis
  int<lower=0,upper=1> EmpMat[N]; //employment experience with machis
  int<lower=0,upper=1> CtyMat[N]; //community experience with machis

  int<lower=0,upper=1> Adol[N];   // adolescent
  int<lower=0,upper=1> Old[N];    // old, not adolescent and not old = mature = default
  int<lower=0,upper=1> Male[N];   // predictor for sex
}

transformed data {
  vector[T] zeros = [0,0,0]';   //column vectors of 0s, note transpose at the end to turn into row vectors
  vector[T] ones = [1,1,1]';    //column vectors of 1s
}

parameters {
  vector[T] zInt[J];        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];       //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2]; //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2]; //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] aFamMat[2];       //coef for family experience with machis
  vector[T] aEmpMat[2];       //coef for employment experience with machis
  vector[T] aCtyMat[2];       //coef for community experience with machis

  vector[T] aAdol[2];
  vector[T] aOld[2];
  vector[T] aMale[2]; 

  vector[T] muBeta;         //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;   //vector of beta stdevs for each target T 

  vector[T] muGamma;        //vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;  //vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;       //matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;  //cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int;                    //matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
                                //Stan manual pg 150-151, Rethinking pg 409
                              //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose.
  matrix[K,T*2] off_quest;                              

  muQuest[1] = muBeta[1];
  muQuest[2] = muBeta[2];
  muQuest[3] = muBeta[3];  
  muQuest[4] = muGamma[1];
  muQuest[5] = muGamma[2];
  muQuest[6] = muGamma[3];

  sigmaQuest[1] = sigmaBeta[1];
  sigmaQuest[2] = sigmaBeta[2];
  sigmaQuest[3] = sigmaBeta[3];
  sigmaQuest[4] = sigmaGamma[1];
  sigmaQuest[5] = sigmaGamma[2];
  sigmaQuest[6] = sigmaGamma[3];

  for (n in 1:N) {
    off_Int[jj[n]] = (diag_pre_multiply(sigmaInt[Machi[n]], L_R_a[Machi[n]]) * zInt[jj[n]])';
  }

  off_quest = (diag_pre_multiply(sigmaQuest, L_R_q) * zQuest)';
}

model {
  vector[N] params;         //temporary vector container of gamma(alpha-beta) for each N
  vector[T] alpha;          //temporary container for an individual's three reconstructed intercepts
  vector[T] beta;
  vector[T] gamma;

  zInt ~ multi_normal(zeros, diag_matrix(ones));  //array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones));   //array of T-vectors of mean intercepts
  aFamMat ~ multi_normal(zeros, diag_matrix(ones));  //array of T-vectors of experience offsets to the mean for each target
  aEmpMat ~ multi_normal(zeros, diag_matrix(ones));
  aCtyMat ~ multi_normal(zeros, diag_matrix(ones));

  aAdol ~ multi_normal(zeros, diag_matrix(ones));
  aOld ~ multi_normal(zeros, diag_matrix(ones));
  aMale ~ multi_normal(zeros, diag_matrix(ones));

  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);  //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4); //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);    //lower the eta to allow more extreme correlations, Rethinking pg 394


  for (n in 1:N) {

    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +         //linear model of intercepts: mean + indiv offset. Vector of T intercepts for each indiv. Note transpose of offsets to column vector
            aFamMat[Machi[n]] * FamMat[n] +
            aEmpMat[Machi[n]] * EmpMat[n] +
            aCtyMat[Machi[n]] * CtyMat[n] +

            aAdol[Machi[n]] * Adol[n] +
            aOld[Machi[n]] * Old[n] +
            aMale[Machi[n]] * Male[n];    

    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];

    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    params[n] = gamma[tt[n]]*( alpha[tt[n]] - beta[tt[n]] );

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {
  matrix[J,T] aInt;   //reconstructed intercept for each individual
  matrix[K,T] rBeta;  //reconstructed betas for each question
  matrix[K,T] rGamma; //reconstructed gammas for each question

  matrix[T,T] R_a[2];   //2-array of TxT correlation matrices
  matrix[T,T] Cov_a[2];   //2-array of TxT variance-covariance matrices
  matrix[T*2,T*2] R_q;    //correlation matrix for betas and gammas
  matrix[T*2,T*2] Cov_q;  //variance-covariance matrix for betas and gammas

  vector[N] log_lik;  //for WAIC
  vector[T] alpha;
  vector[T] beta;
  vector[T] gamma;


  for (n in 1:N) {
    aInt[jj[n]] = muInt[Machi[n]]' + off_Int[jj[n]];        //reconstruct intercepts two at a time for each indiv. Note transpose of means to row vector.

    rBeta[kk[n],1] = muBeta[1]' + off_quest[kk[n],1];
    rBeta[kk[n],2] = muBeta[2]' + off_quest[kk[n],2];
    rBeta[kk[n],3] = muBeta[3]' + off_quest[kk[n],3];

    rGamma[kk[n],1] = exp( muGamma[1]' + off_quest[kk[n],4] );
    rGamma[kk[n],2] = exp( muGamma[2]' + off_quest[kk[n],5] );
    rGamma[kk[n],3] = exp( muGamma[3]' + off_quest[kk[n],6] );
    
    //needed for waic function, see Stan manual pg498
    alpha = muInt[Machi[n]] + off_Int[jj[n]]' +
            aFamMat[Machi[n]] * FamMat[n] +
            aEmpMat[Machi[n]] * EmpMat[n] +
            aCtyMat[Machi[n]] * CtyMat[n] +
            aAdol[Machi[n]] * Adol[n] +
            aOld[Machi[n]] * Old[n] +
            aMale[Machi[n]] * Male[n]; 
    beta[1] = muBeta[1] + off_quest[kk[n],1];
    beta[2] = muBeta[2] + off_quest[kk[n],2];
    beta[3] = muBeta[3] + off_quest[kk[n],3];
    gamma[1] = exp( muGamma[1] + off_quest[kk[n],4] );
    gamma[2] = exp( muGamma[2] + off_quest[kk[n],5] );
    gamma[3] = exp( muGamma[3] + off_quest[kk[n],6] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[tt[n]]*(alpha[tt[n]] - beta[tt[n]]) );
  } //for

  for (Mach in 1:2) {

    R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';               //reconstruct the correlation matrix to look at in output

    Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
            diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';  //construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_20 <- list(
  J = length(unique(d$newID)),    #number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            #total number of responses
  jj = d$newID,           #N vector of person IDs
  kk = d$question,          #N vector of question IDs
  tt = d$target.num,          #N vector of target numbers
  y = d$response,           #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
  FamMat = d$FamMat,      #vector of family experience with machis for each guess
  EmpMat = d$EmpMat,      #vector of employment experience with machis for each guess
  CtyMat = d$CtyMat,     #vector of community experience with machis for each guess

  Adol = d$adol.less20,       #vector of 1 if adolescent, else 0
  Old = d$old.over50,         #vector of 1 id old, else 0
  Male = d$Sex.1male          #vector of 1 if male, 0 if female
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_20 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)), #initialize cholesky factors with 0?

  zQuest = matrix(0, nrow=T*2, ncol=K),
  muBeta = as.array(rep(0, times=T)),
  sigmaBeta = as.array(rep(1, times=T)),
  muGamma = as.array(rep(0, times=T)),
  sigmaGamma = as.array(rep(1, times=T)),
  L_R_q = diag(x=0, nrow=T*2, ncol=T*2),

  aFamMat = array(data=0, dim=c(Mach,T)),
  aEmpMat = array(data=0, dim=c(Mach,T)),
  aCtyMat = array(data=0, dim=c(Mach,T)),

  aAdol = array(data=0, dim=c(Mach,T)),
  aOld = array(data=0, dim=c(Mach,T)),
  aMale = array(data=0, dim=c(Mach,T))
)

set.seed(1)
m20 <- stan( model_code=model_code_20,
            #file=model_file_20,
            data=data_list_20,
            init=list(start_20, start_20), # start_20, start_20), 
                        iter=500 , chains=2, 
                        control=list(adapt_delta=0.99, max_treedepth=10) ) #default treedepth is 10


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

#for dotplots below 
post20 <- extract.samples( m20 )
str(post20)


#look at all traces
pdf(file="./traces_m20.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

for ( z2 in 1:J ) {
  for ( z3 in 1:T ) {
    print(traceplot(m20, pars=paste("zInt[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("muInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
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

for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    print(traceplot(m20, pars=paste("sigmaInt[", z1, ",", z2, "]", sep=""), inc_warmup=T ))
  }
}
for ( z1 in 1:Mach) {
  for ( z2 in 1:T ) {
    for ( z3 in 1:T ) {
      print(traceplot(m20, pars=paste("L_R_a[", z1, ",", z2, ",", z3, "]", sep=""), inc_warmup=T ))
    }
  }
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:K ) {
    print(traceplot(m20, pars=paste("zQuest[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
for ( z2 in 1:T ){
  print(traceplot(m20, pars=paste("muBeta[", z2, "]", sep=""), inc_warmup=T ))
  }
for ( z3 in 1:T ){
  print(traceplot(m20, pars=paste("muGamma[", z3, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m20, pars=paste("sigmaBeta[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:T ){
  print(traceplot(m20, pars=paste("sigmaGamma[", z2, "]", sep=""), inc_warmup=T ))
}
for ( z2 in 1:(T*2) ) {
  for ( z3 in 1:(T*2) ) {
    print(traceplot(m20, pars=paste("L_R_q[", z2, ",", z3, "]", sep=""), inc_warmup=T ))
  }
}
print(traceplot(m20, pars="lp__", inc_warmup=T))
graphics.off()







