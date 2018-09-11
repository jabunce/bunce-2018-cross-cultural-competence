#These are the essential IRT models used in the manuscript.

#Create a folder called Manu_perceptions and put the data file (Manu_perceptions_22aug18.csv) in there.
#Run this R script from that folder.

# m1 : 	line 1061 	(base model with nothing in it except individual-level random effect)
# m4 : 	line 2548 	(full covariance structure and just ethnicity predictor)
# m11 : line 3495	(full covariance structure and all machi predictors)
# m19 : line 4827	(full covariance structure and all mestizo predictors)	

#Scipt until line 307 is just wrangling the data into the right format.

#All the major figures in the manuscript are included here.

#I would recommend running this script in chunks. One chunk per Stan model.
#The models are set to run two chains and not a huge number of samples. They may take about an hour each.
#You can make them run faster by reducing the number of samples (using the "iter" argument of the stan() function)


rm (list = ls(all=TRUE))
library(rstan)
library(rethinking)
library(lattice)
library(MASS)
library(graphics)
library(grid)
library(boot) #for inverse logit function



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





##### samples sizes for different combinations of interviewees

num.indiv.ego <- length(unique(d[which(d$target.num==1),"ID"]))					#number indivs with ego responses
num.indiv.ego.ma <- length(unique(d[which(d$target.num==1 & d$Machi==1),"ID"]))	#number machis with ego responses
num.indiv.ego.me <- length(unique(d[which(d$target.num==1 & d$Machi==0),"ID"])) #number mestizos with ego responses
num.indiv.ego.me.at <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$Community=="A"),"ID"])) #number atalaya mestizos with ego responses
num.indiv.ego.me.bm <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$Community=="B"),"ID"])) #number bocamanu mestizos with ego responses
num.indiv.ego.ma.at <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$Community=="A"),"ID"])) #number atalaya machis with ego responses
num.indiv.ego.ma.bm <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$Community=="B"),"ID"])) #number bocamanu machis with ego responses
num.indiv.ego.ma.ty <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$Community=="T"),"ID"])) #number tayakome machis with ego responses
num.indiv.ego.at <- length(unique(d[which(d$target.num==1 & d$Community=="A"),"ID"])) #number atalaya residents with ego responses
num.indiv.ego.bm <- length(unique(d[which(d$target.num==1 & d$Community=="B"),"ID"])) #number bocamanu residents with ego responses
num.indiv.ego.ty <- length(unique(d[which(d$target.num==1 & d$Community=="T"),"ID"])) #number tayakome residents with ego responses
num.indiv.ego.po <- length(unique(d[which(d$target.num==1 & d$Community=="P"),"ID"])) #number post residents with ego responses

num.indiv.in <- length(unique(d[which(d$target.num==2),"ID"]))					#number indivs with ingroup responses
num.indiv.in.ma <- length(unique(d[which(d$target.num==2 & d$Machi==1),"ID"]))	#number machis with ingroup responses
num.indiv.in.me <- length(unique(d[which(d$target.num==2 & d$Machi==0),"ID"])) #number mestizos with ingroup responses
num.indiv.in.me.at <- length(unique(d[which(d$target.num==2 & d$Machi==0 & d$Community=="A"),"ID"])) #number atalaya mestizos with ingroup responses
num.indiv.in.me.bm <- length(unique(d[which(d$target.num==2 & d$Machi==0 & d$Community=="B"),"ID"])) #number bocamanu mestizos with ingroup responses
num.indiv.in.ma.at <- length(unique(d[which(d$target.num==2 & d$Machi==1 & d$Community=="A"),"ID"])) #number atalaya machis with ingroup responses
num.indiv.in.ma.bm <- length(unique(d[which(d$target.num==2 & d$Machi==1 & d$Community=="B"),"ID"])) #number bocamanu machis with ingroup responses
num.indiv.in.at <- length(unique(d[which(d$target.num==2 & d$Community=="A"),"ID"])) #number atalaya residents with in responses
num.indiv.in.bm <- length(unique(d[which(d$target.num==2 & d$Community=="B"),"ID"])) #number bocamanu residents with in responses
num.indiv.in.ty <- length(unique(d[which(d$target.num==2 & d$Community=="T"),"ID"])) #number tayakome residents with in responses
num.indiv.in.po <- length(unique(d[which(d$target.num==2 & d$Community=="P"),"ID"])) #number post residents with in responses

num.indiv.out <- length(unique(d[which(d$target.num==3),"ID"]))					#number indivs with outgroup responses
num.indiv.out.ma <- length(unique(d[which(d$target.num==3 & d$Machi==1),"ID"]))	#number machis with outgroup responses
num.indiv.out.me <- length(unique(d[which(d$target.num==3 & d$Machi==0),"ID"])) #number mestizos with outgroup responses
num.indiv.out.me.at <- length(unique(d[which(d$target.num==3 & d$Machi==0 & d$Community=="A"),"ID"])) #number atalaya mestizos with outgroup responses
num.indiv.out.me.bm <- length(unique(d[which(d$target.num==3 & d$Machi==0 & d$Community=="B"),"ID"])) #number bocamanu mestizos with outgroup responses
num.indiv.out.at <- length(unique(d[which(d$target.num==3 & d$Community=="A"),"ID"])) #number atalaya residents with in responses
num.indiv.out.bm <- length(unique(d[which(d$target.num==3 & d$Community=="B"),"ID"])) #number bocamanu residents with in responses
num.indiv.out.ty <- length(unique(d[which(d$target.num==3 & d$Community=="T"),"ID"])) #number tayakome residents with in responses
num.indiv.out.po <- length(unique(d[which(d$target.num==3 & d$Community=="P"),"ID"])) #number post residents with in responses

num.indiv.ego.ma.mal <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$Sex.1male==1),"ID"]))	#number machi ego males
num.indiv.ego.ma.fem <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$Sex.1male==0),"ID"]))	#number machi ego females
num.indiv.ego.me.mal <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$Sex.1male==1),"ID"]))	#number mest ego males
num.indiv.ego.me.fem <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$Sex.1male==0),"ID"]))	#number mest ego females

num.indiv.in.ma.mal <- length(unique(d[which(d$target.num==2 & d$Machi==1 & d$Sex.1male==1),"ID"]))	#number machi in males
num.indiv.in.ma.fem <- length(unique(d[which(d$target.num==2 & d$Machi==1 & d$Sex.1male==0),"ID"]))	#number machi in females
num.indiv.in.me.mal <- length(unique(d[which(d$target.num==2 & d$Machi==0 & d$Sex.1male==1),"ID"]))	#number mest in males
num.indiv.in.me.fem <- length(unique(d[which(d$target.num==2 & d$Machi==0 & d$Sex.1male==0),"ID"]))	#number mest in females

num.indiv.out.ma.mal <- length(unique(d[which(d$target.num==3 & d$Machi==1 & d$Sex.1male==1),"ID"]))	#number out machi males
num.indiv.out.ma.fem <- length(unique(d[which(d$target.num==3 & d$Machi==1 & d$Sex.1male==0),"ID"]))	#number out machi females
num.indiv.out.me.mal <- length(unique(d[which(d$target.num==3 & d$Machi==0 & d$Sex.1male==1),"ID"]))	#number out mest males
num.indiv.out.me.fem <- length(unique(d[which(d$target.num==3 & d$Machi==0 & d$Sex.1male==0),"ID"]))	#number out mest females

num.indiv.ego.adol <- length(unique(d[which(d$target.num==1 & d$adol.less20==1),"ID"]))		#number ego adol
num.indiv.in.adol <- length(unique(d[which(d$target.num==2 & d$adol.less20==1),"ID"]))		#number in adol
num.indiv.out.adol <- length(unique(d[which(d$target.num==3 & d$adol.less20==1),"ID"]))		#number out adol

num.indiv.ego.ad <- length(unique(d[which(d$target.num==1 & d$mat.20to50==1),"ID"]))		#number ego ad
num.indiv.in.ad <- length(unique(d[which(d$target.num==2 & d$mat.20to50==1),"ID"]))			#number in ad
num.indiv.out.ad <- length(unique(d[which(d$target.num==3 & d$mat.20to50==1),"ID"]))		#number out ad

num.indiv.ego.old <- length(unique(d[which(d$target.num==1 & d$old.over50==1),"ID"]))		#number ego old
num.indiv.in.old <- length(unique(d[which(d$target.num==2 & d$old.over50==1),"ID"]))		#number in old
num.indiv.out.old <- length(unique(d[which(d$target.num==3 & d$old.over50==1),"ID"]))		#number out old

num.indiv.ego.ma.ed <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EdMes==1),"ID"]))		#number machi ego edu
num.indiv.in.ma.ed <- length(unique(d[which(d$target.num==2 & d$Machi==1 & d$EdMes==1),"ID"]))		#number machi in edu
num.indiv.out.ma.ed <- length(unique(d[which(d$target.num==3 & d$Machi==1 & d$EdMes==1),"ID"]))		#number machi out edu

num.indiv.ego.ma.lab <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$LabMes==1),"ID"]))	#number machi ego lab
num.indiv.in.ma.lab <- length(unique(d[which(d$target.num==2 & d$Machi==1 & d$LabMes==1),"ID"]))	#number machi in lab
num.indiv.out.ma.lab <- length(unique(d[which(d$target.num==3 & d$Machi==1 & d$LabMes==1),"ID"]))	#number machi out lab

num.indiv.ego.ma.com <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$ComMes==1),"ID"]))	#number machi ego com
num.indiv.in.ma.com <- length(unique(d[which(d$target.num==2 & d$Machi==1 & d$ComMes==1),"ID"]))	#number machi in com
num.indiv.out.ma.com <- length(unique(d[which(d$target.num==3 & d$Machi==1 & d$ComMes==1),"ID"]))	#number machi out com

num.indiv.ego.me.fam <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$FamMat==1),"ID"]))	#number mest ego fam
num.indiv.in.me.fam <- length(unique(d[which(d$target.num==2 & d$Machi==0 & d$FamMat==1),"ID"]))	#number mest in fam
num.indiv.out.me.fam <- length(unique(d[which(d$target.num==3 & d$Machi==0 & d$FamMat==1),"ID"]))	#number mest out fam

num.indiv.ego.me.emp <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$EmpMat==1),"ID"]))	#number mest ego emp
num.indiv.in.me.emp <- length(unique(d[which(d$target.num==2 & d$Machi==0 & d$EmpMat==1),"ID"]))	#number mest in emp
num.indiv.out.me.emp <- length(unique(d[which(d$target.num==3 & d$Machi==0 & d$EmpMat==1),"ID"]))	#number mest out emp
num.indiv.ego.ma.emp <- length(unique(d[which(d$target.num==1 & d$Machi==1 & d$EmpMat==1),"ID"]))	#number machi ego emp

num.indiv.ego.me.cty <- length(unique(d[which(d$target.num==1 & d$Machi==0 & d$CtyMat==1),"ID"]))	#number mest ego cty
num.indiv.in.me.cty <- length(unique(d[which(d$target.num==2 & d$Machi==0 & d$CtyMat==1),"ID"]))	#number mest in cty
num.indiv.out.me.cty <- length(unique(d[which(d$target.num==3 & d$Machi==0 & d$CtyMat==1),"ID"]))	#number mest out cty


num.indiv.ego.ty 	#ego tayakome
num.indiv.ego.bm 	#ego boca manu
num.indiv.ego.at 	#ego atalaya
num.indiv.ego.ma.bm #machis in boca manu
num.indiv.ego.ma.at #machis in atalaya

(num.indiv.ego.ty + 1)/79 #79 adults in Tayakome (plus technician)
num.indiv.ego.bm/80 #approx 80 adults in Boca Manu
num.indiv.ego.at/65 #approx 65 adults in Atalaya


num.indiv.ego.ma 	#ego machis
num.indiv.ego.me 	#ego mestizos
num.indiv.in.ma 	#in machis
num.indiv.in.me 	#in mestizos
num.indiv.out.ma 	#out machis
num.indiv.out.me 	#out mestizos

num.indiv.ego.ma.ty #machis in tayakome
num.indiv.ego.me.bm #mestizos in boca manu
num.indiv.ego.me.at #mestizos in atalaya


num.indiv.ego.ma.fem 	#number machi ego females
num.indiv.ego.ma.mal	#number machi ego males
num.indiv.in.ma.fem 	#number machi in females
num.indiv.in.ma.mal 	#number machi in males
num.indiv.out.ma.fem 	#number out machi females
num.indiv.out.ma.mal 	#number out machi males

num.indiv.ego.me.mal 	#number mest ego males
num.indiv.ego.me.fem 	#number mest ego females
num.indiv.in.me.mal 	#number mest in males
num.indiv.in.me.fem 	#number mest in females
num.indiv.out.me.mal 	#number out mest males
num.indiv.out.me.fem 	#number out mest females

num.indiv.ego.adol		#number ego adol
num.indiv.in.adol 		#number in adol
num.indiv.out.adol 		#number out adol

num.indiv.ego.ad 		#number ego ad
num.indiv.in.ad 		#number in ad
num.indiv.out.ad 		#number out ad

num.indiv.ego.old 		#number ego old
num.indiv.in.old 		#number in old
num.indiv.out.old 		#number out old

num.indiv.ego.ma.ed 	#number machi ego edu
num.indiv.in.ma.ed 		#number machi in edu
num.indiv.out.ma.ed 	#number machi out edu

num.indiv.ego.ma.lab 	#number machi ego lab
num.indiv.in.ma.lab 	#number machi in lab
num.indiv.out.ma.lab 	#number machi out lab

num.indiv.ego.ma.com 	#number machi ego com
num.indiv.in.ma.com 	#number machi in com
num.indiv.out.ma.com 	#number machi out com

num.indiv.ego.me.fam 	#number mest ego fam
num.indiv.in.me.fam 	#number mest in fam
num.indiv.out.me.fam 	#number mest out fam

num.indiv.ego.me.emp 	#number mest ego emp
num.indiv.in.me.emp 	#number mest in emp
num.indiv.out.me.emp 	#number mest out emp
num.indiv.ego.ma.emp 	#number machi ego emp

num.indiv.ego.me.cty 	#number mest ego cty
num.indiv.in.me.cty 	#number mest in cty
num.indiv.out.me.cty 	#number mest out cty

num.indiv.in.ty
num.indiv.in.po
num.indiv.out.bm
num.indiv.in.at
num.indiv.in.ma.at
num.indiv.in.ma.bm

#number of people answering each question, by ethnicity, by target
table( d$question, d$target.num, d$Machi )


K <- length(unique(d$question)) 						#number of questions
J <- length(unique(d$newID)) 							#number of people
N <- nrow(d) 											#number of responses across all targets
nummach <- length(unique(d[which(d$Machi==1),"ID"]))	#number machis
nummest <- length(unique(d[which(d$Machi==0),"ID"]))	#number mestizos






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
print(props_e, digits=2)

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
print(props_o, digits=2)



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
print(props_i, digits=2)

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

pdf(file="./Fig_raw_props.pdf", 
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
print(props_BA, digits=2)

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
print(props_Bc, digits=2)

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
print(props_Ac, digits=2)

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
print(props_Tc, digits=2)

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

pdf(file="./Fig_ego_raw_props_cards.pdf", 
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



########################### density plotting function

#Richard's density plot function
denschart3 <- function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
    pt.cex = cex, bg = par("bg"), 
    color = "grey20", colorHPDI ="grey60", HPDI=0.9, vline = NULL,
    gcolor = par("fg"), lcolor = "gray", xlim = range(unlist(x)), yvals = 1:length(x), yextra=0.7,
    main = NULL, xlab = NULL, ylab = NULL, height=0.7 , border=NA, adjust=1, ...) 
  {
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.list(x)) 
        stop("'x' must be a list of vectors or matrices")
    n <- length(x)
    glabels <- NULL
    if (is.list(x)) {
        if (is.null(labels)) 
            labels <- names(x)
        if (is.null(labels)) 
            labels <- as.character(1L:n)
        labels <- rep_len(labels, n)
        #if (is.null(groups)) 
        #    groups <- col(x, as.factor = TRUE)
        #glabels <- levels(groups)
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- yvals #o    						#vertical spacing
        ylim <- c(0.2, max(y) + yextra) #n + 1)
    }
    else {
        # sub-groups, so need more rows
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        #mtext(labs, side = 2, line = -1 #0.4, #loffset,           #### y-labels
        #      at = y, adj = 1, 
        #      col = "black", las = 2, cex = cex, ...)
        
        #text(labels=labs, x=-5.2, y=y, pos=2, adj=1)
    }

    #vertical lines
    if ( !is.null(vline) ) {
    	for ( t in 1:length(vline) ) {
    		lines(x=list( x=c(vline[t],vline[t]), y=c(0,max(y)+0.5) ), lty=2, lwd=0.75)
    	} # for t
    } #if


    # draw densities at each y offset
    for ( i in 1:n ) {
        a <- density( x[[i]] , adjust=adjust )
        a$y <- a$y/max(a$y) * height + y[i] - 0.3
        polygon( a$x , a$y , col=color , border=NA )    ################################border=border
        Cuts <- HPDI( x[[i]] , HPDI)
        XX <- a$x[which(a$x > Cuts[1] & a$x < Cuts[2])]
        YY <- a$y[which(a$x > Cuts[1] & a$x < Cuts[2])]
        polygon( c(min(XX), XX, max(XX)), c(min(a$y), YY, min(a$y)),
          col=colorHPDI, border=NA )
    }

    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
        }
    }
    #axis(side=1, at=c(-6,-4,-2,0,2,4,6))
    #box()
    #title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}








####### m1: IRT for responses by target, rand intercept for each person for each target  ############################
#######				separate axes for each target

#model_file_1 <- "./models/m1.stan"

model_code_1 <- "
data {
  int<lower=1> J;         		// number of interviewees
  int<lower=1> K;         		// number of questions
  int<lower=1> T;       		// number of targets (ego, ingroup, outgroup)
  int<lower=1> N;         		// number of answers to questions (observations)
  int<lower=1,upper=J> jj[N];   // interviewee ID for observation n
  int<lower=1,upper=K> kk[N];   // question for observation n
  int<lower=1,upper=T> tt[N]; 	// target for observation n
  int<lower=0,upper=1> y[N];  	// response (1 or 0) for obs n
}

parameters {
  real beta[T,K];       		//separate betas and gammas for each target
  real<lower=0> gamma[T,K];
  real a0[T];             		// mean interviewee location in latent dimension (mean ability intercept), one for each target
  real aIndiv[T,J];       		// location of people (differing from the mean), i.e., random effect for person. rows=targets, columns=individuals
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




##################
#plot logistic response functions for a given question and latent trait estimates of indivs

questions <- quest_names.e #from above

# ego responses
pdf(file="./m1_ego_logistic_responses.pdf", 
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

pdf(file="./m1_in_logistic_responses.pdf", 
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

pdf(file="./m1_out_logistic_responses.pdf", 
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

str(disc_list.e)
disc_list.e <- rev(disc_list.e) #reverse order for plotting
str(disc_list.e)


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

str(disc_list.i)
disc_list.i <- rev(disc_list.i) #reverse order for plotting
str(disc_list.i)


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

str(disc_list.o)
disc_list.o <- rev(disc_list.o) #reverse order for plotting
str(disc_list.o)



#### density plot of responses
pdf(file="./m1_disc_dens.pdf", 
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
names(all_ego)
colnames(all_ego)[12] <- c("ord")

pdf(file="./m1_ego_location_highlights.pdf",
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

ID_key2 #match newID to original ID



############################### plot ego vs. ingroup guess individual positions on latent axes, highlighting experience types

pdf(file="./m1_ego_vs_in.pdf", 
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

pdf(file="./m1_ego_vs_out.pdf", 
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







####### m2: IRT for responses by target, covarying rand intercepts for each person for each target  ############################
#######				separate axes for each target, covarying disriminations and difficulties
####### Doesn't fit well with this parameterization

#model_file_2 <- "./models/m2.stan"

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
m2 <- stan( model_code=model_code_2,
            #file=model_file_2,
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
####### 	No predictors

#model_file_3 <- "./models/m3.stan"

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
m3 <- stan( model_code=model_code_3,
            #file=model_file_3,
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

#model_file_4 <- "./models/m4.stan"

model_code_4 <- "
data {
  int<lower=1> J; 				        // number of interviewees
  int<lower=1> K; 				        // number of questions
  int<lower=1> T;				        // number of targets (ego, ingroup, outgroup)
  int<lower=1> N; 				        // number of answers to questions (observations)
  int<lower=1,upper=J> jj[N]; 	  		// interviewee ID for observation n
  int<lower=1,upper=K> kk[N]; 	  		// question for observation n
  int<lower=1,upper=T> tt[N];	    	// target for observation n
  int<lower=0,upper=1> y[N]; 	    	// response (1 or 0) for obs n

  int<lower=1,upper=2> Machi[N];		//Matsigenka ethnicity: 1=yes, 2=no, for each obs n
}

transformed data {
  vector[3] zeros = [0,0,0]'; 		//column vectors of 0s, note transpose at the end to turn into row vectors
  vector[3] ones = [1,1,1]'; 	  	//column vectors of 1s
}

parameters {
  vector[T] zInt[J]; 				        //J-array of column T-vectors of intercept z-scores for each individual
  vector[T] muInt[2];				        //intercept means: 2-array of T-vectors, one vector for each ethnicity
  vector<lower=0>[T] sigmaInt[2];	   		//intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[T] L_R_a[2];	 		//2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[T] muBeta;					        //vector of beta means for each target T
  vector<lower=0>[T] sigmaBeta;		   		//vector of beta stdevs for each target T	

  vector[T] muGamma;		             	//vector of gamma means for each target T
  vector<lower=0>[T] sigmaGamma;	   		//vector of gamma stdevs for each target T

  matrix[T*2,K] zQuest;				       	//matrix of question beta and gamma z-scores for each question, rows = betas and gammas, cols = questions
  cholesky_factor_corr[T*2] L_R_q;	 		//cholesky factor for correlation matrix for betas and gammas, a T*2xT*2 matrix
}

transformed parameters {
  vector[T*2] muQuest;
  vector[T*2] sigmaQuest;

  matrix[J,T] off_Int; 							//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
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

  zInt ~ multi_normal(zeros, diag_matrix(ones));	   	//array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 	 	//array of T-vectors of mean intercepts
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				    //use cauchy(0,2)?, array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);				    //array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  }

  to_vector(zQuest) ~ normal(0,1);
  muBeta ~ normal(0,1);
  muGamma ~ normal(0,1);
  sigmaBeta ~ exponential(2);     			//increase expoential parameter to concentrate probability closer to 0
  sigmaGamma ~ exponential(2);
  L_R_q ~ lkj_corr_cholesky(40);			//lower the eta to allow more extreme correlations, Rethinking pg 394


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

  matrix[T,T] R_a[2];		 //2-array of TxT correlation matrices
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

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';								    //reconstruct the correlation matrix to look at in output

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



################### Contrasts of abilities

str(post4)
post0 <- post4
num_samp <- length(post0$lp__)	

machiID <- 	unique(d[,c("newID","Machi")])
Machi <- machiID[,2]

#ego
probs.e.ma <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )        #mean prob for machis
probs.e.ma.indiv <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )  #prob with indiv-level variation
probs.e.me <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_names.e) )		  #mean prob for mestizos
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

p_ma.e_ma.i <- probs.e.ma - probs.i.ma  #inaccuracy in
p_ma.e_me.o <- probs.e.ma - probs.o.me  #inaccuracy out

p_me.e_me.i <- probs.e.me - probs.i.me  #inaccuracy in
p_me.e_ma.o <- probs.e.me - probs.o.ma  #inaccuracy out

p_ma.i_ma.o <- probs.i.ma - probs.o.ma
p_me.i_me.o <- probs.i.me - probs.o.me

p_ma.e_ma.o <- probs.e.ma - probs.o.ma  
p_me.e_me.o <- probs.e.me - probs.o.me

p_accu_diff_out <- abs(p_ma.e_me.o) - abs(p_me.e_ma.o) #contrast of outgroup guess inaccuracies: mestizo - machi 
p_accu_diff_in <- abs(p_ma.e_ma.i) - abs(p_me.e_me.i)




probs.e.ma[probs.e.ma == 0] <- 0.0001 		#to keep kl and jeffreys divergence from exploding, rethinking pg 179
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
dim(p_me.e_ma.o)
mean_inacc_mach_out <- rowMeans(abs(p_me.e_ma.o)) #mean inaccuracy of machi out-group guesses
mean_inacc_mach_in <- rowMeans(abs(p_ma.e_ma.i))

mean_inacc_mest_out <- rowMeans(abs(p_ma.e_me.o))
mean_inacc_mest_in <- rowMeans(abs(p_me.e_me.i))

mean_inacc_out <- cbind(mean_inacc_mach_out, mean_inacc_mest_out)
mean_inacc_in <- cbind(mean_inacc_mach_in, mean_inacc_mest_in)

mean(mean_inacc_mach_out)
mean(mean_inacc_mest_out)

mean(rowMeans(abs(p_me.e_ma.o)))
mean(rowMeans(abs(p_ma.e_me.o)))

mean(rowMeans(p_accu_diff_out))


#mean kl divergence of outgroup and ingroup guesses
mean_kl_mach_out <- rowMeans(kl_me.e_ma.o)
mean_kl_mach_in <- rowMeans(kl_ma.e_ma.i)

mean_kl_mest_out <- rowMeans(kl_ma.e_me.o)
mean_kl_mest_in <- rowMeans(kl_me.e_me.i)



#contrasts
mean_inacc_contr_out <- mean_inacc_mach_out - mean_inacc_mest_out  #contrast of mean outgroup guess inaccuracies: machi - mestizo 
mean_inacc_contr_in <- mean_inacc_mach_in - mean_inacc_mest_in

mean_inacc_contr <- cbind(mean_inacc_contr_out, mean_inacc_contr_in)


mean_jef_ma_me <- rowMeans(jef_ma.e_me.e)				  	#mean jeffreys divergence of mach and mest ego responses
mean_kl_contr_out <- mean_kl_mach_out - mean_kl_mest_out 	#contrast of mean outgroup guess inaccuracies: machi - mestizo
mean_kl_contr_out2 <- rowMeans(kl_me.e_ma.o - kl_ma.e_ma.i) #mean contrast of outgroup guess inaccuracies: machi - mestizo 
mean_kl_contr_in <- mean_kl_mach_in - mean_kl_mest_in
mean_kl_contr_in2 <- rowMeans(kl_ma.e_me.o - kl_me.e_me.i)

mean_kl_contr <- cbind(mean_jef_ma_me, mean_kl_contr_out, mean_kl_contr_in)
mean_kl_contr2 <- cbind(mean_jef_ma_me, mean_kl_contr_out2, mean_kl_contr_in2)



#### density plot of responses
pdf(file="./m4_response_dens.pdf", 
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
pdf(file="./m4_kl_dens.pdf", 
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
pdf(file="./m4_contrast_dens.pdf", 
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
pdf(file="./m4_mean_kl_dens.pdf", 
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



#### density plot of mean contrasts of kl divergence
pdf(file="./m4_mean_kl_dens2.pdf", 
height=5, width=5)
par(mfcol=c(1,1))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(mean_kl_contr2, col(mean_kl_contr2))),
      labels="",
      adjust=1,
      color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 0.75),
          clip(0.05,1,0,16) #x1, x2, y1, y2	clip at x=0   
 )
text(x=0.375, y=3.7, labels="Matsi - Mest", cex=0.75)
text(x=0.5, y=2.6, labels="Mean Jeffreys Divergence", cex=0.65)
axis(side=2,
	col="white",
	at=c(1:3), 
	labels=c("In-group","Out-Group","Ego"), las=1, cex.axis=1)	#left
axis(side=1, at=c(0, 0.25, 0.5, 0.75), labels=c(0, 0.25, 0.5, 0.75), cex.axis=0.75) #bottom
lines(x=list( x=c(0,0), y=c(0,3.5) ), lty=2, lwd=0.75)


mtext("Mean Difference in KL Divergence", side = 1, outer = TRUE, cex = 0.7, line = 2.2)

graphics.off()







####### m11: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									all machi predictors
####### 		separate axes for each target, covarying disriminations and difficulties

#model_file_11 <- "./m11.stan"

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

  matrix[J,T] off_Int; 				//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
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
  aEdMes ~ multi_normal(zeros, diag_matrix(ones));	//array of T-vectors of experience offsets to the mean for each target
  aLabMes ~ multi_normal(zeros, diag_matrix(ones));
  aComMes ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);				//array of T-vectors of stdevs from mean of each intercept
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
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';		//construct cov matrix from colesky factors of cov matrix to look at in output
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
m11 <- stan( model_code=model_code_11,
            #file=model_file_11,
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


################### Contrasts of probabilities of answer=positive

str(post11)
post0 <- post11
num_samp <- length(post0$lp__)	


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


p_ma.acc.no_typ11 <- probs.o.ma.no11 - probs.e.me.typ11 	#inaccuracy of machi outgroup guesses: machi no exp outgroup guess - average mestizo ego response
p_ma.acc.com_typ11 <- probs.o.ma.com11 - probs.e.me.typ11
p_ma.acc.lab_typ11 <- probs.o.ma.lab11 - probs.e.me.typ11
p_ma.acc.ed_typ11 <- probs.o.ma.ed11 - probs.e.me.typ11


probs.e.me.typ11[probs.e.me.typ11 == 0] <- 0.0001  #to keep kl divergence from exploding, rethinking pg 179
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




p_ma.acc.no_ed.o11 <- abs(p_ma.acc.no_typ11) - abs(p_ma.acc.ed_typ11)		#difference in inaccuracy of outgroup guesses: machi no exp inaccuracy - machi edu exp inaccuracy
mean(colMeans(p_ma.acc.no_ed.o11))
mean(rowMeans(p_ma.acc.no_ed.o11))

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

#propotion of machis giving ego responses, with each experience combination
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


sum(prop_mach_ed, prop_mach_lab, prop_mach_com,
    prop_mach_ed_lab, prop_mach_ed_com, prop_mach_lab_com,
    prop_mach_no, prop_mach_ed_lab_com)


dim(probs.e.ma.no11)


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



probs.e.ma.weighted.mean[probs.e.ma.weighted.mean == 0] <- 0.0001  #to keep kl divergence from exploding, rethinking pg 179
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





#mean inaccuracy of machi outgroup and ingroup guesses, by experience type
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



######################## contrasts of inaccuracy of outgroup and ingroup guesses by experience type for machis

##out
ma.acc.o.no_ed <- mean_inacc_mach_no_out - mean_inacc_mach_ed_out
ma.acc.o.no_lab <- mean_inacc_mach_no_out - mean_inacc_mach_lab_out
ma.acc.o.no_com <- mean_inacc_mach_no_out - mean_inacc_mach_com_out

ma.acc.o.ed_lab <- mean_inacc_mach_ed_out - mean_inacc_mach_lab_out
ma.acc.o.ed_com <- mean_inacc_mach_ed_out - mean_inacc_mach_com_out

ma.acc.o.lab_com <- mean_inacc_mach_lab_out - mean_inacc_mach_com_out

##in
ma.acc.i.ed_no <- mean_inacc_mach_ed_in - mean_inacc_mach_no_in
ma.acc.i.lab_no <- mean_inacc_mach_lab_in - mean_inacc_mach_no_in
ma.acc.i.com_no <- mean_inacc_mach_com_in - mean_inacc_mach_no_in

ma.acc.i.lab_ed <- mean_inacc_mach_lab_in - mean_inacc_mach_ed_in
ma.acc.i.com_ed <- mean_inacc_mach_com_in - mean_inacc_mach_ed_in

ma.acc.i.com_lab <- mean_inacc_mach_com_in - mean_inacc_mach_lab_in


#make list for plotting
cont_list.abils <- list(
                  ma.acc.o.no_ed,   #1
                  ma.acc.o.no_lab,  #2
                  ma.acc.o.no_com,  #3
                          
                  ma.acc.o.ed_lab,  #4
                  ma.acc.o.ed_com,  #5

                  ma.acc.o.lab_com, #6

                  ma.acc.i.ed_no,  #7
                  ma.acc.i.lab_no,  #8
                  ma.acc.i.com_no,  #9
                          
                  ma.acc.i.lab_ed, #10
                  ma.acc.i.com_ed, #11

                  ma.acc.i.com_lab  #12
        )


names(cont_list.abils) <- c("InAccu.o none - edu",
                            "InAccu.o none - lab",
                            "InAccu.o none - com",
                            "InAccu.o edu - lab",
                            "InAccu.o edu - com",
                            "InAccu.o lab - com",

                            "InAccu.i edu - none",
                            "InAccu.i lab - none",
                            "InAccu.i com - none",
                            "InAccu.i lab - edu",
                            "InAccu.i com - edu",
                            "InAccu.i com - lab")

str(cont_list.abils)


cont_list_col1 <- list( cont_list.abils[[1]], #ma.acc.o.no_ed
            cont_list.abils[[7]], #me.acc.i.ed_no
            cont_list.abils[[8]], #me.acc.i.lab_no
            cont_list.abils[[9]]  #me.acc.i.com_no
             )
names(cont_list_col1) <- names(cont_list.abils)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
str(cont_list_col1)


cont_list_col2 <- list( cont_list.abils[[2]], #ma.acc.o.no_lab
            cont_list.abils[[4]], #ma.acc.o.ed_lab
            cont_list.abils[[10]], #me.acc.i.lab_ed
            cont_list.abils[[11]] #me.acc.i.com_ed
             )
names(cont_list_col2) <- rownames(cont_list.abils)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
str(cont_list_col2)


cont_list_col3 <- list( cont_list.abils[[3]], #ma.acc.o.no_com
            cont_list.abils[[5]], #ma.acc.o.ed_com
            cont_list.abils[[6]], #ma.acc.o.lab_com
            cont_list.abils[[12]] #me.acc.i.com_lab
             )
names(cont_list_col3) <- rownames(cont_list.abils)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
str(cont_list_col3)




#### density plot of contrasts
pdf(file="./m11_inacc_exp_contrasts.pdf", 
height=5, width=5)
#op <- par(no.readonly = TRUE)  #save current graphics settings
par(xpd=TRUE) #clip at figure region, not plot region
#par(mfcol=c(1,3))
par(oma = c(4, 4, 4, 4)) # oma for outer margins: bottom, left, top, right


########## column 1

denschart3( cont_list_col1 , adjust=1 , color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=T, yaxt="n",
          cex=0.8, height=0.7,
          yvals=c(0.75,1.75,2.75,3.75),
          labels=c("","","",""),
          xlim=c(-0.25, 0.25 + 1.5) #(-5, 6+35)
              #range( min(unlist(cont_list_col1))-0.5, max(unlist(cont_list_col1))+33 ) 
 )
axis(side=1, at=c(-0.1,0,0.1), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


lines( list( x=c(-0.25,0.25),
  y=c(1.25,1.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(2.25,2.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(3.25,3.25) ), lwd=4 )

rect(xleft=-0.25,
   ybottom=0.2,
   xright=0.25,
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
          xlim=c(-0.25 - 0.5, 0.25 + 1)
 )
axis(side=1, at=c(-0.1,0,0.1), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


lines( list( x=c(-0.25,0.25),
  y=c(1.25,1.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(2.25,2.25) ), lwd=4 )
lines( list( x=c(-0.25,0.25),
  y=c(3.25,3.25) ), lwd=0.5 )
lines( list( x=c(-0.25,-0.25), y=c(2.25, 3.25) ), lwd=4 )

rect(xleft=-0.25, 
   ybottom=0.2,
   xright=0.25, 
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
          xlim=c(-0.25 - 1, 0.25 + 0.5)
 )
axis(side=1, at=c(-0.1,0,0.1), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


lines( list( x=c(-0.25,0.25),
  y=c(1.25,1.25) ), lwd=4 )
lines( list( x=c(-0.25,0.25),
  y=c(2.25,2.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(3.25,3.25) ), lwd=0.5 )
lines( list( x=c(-0.25,-0.25), y=c(1.25, 2.25) ), lwd=4 )

rect(xleft=-0.25, 
   ybottom=0.2,
   xright=0.25,
   ytop=4.25, 
   lwd = 0.5)

lines( list( x=c(0,0), y=c(0.2,4.25) ), lty=2, lwd=1 ) #vertical line at 0


text(labels="Lab", x=0, y=-0.4, las=1, cex=0.75)  #bottom
text(labels="Edu", x=-0.5, y=-0.4, las=1, cex=0.75)
text(labels="no exp", x=-1, y=-0.4, las=1, cex=0.75)

text(labels="Com", x=0, y=4.5, las=1, cex=0.75)   #top
text(labels="Lab", x=-0.5, y=4.5, las=1, cex=0.75)
text(labels="Edu", x=-1, y=4.5, las=1, cex=0.75)

text(labels="Lab", x=0.4, y=1.75, las=1, cex=0.75)  #right
text(labels="Edu", x=0.4, y=2.75, las=1, cex=0.75)
text(labels="no exp", x=0.4, y=3.75, las=1, cex=0.75, adj=0.3)

text(labels="Com", x=-1.4, y=0.75, las=1, cex=0.75)  #left
text(labels="Lab", x=-1.4, y=1.75, las=1, cex=0.75)
text(labels="Edu", x=-1.4, y=2.75, las=1, cex=0.75)

#text(labels="Mestizos", x=-37, y=2.25, las=3, cex=1, srt=90) #left
mtext("In-Group Guesses", side=2, line=3.4, cex=1, adj=0.15)
text(labels="Out-Group Guesses", x=0.75, y=2.25, cex=1, adj=0.65, srt=270) #right
text(labels="Out-Group Guesses", x=-0.5, y=5, cex=1)  #top
text(labels="In-Group Guesses", x=-0.5, y=-0.9, cex=1) #bottom

graphics.off()




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

str(kl_list.abils)


cont_list_col1 <- list( kl_list.abils[[1]], #ma.acc.o.no_ed
            kl_list.abils[[7]], #me.acc.i.ed_no
            kl_list.abils[[8]], #me.acc.i.lab_no
            kl_list.abils[[9]]  #me.acc.i.com_no
             )
names(cont_list_col1) <- names(kl_list.abils)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
str(cont_list_col1)


cont_list_col2 <- list( kl_list.abils[[2]], #ma.acc.o.no_lab
            kl_list.abils[[4]], #ma.acc.o.ed_lab
            kl_list.abils[[10]], #me.acc.i.lab_ed
            kl_list.abils[[11]] #me.acc.i.com_ed
             )
names(cont_list_col2) <- names(kl_list.abils)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
str(cont_list_col2)


cont_list_col3 <- list( kl_list.abils[[3]], #ma.acc.o.no_com
            kl_list.abils[[5]], #ma.acc.o.ed_com
            kl_list.abils[[6]], #ma.acc.o.lab_com
            kl_list.abils[[12]] #me.acc.i.com_lab
             )
names(cont_list_col3) <- names(kl_list.abils)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
str(cont_list_col3)




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
pdf(file="./m11_kl_exp_contrasts.pdf", 
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


#### density plot of inaccuracy
pdf(file="./m11_inaccuracy_dens.pdf", 
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
pdf(file="./m11_kl_dens.pdf", 
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






####### m19: IRT for responses by target, non-centered cholesky covarying rand intercepts for each person for each target  ############################
#######									all mestizo predictors

#model_file_19 <- "./m19.stan"

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
  vector[T] ones = [1,1,1]'; 	//column vectors of 1s
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

  matrix[J,T] off_Int; 				//matrix of random offsets for each individual from mean of each intercept, rows = indivs, cols = intercepts
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

  zInt ~ multi_normal(zeros, diag_matrix(ones));		//array of intercept z-scores for each indiv, sample three at a time from a normal
  muInt ~ multi_normal(zeros, diag_matrix(ones)); 		//array of T-vectors of mean intercepts
  aFamMat ~ multi_normal(zeros, diag_matrix(ones));  	//array of T-vectors of experience offsets to the mean for each target
  aEmpMat ~ multi_normal(zeros, diag_matrix(ones));
  aCtyMat ~ multi_normal(zeros, diag_matrix(ones));
  for (Mach in 1:2) {
    sigmaInt[Mach] ~ exponential(2);					//array of T-vectors of stdevs from mean of each intercept
    L_R_a[Mach] ~ lkj_corr_cholesky(4);					//array of cholesky factor TxT matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
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

  	R_a[Mach] = L_R_a[Mach] * L_R_a[Mach]';							//reconstruct the correlation matrix to look at in output

  	Cov_a[Mach] = diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach]) * 
  				  diag_pre_multiply(sigmaInt[Mach], L_R_a[Mach])';	//construct cov matrix from colesky factors of cov matrix to look at in output
  } //for

  R_q = L_R_q * L_R_q';
  Cov_q = diag_pre_multiply(sigmaQuest, L_R_q) * diag_pre_multiply(sigmaQuest, L_R_q)';
}
"

data_list_19 <- list(
  J = length(unique(d$newID)),    	#number of people
  K = length(unique(d$question)),   #number of questions
  T = length(unique(d$target.num)), #number of targets
  N = nrow(d),            			#total number of responses
  jj = d$newID,          			#N vector of person IDs
  kk = d$question,          		#N vector of question IDs
  tt = d$target.num,          		#N vector of target numbers
  y = d$response,          			 #N vector of responses

  Machi = ifelse(d[,"Machi"]==1, 1, 2),     #ethnicity converted to index
	FamMat = d$FamMat,						#vector of family experience with machis for each guess
	EmpMat = d$EmpMat,						#vector of employment experience with machis for each guess
	CtyMat = d$CtyMat						#vector of community experience with machis for each guess
)

J <- length(unique(d$newID))
K <- length(unique(d$question))
T <- length(unique(d$target.num))
Mach <- 2

start_19 <- list(
  zInt = matrix(0, nrow=J, ncol=T),
  muInt = array(data=0, dim=c(Mach,T)),
  sigmaInt = array(data=1, dim=c(Mach,T)),
  L_R_a = array(data=0, dim=c(Mach,T,T)),	#initialize cholesky factors with 0

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




################### Contrasts of probabilities of answer=positive

str(post19)
post0 <- post19
num_samp <- length(post0$lp__)	


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

p_me.acc.no_typ <- probs.o.me.no - probs.e.ma.typ 	#inaccuracy of mestizo outgroup guesses
p_me.acc.fam_typ <- probs.o.me.fam - probs.e.ma.typ
p_me.acc.emp_typ <- probs.o.me.emp - probs.e.ma.typ
p_me.acc.cty_typ <- probs.o.me.cty - probs.e.ma.typ



probs.e.ma.typ[probs.e.ma.typ == 0] <- 0.0001  #to keep kl divergence from exploding, rethinking pg 179
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



p_me.acc.no_fam <- abs(p_me.acc.no_typ) - abs(p_me.acc.fam_typ)   #difference in inaccuracy of outgroup guesses
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


sum(prop_mest_fam, prop_mest_emp, prop_mest_cty,
    prop_mest_fam_emp, prop_mest_fam_cty, prop_mest_emp_cty,
    prop_mest_no, prop_mest_fam_emp_cty)


dim(probs.e.me.no)



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




probs.e.me.weighted.mean[probs.e.me.weighted.mean == 0] <- 0.0001  #to keep kl divergence from exploding, rethinking pg 179
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





#mean inaccuracy of mestizo outgroup and ingroup guesses, by experience type
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





######################## contrasts of inaccuracy of outgroup and ingroup guesses by experience type for mestizos

##out
me.acc.o.no_fam <- mean_inacc_mest_no_out - mean_inacc_mest_fam_out
me.acc.o.no_emp <- mean_inacc_mest_no_out - mean_inacc_mest_emp_out
me.acc.o.no_cty <- mean_inacc_mest_no_out - mean_inacc_mest_cty_out

me.acc.o.fam_emp <- mean_inacc_mest_fam_out - mean_inacc_mest_emp_out
me.acc.o.fam_cty <- mean_inacc_mest_fam_out - mean_inacc_mest_cty_out

me.acc.o.emp_cty <- mean_inacc_mest_emp_out - mean_inacc_mest_cty_out

##in
me.acc.i.fam_no <- mean_inacc_mest_fam_in - mean_inacc_mest_no_in
me.acc.i.emp_no <- mean_inacc_mest_emp_in - mean_inacc_mest_no_in
me.acc.i.cty_no <- mean_inacc_mest_cty_in - mean_inacc_mest_no_in

me.acc.i.emp_fam <- mean_inacc_mest_emp_in - mean_inacc_mest_fam_in
me.acc.i.cty_fam <- mean_inacc_mest_cty_in - mean_inacc_mest_fam_in

me.acc.i.cty_emp <- mean_inacc_mest_cty_in - mean_inacc_mest_emp_in


#make list for plotting
cont_list.abils.me <- list(
                  me.acc.o.no_fam,   #1
                  me.acc.o.no_emp,  #2
                  me.acc.o.no_cty,  #3
                          
                  me.acc.o.fam_emp,  #4
                  me.acc.o.fam_cty,  #5

                  me.acc.o.emp_cty, #6

                  me.acc.i.fam_no,  #7
                  me.acc.i.emp_no,  #8
                  me.acc.i.cty_no,  #9
                          
                  me.acc.i.emp_fam, #10
                  me.acc.i.cty_fam, #11

                  me.acc.i.cty_emp  #12
        )


names(cont_list.abils.me) <- c("InAccu.o none - fam",
                            "InAccu.o none - emp",
                            "InAccu.o none - cty",
                            "InAccu.o fam - emp",
                            "InAccu.o fam - cty",
                            "InAccu.o emp - cty",

                            "InAccu.i fam - none",
                            "InAccu.i emp - none",
                            "InAccu.i cty - none",
                            "InAccu.i emp - fam",
                            "InAccu.i cty - fam",
                            "InAccu.i cty - emp")

str(cont_list.abils.me)


cont_list_col1 <- list( cont_list.abils.me[[1]], #me.acc.o.no_fam
            cont_list.abils.me[[7]], #me.acc.i.fam_no
            cont_list.abils.me[[8]], #me.acc.i.emp_no
            cont_list.abils.me[[9]]  #me.acc.i.cty_no
             )
names(cont_list_col1) <- names(cont_list.abils.me)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
str(cont_list_col1)


cont_list_col2 <- list( cont_list.abils.me[[2]], #ma.acc.o.no_emp
            cont_list.abils.me[[4]], #ma.acc.o.fam_emp
            cont_list.abils.me[[10]], #me.acc.i.emp_fam
            cont_list.abils.me[[11]] #me.acc.i.cty_fam
             )
names(cont_list_col2) <- rownames(cont_list.abils.me)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
str(cont_list_col2)


cont_list_col3 <- list( cont_list.abils.me[[3]], #ma.acc.o.no_cty
            cont_list.abils.me[[5]], #ma.acc.o.fam_cty
            cont_list.abils.me[[6]], #ma.acc.o.emp_cty
            cont_list.abils.me[[12]] #me.acc.i.cty_emp
             )
names(cont_list_col3) <- rownames(cont_list.abils)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
str(cont_list_col3)




#### density plot of contrasts
pdf(file="./m19_inacc_exp_contrasts.pdf", 
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
          xlim=c(-0.25, 0.25 + 1.5) 
 )
axis(side=1, at=c(-0.1,0,0.1), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)

#abline(v=0)
lines( list( x=c(-0.25,0.25), 
  y=c(1.25,1.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(2.25,2.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(3.25,3.25) ), lwd=4 )

rect(xleft=-0.25, 
   ybottom=0.2,
   xright=0.25, 
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
          xlim=c(-0.25 - 0.5, 0.25 + 1)
 )
axis(side=1, at=c(-0.1,0,0.1), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)

#abline(v=0)
lines( list( x=c(-0.25,0.25),
  y=c(1.25,1.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(2.25,2.25) ), lwd=4 )
lines( list( x=c(-0.25,0.25),
  y=c(3.25,3.25) ), lwd=0.5 )
lines( list( x=c(-0.25,-0.25), y=c(2.25, 3.25) ), lwd=4 )

rect(xleft=-0.25, 
   ybottom=0.2,
   xright=0.25, 
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
          xlim=c(-0.25 - 1, 0.25 + 0.5)
 )
axis(side=1, at=c(-0.1,0,0.1), col="white", col.ticks="black", cex.axis=0.5, tcl=-0.3, padj=-2)


lines( list( x=c(-0.25,0.25),
  y=c(1.25,1.25) ), lwd=4 )
lines( list( x=c(-0.25,0.25),
  y=c(2.25,2.25) ), lwd=0.5 )
lines( list( x=c(-0.25,0.25),
  y=c(3.25,3.25) ), lwd=0.5 )
lines( list( x=c(-0.25,-0.25), y=c(1.25, 2.25) ), lwd=4 )

rect(xleft=-0.25,
   ybottom=0.2,
   xright=0.25, 
   ytop=4.25, 
   lwd = 0.5)

lines( list( x=c(0,0), y=c(0.2,4.25) ), lty=2, lwd=1 ) #vertical line at 0


text(labels="Emp", x=0, y=-0.4, las=1, cex=0.75)  #bottom
text(labels="Fam", x=-0.5, y=-0.4, las=1, cex=0.75)
text(labels="no exp", x=-1, y=-0.4, las=1, cex=0.75)

text(labels="Cty", x=0, y=4.5, las=1, cex=0.75)   #top
text(labels="Emp", x=-0.5, y=4.5, las=1, cex=0.75)
text(labels="Fam", x=-1, y=4.5, las=1, cex=0.75)

text(labels="Emp", x=0.4, y=1.75, las=1, cex=0.75)  #right
text(labels="Fam", x=0.4, y=2.75, las=1, cex=0.75)
text(labels="no exp", x=0.4, y=3.75, las=1, cex=0.75, adj=0.3)

text(labels="Cty", x=-1.4, y=0.75, las=1, cex=0.75)  #left
text(labels="Emp", x=-1.4, y=1.75, las=1, cex=0.75)
text(labels="Fam", x=-1.4, y=2.75, las=1, cex=0.75)

#text(labels="Mestizos", x=-37, y=2.25, las=3, cex=1, srt=90) #left
mtext("In-Group Guesses", side=2, line=3.4, cex=1, adj=0.15)
text(labels="Out-Group Guesses", x=0.75, y=2.25, cex=1, adj=0.65, srt=270) #right
text(labels="Out-Group Guesses", x=-0.5, y=5, cex=1)  #top
text(labels="In-Group Guesses", x=-0.5, y=-0.9, cex=1) #bottom

graphics.off()




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

str(kl_list.abils.me)


cont_list_col1 <- list( kl_list.abils.me[[1]], #me.acc.o.no_fam
            kl_list.abils.me[[7]], #me.acc.i.fam_no
            kl_list.abils.me[[8]], #me.acc.i.emp_no
            kl_list.abils.me[[9]]  #me.acc.i.cty_no
             )
names(cont_list_col1) <- names(kl_list.abils.me)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
str(cont_list_col1)


cont_list_col2 <- list( kl_list.abils.me[[2]], #ma.acc.o.no_emp
            kl_list.abils.me[[4]], #ma.acc.o.fam_emp
            kl_list.abils.me[[10]], #me.acc.i.emp_fam
            kl_list.abils.me[[11]] #me.acc.i.cty_fam
             )
names(cont_list_col2) <- rownames(kl_list.abils.me)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
str(cont_list_col2)


cont_list_col3 <- list( kl_list.abils.me[[3]], #ma.acc.o.no_cty
            kl_list.abils.me[[5]], #ma.acc.o.fam_cty
            kl_list.abils.me[[6]], #ma.acc.o.emp_cty
            kl_list.abils.me[[12]] #me.acc.i.cty_emp
             )
names(cont_list_col3) <- rownames(kl_list.abils)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
str(cont_list_col3)




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
pdf(file="./m19_kl_exp_contrasts.pdf", 
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
pdf(file="./m19_inaccuracy_dens.pdf", 
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
pdf(file="./m19_inaccuracy_in_dens.pdf", 
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
pdf(file="./m19_kl_dens.pdf", 
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
pdf(file="./m19_kl_in_dens.pdf", 
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





######################################################################################################################

##################################Combined plots




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

str(mean_ego_list)


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

str(mean_kl_out_list)
str(mean_kl_in_list)


#
pdf(file="./mean_guess_kl_all_dens2.pdf", 
height=7, width=7)
#par(mfcol=c(1,3))
layout( t(c(1,2,3)), widths=c(1.5,1,1) )
par(mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(mean_ego_list),
		yvals=c(1,2,3,4, 5.5,6.5,7.5,8.5, 10,11), 
      	labels="",
      	adjust=1,
      	color="black",
        colorHPDI=grey(0.45),
        HPDI=0.9,
        border=NA, yaxt="n",
        cex=0.8, height=0.7,
        xlim=range(-0.15, 0.8),
        clip(0.07,1,0,16), #x1, x2, y1, y2	clip at x=0 
        vline=c(0.35,0.5,0.65) 
 )
text(x=0.5, y=12, labels="Mean Ego Response", cex=1)

text( x=rep(-0.1,10), y=c(1:4, 5.5,6.5,7.5,8.5, 10,11), labels=rev(names(mean_ego_list)), cex=1, adj=0 )
axis(side=1, at=c(0.2,0.5,0.8), labels=c(0.2,0.5,0.8), cex.axis=0.75) #bottom


lines(x=list( x=c(-0.5,1.5), y=c(4.75,4.75) ), lty=1, lwd=0.75) #horizontal line
lines(x=list( x=c(-0.5,1.5), y=c(9.25,9.25) ), lty=1, lwd=0.75) #horizontal line

mtext("Probability Response=Positive", side = 1, outer = TRUE, cex = 0.7, line = 2.2, adj=0.21)



denschart3( rev(mean_kl_out_list),
			yvals=c(1,2,3,4, 5.5,6.5,7.5,8.5, 10,11), 
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
text(x=0.5, y=12, labels="Mean Out-Group Guess", cex=1)

axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75) #bottom


lines(x=list( x=c(-0.5,1.5), y=c(4.75,4.75) ), lty=1, lwd=0.75) #horizontal line
lines(x=list( x=c(-0.5,1.5), y=c(9.25,9.25) ), lty=1, lwd=0.75) #horizontal line



denschart3( rev(mean_kl_in_list),
			yvals=c(1,2,3,4, 5.5,6.5,7.5,8.5, 10,11), 
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
text(x=0.15, y=12, labels="Mean In-Group Guess", cex=1)

axis(side=1, at=c(0,0.15,0.3), labels=c(0,0.15,0.3), cex.axis=0.75) #bottom


lines(x=list( x=c(-0.5,0.5), y=c(4.75,4.75) ), lty=1, lwd=0.75) #horizontal line
lines(x=list( x=c(-0.5,0.5), y=c(9.25,9.25) ), lty=1, lwd=0.75) #horizontal line

mtext("Mean KL Divergence from Ego Response Probability", side = 1, outer = TRUE, cex = 0.7, line = 2.2, adj=0.9)
mtext("A", side = 2, outer = TRUE, cex = 1.5, line = 0, adj=0, padj=-14.75, las=1)
mtext("B", side = 2, outer = TRUE, cex = 1.5, line = 0, adj=0, padj=-7.75, las=1)
mtext("C", side = 2, outer = TRUE, cex = 1.5, line = 0, adj=0, padj=4.75, las=1)

graphics.off()




#### density plot of innaccuracy contrasts, just m11 and m19
pdf(file="./mean_guess_kl_m11m19_dens.pdf", 
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
pdf(file="./mean_guess_kl_m4_dens.pdf", 
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





##################### compare in-group reconstructions

pdf(file="./ingroup_compare_dens.pdf", 
height=5, width=10)
par(mfcol=c(1,4))
par(mar = c(0, 0, 0, 0), oma = c(4, 12, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


denschart3( rev(split(probs.e.ma, col(probs.e.ma))),
		  yextra=1.5, #writing space at top of column
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1) 
 )
text(x=0.5, y=15, labels="Mats ego", cex=1)
axis(side=2,
	col="white",
	at=c(1:14), 
	labels=rev(quest_names.e), las=1, cex.axis=1)	#left
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(probs.e.ma.weighted.mean, col(probs.e.ma.weighted.mean))),
		  yextra=1.5, #writing space at top of column
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1)  
 )
text(x=0.5, y=15, labels="Mats m11 approx", cex=1)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(probs.e.me, col(probs.e.me))),
		  yextra=1.5, #writing space at top of column
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1)  
 )
text(x=0.5, y=15, labels="Mest ego", cex=1)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)


denschart3( rev(split(probs.e.me.weighted.mean, col(probs.e.me.weighted.mean))),
		  yextra=1.5, #writing space at top of column
		  labels="",
		  adjust=1,
		  color="black",
          colorHPDI=grey(0.45),
          HPDI=0.9,
          border=NA, yaxt="n",
          cex=0.8, height=0.7,
          xlim=range(0, 1)  
 )
text(x=0.5, y=15, labels="Mest m19 approx", cex=1)
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=0.75) #bottom
lines(x=list( x=c(0.5,0.5), y=c(0,14.5) ), lty=2, lwd=0.75)


mtext("Probability Response=Positive", side = 1, outer = TRUE, cex = 0.7, line = 2.2)

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
str(cont_mean_resp_list)


cont_list_col1 <- list( cont_mean_resp_list[[1]], #ma.resp.no_ed
            cont_mean_resp_list[[7]], #me.resp.fam_no
            cont_mean_resp_list[[8]], #me.resp.emp_no
            cont_mean_resp_list[[9]]  #me.resp.cty_no
             )
names(cont_list_col1) <- names(cont_mean_resp_list)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
str(cont_list_col1)


cont_list_col2 <- list( cont_mean_resp_list[[2]], #ma.acc.no_lab
            cont_mean_resp_list[[4]], #ma.resp.ed_lab
            cont_mean_resp_list[[10]], #me.resp.emp_fam
            cont_mean_resp_list[[11]] #me.resp.cty_fam
             )
names(cont_list_col2) <- rownames(cont_mean_resp_list)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
str(cont_list_col2)


cont_list_col3 <- list( cont_mean_resp_list[[3]], #ma.acc.no_com
            cont_mean_resp_list[[5]], #ma.resp.ed_com
            cont_mean_resp_list[[6]], #ma.resp.lab_com
            cont_mean_resp_list[[12]] #me.resp.cty_emp
             )
names(cont_list_col3) <- rownames(cont_mean_resp_list)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
str(cont_list_col3)



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
pdf(file="./mean_ego_contrasts.pdf", 
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
str(jef_mean_resp_list)


cont_list_col1 <- list( jef_mean_resp_list[[1]], #ma.resp.no_ed
            jef_mean_resp_list[[7]], #me.resp.fam_no
            jef_mean_resp_list[[8]], #me.resp.emp_no
            jef_mean_resp_list[[9]]  #me.resp.cty_no
             )
names(cont_list_col1) <- names(jef_mean_resp_list)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
str(cont_list_col1)


cont_list_col2 <- list( jef_mean_resp_list[[2]], #ma.acc.no_lab
            jef_mean_resp_list[[4]], #ma.resp.ed_lab
            jef_mean_resp_list[[10]], #me.resp.emp_fam
            jef_mean_resp_list[[11]] #me.resp.cty_fam
             )
names(cont_list_col2) <- rownames(jef_mean_resp_list)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
str(cont_list_col2)


cont_list_col3 <- list( jef_mean_resp_list[[3]], #ma.acc.no_com
            jef_mean_resp_list[[5]], #ma.resp.ed_com
            jef_mean_resp_list[[6]], #ma.resp.lab_com
            jef_mean_resp_list[[12]] #me.resp.cty_emp
             )
names(cont_list_col3) <- rownames(jef_mean_resp_list)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
str(cont_list_col3)



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
pdf(file="./mean_ego_jef_contrasts.pdf", 
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
str(post0)

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


str(cov_list11)
cov_list11 <- rev(cov_list11) #reverse order for plotting
str(cov_list11)





######### mestizo predictors

post0 <- post19 
str(post0)

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


str(cov_list19)
cov_list19 <- rev(cov_list19) #reverse order for plotting
str(cov_list19)




cov_names <- c("Ego var", "In-group var", "Out-group var",
              "Ego-In cov", "Ego-Out cov", "In-Out cov")
cov_names <- rev(cov_names)


#### density plot of mean guess inaccuracy
pdf(file="./var_cov_dens.pdf", 
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

str(kl_list.abils)


cont_list_col1 <- list( kl_list.abils[[1]], #ma.acc.o.no_ed
            kl_list.abils[[7]], #me.acc.i.ed_no
            kl_list.abils[[8]], #me.acc.i.lab_no
            kl_list.abils[[9]]  #me.acc.i.com_no
             )
names(cont_list_col1) <- names(kl_list.abils)[c(1,7,8,9)]
cont_list_col1 <- rev(cont_list_col1) #reverse order for plotting
str(cont_list_col1)


cont_list_col2 <- list( kl_list.abils[[2]], #ma.acc.o.no_lab
            kl_list.abils[[4]], #ma.acc.o.ed_lab
            kl_list.abils[[10]], #me.acc.i.lab_ed
            kl_list.abils[[11]] #me.acc.i.com_ed
             )
names(cont_list_col2) <- names(kl_list.abils)[c(2,4,10,11)]
cont_list_col2 <- rev(cont_list_col2) #reverse order for plotting
str(cont_list_col2)


cont_list_col3 <- list( kl_list.abils[[3]], #ma.acc.o.no_com
            kl_list.abils[[5]], #ma.acc.o.ed_com
            kl_list.abils[[6]], #ma.acc.o.lab_com
            kl_list.abils[[12]] #me.acc.i.com_lab
             )
names(cont_list_col3) <- names(kl_list.abils)[c(3,5,6,12)]
cont_list_col3 <- rev(cont_list_col3) #reverse order for plotting
str(cont_list_col3)


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
pdf(file="./combined_kl_exp_contrasts.pdf", 
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



