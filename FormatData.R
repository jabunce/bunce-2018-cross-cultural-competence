#Read the data from the csv data file into R:
Interviews.raw <- read.csv(file="./Data/Manu_perceptions_11sep18.csv", header=TRUE)


#Check the variable names and dimensions in the data frame Interviews.raw
#names(Interviews.raw)
#dim(Interviews.raw)


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

#d.wide[,c(1,2,4,14,15,16,17)]

#add consecutive newID
num_indivs <- length( unique(d.wide$ID) )
ID_key <- cbind( unique(d.wide$ID), 1:num_indivs)

d.wide$newID <- as.numeric( factor(d.wide$ID, levels=unique(d.wide$ID)) ) #trick to assign consecutive numbers to IDs 
#d.wide[,c("ID","newID")]


############################### convert dataframe from wide to long format

#names(d.wide)

question_cols <- grep("^q\\d+", colnames(d.wide)) 			#regular expression to get indices for all columns beginning with "q" followed by a number

names(d.wide)[question_cols] <- sprintf( "q.%d", 1:14 ) 	##rename question columns with "." separator before number
d.wide$respID <- 1:nrow(d.wide)								##add column with unique ID for each person-target combination
names(d.wide)							
#d.wide[,c("ID","newID","respID")]

d.long <- reshape(d.wide, varying=names(d.wide)[question_cols], sep=".",	### column indices of the questions
					 idvar="respID",
					 direction="long")

#dim(d.long)
#names(d.long)
d.long <- d.long[order(d.long$newID),]					#order questions by person ID
#d.long[1:100, c("ID","Machi","time","q")]


#re-name columns
names(d.long)[c(length(names(d.long)) - 4,
				length(names(d.long)) - 1,
				length(names(d.long)))] <- c("target.num", "question", "response") #rename


#dim(d.long)
#names(d.long)
#d.long[1:100, c("newID","Machi","target.num","question","response" )]


d.na.resp <- d.long[which(is.na(d.long$response)==TRUE),] 		#NA responses
#unique(d.na.resp$ID)

num.na.resp.ego <- length(which(d.na.resp$target.num==1)) 		#number of NA responses by target, conditional on people being asked the questions
num.na.resp.in <- length(which(d.na.resp$target.num==2))
num.na.resp.out <- length(which(d.na.resp$target.num==3))

num.all.resp.ego <- length(which(d.long$target.num==1))			#number of non-NA ego responses
#num.na.resp.ego/num.all.resp.ego 								#proportion of NA ego responses
 
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
#d[173:220, c("target.num","question", "response", "ResponseFlipped.e", "ResponseFlipped.io")]


pdf(file="./Plots/Flip_check.e.pdf", 
height=4, width=4)
par(mfrow=c(1,1))

plot( jitter(d$ResponseFlipped.e) ~ jitter(d$response), 
  col=ifelse(Flip.e==1,"red","black") )

graphics.off()

pdf(file="./Plots/Flip_check.io.pdf", 
height=4, width=4)
par(mfrow=c(1,1))

plot( jitter(d$ResponseFlipped.io) ~ jitter(d$response), 
  col=ifelse(Flip.io==1,"red","black") )

graphics.off()


#rename response columns
#names(d)
names(d)[c(length(names(d))-2, length(names(d))-1, length(names(d)))] <- c("resp.original", "response.e", "response")
#names(d)






#######################################remove people with NA predictors

#get row indices with NAs
na.rows <- 0
for (n in 1:N) {
	for (p in 1:dim(d)[2]) {
		if ( is.na(d[n,p]) ) na.rows <- c(na.rows, n)
	}
}
na.rows <- unique(na.rows[-1]) #get rid of initial 0
# d[c(112:152, 1265:1280, 1538:1553, 4748:4763), #look at NA rows
# 	c("ID", "Machi",
# 		"EdMes", "LabMes", "ComMes",
# 		"FamMat", "EmpMat", "CtyMat",
# 		"target.num", "question")]

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

#d[1:20,c("ID","newID")]
#ID_key





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


# num.indiv.ego.ty 	#ego tayakome
# num.indiv.ego.bm 	#ego boca manu
# num.indiv.ego.at 	#ego atalaya
# num.indiv.ego.ma.bm #machis in boca manu
# num.indiv.ego.ma.at #machis in atalaya

# (num.indiv.ego.ty + 1)/79 #79 adults in Tayakome (plus technician)
# num.indiv.ego.bm/80 #approx 80 adults in Boca Manu
# num.indiv.ego.at/65 #approx 65 adults in Atalaya


# num.indiv.ego.ma 	#ego machis
# num.indiv.ego.me 	#ego mestizos
# num.indiv.in.ma 	#in machis
# num.indiv.in.me 	#in mestizos
# num.indiv.out.ma 	#out machis
# num.indiv.out.me 	#out mestizos

# num.indiv.ego.ma.ty #machis in tayakome
# num.indiv.ego.me.bm #mestizos in boca manu
# num.indiv.ego.me.at #mestizos in atalaya


# num.indiv.ego.ma.fem 	#number machi ego females
# num.indiv.ego.ma.mal	#number machi ego males
# num.indiv.in.ma.fem 	#number machi in females
# num.indiv.in.ma.mal 	#number machi in males
# num.indiv.out.ma.fem 	#number out machi females
# num.indiv.out.ma.mal 	#number out machi males

# num.indiv.ego.me.mal 	#number mest ego males
# num.indiv.ego.me.fem 	#number mest ego females
# num.indiv.in.me.mal 	#number mest in males
# num.indiv.in.me.fem 	#number mest in females
# num.indiv.out.me.mal 	#number out mest males
# num.indiv.out.me.fem 	#number out mest females

# num.indiv.ego.adol		#number ego adol
# num.indiv.in.adol 		#number in adol
# num.indiv.out.adol 		#number out adol

# num.indiv.ego.ad 		#number ego ad
# num.indiv.in.ad 		#number in ad
# num.indiv.out.ad 		#number out ad

# num.indiv.ego.old 		#number ego old
# num.indiv.in.old 		#number in old
# num.indiv.out.old 		#number out old

# num.indiv.ego.ma.ed 	#number machi ego edu
# num.indiv.in.ma.ed 		#number machi in edu
# num.indiv.out.ma.ed 	#number machi out edu

# num.indiv.ego.ma.lab 	#number machi ego lab
# num.indiv.in.ma.lab 	#number machi in lab
# num.indiv.out.ma.lab 	#number machi out lab

# num.indiv.ego.ma.com 	#number machi ego com
# num.indiv.in.ma.com 	#number machi in com
# num.indiv.out.ma.com 	#number machi out com

# num.indiv.ego.me.fam 	#number mest ego fam
# num.indiv.in.me.fam 	#number mest in fam
# num.indiv.out.me.fam 	#number mest out fam

# num.indiv.ego.me.emp 	#number mest ego emp
# num.indiv.in.me.emp 	#number mest in emp
# num.indiv.out.me.emp 	#number mest out emp
# num.indiv.ego.ma.emp 	#number machi ego emp

# num.indiv.ego.me.cty 	#number mest ego cty
# num.indiv.in.me.cty 	#number mest in cty
# num.indiv.out.me.cty 	#number mest out cty

# num.indiv.in.ty
# num.indiv.in.po
# num.indiv.out.bm
# num.indiv.in.at
# num.indiv.in.ma.at
# num.indiv.in.ma.bm

# #number of people answering each question, by ethnicity, by target
# table( d$question, d$target.num, d$Machi )


#recalculate dataset characteristics
K <- length(unique(d$question)) 						#number of questions
J <- length(unique(d$newID)) 							#number of people
N <- nrow(d) 											#number of responses across all targets
nummach <- length(unique(d[which(d$Machi==1),"ID"]))	#number machis
nummest <- length(unique(d[which(d$Machi==0),"ID"]))	#number mestizos

