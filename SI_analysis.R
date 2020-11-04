##########################################
# HOW POLITICAL PARTIES SHAPE PUBLIC OPINION IN THE REAL WORLD
# AUTHORS: Martin Bisgaard & Rune Slothuus
# VERSION: MARCH 2020 
# THIS SCRIPT: SUPPLEMENTAL ANALYSES IN SUPPORTING INFORMATION APPENDIX
#########################

rm(list=ls())

Sys.setlocale("LC_TIME", "English") 

#set correct working directory
#setwd("mydirectory")


#installing required packages
install.packages("rio")
install.packages("foreign")
install.packages("car")
install.packages("plyr")
install.packages("memisc")
install.packages("plm")
install.packages("lme4")
install.packages("lmtest")
install.packages("stargazer")
install.packages("xtable")
install.packages("psych")
install.packages("grDevices")
install.packages("plyr")
install.packages("reshape2")
install.packages("pBrackets")
install.packages("irr")
install.packages("MatchIt")
install.packages("sandwich")

#loading required packages
require(rio)
require(foreign)
require(car)
require(plyr)
require(plm)
require(lme4)
require(lmtest)
require(stargazer)
require(xtable)
require(psych)
require(grDevices)
require(plyr)
require(reshape2)
require(irr)
require(MatchIt)
require(sandwich)

#############
# A: Human coding of party positions
############

# Distribution of party positions
mediadata	<-	import('partypositions.sav')
dim(mediadata)
names(mediadata)

dist<-melt(mediadata[,12:18])
dist$value<-recode(dist$value,'-9=NA')

#clean up dim names
colnames(dist)<-c("Party","Coded position")
str(dist)
levels(dist$Party)<-c("Unity list","Socialists","Social Democrats","Social Liberals","Conservatives","Liberals","Danish People's Party")

#produce table with overall frequencies
tab<-table(dist)
tab<-cbind(tab,apply(tab,1,sum))
tab<-rbind(tab,apply(tab,2,sum))
rownames(tab)[8]<-"Total"
colnames(tab)[7]<-"Total"

xtable(tab) #print table

#data for intercoder reliability
reliability_all<-import('reliability_all.sav')

#how many unique articles were used for reliability check?
length(unique(reliability_all$ID))

stacked_c1<-melt(reliability_all[,12:18])
stacked_c2<-melt(reliability_all[,29:35])


###
#confusion matrix/joint distribution
cm<-table(stacked_c1$value,stacked_c2$value)

cm<-cbind(cm,apply(cm,1,sum))
cm<-rbind(cm,apply(cm,2,sum))
colnames(cm)[7]<-"Total"
rownames(cm)[7]<-"Total"

cmp<-prop.table(table(stacked_c1$value,stacked_c2$value),1)
cmp<-cbind(round(cmp*100,0),rep(100,nrow(cmp)))
cmp<- paste0("(",paste0(unlist(cmp),"%"),")")
cmp<-matrix(cmp,nr=6)

#paste tables together as they appear in SI (note that table has been edited slightly in SI (grey areas etc).)
cm_out<-rbind(cm[1,],cmp[1,])
for (i in 1:nrow(cmp)-1){
  cm_out<-rbind(cm_out,cm[i+1,],cmp[i+1,])
}
cm_out<-rbind(cm_out,cm[7,])

xtable(cm_out) #print table
 


#inter coder agreement rate
sum(diag(prop.table(table(stacked_c1$value,stacked_c2$value))))

#inter coder agreement rate when "true" 0's are excluded (cases where c1 and c2 agree on '0') (reported in text)
datin<-cbind(stacked_c1$value,stacked_c2$value)
datin_restrict<-datin[apply(datin,1,FUN=function(x){x[1]!=0&x[2]!=0}),]
sum(diag(prop.table(table(datin_restrict[,1],datin_restrict[,2]))))

#krippendorffs alpha
datin<-rbind(stacked_c1$value,stacked_c2$value)
kripp.alpha(datin)

#krippendorffs alpha when '0' is excluded (reported in text)
datin_restrict<-datin[,apply(datin,2,FUN=function(x){x[1]!=0&x[2]!=0})]
kripp.alpha(datin_restrict)


########
# B: TREND IN POSITION-TAKING ACROSS ALL PARTIES
########

#break up data file into each issue
retiredata <- mediadata[mediadata$issue==2,]
unempdata <- mediadata[mediadata$issue==1,]
dim(retiredata)
names(retiredata)
dim(unempdata)
names(unempdata)


#### ISSUE: RETIREMENT PERIOD ####

#Positions of all parties
Vpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posVny,"party"="V")
Kpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posKny,"party"="C")
DFpos<-data.frame("tid"=unempdata$tid,"pos"=unempdata$posDFny,"party"="DF")
RVpos<-data.frame("tid"=unempdata$tid,"pos"=unempdata$posRVny,"party"="RV")
SFpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posSny,"party"="S")
Spos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posSFny,"party"="SF")

#clean codes
Vpos$date<-as.Date(as.character(Vpos$tid),"%Y%m%d")
Vpos<-na.omit(Vpos)
Kpos$date<-as.Date(as.character(Kpos$tid),"%Y%m%d")
Kpos<-na.omit(Kpos)
DFpos$date<-as.Date(as.character(DFpos$tid),"%Y%m%d")
DFpos<-na.omit(DFpos)
RVpos$date<-as.Date(as.character(RVpos$tid),"%Y%m%d")
RVpos<-na.omit(RVpos)
SFpos$date<-as.Date(as.character(SFpos$tid),"%Y%m%d")
SFpos<-na.omit(SFpos)
Spos$date<-as.Date(as.character(Spos$tid),"%Y%m%d")
Spos<-na.omit(Spos)

#PLOTTING
#pdf("benefitsCueALLparties.pdf",width=5,height=7)

cols1=rgb(.1,.1,.1,alpha=.175)
cols2=rgb(.1,.1,.1,alpha=.175)
par(omd=c(.1,1,0,1),mfrow=c(3,2),mai=c(.4,.1,.25,.25))

#LIBERAL PARTY
plot(Vpos$date,jitter(Vpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(Vpos$date,jitter(Vpos$pos,.5),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(Vpos$pos~Vpos$date,spar=1)
lines(m)
mtext(3,text="The Liberal Party",line=.5,font=1)

#CONSERVATIVES
plot(Kpos$date,jitter(Kpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(Kpos$date,jitter(Kpos$pos,.5),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=NA,cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(Kpos$pos~Kpos$date,spar=1)
lines(m)
mtext(3,text="Conservatives",line=.5,font=1)


#DANISH PEOPLES PARTY
plot(DFpos$date,jitter(DFpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(DFpos$date,jitter(DFpos$pos,.5),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(DFpos$pos~DFpos$date,spar=1)
lines(m)
mtext(3,text="Danish People's Party",line=.5,font=1)

#SOCIAL LIBERALS
plot(RVpos$date,jitter(RVpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(RVpos$date,jitter(RVpos$pos,1),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=NA,cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(RVpos$pos~RVpos$date,spar=1)
lines(m)
mtext(3,text="Social-Liberals",line=.5,font=1)

#SOCIAL DEMOCRATS
plot(Spos$date,jitter(Spos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(Spos$date,jitter(Spos$pos,1),pch=21,bg=cols2,col=NA)
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(Spos$pos~Spos$date,spar=1)
lines(m)
mtext(3,text="Social Democrats",line=.5,font=1)

#SOCIALISTS
plot(SFpos$date,jitter(SFpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(SFpos$date,jitter(SFpos$pos,1),pch=21,bg=cols2,col=NA)
axis(2,at=1:5,las=2,labels=NA,cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(SFpos$pos~SFpos$date,spar=1)
lines(m)
mtext(3,text="Socialists",line=.5,font=1)

#dev.off()




##########
# PARTY POSITIONS ON OTHER ISSUE: ABOLISHING EARLY RETIREMENT

Vpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posVny,"party"="V")
Kpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posKny,"party"="C")
DFpos<-data.frame("tid"=retiredata$tid,"pos"=retiredata$posDFny,"party"="DF")
RVpos<-data.frame("tid"=retiredata$tid,"pos"=retiredata$posRVny,"party"="RV")
SFpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posSny,"party"="S")
Spos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posSFny,"party"="SF")

#clean codes
Vpos$date<-as.Date(as.character(Vpos$tid),"%Y%m%d")
Vpos<-na.omit(Vpos)
Kpos$date<-as.Date(as.character(Kpos$tid),"%Y%m%d")
Kpos<-na.omit(Kpos)
DFpos$date<-as.Date(as.character(DFpos$tid),"%Y%m%d")
DFpos<-na.omit(DFpos)
RVpos$date<-as.Date(as.character(RVpos$tid),"%Y%m%d")
RVpos<-na.omit(RVpos)
SFpos$date<-as.Date(as.character(SFpos$tid),"%Y%m%d")
SFpos<-na.omit(SFpos)
Spos$date<-as.Date(as.character(Spos$tid),"%Y%m%d")
Spos<-na.omit(Spos)

#PLOTTING
#pdf("retireCueALLparties.pdf",width=5,height=7)

cols1=rgb(.1,.1,.1,alpha=.175)
cols2=rgb(.1,.1,.1,alpha=.175)
par(omd=c(.1,1,0,1),mfrow=c(3,2),mai=c(.4,.1,.25,.25))


#LIBERAL PARTY
plot(Vpos$date,jitter(Vpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(Vpos$date,jitter(Vpos$pos,.5),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(Vpos$pos~Vpos$date,spar=1)
lines(m)
mtext(3,text="The Liberal Party",line=.5,font=1)

#CONSERVATIVES
plot(Kpos$date,jitter(Kpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(Kpos$date,jitter(Kpos$pos,.5),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=NA,cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(Kpos$pos~Kpos$date,spar=1)
lines(m)
mtext(3,text="Conservatives",line=.5,font=1)


#DANISH PEOPLES PARTY
plot(DFpos$date,jitter(DFpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(DFpos$date,jitter(DFpos$pos,.5),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(DFpos$pos~DFpos$date,spar=1)
lines(m)
mtext(3,text="Danish People's Party",line=.5,font=1)

#SOCIAL LIBERALS
plot(RVpos$date,jitter(RVpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(RVpos$date,jitter(RVpos$pos,1),pch=21,bg=cols1,col=NA)
axis(2,at=1:5,las=2,labels=NA,cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(RVpos$pos~RVpos$date,spar=1)
lines(m)
mtext(3,text="Social-Liberals",line=.5,font=1)

#SOCIAL DEMOCRATS
plot(Spos$date,jitter(Spos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(Spos$date,jitter(Spos$pos,1),pch=21,bg=cols2,col=NA)
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(Spos$pos~Spos$date,spar=1)
lines(m)
mtext(3,text="Social Democrats",line=.5,font=1)

#SOCIALISTS
plot(SFpos$date,jitter(SFpos$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5))
points(SFpos$date,jitter(SFpos$pos,1),pch=21,bg=cols2,col=NA)
axis(2,at=1:5,las=2,labels=NA,cex.axis=.8)
axis(1,at=seq(as.Date("2010-01-01"),as.Date("2011-02-01"), by="month", format="%M"),labels=NA)
m<-smooth.spline(SFpos$pos~SFpos$date,spar=1)
lines(m)
mtext(3,text="Socialists",line=.5,font=1)

#dev.off()



#############
# D: PERCEIVED PARTY POSITIONS AMONG RESPONDENTS
#############

paneldat<-read.dta("paneldata_wide.dta")


######
# Issue: Unemployment benefits
######

tab<-matrix(table(paneldat$know_dagpenge))
tab<-matrix(c(tab[1:5,1],0,tab[6:9,1]),nc=1)	# one category has "0" frequency and still needs to be displayed
rownames(tab)<-c("Half a year","1 year","2 years","3 years","4 years","5 years","6 years","More than 6 years","There is no upper limit","Do not recall")
colnames(tab)<-NA
tab<-tab/sum(tab)

out<-xtable(tab,label="benefitComply",
	caption="Knowledge of what Folketinget (Danish parliament) decided on with respect to the unemployment benefit period (dagpenge). Proportions. Question: ``Recently Folketinget agreed to change the unemployment benefit period. Do you recall, what the maximum amount of time is that you can now receive unemployment benefits?''")
print(out,table.placement="H",caption.placement="top")


######
# Issue: Early retirement
######

main<-	rbind(table(paneldat$partypos_V),
	table(paneldat$partypos_C),
	table(paneldat$partypos_DF),
	table(paneldat$partypos_RV),
	table(paneldat$partypos_S),
	table(paneldat$partypos_SF))
main<-main/1804	#percentages (1804 respondents answered all questions)
rownames(main)<-c("Liberals","Conservatives","Danish Peoples Party","Social-Liberals","Social-Democrats","Socialists")
colnames(main)<-c("Keep","Limit access","Abolish","Don't know")

out<-xtable(main,caption="Perceived party position on whether the early retirement scheme should be kept, limited or abolished. Row proportions. Question: ''Recently there has been debate about the early retirement scheme. Do you recall what each of the following political parties think should happen to the early retirement scheme?''",
	label="retireComply")
print(out,table.placement="H",caption.placement="top")



#####################################
# E: PANEL ATTRITION AND MISSINGNESS	
#####################################

#load the five wave panel data in wide format
paneldat<-read.dta("paneldata_wide.dta",convert.underscore=TRUE)

# Produce table with overview of completed interviews

#Participation in waves (first column)
c1<-c(table(is.na(paneldat$benefit.1))[1],
	table(is.na(paneldat$benefit.2))[1],
	table(is.na(paneldat$benefit.3))[1],
	table(is.na(paneldat$benefit.4))[1],
	table(is.na(paneldat$benefit.5))[1])

#participation in waves conditional on participating in all consecutive waves (second column)
c2<-c(table(is.na(paneldat$benefit.1))[1],
	table(is.na(paneldat$benefit.2[!is.na(paneldat$benefit.1)]))[1],
	table(is.na(paneldat$benefit.3[!is.na(paneldat$benefit.1)&!is.na(paneldat$benefit.2)]))[1],
	table(is.na(paneldat$benefit.4[!is.na(paneldat$benefit.1)&!is.na(paneldat$benefit.2)&!is.na(paneldat$benefit.3)]))[1],
	table(is.na(paneldat$benefit.5[!is.na(paneldat$benefit.1)&!is.na(paneldat$benefit.2)&!is.na(paneldat$benefit.3)&!is.na(paneldat$benefit.4)]))[1])

table_S3<-cbind(1:5,c1,c2)
rownames(table_S3)<-NULL
colnames(table_S3)<-c("Wave","Completed Interviews","Respondents in All Prior Waves")
table_S3 #note: the table has been edited slightly as it appears in the SI.

#calculation of attrition rate (reported in text)
1206/2902 	#for all respondents

#########################################
# E.1 COMPARING WAVE 1 AND FINAL SAMPLE
#########################################


dat.balance<-read.dta("paneldata_long.dta")
dat.balance$partyAll<-recode(dat.balance$partyAll,'"NA"=NA') #fixing NA value on party variable 
dat<-dat.balance

#Final sample dataset
dat.balance<-dat.balance[dat.balance$completion==1,]
dim(dat.balance)

#Wave 1 dataset
datw1<-dat[dat$time==1,]

#Table with overview of balance on sociodemographics 
covar_attrition<-rbind(
cbind(prop.table(table(datw1$education)),prop.table(table(dat.balance$education))),
cbind(prop.table(table(datw1$occupation)),prop.table(table(dat.balance$occupation))),
cbind(prop.table(table(datw1$sex))[1],prop.table(table(dat.balance$sex))[1]),
cbind(prop.table(table(datw1$income)),prop.table(table(dat.balance$income))),
cbind(mean(datw1$age,na.rm=T),mean(dat.balance$age,na.rm=T)))

covar_attrition<-round(covar_attrition,digits=2)
colnames(covar_attrition)<-c("Wave 1 only","All waves")
rownames(covar_attrition)<-c("Vocational school","Commercial college (HHX or HTX)","Elementary school","High school (STX)","Higher education, short","Higher education, long","Higher education, middle",
	"Other","Unemployed","Skilled worker","House-making wife / husband","White-collar worker (w/ managerial resp.)","White-collar worker (w/o managerial resp.)",
	"Assisting spouse","Retired","Self-employed","Unskilled worker","Student","Do not wish to report",
	"Female","100.000 - 199.999 kr.","200.000 - 299.999 kr.","300.000 - 399.999 kr.","400.000 - 499.999 kr.",
	"500.000 - 599.999 kr.","600.000 - 699.999 kr.","700.000 or above","Below 99.999 kr.","Do not wish to report","Age")
covar_attrition<-cbind(rownames(covar_attrition),covar_attrition)
covar_attrition<-rbind(covar_attrition,c("Number of Observations",nrow(datw1),nrow(dat.balance)/5))
rownames(covar_attrition)<-NULL


print(xtable(covar_attrition),include.rownames=FALSE)
#note table has been edited manually in latex.


################################################
# E.2 Trend in missingness conditional on PID
################################################

dat.balance<-dat
dat.balance<-dat[!is.na(dat$partyAll),]
dim(dat.balance)

#generate missing variable
dat.balance$miss<-NA
dat.balance$miss[is.na(dat.balance$benefitClean)|is.na(dat.balance$retire_cutClean)]<-1 #missing on DV includes both D/K answers and dropout after wave 1
dat.balance$miss[!is.na(dat.balance$benefitClean)&!is.na(dat.balance$retire_cutClean)]<-0

#calculate missingness conditional on PID and time of interview
miss.out<-matrix(NA,nc=3,nr=5)
for(i in 1:5){ 
	miss.out[i,]=c(mean(dat.balance$miss[dat.balance$time==i&dat.balance$partyAll!="DF"&dat.balance$partyAll!="V"],na.rm=T),
	mean(dat.balance$miss[dat.balance$time==i&dat.balance$partyAll=="V"],na.rm=T),
	mean(dat.balance$miss[dat.balance$time==i&dat.balance$partyAll=="DF"],na.rm=T))}
#miss.out


par(mfrow=c(1,1),omd=c(0,1,0,1),mai=c(1,1,.25,.25)) 			#sets plotting region
x<-c("2010-02-22","2010-04-05","2010-06-15","2011-01-11","2011-06-25") 
x<-as.Date(x)
jit=.1 									#adds space to separate the points in the plot

#build plot
#pdf("attrition.pdf",width=6,height=5)
	plot(x,miss.out[,2],ylim=c(0,1),xaxt='n',xlab="",ylab="Proportion Missing",col="white",las=2)
	abline(v=3,lty=3)
	abline(v=11,lty=3)
	lines(x-jit,miss.out[,2],lty=1);points(x-jit,miss.out[,2],pch=21,bg="white")
	lines(x+jit,miss.out[,3],lty=1);points(x+jit,miss.out[,3],pch=21,bg="black")
	lines(x,miss.out[,1],lty=1);points(x,miss.out[,1],pch=21,bg="grey70")
 	axis.Date(1,at=seq(as.Date("2010-03-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=1)
 	axis.Date(1,at=c(as.Date("2010-03-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=1,line=1,tick="FALSE")	
	legend('bottomright',legend=c("Liberal supporters","DPP supporters","All other voters"),bty='n',pch=21,pt.bg=c("white","black","grey70"),lty=1)
 	abline(v=as.Date("2010-05-21"),lty=2)
  	abline(v=as.Date("2011-01-01"),lty=2)
#dev.off()




##########################################################################
# E.3 Explaining missingness conditional on PID and prior issue position
##########################################################################


#clean the lagged versions of the DVs and rescale to 0-1
dat.balance$benefitW1clean<-recode(dat.balance$benefitW1,'1=5;2=4;3=3;4=2;5=1;6=NA')
dat.balance$benefitW1clean<-(dat.balance$benefitW1clean-1)/4
dat.balance$retireW1clean<-recode(dat.balance$retireW1,'1=5;2=4;3=3;4=2;5=1;6=NA')
dat.balance$retireW1clean<-(dat.balance$retireW1clean-1)/4

#ISSUE: unemployment benefits
dat.balance$treated<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
dat.balance$benefitcue<-recode(dat.balance$time,'1:2=0;3:5=1')


m1<-lm(miss~treated*benefitcue,data=dat.balance[dat.balance$time%in%2:3,])
	names(m1$coefficients)<-c("Constant","Treated (t=1)","After","After x Treated (t=1)")
m2<-lm(miss~treated*benefitcue+benefitW1clean,data=dat.balance[dat.balance$time%in%2:3,])
	names(m2$coefficients)<-c("Constant","Treated (t=1)","After","Policy opinion (t=1)","After x Treated (t=1)")
m3<-lm(miss~treated*benefitcue+benefitW1clean*treated,data=dat.balance[dat.balance$time%in%2:3,])
	names(m3$coefficients)<-c("Constant","Treated (t=1)","After","Policy opinion (t=1)","After x Treated (t=1)","Policy opinion (t=1) x Treated (t=1)")
m4<-lm(miss~treated*benefitcue*benefitW1clean,data=dat.balance[dat.balance$time%in%2:3,])
	names(m4$coefficients)<-c("Constant","Treated (t=1)","After","Policy opinion (t=1)","After x Treated (t=1)","Policy opinion (t=1) x Treated (t=1)","After x Policy opinion  (t=1)","After x Opinion (t=1) x Treated (t=1)")

stargazer(m1,m2,m3,m4,no.space=T,keep.stat="n",dep.var.labels="Missing (0/1)",
	title="Explaining the probability of missingness over time conditional on party affiliation 
	and prior policy opinion towards unemployment benefits (measured in wave 1). For simplicity, the model 
	focuses on the two-wave case pre and post the change in party cues (wave 2 and 3). 
	Entries are unstandardized coefficients from linear probability models.",
	label="benefitsmissing")

#ISSUE: early retirement
dat.balance$treated<-recode(dat.balance$partyAll,'"V"=1;else=0')
dat.balance$retirecue<-recode(dat.balance$time,'1:3=0;4:5=1')

m1<-lm(miss~treated*retirecue,data=dat.balance[dat.balance$time%in%3:4,])
	names(m1$coefficients)<-c("Constant","Treated (t=1)","After","After x Treated (t=1)")
m2<-lm(miss~treated*retirecue+retireW1clean,data=dat.balance[dat.balance$time%in%3:4,])
	names(m2$coefficients)<-c("Constant","Treated (t=1)","After","Policy opinion (t=1)","After x Treated (t=1)")
m3<-lm(miss~treated*retirecue+retireW1clean*treated,data=dat.balance[dat.balance$time%in%3:4,])
	names(m3$coefficients)<-c("Constant","Treated (t=1)","After","Policy opinion (t=1)","After x Treated (t=1)","Policy opinion (t=1) x Treated (t=1)")
m4<-lm(miss~treated*retirecue*retireW1clean,data=dat.balance[dat.balance$time%in%3:4,])
	names(m4$coefficients)<-c("Constant","Treated (t=1)","After","Policy opinion (t=1)","After x Treated (t=1)","Policy opinion (t=1) x Treated (t=1)","After x Policy opinion  (t=1)","After x Opinion (t=1) x Treated (t=1)")

stargazer(m1,m2,m3,m4,no.space=T,keep.stat="n",dep.var.labels="Missing (0/1)",
	title="Explaining the probability of missingness over time conditional on party affiliation 
	and prior policy opinion towards early retirement (measured in wave 1). The model 
	focuses on the two-wave case pre and post the change in party cues on early retirement (wave 3 and 4). 
	Entries are unstandardized coefficients from linear probability models.",
	label="retiremissing")




#############################################################
# E.4 Examining Panel Effects Using an Independent Survey
#############################################################


#load independent cross-sectional study 
followup<-import("followup_study.sav")

notpick<-1:15
followup$treated1<-NA
followup$treated1[followup$bagg6 %in% c(6,7) | followup$bagg7 %in% c(6,7)]<-1
followup$treated1[followup$bagg6 %in% notpick[-c(6,7)] | followup$bagg7 %in% notpick[-c(6,7)]] <-0
table(followup$treated1)

followup$treated2<-NA
followup$treated2[followup$bagg6 %in% c(7) | followup$bagg7 %in% c(7)]<-1
followup$treated2[followup$bagg6 %in% notpick[-c(7)] | followup$bagg7 %in% notpick[-c(7)]] <-0
table(followup$treated2)

followup$benefit<-recode(followup$q8,'6=NA;5=1;4=2;3=3;2=4;1=5')
followup$benefit<-(followup$benefit-1)/4

followup$retire<-recode(followup$q9,'6=NA;5=1;4=2;3=3;2=4;1=5')
followup$retire<-(followup$retire-1)/4

table(followup$treated1)
table(followup$treated2)


#balancing
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$benefitClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

SE=function(x)sd(x)/sqrt(length(x)-1) #function to calculate naive SEs

c1<-aggregate(benefit~treated1,data=followup,mean)[,2]
c1_se<-aggregate(benefit~treated1,data=followup,SE)[,2]

dat.balance$treated<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
cout<-aggregate(benefitClean~treated*time,data=dat.balance,mean)
cout_se<-aggregate(benefitClean~treated*time,data=dat.balance,SE)

#other issue (balancing)
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$retire_cutClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

b1<-aggregate(retire~treated2,data=followup,mean)[,2]
b1_se<-aggregate(retire~treated2,data=followup,SE)[,2]

dat.balance$treated<-recode(dat.balance$partyAll,'"V"=1;else=0')
bout<-aggregate(retire_cutClean~treated*time,data=dat.balance,mean)
bout_se<-aggregate(retire_cutClean~treated*time,data=dat.balance,SE)

######
#PLOTTING
#####

#pdf('benefits_paneleffects.pdf',width=6,height=4.25)

x<-c("2010-02-22","2010-04-05","2010-06-15","2011-01-11","2011-06-25") #sets correct spacing between waves
x<-as.Date(x)

par(omd=c(0.05,1,0,1),mfrow=c(1,1),mai=c(1,1,1,.5))
y=cout[cout$treated==0,3]
se=cout_se[cout_se$treated==0,3]
plot(x,y,ylim=c(.2,.8),xlim=c(as.Date("2010-02-01"),as.Date("2011-08-01")),main="",xaxt="n",yaxt="n",ylab="",xlab="")
segments(x,y+1.96*se,x,y-1.96*se,col="grey70")
text(x[5]+.75,y[5],labels=round(y[5],2),cex=.6,pos=4)
points(x,y,pch=21,bg="grey70",col="grey70")
lines(x,y,lty=1,col="grey70")
y=cout[cout$treated==1,3]
se=cout_se[cout_se$treated==1,3]
points(x,y,pch=21,bg="black")
segments(x,y+1.96*se,x,y-1.96*se)
text(x[5]+.75,y[5],labels=round(y[5],2),cex=.6,pos=4)
lines(x,y,lty=1)
axis(2,las=2)
axis(2,at=seq(0,1,.05),tck=-.015,labels=NA)
mtext(2,text="Policy Support",line=3)
axis.Date(1,at=seq(as.Date("2010-02-01"),as.Date("2011-08-01"), by="month"),format="%b-%Y",las=2,cex.axis=.8,labels=NA)
xpos<-as.Date("2011-06-20")
points(y=c1,x=rep(xpos,2),pch=22,bg=c("grey70","black"),col=c("grey70","black"))
segments(rep(xpos,2),c1-1.96*c1_se,rep(xpos,2),c1+1.96*c1_se,col=c("grey70","black"))
text(x=rep(xpos,2),y=c1+c(.025,0),labels=round(c1,2),pos=4,cex=.6)

text(x=as.Date("2011-05-01"),y=.5,labels="Independent sample",cex=.8,pos=2)
segments(as.Date("2011-04-20"),.49,as.Date("2011-06-10"),c1[1])
segments(as.Date("2011-04-20"),.51,as.Date("2011-06-10"),c1[2])

#dev.off()


#pdf('retire_paneleffects.pdf',width=6,height=4.25)

par(omd=c(0.05,1,0,1),mfrow=c(1,1),mai=c(1,1,1,.5))
y=bout[bout$treated==0,3]
se=bout_se[bout_se$treated==0,3]
plot(x,y,ylim=c(.4,1),xlim=c(as.Date("2010-02-01"),as.Date("2011-08-01")),main="",xaxt="n",yaxt="n",ylab="",xlab="")
points(x,y,pch=21,col="grey70",bg="grey70")
segments(x,y+1.96*se,x,y-1.96*se,col="grey70")
text(x[5]+.75,y[5],labels=round(y[5],2),cex=.6,pos=4)
lines(x,y,lty=1,col="grey70")
y=bout[bout$treated==1,3]
se=bout_se[bout_se$treated==1,3]
points(x,y,pch=21,bg="black")
segments(x,y+1.96*se,x,y-1.96*se)
text(x[5]+.75,y[5],labels=round(y[5],2),cex=.6,pos=4)
lines(x,y,lty=1)
axis(2,las=2)
axis(2,at=seq(0,1,.05),tck=-.015,labels=NA)
mtext(2,text="Policy Support",line=3)
axis.Date(1,at=seq(as.Date("2010-02-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=.8)
axis.Date(1,at=c(as.Date("2010-02-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=.8,line=1,tick="FALSE")


xpos<-as.Date("2011-06-20")
points(y=b1,x=rep(xpos,2),pch=22,bg=c("grey70","black"),col=c("grey70","black"))
segments(rep(xpos,2),b1-1.96*b1_se,rep(xpos,2),b1+1.96*b1_se,col=c("grey70","black"))
text(x=rep(xpos,2),y=b1-.025,labels=round(b1,2),pos=4,cex=.6)

text(x=as.Date("2011-05-01"),y=.6,labels="Independent sample",cex=.8,pos=2)
segments(as.Date("2011-04-20"),.58,as.Date("2011-06-10"),b1[1])
segments(as.Date("2011-04-20"),.6,as.Date("2011-06-10"),b1[2])

#dev.off()





###################################
# F ROBUSTNESS TO BALANCING
###################################

#Conditional means for unbalanced panel
dat.balance<-dat[!is.na(dat$partyAll),]

### UNEMPLOYMENT BENEFIT PERIOD ###

## matched control group ##
	covariates<-c("pressure","unemployed","education","sex","age","income","benefitW1","benefitW2")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;"DF"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~factor(education)+pressure+unemployed+sex+age+factor(income)+factor(benefitW1)+factor(benefitW2),
                     method = "nearest", data = dat_match)
	
	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting
	dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
	dat.balance$partygroup<-factor(dat.balance$partygroup)
	dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=NA')
	dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

SE=function(x)sd(x)/sqrt(length(x)-1) #function to calculate naive SEs
out<-aggregate(benefitClean~partygroup*time,data=dat.balance,mean)
outm<-aggregate(benefitClean~partygroup_matched*time,data=dat.balance,mean)

#store results in one object
benefit_unbalanced<-rbind(out[out$partygroup==1,3],out[out$partygroup==0,3],outm[outm$partygroup_matched==0,3])


### EARLY RETIREMENT ###


## matched control group ##
	covariates<-c("occupation","unemployed","pressure","education","sex","age","income","retireW1","retireW2","retireW3")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$occupation<-as.factor(as.numeric(as.factor(dat_match$occupation)))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~ factor(retireW1)+factor(retireW2)+factor(retireW3)+pressure+sex+unemployed+factor(income)+factor(education)+age,
                     method = "nearest", data = dat_match)

	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;else=0')
dat.balance$partygroup<-factor(dat.balance$partygroup)

dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;else=NA')
dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

#calculate quantities of interest
SE=function(x)sd(x)/sqrt(length(x)-1) #function to calculate naive SEs
out<-aggregate(retire_cutClean~partygroup*time,data=dat.balance,mean)
outm<-aggregate(retire_cutClean~partygroup_matched*time,data=dat.balance,mean)

retire_unbalanced<-rbind(out[out$partygroup==1,3],out[out$partygroup==0,3],outm[outm$partygroup_matched==0,3])



#####
# Conditional means for balanced panel

##
# Unemployment benefit period #

dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$benefitClean)|is.na(dat.balance$partyAll),]$id),]

## matched control group ##
	covariates<-c("occupation","unemployed","pressure","education","sex","age","income","benefitW1","benefitW2")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;"DF"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$occupation<-as.factor(as.numeric(as.factor(dat_match$occupation)))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~factor(education)+pressure+unemployed+sex+age+factor(income)+factor(benefitW1)+factor(benefitW2),
                     method = "nearest", data = dat_match)
	
	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting
	dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
	dat.balance$partygroup<-factor(dat.balance$partygroup)
	dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=NA')
	dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

SE=function(x)sd(x)/sqrt(length(x)-1) #function to calculate naive SEs
out<-aggregate(benefitClean~partygroup*time,data=dat.balance,mean)
outm<-aggregate(benefitClean~partygroup_matched*time,data=dat.balance,mean)

#store results in one object

benefit_balanced<-rbind(out[out$partygroup==1,3],out[out$partygroup==0,3],outm[outm$partygroup_matched==0,3])


## EARLY RETIREMENT ##
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$retire_cutClean)|is.na(dat.balance$partyAll),]$id),]

## matched control group ##
	covariates<-c("occupation","unemployed","pressure","education","sex","age","income","retireW1","retireW2","retireW3")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$occupation<-as.factor(as.numeric(as.factor(dat_match$occupation)))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~ factor(retireW1)+factor(retireW2)+factor(retireW3)+pressure+sex+unemployed+factor(income)+factor(education)+age,
                     method = "nearest", data = dat_match)
	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;else=0')
dat.balance$partygroup<-factor(dat.balance$partygroup)

dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;else=NA')
dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

#calculate quantities of interest
SE=function(x)sd(x)/sqrt(length(x)-1) #function to calculate naive SEs
out<-aggregate(retire_cutClean~partygroup*time,data=dat.balance,mean)
outm<-aggregate(retire_cutClean~partygroup_matched*time,data=dat.balance,mean)

retire_balanced<-rbind(out[out$partygroup==1,3],out[out$partygroup==0,3],outm[outm$partygroup_matched==0,3])




######
# PLOTTING
#####

x<-c("2010-02-22","2010-04-05","2010-06-15","2011-01-11","2011-06-25") #sets correct spacing between waves
x<-as.Date(x)

#pdf('balance_benefit.pdf',width=6,height=5)

y<-benefit_balanced[3,]
par(mfrow=c(1,1))
plot(x,y,ylim=c(.2,.8),xlim=c(as.Date("2010-02-01"),as.Date("2011-08-01")),main="",xaxt="n",yaxt="n",ylab="",xlab="",col="white")
points(x,y,pch=21,bg="grey70",col="grey70");lines(x,y,col="grey70")
y<-benefit_balanced[2,]
points(x,y,pch=24,bg="grey70",col="grey70");lines(x,y,col="grey70")

y<-benefit_unbalanced[2,]
points(x,y,pch=24,bg="grey70",col="grey70");lines(x,y,col="grey70",lty=2)

y<-benefit_unbalanced[3,]
points(x,y,pch=21,bg="grey70",col="grey70");lines(x,y,col="grey70",lty=2)

y<-benefit_balanced[1,]
points(x,y,pch=21,bg="black",col="black");lines(x,y,col="black")

y<-benefit_unbalanced[1,]
points(x,y,pch=21,bg="black");lines(x,y,lty=2)

axis(2,las=2)
axis(2,at=seq(0,1,.05),tck=-.015,labels=NA)
mtext(2,text="Policy Support",line=3)
abline(v=as.Date("2010-05-19"),lty=2)

legend('topright',lty=c(2,1),legend=c("Balanced panel","Unbalanced panel"),bty='n',cex=.8)
legend('bottomright',pch=c(21,24,21),pt.bg=c("black","grey70","grey70"),col=c("black","grey70","grey70"),legend=c("Treated","Control (all)","Control (matched)"),bty='n',cex=.8)

#dev.off()

###
#ISSUE: Early retirement

#pdf('balance_retire.pdf',width=6,height=5)

y<-retire_balanced[3,]
par(mfrow=c(1,1))
plot(x,y,ylim=c(.4,1),xlim=c(as.Date("2010-02-01"),as.Date("2011-08-01")),main="",xaxt="n",yaxt="n",ylab="",xlab="",col="white")
points(x,y,pch=21,bg="grey70",col="grey70");lines(x,y,col="grey70")
y<-retire_balanced[2,]
points(x,y,pch=24,bg="grey70",col="grey70");lines(x,y,col="grey70")

y<-retire_unbalanced[2,]
points(x,y,pch=24,bg="grey70",col="grey70");lines(x,y,col="grey70",lty=2)

y<-retire_unbalanced[3,]
points(x,y,pch=21,bg="grey70",col="grey70");lines(x,y,col="grey70",lty=2)

y<-retire_balanced[1,]
points(x,y,pch=21,bg="black",col="black");lines(x,y,col="black")

y<-retire_unbalanced[1,]
points(x,y,pch=21,bg="black");lines(x,y,lty=2)

axis(2,las=2)
axis(2,at=seq(0,1,.05),tck=-.015,labels=NA)
mtext(2,text="Policy Support",line=3)
axis.Date(1,at=seq(as.Date("2010-02-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=.8)
axis.Date(1,at=c(as.Date("2010-02-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=.8,line=1,tick="FALSE")
abline(v=as.Date("2011-01-01"),lty=2)

legend('topright',lty=c(2,1),legend=c("Balanced panel","Unbalanced panel"),bty='n',cex=.8)
legend('bottomright',pch=c(21,24,21),pt.bg=c("black","grey70","grey70"),col=c("black","grey70","grey70"),legend=c("Treated","Control (all)","Control (matched)"),bty='n',cex=.8)

#dev.off()



##############
# ROBUSTNESS TO BALANCING
# FOR ANALYSIS CONDITIONING ON PRIOR DV
##############


#### RESULTS USING UNBALANCED PANEL

dat.balance<-dat[!is.na(dat$partyAll),]
dat.balance$benefitW2<-recode(dat.balance$benefitW2,'6=NA')

## matched control group ##
	covariates<-c("occupation","unemployed","pressure","education","sex","age","income","benefitW1","benefitW2")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;"DF"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$occupation<-as.factor(as.numeric(as.factor(dat_match$occupation)))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~factor(education)+pressure+unemployed+sex+age+factor(income)+factor(benefitW1)+factor(benefitW2),
                     method = "nearest", data = dat_match)

	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting

dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=NA')
dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

#fit model
fit<-plm(benefitClean~partygroup_matched*time*factor(benefitW2),data=dat.balance[dat.balance$time%in%2:3,],index=c("id","time"),model="within")

#retrieve quantities of interest from panel model
cf<-coef(fit)
vmat<-vcovHC(fit,cluster="group")

#storing results
did_est<-c(cf[2],cf[2]+cf[7],cf[2]+cf[8],cf[2]+cf[9],cf[2]+cf[10])
did_se<-c(sqrt(vmat[2,2]),sqrt(vmat[2,2]+vmat[7,7]+2*vmat[2,7]),
	sqrt(vmat[2,2]+vmat[8,8]+2*vmat[2,8]),
	sqrt(vmat[2,2]+vmat[9,9]+2*vmat[2,9]),
	sqrt(vmat[2,2]+vmat[10,10]+2*vmat[10,2]))

#using all other voters as counterfactual
dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
fit<-plm(benefitClean~partygroup*time*factor(benefitW2),data=dat.balance[dat.balance$time%in%2:3,],index=c("id","time"),model="within")
	cf<-coef(fit)
	vmat<-vcovHC(fit,cluster="group")

#attaching results to rows in stored object
did_est<-rbind(did_est,c(cf[2],cf[2]+cf[7],cf[2]+cf[8],cf[2]+cf[9],cf[2]+cf[10]))
	
did_se<-rbind(did_se,c(sqrt(vmat[2,2]),sqrt(vmat[2,2]+vmat[7,7]+2*vmat[2,7]),
	sqrt(vmat[2,2]+vmat[8,8]+2*vmat[2,8]),
	sqrt(vmat[2,2]+vmat[9,9]+2*vmat[2,9]),
	sqrt(vmat[2,2]+vmat[10,10]+2*vmat[10,2])))


####
# ISSUE: EARLY RETIREMENT

dat.balance<-dat[!is.na(dat$partyAll),]
dat.balance$retireW3<-recode(dat.balance$retireW3,'6=NA')

## matched control group ##
	covariates<-c("occupation","unemployed","pressure","education","sex","age","income","retireW1","retireW2","retireW3")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$occupation<-as.factor(as.numeric(as.factor(dat_match$occupation)))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))


	match <- matchit(treated ~ factor(retireW1)+factor(retireW2)+factor(retireW3)+pressure+sex+unemployed+factor(income)+factor(education)+age,
                     method = "nearest", data = dat_match)

	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting

dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;else=NA')
dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

#fit model
fit2<-plm(retire_cutClean~partygroup_matched*time*factor(retireW3),data=dat.balance[dat.balance$time%in%3:4,],index=c("id","time"),model="within")
summary(fit2)

#retrieve quantities of interest from panel model
cf2<-coef(fit2)
vmat2<-vcovHC(fit2,cluster="group")

#store results
did_est2<-c(cf2[2],cf2[2]+cf2[7],cf2[2]+cf2[8],cf2[2]+cf2[9],cf2[2]+cf2[10])
did_se2<-c(sqrt(vmat2[2,2]),sqrt(vmat2[2,2]+vmat2[7,7]+2*vmat2[2,7]),
	sqrt(vmat2[2,2]+vmat2[8,8]+2*vmat2[2,8]),
	sqrt(vmat2[2,2]+vmat2[9,9]+2*vmat2[2,9]),
	sqrt(vmat2[2,2]+vmat2[10,10]+2*vmat2[10,2]))

#get results using all other voters as counterfactual

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;else=0')

#fit model
fit2<-plm(retire_cutClean~partygroup*time*factor(retireW3),data=dat.balance[dat.balance$time%in%3:4,],index=c("id","time"),model="within")
cf2<-coef(fit2)
vmat2<-vcovHC(fit2,cluster="group")

#store results
did_est2<-rbind(did_est2,c(cf2[2],cf2[2]+cf2[7],cf2[2]+cf2[8],cf2[2]+cf2[9],cf2[2]+cf2[10]))
did_se2<-rbind(did_se2,c(sqrt(vmat2[2,2]),sqrt(vmat2[2,2]+vmat2[7,7]+2*vmat2[2,7]),
	sqrt(vmat2[2,2]+vmat2[8,8]+2*vmat2[2,8]),
	sqrt(vmat2[2,2]+vmat2[9,9]+2*vmat2[2,9]),
	sqrt(vmat2[2,2]+vmat2[10,10]+2*vmat2[10,2])))


########
# RESULTS USING BALANCED PANEL


#balancing panel
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$benefitClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)
dat.balance$benefitW2<-recode(dat.balance$benefitW2,'6=NA')

## matched control group ##
	covariates<-c("occupation","unemployed","pressure","education","sex","age","income","benefitW1","benefitW2")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;"DF"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$occupation<-as.factor(as.numeric(as.factor(dat_match$occupation)))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))


	match <- matchit(treated ~factor(education)+pressure+unemployed+sex+age+factor(income)+factor(benefitW1)+factor(benefitW2),
                     method = "nearest", data = dat_match)
	
	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting

dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=NA')
dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

#fit model
fit<-plm(benefitClean~partygroup_matched*time*factor(benefitW2),data=dat.balance[dat.balance$time%in%2:3,],index=c("id","time"),model="within")

#retrieve quantities of interest from panel model
cf<-coef(fit)
vmat<-vcovHC(fit,cluster="group")

#storing results
did_est_unb<-c(cf[2],cf[2]+cf[7],cf[2]+cf[8],cf[2]+cf[9],cf[2]+cf[10])
did_se_unb<-c(sqrt(vmat[2,2]),sqrt(vmat[2,2]+vmat[7,7]+2*vmat[2,7]),
	sqrt(vmat[2,2]+vmat[8,8]+2*vmat[2,8]),
	sqrt(vmat[2,2]+vmat[9,9]+2*vmat[2,9]),
	sqrt(vmat[2,2]+vmat[10,10]+2*vmat[10,2]))

#using all other voters as counterfactual
dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
fit<-plm(benefitClean~partygroup*time*factor(benefitW2),data=dat.balance[dat.balance$time%in%2:3,],index=c("id","time"),model="within")
	cf<-coef(fit)
	vmat<-vcovHC(fit,cluster="group")

#attaching results to rows in stored object
did_est_unb<-rbind(did_est_unb,c(cf[2],cf[2]+cf[7],cf[2]+cf[8],cf[2]+cf[9],cf[2]+cf[10]))
	
did_se_unb<-rbind(did_se_unb,c(sqrt(vmat[2,2]),sqrt(vmat[2,2]+vmat[7,7]+2*vmat[2,7]),
	sqrt(vmat[2,2]+vmat[8,8]+2*vmat[2,8]),
	sqrt(vmat[2,2]+vmat[9,9]+2*vmat[2,9]),
	sqrt(vmat[2,2]+vmat[10,10]+2*vmat[10,2])))


####
# ISSUE: EARLY RETIREMENT

#balancing panel
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$retire_cutClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)
dat.balance$retireW3<-recode(dat.balance$retireW3,'6=NA')

## matched control group ##
	covariates<-c("occupation","pressure","unemployed","education","sex","age","income","retireW1","retireW2","retireW3")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$occupation<-as.factor(as.numeric(as.factor(dat_match$occupation)))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~ factor(retireW1)+factor(retireW2)+factor(retireW3)+pressure+sex+unemployed+factor(income)+factor(education)+age,
                     method = "nearest", data = dat_match)
	summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting

dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;else=NA')
dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

#fit model
fit2<-plm(retire_cutClean~partygroup_matched*time*factor(retireW3),data=dat.balance[dat.balance$time%in%3:4,],index=c("id","time"),model="within")
summary(fit2)

#retrieve quantities of interest from panel model
cf2<-coef(fit2)
vmat2<-vcovHC(fit2,cluster="group")

#store results
did_est2_unb<-c(cf2[2],cf2[2]+cf2[7],cf2[2]+cf2[8],cf2[2]+cf2[9],cf2[2]+cf2[10])
did_se2_unb<-c(sqrt(vmat2[2,2]),sqrt(vmat2[2,2]+vmat2[7,7]+2*vmat2[2,7]),
	sqrt(vmat2[2,2]+vmat2[8,8]+2*vmat2[2,8]),
	sqrt(vmat2[2,2]+vmat2[9,9]+2*vmat2[2,9]),
	sqrt(vmat2[2,2]+vmat2[10,10]+2*vmat2[10,2]))

#get results using all other voters as counterfactual

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;else=0')

#fit model
fit2<-plm(retire_cutClean~partygroup*time*factor(retireW3),data=dat.balance[dat.balance$time%in%3:4,],index=c("id","time"),model="within")
cf2<-coef(fit2)
vmat2<-vcovHC(fit2,cluster="group")

#store results
did_est2_unb<-rbind(did_est2_unb,c(cf2[2],cf2[2]+cf2[7],cf2[2]+cf2[8],cf2[2]+cf2[9],cf2[2]+cf2[10]))
did_se2_unb<-rbind(did_se2_unb,c(sqrt(vmat2[2,2]),sqrt(vmat2[2,2]+vmat2[7,7]+2*vmat2[2,7]),
	sqrt(vmat2[2,2]+vmat2[8,8]+2*vmat2[2,8]),
	sqrt(vmat2[2,2]+vmat2[9,9]+2*vmat2[2,9]),
	sqrt(vmat2[2,2]+vmat2[10,10]+2*vmat2[10,2])))



###########
# PLOTTING RESULTS

#pdf("priorDVrobust1.pdf",width=7,height=4)

par(mfrow=c(1,1),mai=c(1,2,.5,.1))
plot(did_est[1,],1:5,xlim=c(-.1,.5),xlab="Estimated Difference-in-Differences",yaxt='n',ylab=NA)
points(did_est[2,],1:5,pch=4)
points(did_est_unb[1,],1:5,pch=6)
points(did_est_unb[2,],1:5,pch=10)
abline(v=0,lty=2)
legend('bottomright',pch=c(1,4,6,10),legend=c("Unbalanced | Matched","Unbalanced | All","Balanced | Matched","Balanced | All"),bty='n',cex=.8)
axis(2,at=1:5,labels=c("Before:\nSupport","Before:\nPartly support","Before:\nNeither","Before:\nPartly oppose","Before:\nOppose"),las=2)
mtext(3,line=.5,text="Issue: Unemployment Benefit Period")

#dev.off()

#pdf("priorDVrobust2.pdf",width=7,height=4)

par(mfrow=c(1,1),mai=c(1,2,.5,.1))
plot(did_est2[1,],1:5,xlim=c(-.1,.5),xlab="Estimated Difference-in-Differences",yaxt='n',ylab=NA)
points(did_est2[2,],1:5,pch=4)
points(did_est2_unb[1,],1:5,pch=6)
points(did_est2_unb[2,],1:5,pch=10)
abline(v=0,lty=2)
legend('bottomright',pch=c(1,4,6,10),legend=c("Unbalanced | Matched","Unbalanced | All","Balanced | Matched","Balanced | All"),bty='n',cex=.8)
#axis(2,at=1:5,labels=NA,las=2)
axis(2,at=1:5,labels=c("Before:\nSupport","Before:\nPartly support","Before:\nNeither","Before:\nPartly oppose","Before:\nOppose"),las=2)
mtext(3,line=.5,text="Issue: Early Retirement")

#dev.off()


###########################
# G FURTHER RULING OUT ALTERNATIVE EVENTS
###########################

#balancing
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$benefitClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=NA;else=0')
dat.balance$partygroup<-factor(dat.balance$partygroup)

dat.balance$bb_clean<-recode(dat.balance$bb,'9=NA')
dat.balance$ub_clean<-recode(dat.balance$ub,'11=NA')

ub_time<-aggregate(ub_clean~partygroup*time,data=dat.balance,mean)
bb_time<-aggregate(bb_clean~partygroup*time,data=dat.balance,mean)


x<-c("2010-02-22","2010-04-05","2010-06-15","2011-01-11","2011-06-25") 
x<-as.Date(x)

#PLOTTING
#pdf("budgetfacts.pdf",width=6,height=5)

par(mfrow=c(1,1))
y=bb_time$bb_clean[bb_time$partygroup==1]

plot(x,y,ylim=c(2,8),main="",yaxt='n',xaxt='n',xlab="",ylab="")
lines(x,y)
points(x,y,pch=21,bg="black")
y=bb_time$bb_clean[bb_time$partygroup==0]
points(x,y,pch=24,bg="grey70",col="grey70")
lines(x,y,col="grey70")
  axis.Date(1,at=seq(as.Date("2010-03-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=1)
  axis.Date(1,at=c(as.Date("2010-03-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=1,line=1,tick="FALSE")
  abline(v=as.Date("2010-05-21"),lty=2)
  abline(v=as.Date("2011-01-01"),lty=2)
  legend('bottomright',pch=c(21,24),pt.bg=c("black","grey70"),col=c("black","grey70"),legend=c("Treated (Liberals only)","Control (all, excluding DPP)"),cex=1,pt.cex=1,bty='n')
axis(2,at=2:8,label=c("<25","37","62","87","112","137",">150"),las=2)
mtext(2,text="Billion DKR",padj=-4)

#dev.off()


#pdf("unemploymentfacts.pdf",width=6,height=5)

y=ub_time$ub[ub_time$partygroup==1]
plot(x,y,ylim=c(1,10),main="",yaxt='n',xaxt='n',xlab="",ylab="")
lines(x,y)
points(x,y,pch=21,bg="black")
lines(x,y)
points(x,y,pch=21,bg="black")
y=ub_time$ub[ub_time$partygroup==0]
points(x,y,pch=24,bg="grey70",col="grey70")
lines(x,y,col="grey70")
axis(2,at=1:10,label=c("<50","63","88","113","138","163","188","213","238",">250"),las=2)
mtext(2.5,text="Number of Unemployed (thousands)",padj=-4)
 axis.Date(1,at=seq(as.Date("2010-03-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=1)
 axis.Date(1,at=c(as.Date("2010-03-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=1,line=1,tick="FALSE")
 abline(v=as.Date("2010-05-21"),lty=2)
 abline(v=as.Date("2011-01-01"),lty=2)
 legend('bottomright',pch=c(21,24),pt.bg=c("black","grey70"),col=c("black","grey70"),legend=c("Treated (Liberals only)","Control (all, excluding DPP)"),cex=1,pt.cex=1,bty='n')

#dev.off()




#################
# G.2 RULING OUT ALTERNATIVE EVENTS: COMPARING SD and the DPP
# Note: Only possible on unemp benefits issue
#################

#balancing
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$benefitClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

table(dat.balance$partyAll)

dat.balance$partygroup<-recode(dat.balance$partyAll,'"DF"=1;"S"=0;else=NA')
dat.balance$partygroup<-factor(dat.balance$partygroup)
table(dat.balance$partygroup)/5

out<-aggregate(benefitClean~time*partygroup,data=dat.balance,mean)
out.se<-aggregate(benefitClean~time*partygroup,data=dat.balance,SE)

#pdf("dpp-spd.pdf",width=6,height=5)

y=out$benefitClean[out$partygroup==0]
se=out.se$benefitClean[out.se$partygroup==0]
par(mfrow=c(1,1))
plot(x,y,ylim=c(0,1),yaxt='n',xaxt='n',xlab="",ylab="")
points(x,y,pch=21,col="grey80",bg="grey80")
lines(x,y,col="grey80")
segments(x,y-1.96*se,x,y+1.96*se,col="grey80")

y=out$benefitClean[out$partygroup==1]
se=out.se$benefitClean[out.se$partygroup==0]

  points(x,y,pch=21,bg="black")
  lines(x,y)
  segments(x,y-1.96*se,x,y+1.96*se)
  legend('topright',pch=21,pt.bg=c("black","grey80"),col=c("black","grey80"),legend=c("DPP","Social democrats"),bty='n',cex=1,pt.cex=1)
  axis(2,las=2)
  axis(2,at=seq(0,1,.025),tck=-.015,labels=NA)
  mtext(2,text="Policy Support",line=3)
  axis.Date(1,at=seq(as.Date("2010-03-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=1)
  axis.Date(1,at=c(as.Date("2010-03-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=1,line=1,tick="FALSE")
  abline(v=as.Date("2010-05-21"),lty=2)

#dev.off()



#DID MODEL ESTIMATES
pfit1<-plm(benefitClean~partygroup*time,data=dat.balance[dat.balance$time%in%c(2,3),],model="within",index=c("id","time"))
	pfit1_out<-coeftest(pfit1,vcov=vcovHC(pfit1,cluster="group"))["partygroup1:time3",]
	eff1<-pfit1_out[1]
	se1<-pfit1_out[2]
coeftest(pfit1,vcov=vcovHC(pfit1,cluster="group"))

#######################################################
# H CLUSTERING STANDARD ERRORS AT THE PARTY LEVEL
#######################################################


##################
# DIFFERENT WAYS OF CALCULATING SEs
################

#balancing
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$benefitClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
dat.balance$partygroup<-factor(dat.balance$partygroup)

fit1<-lm(benefitClean~time*partygroup,data=dat.balance[dat.balance$time%in%c(2,3),])
fit_party<-lmer(benefitClean~time*partygroup+(1|partyAll),data=dat.balance[dat.balance$time%in%c(2,3),])

cf<-c(fixef(fit_party)[4],coef(fit1)[4],coef(fit1)[4])
se<-c(sqrt(diag(vcov(fit_party)))[4],
	sqrt(diag(vcovCL(fit1,cluster=~partyAll,type="HC1")))[4],
	sqrt(diag(vcovCL(fit1,cluster=~id,type="HC1")))[4]
	)

#PLOTTING

#pdf("robustSE.pdf",width=5,height=6)

par(mai=c(.5,1.5,1,.5))
plot(1:3,x=cf,xlim=c(-.025,0.25),ylim=c(0.5,3.5),xaxt='n',ylab=NA,xlab="",pch=16,yaxt='n')
segments(cf-1.96*se,1:3,cf+1.96*se,1:3)
axis(2,1:3,labels=c("MLE Mixed-Effects DiD\nParty-Specific RE","OLS DiD\nCluster Robust SE\n(party-level, HC1)","OLS DiD\nCluster Robust SE\n(unit-level, HC1)"),srt=90,adj=1,cex.axis=.6,las=2)
abline(v=0,lty=2)
axis(3)
mtext(3,line=2.5,text="Estimated Difference-in-Differences")

#dev.off()


#EARLY RETIREMENT

#balancing
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$retire_cutClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;else=0')
dat.balance$partygroup<-factor(dat.balance$partygroup)

fit1<-lm(retire_cutClean~time*partygroup,data=dat.balance[dat.balance$time%in%c(3,4),])
coeftest(fit1,vcovCL(fit1,cluster=~id,type="HC1"))
coeftest(fit1,vcovCL(fit1,cluster=~partyAll,type="HC1")) 

fit_party<-lmer(retire_cutClean~time*partygroup+(1|partyAll),data=dat.balance[dat.balance$time%in%c(3,4),])
summary(fit_party)

cf<-c(fixef(fit_party)[4],coef(fit1)[4],coef(fit1)[4])
se<-c(sqrt(diag(vcov(fit_party)))[4],
sqrt(diag(vcovCL(fit1,cluster=~partyAll,type="HC1")))[4],	
sqrt(diag(vcovCL(fit1,cluster=~id,type="HC1")))[4]	
	)

#PLOTTING

#pdf("robustSE2.pdf",width=5,height=6)
par(mai=c(.5,1.5,1,.5))
plot(1:3,x=cf,xlim=c(-.025,0.25),ylim=c(0.5,3.5),xaxt='n',ylab=NA,xlab="",pch=16,yaxt='n')
segments(cf-1.96*se,1:3,cf+1.96*se,1:3)
axis(2,1:3,labels=c("MLE Mixed-Effects DiD\nParty-Specific RE","OLS DiD\nCluster Robust SE\n(party-level, HC1)","OLS DiD\nCluster Robust SE\n(unit-level, HC1)"),srt=90,adj=1,cex.axis=.6,las=2)
abline(v=0,lty=2)
axis(3)
mtext(3,line=2.5,text="Estimated Difference-in-Differences")
#dev.off()



###################
# I MODERATORS	#
###################

ptitle=c("Pressure on welfare state","Gov't crisis responsible","Economy important","Unemployment important","Early retire important",
	"Trust in politicians","Political sophistication","Issue knowledge","Need for cognition","PID strength")


varnames<-c(
  "pressure3",
  "govRespScale3",
  "persimp_econ3","persimp_unemp3","persimp_efterloen3",
  "trust_index3",
  "polsoph3","issueknow3","nfc_index3","partystrinvar")


#####
# ISSUE: Unemployment Benefits

dat.balance<-dat[!is.na(dat$partyAll),]


dat.balance$cue<-recode(as.numeric(dat.balance$time),'2=0;3=1;else=NA')
dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')

treated.out<-list()
control.out<-list()

for (i in 1:length(varnames)){
	model<-as.formula(paste0("benefitClean~cue*",paste0(paste0("as.factor(",varnames[i]),")")))

#fits model for treatment group
	model_fit<-plm(model,data=dat.balance[dat.balance$partygroup==1,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+1.96*model_vcov[-1,1]))
	treated.out[[i]] <- cbind(meff,meff_se)

#fits model for control group
	model_fit<-plm(model,data=dat.balance[dat.balance$partygroup==0,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+1.96*model_vcov[-1,1]))
	control.out[[i]] <- cbind(meff,meff_se)
}

treated_coef<-do.call(rbind.data.frame,treated.out)
control_coef<-do.call(rbind.data.frame,control.out)


######
# OTHER ISSUE

dat.balance$cue<-recode(as.numeric(dat.balance$time),'3=0;4=1;else=NA')
dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;else=0')

treated.out<-list()
control.out<-list()

for (i in 1:length(varnames)){
	model<-as.formula(paste0("retire_cutClean~cue*",paste0(paste0("as.factor(",varnames[i]),")")))

#fits model for treatment group
	model_fit<-plm(model,data=dat.balance[dat.balance$partygroup==1,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+1.96*model_vcov[-1,1]))
	treated.out[[i]] <- cbind(meff,meff_se)

#fits model for control group
	model_fit<-plm(model,data=dat.balance[dat.balance$partygroup==0,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+1.96*model_vcov[-1,1]))
	control.out[[i]] <- cbind(meff,meff_se)
}

treated_coef2<-do.call(rbind.data.frame,treated.out)
control_coef2<-do.call(rbind.data.frame,control.out)



##########
# PLOTTING MODEL RESULTS

#pdf('moderators_combi.pdf',width=7,height=6.25)

jit=0.1
par(omd=c(.25,1,0,1),mfrow=c(1,2),mai=c(1,.15,.15,.15))
plot(x=treated_coef[,1],y=1:nrow(treated_coef),xlim=c(-.2,.4),xaxt='n',yaxt='n',xlab="",ylab="",ylim=c(1,nrow(treated_coef)+1),col="white")

abline(h=1:nrow(treated_coef),col="grey80")
abline(v=seq(-.5,.5,.05),lty=2,col="grey80")
abline(v=0,lty=1)

#connect categories
lines(treated_coef[1:3,1],1:3+jit)
lines(treated_coef[4:6,1],4:6+jit)
lines(treated_coef[7:9,1],7:9+jit)
lines(treated_coef[10:12,1],10:12+jit)
lines(treated_coef[13:15,1],13:15+jit)
lines(treated_coef[16:18,1],16:18+jit)
lines(treated_coef[19:21,1],19:21+jit)
lines(treated_coef[22:24,1],22:24+jit)
lines(treated_coef[25:27,1],25:27+jit)
lines(treated_coef[28:29,1],28:29+jit)

lines(control_coef[1:3,1],1:3-jit)
lines(control_coef[4:6,1],4:6-jit)
lines(control_coef[7:9,1],7:9-jit)
lines(control_coef[10:12,1],10:12-jit)
lines(control_coef[13:15,1],13:15-jit)
lines(control_coef[16:18,1],16:18-jit)
lines(control_coef[19:21,1],19:21-jit)
lines(control_coef[22:24,1],22:24-jit)
lines(control_coef[25:27,1],25:27-jit)
lines(control_coef[28:29,1],28:29-jit)

segments(treated_coef[,1]+2*treated_coef[,2],1:nrow(treated_coef)+jit,treated_coef[,1]-2*treated_coef[,2],1:nrow(treated_coef)+jit)
segments(control_coef[,1]+2*control_coef[,2],1:nrow(control_coef)-jit,control_coef[,1]-2*control_coef[,2],1:nrow(control_coef)-jit)

points(y=1:nrow(treated_coef)+jit,x=treated_coef[,1],pch=21,bg="white")
points(y=1:nrow(control_coef)-jit,x=control_coef[,1],pch=21,bg="black")


axis(2,1:27,labels=c(rep(c("L","M","H"),9)),las=2,cex.axis=0.6,tick=FALSE,line=-.5)
axis(2,28:29,labels=c("L","H"),las=2,cex.axis=0.6,tick=FALSE,line=-.5)
axis(2,1:29,labels=NA,las=2,cex.axis=0.6,tck=-0.025)

axis(2,c(1,3),line=1.5,tck=.025,labels=NA)
axis(2,2,line=1,tick=FALSE,labels="Pressure on\nwelfare state",las=2,cex.axis=.8)
axis(2,c(4,6),line=1.5,tck=.025,labels=NA)
axis(2,5,line=1,tick=FALSE,labels="Gov't crisis\nresponsible",las=2,cex.axis=.8)
axis(2,c(7,9),line=1.5,tck=.025,labels=NA)
axis(2,8,line=1,tick=FALSE,labels="Economy\nimportant",las=2,cex.axis=.8)
axis(2,c(10,12),line=1.5,tck=.025,labels=NA)
axis(2,11,line=1,tick=FALSE,labels="Unemployment\nimportant",las=2,cex.axis=.8)
axis(2,c(13,15),line=1.5,tck=.025,labels=NA)
axis(2,14,line=1,tick=FALSE,labels="Early retire\nimportant",las=2,cex.axis=.8)
axis(2,c(16,18),line=1.5,tck=.025,labels=NA)
axis(2,17,line=1,tick=FALSE,labels="Trust in\npoliticians",las=2,cex.axis=.8)
axis(2,c(19,21),line=1.5,tck=.025,labels=NA)
axis(2,20,line=1,tick=FALSE,labels="Political\nsophistication",las=2,cex.axis=.8)
axis(2,c(22,24),line=1.5,tck=.025,labels=NA)
axis(2,23,line=1,tick=FALSE,labels="Issue\nknowledge",las=2,cex.axis=.8)
axis(2,c(25,27),line=1.5,tck=.025,labels=NA)
axis(2,26,line=1,tick=FALSE,labels="Need for\ncognition",las=2,cex.axis=.8)
axis(2,c(28,29),line=1.5,tck=.025,labels=NA)
axis(2,28.5,line=1,tick=FALSE,labels="PID strength",las=2,cex.axis=.8)


axis(1)
mtext(1,line=3.5,text="First-Differences\n(Wave 3 - Wave 2)")

rng<-par("usr")
rect(rng[1],29.75,rng[2],rng[4],col="grey90")
mtext(3,line=-1,text="Unemployment benefits",cex=.8)
text(y=27.5,x=.25,label="Treated",cex=.6)
text(y=27.5,x=-.1,label="Control",cex=.6)



###
## OTHER ISSUE
par(omd=c(.25,1,0,1),new=TRUE)
plot(x=treated_coef2[,1],y=1:nrow(treated_coef2),xlim=c(-.2,.4),xaxt='n',yaxt='n',xlab="",ylab="",ylim=c(1,nrow(treated_coef2)+1),col="white")



abline(h=1:nrow(treated_coef2),col="grey80")
abline(v=seq(-.5,.5,.05),lty=2,col="grey80")
abline(v=0,lty=1)

#connect categories
lines(treated_coef2[1:3,1],1:3+jit)
lines(treated_coef2[4:6,1],4:6+jit)
lines(treated_coef2[7:9,1],7:9+jit)
lines(treated_coef2[10:12,1],10:12+jit)
lines(treated_coef2[13:15,1],13:15+jit)
lines(treated_coef2[16:18,1],16:18+jit)
lines(treated_coef2[19:21,1],19:21+jit)
lines(treated_coef2[22:24,1],22:24+jit)
lines(treated_coef2[25:27,1],25:27+jit)
lines(treated_coef2[28:29,1],28:29+jit)

lines(control_coef2[1:3,1],1:3-jit)
lines(control_coef2[4:6,1],4:6-jit)
lines(control_coef2[7:9,1],7:9-jit)
lines(control_coef2[10:12,1],10:12-jit)
lines(control_coef2[13:15,1],13:15-jit)
lines(control_coef2[16:18,1],16:18-jit)
lines(control_coef2[19:21,1],19:21-jit)
lines(control_coef2[22:24,1],22:24-jit)
lines(control_coef2[25:27,1],25:27-jit)
lines(control_coef2[28:29,1],28:29+jit)

segments(lty=1,treated_coef2[,1]+2*treated_coef2[,2],1:nrow(treated_coef2)+jit,treated_coef2[,1]-2*treated_coef2[,2],1:nrow(treated_coef2)+jit)
segments(lty=1,control_coef2[,1]+2*control_coef2[,2],1:nrow(control_coef2)-jit,control_coef2[,1]-2*control_coef2[,2],1:nrow(control_coef2)-jit)
points(y=1:nrow(treated_coef2)+jit,x=treated_coef2[,1],pch=21,bg="white")
points(y=1:nrow(control_coef2)-jit,x=control_coef2[,1],pch=21,bg="black")

axis(1)
mtext(1,line=3.5,text="First-Differences\n(Wave 4 - Wave 3)")

rng<-par("usr")
rect(rng[1],29.75,rng[2],rng[4],col="grey90")
mtext(3,line=-1,text="Early retirement",cex=.8)
text(y=27.5,x=.25,label="Treated",cex=.6)
text(y=27.5,x=-.125,label="Control",cex=.6)

#dev.off()




#########
# MODERATOR: PLACEBO ANALYSIS


##########
# ISSUE: Unemployment Benefit Period



varnames<-c(
	"pressure3",
	"govRespScale3",
	"persimp_econ3","persimp_unemp3","persimp_efterloen3",
	"trust_index3",
	"polsoph3","issueknow3","nfc_index3","partystrinvar")

dat.balance<-dat[!is.na(dat$partyAll),]

dat.balance$cue<-recode(as.numeric(dat.balance$time),'1=0;2=1;else=NA')
dat.balance$treated<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')

treated.out<-list()
control.out<-list()

for (i in 1:length(varnames)){
	model<-as.formula(paste0("benefitClean~cue*",paste0(paste0("as.factor(",varnames[i]),")")))

#fits model for control	
	model_fit<-plm(model,data=dat.balance[dat.balance$treated==0,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+2*model_vcov[-1,1]))
	treated.out[[i]] <- cbind(meff,meff_se)

#fits model for treated	
	model_fit<-plm(model,data=dat.balance[dat.balance$treated==1,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+2*model_vcov[-1,1]))
	control.out[[i]] <- cbind(meff,meff_se)
}

treated_coef<-do.call(rbind.data.frame,treated.out)
control_coef<-do.call(rbind.data.frame,control.out)


######
# OTHER ISSUE

dat.balance$cue<-recode(as.numeric(dat.balance$time),'1=0;2=1;else=NA')
dat.balance$treated<-recode(dat.balance$partyAll,'"V"=1;else=0')

treated.out<-list()
control.out<-list()

for (i in 1:length(varnames)){
	model<-as.formula(paste0("retire_cutClean~cue*",paste0(paste0("as.factor(",varnames[i]),")")))

#fits model for control	
	model_fit<-plm(model,data=dat.balance[dat.balance$treated==0,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+2*model_vcov[-1,1]))
	treated.out[[i]] <- cbind(meff,meff_se)

#fits model for treated
	model_fit<-plm(model,data=dat.balance[dat.balance$treated==1,],index=c('id','time'),model="within")
	model_vcov<-vcovHC(model_fit,cluster="group")
	meff		<- c(coef(model_fit)[1],coef(model_fit)[1]+coef(model_fit)[-1])
	meff_se	<- c(sqrt(model_vcov[1,1]),sqrt(model_vcov[1,1]+diag(model_vcov)[-1]+2*model_vcov[-1,1]))
	control.out[[i]] <- cbind(meff,meff_se)
}

treated_coef2<-do.call(rbind.data.frame,treated.out)
control_coef2<-do.call(rbind.data.frame,control.out)



##########
# PLOTTING MODEL RESULTS

#pdf('moderators_combi_placebo.pdf',width=7,height=6.25)

jit=0.1
par(omd=c(.25,1,0,1),mfrow=c(1,2),mai=c(1,.15,.15,.15))
plot(x=treated_coef[,1],y=1:nrow(treated_coef),xlim=c(-.2,.4),xaxt='n',yaxt='n',xlab="",ylab="",ylim=c(1,nrow(treated_coef)+1),col="white")

abline(h=1:nrow(treated_coef),col="grey80")
abline(v=seq(-.5,.5,.05),lty=2,col="grey80")
abline(v=0,lty=1)

#connect categories
lines(treated_coef[1:3,1],1:3+jit)
lines(treated_coef[4:6,1],4:6+jit)
lines(treated_coef[7:9,1],7:9+jit)
lines(treated_coef[10:12,1],10:12+jit)
lines(treated_coef[13:15,1],13:15+jit)
lines(treated_coef[16:18,1],16:18+jit)
lines(treated_coef[19:21,1],19:21+jit)
lines(treated_coef[22:24,1],22:24+jit)
lines(treated_coef[25:27,1],25:27+jit)
lines(treated_coef[28:30,1],28:30+jit)

lines(control_coef[1:3,1],1:3-jit)
lines(control_coef[4:6,1],4:6-jit)
lines(control_coef[7:9,1],7:9-jit)
lines(control_coef[10:12,1],10:12-jit)
lines(control_coef[13:15,1],13:15-jit)
lines(control_coef[16:18,1],16:18-jit)
lines(control_coef[19:21,1],19:21-jit)
lines(control_coef[22:24,1],22:24-jit)
lines(control_coef[25:27,1],25:27-jit)
lines(control_coef[28:30,1],28:30-jit)


segments(treated_coef[,1]+2*treated_coef[,2],1:nrow(treated_coef)+jit,treated_coef[,1]-2*treated_coef[,2],1:nrow(treated_coef)+jit)
segments(control_coef[,1]+2*control_coef[,2],1:nrow(control_coef)-jit,control_coef[,1]-2*control_coef[,2],1:nrow(control_coef)-jit)

points(y=1:nrow(treated_coef)+jit,x=treated_coef[,1],pch=21,bg="white")
points(y=1:nrow(control_coef)-jit,x=control_coef[,1],pch=21,bg="black")


axis(2,1:27,labels=c(rep(c("L","M","H"),9)),las=2,cex.axis=0.6,tick=FALSE,line=-.5)
axis(2,1:29,labels=NA,las=2,cex.axis=0.6,tck=-0.025)
axis(2,28:29,labels=c("L","H"),las=2,cex.axis=0.6,tick=FALSE,line=-.5)



axis(2,c(1,3),line=1.5,tck=.025,labels=NA)
axis(2,2,line=1,tick=FALSE,labels="Pressure on\nwelfare state",las=2,cex.axis=.8)
axis(2,c(4,6),line=1.5,tck=.025,labels=NA)
axis(2,5,line=1,tick=FALSE,labels="Gov't crisis\nresponsible",las=2,cex.axis=.8)
axis(2,c(7,9),line=1.5,tck=.025,labels=NA)
axis(2,8,line=1,tick=FALSE,labels="Economy\nimportant",las=2,cex.axis=.8)
axis(2,c(10,12),line=1.5,tck=.025,labels=NA)
axis(2,11,line=1,tick=FALSE,labels="Unemployment\nimportant",las=2,cex.axis=.8)
axis(2,c(13,15),line=1.5,tck=.025,labels=NA)
axis(2,14,line=1,tick=FALSE,labels="Early retire\nimportant",las=2,cex.axis=.8)
axis(2,c(16,18),line=1.5,tck=.025,labels=NA)
axis(2,17,line=1,tick=FALSE,labels="Trust in\npoliticians",las=2,cex.axis=.8)
axis(2,c(19,21),line=1.5,tck=.025,labels=NA)
axis(2,20,line=1,tick=FALSE,labels="Political\nsophistication",las=2,cex.axis=.8)
axis(2,c(22,24),line=1.5,tck=.025,labels=NA)
axis(2,23,line=1,tick=FALSE,labels="Issue\nknowledge",las=2,cex.axis=.8)
axis(2,c(25,27),line=1.5,tck=.025,labels=NA)
axis(2,26,line=1,tick=FALSE,labels="Need for\ncognition",las=2,cex.axis=.8)
axis(2,c(28,29),line=1.5,tck=.025,labels=NA)
axis(2,28.5,line=1,tick=FALSE,labels="PID strength",las=2,cex.axis=.8)



axis(1)
mtext(1,line=3.5,text="Placebo: First-Differences\n(Wave 2 - Wave 1)")

rng<-par("usr")
rect(rng[1],29.75,rng[2],rng[4],col="grey90")
mtext(3,line=-1,text="Unemployment benefits",cex=.8)

#text(y=28.5,x=.25,label="Center-Right",cex=.6)
#text(y=28.5,x=-.1,label="Center-Left",cex=.6)
#legend('bottomright',pch=21,pt.bg=c("white","black"), legend=c("Center-Right","Center-Left"),cex=.8,pt.cex=1,bg="white")


###
## OTHER ISSUE
par(omd=c(.25,1,0,1),new=TRUE)
plot(x=treated_coef2[,1],y=1:nrow(treated_coef2),xlim=c(-.2,.4),xaxt='n',yaxt='n',xlab="",ylab="",ylim=c(1,nrow(treated_coef2)+1),col="white")

abline(h=1:nrow(treated_coef2),col="grey80")
abline(v=seq(-.5,.5,.05),lty=2,col="grey80")
abline(v=0,lty=1)

#connect categories
lines(treated_coef2[1:3,1],1:3+jit)
lines(treated_coef2[4:6,1],4:6+jit)
lines(treated_coef2[7:9,1],7:9+jit)
lines(treated_coef2[10:12,1],10:12+jit)
lines(treated_coef2[13:15,1],13:15+jit)
lines(treated_coef2[16:18,1],16:18+jit)
lines(treated_coef2[19:21,1],19:21+jit)
lines(treated_coef2[22:24,1],22:24+jit)
lines(treated_coef2[25:27,1],25:27+jit)
lines(treated_coef2[28:29,1],28:29+jit)


lines(control_coef2[1:3,1],1:3-jit)
lines(control_coef2[4:6,1],4:6-jit)
lines(control_coef2[7:9,1],7:9-jit)
lines(control_coef2[10:12,1],10:12-jit)
lines(control_coef2[13:15,1],13:15-jit)
lines(control_coef2[16:18,1],16:18-jit)
lines(control_coef2[19:21,1],19:21-jit)
lines(control_coef2[22:24,1],22:24-jit)
lines(control_coef2[25:27,1],25:27-jit)
lines(control_coef2[28:29,1],28:29+jit)

segments(lty=1,treated_coef2[,1]+2*treated_coef2[,2],1:nrow(treated_coef2)+jit,treated_coef2[,1]-2*treated_coef2[,2],1:nrow(treated_coef2)+jit)
segments(lty=1,control_coef2[,1]+2*control_coef2[,2],1:nrow(control_coef2)-jit,control_coef2[,1]-2*control_coef2[,2],1:nrow(control_coef2)-jit)
points(y=1:nrow(treated_coef2)+jit,x=treated_coef2[,1],pch=21,bg="white")
points(y=1:nrow(control_coef2)-jit,x=control_coef2[,1],pch=21,bg="black")

axis(1)
mtext(1,line=3.5,text="Placebo: First-Differences\n(Wave 2 - Wave 1)")


rng<-par("usr")
rect(rng[1],29.75,rng[2],rng[4],col="grey90")
mtext(3,line=-1,text="Early retirement",cex=.8)

#dev.off()


#######################
# J TRUST IN POLITICAL PARTIES IN DENMARK
#######################


##################
# ESS ANALYSIS: TRUST ACROSS DIFFERENT EUROPEAN COUNTRIES
##################

ess<-import("ESS1-8e01.dta")
str(ess)
names(ess)

#TRUST IN PARTIES

trust<-aggregate(trstprt~cntry*essround,data=ess,mean)
cnt<-unique(ess$cntry)

y<-trust$trstprt[trust$cntry==cnt[1]]
x<-trust$essround[trust$cntry==cnt[1]]


#pdf('esstrust.pdf',width=6,height=5)

par(mfrow=c(1,1),mai=c(1,1,.5,.5),omd=c(0,1,0,1))
plot(x,y,ylim=c(0,10),xlim=c(2,8),col="white",yaxt='n',xaxt='n',ylab="Trust in parties",xlab="ESS Round (Year)")
axis(1,at=1:8,labels=c(2002,2004,2006,2008,2010,2012,2014,2016))
axis(2,las=2,at=seq(0,10,2))
axis(2,las=2,at=1:10,labels=NA)
axis(2,las=2,at=seq(0,10,.2),tck=-0.015,labels=NA)


for (i in 1:length(cnt)){
	y<-trust$trstprt[trust$cntry==cnt[i]]
	x<-trust$essround[trust$cntry==cnt[i]]
	text(x,y,cnt[i],cex=.6,col="grey80")
	lines(x,y,col="grey80")
}

	i<-which(cnt=="DK")
	y<-trust$trstprt[trust$cntry==cnt[i]]
	x<-trust$essround[trust$cntry==cnt[i]]
	text(x,y,cnt[i],cex=.6,col="white")
	lines(x,y,col="black")
	points(x,y,bg="black",pch=21,cex=.6)
	text(x,y+.3,cnt[i],cex=.6,col="black")


#dev.off()


######################
# K VOTE TRANSITIONS
######################


paneldat<-read.dta("paneldata_wide.dta",convert.underscore=TRUE)

paneldat<-paneldat[paneldat$completion==1,]

paneldat$vote.1cat<-recode(paneldat$vote.1,'8="V";7="DPP";1="Left";2="Right";3="Right";4="Left";5="Right";9="Left";15="Doubt";else=NA')
paneldat$vote.2cat<-recode(paneldat$vote.2,'8="V";7="DPP";1="Left";2="Right";3="Right";4="Left";5="Right";9="Left";15="Doubt";else=NA')
paneldat$vote.3cat<-recode(paneldat$vote.3,'8="V";7="DPP";1="Left";2="Right";3="Right";4="Left";5="Right";9="Left";15="Doubt";else=NA')
paneldat$vote.4cat<-recode(paneldat$vote.4,'8="V";7="DPP";1="Left";2="Right";3="Right";4="Left";5="Right";9="Left";15="Doubt";else=NA')
paneldat$vote.5cat<-recode(paneldat$vote.5,'8="V";7="DPP";1="Left";2="Right";3="Right";4="Left";5="Right";9="Left";15="Doubt";else=NA')

paneldat$vote.1cat<-factor(paneldat$vote.1cat,levels=c("V","DPP","Right","Left","Doubt"))
paneldat$vote.2cat<-factor(paneldat$vote.2cat,levels=c("V","DPP","Right","Left","Doubt"))
paneldat$vote.3cat<-factor(paneldat$vote.3cat,levels=c("V","DPP","Right","Left","Doubt"))
paneldat$vote.4cat<-factor(paneldat$vote.4cat,levels=c("V","DPP","Right","Left","Doubt"))
paneldat$vote.5cat<-factor(paneldat$vote.5cat,levels=c("V","DPP","Right","Left","Doubt"))

transW1W2<-prop.table(table(paneldat$vote.1cat,paneldat$vote.2cat))*100
transW2W3<-prop.table(table(paneldat$vote.2cat,paneldat$vote.3cat))*100
transW3W4<-prop.table(table(paneldat$vote.3cat,paneldat$vote.4cat))*100
transW4W5<-prop.table(table(paneldat$vote.4cat,paneldat$vote.5cat))*100

gain<-rbind(transW1W2[3:5,1],transW2W3[3:5,1],transW3W4[3:5,1],transW4W5[3:5,1])
loss<-rbind(transW1W2[1,3:5],transW2W3[1,3:5],transW3W4[1,3:5],transW4W5[1,3:5])
loss<- -loss


#CUMULATIVE LOSS/GAIN GRAPH

gain<-rbind(transW1W2[2:5,1],transW2W3[2:5,1],transW3W4[2:5,1],transW4W5[2:5,1])
loss<-rbind(transW1W2[1,2:5],transW2W3[1,2:5],transW3W4[1,2:5],transW4W5[1,2:5])
loss<- -loss

#pdf('votetrans.pdf',width=5,height=6)

par(mai=c(.75,1,1,.5),omd=c(0,1,0,1),mfrow=c(1,1))

shade<-c("skyblue","palegreen","grey80","white")
plot(loss[,1],xlim=c(.75,4.25),ylim=c(-5,5),xaxt='n',las=2,xlab="",ylab="",col="white",main="")

polygon(x=c(1,1:4,4),y=c(0,loss[,1]+loss[,2]+loss[,3]+loss[,4],0),col=shade[4],border="black")
polygon(x=c(1,1:4,4),y=c(0,loss[,1]+loss[,2]+loss[,3],0),col=shade[3],border="black")
polygon(x=c(1,1:4,4),y=c(0,loss[,1]+loss[,2],0),col=shade[2],border="black")
polygon(x=c(1,1:4,4),y=c(0,loss[,1],0),col=shade[1],border="black")

polygon(x=c(1,1:4,4),y=c(0,gain[,1]+gain[,2]+gain[,3]+gain[,4],0),col=shade[4],border="black")
polygon(x=c(1,1:4,4),y=c(0,gain[,1]+gain[,2]+gain[,3],0),col=shade[3],border="black")
polygon(x=c(1,1:4,4),y=c(0,gain[,1]+gain[,2],0),col=shade[2],border="black")
polygon(x=c(1,1:4,4),y=c(0,gain[,1],0),col=shade[1],border="black")

abline(h=0,lty=3)
axis(1,1:4,labels=c("W1-W2","W2-W3","W3-W4","W4-W5"))
mtext(2,text="Change in Liberals' vote share, percent",line=2.5)
axis(2,seq(-5,5,.2),tck=-0.005,labels=NA)
legend('topleft',legend=c("DPP","Party on Right","Party on Left","Undecided"),pch=22,pt.cex=1.5,pt.bg=shade,bty='n',cex=.8)
text(1:4,apply(loss,1,sum),labels=round(apply(loss,1,sum),1),cex=.8,pos=1)
text(1:4,apply(gain,1,sum),labels=sprintf("%.1f",round(apply(gain,1,sum),1)),cex=.8,pos=3)

#dev.off()


####################
# CONDITIONING ON PRIOR OPINION
####################

paneldat$benefit_prior<-recode(paneldat$benefit.1,'1=1;2=1;3=NA;4=2;5=2;6=NA')
table(paneldat$benefit_prior)
paneldat$retire_prior<-recode(paneldat$retire.1,'1=1;2=1;3=NA;4=2;5=2;6=NA')
table(paneldat$retire_prior)

#UNEMPLOYMENT BENEFIT PERIOD
transW1W2_cut<-prop.table(table(paneldat$vote.1cat[paneldat$benefit_prior==1],paneldat$vote.2cat[paneldat$benefit_prior==1]),1)*100
transW1W2_keep<-prop.table(table(paneldat$vote.1cat[paneldat$benefit_prior==2],paneldat$vote.2cat[paneldat$benefit_prior==2]),1)*100
transW2W3_cut<-prop.table(table(paneldat$vote.2cat[paneldat$benefit_prior==1],paneldat$vote.3cat[paneldat$benefit_prior==1]),1)*100
transW2W3_keep<-prop.table(table(paneldat$vote.2cat[paneldat$benefit_prior==2],paneldat$vote.3cat[paneldat$benefit_prior==2]),1)*100

tab<-rbind(transW1W2_cut[1,],transW2W3_cut[1,],transW1W2_keep[1,],transW2W3_keep[1,])
xtable(tab,digits=1)

#number of observations
transW1W2_cut<-table(paneldat$vote.1cat[paneldat$benefit_prior==1],paneldat$vote.2cat[paneldat$benefit_prior==1])
transW1W2_keep<-table(paneldat$vote.1cat[paneldat$benefit_prior==2],paneldat$vote.2cat[paneldat$benefit_prior==2])
transW2W3_cut<-table(paneldat$vote.2cat[paneldat$benefit_prior==1],paneldat$vote.3cat[paneldat$benefit_prior==1])
transW2W3_keep<-table(paneldat$vote.2cat[paneldat$benefit_prior==2],paneldat$vote.3cat[paneldat$benefit_prior==2])

tab<-rbind(transW1W2_cut[1,],transW2W3_cut[1,],transW1W2_keep[1,],transW2W3_keep[1,])
xtable(tab,digits=0)



#EARLY RETIREMENT

#Percentages
transW1W2_cut<-prop.table(table(paneldat$vote.1cat[paneldat$retire_prior==1],paneldat$vote.2cat[paneldat$retire_prior==1]),1)*100
transW1W2_keep<-prop.table(table(paneldat$vote.1cat[paneldat$retire_prior==2],paneldat$vote.2cat[paneldat$retire_prior==2]),1)*100
transW3W4_cut<-prop.table(table(paneldat$vote.3cat[paneldat$retire_prior==1],paneldat$vote.4cat[paneldat$retire_prior==1]),1)*100
transW3W4_keep<-prop.table(table(paneldat$vote.3cat[paneldat$retire_prior==2],paneldat$vote.4cat[paneldat$retire_prior==2]),1)*100

tab<-rbind(transW1W2_cut[1,],transW3W4_cut[1,],transW1W2_keep[1,],transW3W4_keep[1,])
xtable(tab,digits=1)

#Number of obs

transW1W2_cut<-table(paneldat$vote.1cat[paneldat$retire_prior==1],paneldat$vote.2cat[paneldat$retire_prior==1])
transW1W2_keep<-table(paneldat$vote.1cat[paneldat$retire_prior==2],paneldat$vote.2cat[paneldat$retire_prior==2])
transW3W4_cut<-table(paneldat$vote.3cat[paneldat$retire_prior==1],paneldat$vote.4cat[paneldat$retire_prior==1])
transW3W4_keep<-table(paneldat$vote.3cat[paneldat$retire_prior==2],paneldat$vote.4cat[paneldat$retire_prior==2])

tab<-rbind(transW1W2_cut[1,],transW3W4_cut[1,],transW1W2_keep[1,],transW3W4_keep[1,])
xtable(tab,digits=0)



### END ###







