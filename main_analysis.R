##########################################
# HOW POLITICAL PARTIES SHAPE PUBLIC OPINION IN THE REAL WORLD
# AUTHORS: Martin Bisgaard & Rune Slothuus
# VERSION: SEPTEMBER 2020 
# THIS SCRIPT: REPRODUCES ANALYSES IN MAIN ARTICLE
###########################################

rm(list=ls())

#Set user specific directory
#setwd("/mydirectory")

Sys.setenv(LANGUAGE="en")
Sys.setlocale("LC_TIME", "English") 


#installing required packages
install.packages("rio")
install.packages("foreign")
install.packages("car")
install.packages("plyr")
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
require(pBrackets)
require(MatchIt)
require(sandwich)


#load panel data
dat.balance<-read.dta("paneldata_long.dta",convert.factors=TRUE)
paneldat<-read.dta("paneldata_wide.dta",convert.factors=TRUE)
dat.balance$partyAll<-recode(dat.balance$partyAll,'"NA"=NA') #fixing NA value on party variable 

##loaded dataset is unbalanced. Store it as separate object to load it easily for future use.
dat<-dat.balance


###############################
# ANALYSIS OF PARTY POSITIONS #
###############################

#load data
mediadata	<-	import('partypositions.sav')
dim(mediadata)

#how many articles? (number reported in footnote)
length(unique(mediadata$ID))

#how many coded parti positions? (number reported in footnote)
sum(table(unlist(mediadata[,22:28])))

#break up data file into each issue
retiredata <- mediadata[mediadata$issue==2,]
unempdata <- mediadata[mediadata$issue==1,]
dim(retiredata)
names(retiredata)
dim(unempdata)
names(unempdata)


#### ISSUE: UNEMPLOYMENT BENEFIT PERIOD

#Position of PM party + DPP
Vpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posVny,"party"="V")
DPPpos<- data.frame("tid"=unempdata$tid,"pos"=unempdata$posDFny,"party"="DF")
treatment<-rbind(Vpos,DPPpos)

#CENTER-LEFT 
SFpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posSny,"party"="S")
Spos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posSFny,"party"="SF")
ELpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posELny,"party"="EL")
centerLeft<-rbind(SFpos,Spos)

#CENTER-RIGHT 
RVpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posRVny,"party"="RV")
Kpos <- data.frame("tid"=unempdata$tid,"pos"=unempdata$posKny,"party"="K")
centerRight<-rbind(RVpos,Kpos)


#clean and convert date variable
treatment$date <- as.Date(as.character(treatment$tid),"%Y%m%d")
centerLeft$date <- as.Date(as.character(centerLeft$tid),"%Y%m%d") 
centerRight$date <- as.Date(as.character(centerRight$tid),"%Y%m%d") 

#remove '0' from vector (0 = position is not present in article)
treatment$pos[treatment$pos==0]<-NA
centerLeft$pos[centerLeft$pos==0]<-NA
centerRight$pos[centerRight$pos==0]<-NA

#drop NA cases
treatment	<-	na.omit(treatment) 
centerLeft	<-	na.omit(centerLeft)
centerRight	<-	na.omit(centerRight)
dim(treatment)
dim(centerLeft)
dim(centerRight)


###
# REPRODUCING FIGURE 1
###

#pdf("benefitsCue.pdf",width=6,height=4)

cols1=rgb(.1,.1,1,alpha=.175)
cols2=rgb(1,.1,.1,alpha=.175)
cols3=rgb(0,.1,0,alpha=.1)

par(omd=c(.1,1,0,1))
plot(treatment$date,jitter(treatment$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5),xlim=c(as.Date("2010-01-01"),as.Date("2011-03-01")),xaxt='n')
axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
m<-smooth.spline(treatment$pos~treatment$date,spar=1)
lines(m,lwd=1.5)
m<-smooth.spline(centerLeft$pos~centerLeft$date,spar=1)
lines(m,lwd=1.5,col="grey60")
m<-smooth.spline(centerRight$pos~centerRight$date,spar=1)
lines(m,lwd=1.5,lty=2,col="grey60")
text(x=as.Date("2010-06-01"),y=3.4,label="Liberals\nDPP",cex=.8)
text(x=as.Date("2010-10-01"),y=2.4,label="Social Democrats\nSPP",cex=.8,col="grey60")
text(x=as.Date("2010-03-15"),y=5,label="Conservatives\nSocial Liberals",cex=.8,col="grey60")
rng<-par("usr")
rect(15015,rng[3],rng[2],rng[4],col="grey90")
mtext(4,line=-1.35,text="Reduce Unemp. Benefits")

	dens<-c(centerRight$date,centerLeft$date,treatment$date)
	rug(dens)

#dev.off()

##########
# PARTY POSITIONS ON OTHER ISSUE: ABOLISHING EARLY RETIREMENT


#Position of PM party (liberals)
treatment <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posVny,"party"="V")

#Position of CENTER LEFT opposition + DPP
SFpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posSny,"party"="S")
Spos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posSFny,"party"="SF")
DPPpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posDFny,"party"="DF")
centerLeft<-rbind(SFpos,Spos,DPPpos)

#Position of CENTER RIGHT
RVpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posRVny,"party"="RV")
Kpos <- data.frame("tid"=retiredata$tid,"pos"=retiredata$posKny,"party"="K")
centerRight<-rbind(RVpos,Kpos)



#clean and convert date variable
centerLeft$date <- as.Date(as.character(centerLeft$tid),"%Y%m%d") 
centerRight$date <- as.Date(as.character(centerRight$tid),"%Y%m%d") 
treatment$date<-as.Date(as.character(treatment$tid),"%Y%m%d")

#remove missing data
treatment<-na.omit(treatment)
centerLeft	<-	na.omit(centerLeft) 
centerRight	<-	na.omit(centerRight) 

#remove 0's
centerLeft$pos[centerLeft$pos==0]<-NA
centerRight$pos[centerRight$pos==0]<-NA
treatment$pos[treatment$pos==0]<-NA

cols3=rgb(0,.1,0,alpha=.1)

#pdf("retireCue.pdf",width=6,height=4)
    cols1=rgb(.1,.1,1,alpha=.175)
    cols2=rgb(1,.1,.1,alpha=.175)
    par(omd=c(.1,1,0,1))
    plot(treatment$date,jitter(treatment$pos),col="white",yaxt='n',ylab="",xlab="",ylim=c(.5,5.5),xlim=c(as.Date("2010-01-01"),as.Date("2011-03-01")),xaxt='n')
    axis(2,at=1:5,las=2,labels=c("Oppose","Partly\noppose","Ambiguous","Partly\nsupport","Support"),cex.axis=.8)
    axis.Date(1,at=seq(as.Date("2010-01-01"),as.Date("2011-01-01"), by="month"), format="%b",las=1,cex.axis=.8)
    axis.Date(1,at=c(as.Date("2010-01-01"),as.Date("2011-01-01")), format="%Y",las=1,line=1,tick="FALSE",cex.axis=.8)
    m<-smooth.spline(treatment$pos~treatment$date,spar=1)
    lines(m,lwd=1.5)
    m<-smooth.spline(centerLeft$pos~centerLeft$date,spar=1)
    lines(m,lwd=1.5,col="grey60")
    m<-smooth.spline(centerRight$pos~centerRight$date,spar=1)
    lines(m,lwd=1.5,col="grey60",lty=2)

    rng<-par("usr")
    rect(15015,rng[3],rng[2],rng[4],col="grey90")
    mtext(4,line=-1.35,text="Abolish Early Retirement")
    text(x=as.Date("2010-11-01"),y=3.5,label="Liberals",cex=.8)
    text(x=as.Date("2010-07-01"),y=1,label="Social Democrats\nSPP and DPP",cex=.8,col="grey60")
    text(x=as.Date("2010-5-01"),y=5,label="Conservatives\nSocial Liberals",cex=.8,col="grey60")

	dens<-c(centerRight$date,centerLeft$date,treatment$date)
	rug(dens)

#dev.off()




########################
# Reproducing Figure 2 #
########################

#pdf("timeline.pdf",height=2,width=6)

par(mai=c(.1,.1,.1,.1),omd=c(0,1,0,1),mfrow=c(1,1),lend=1)
period<-seq(as.Date("2010-01-01"),as.Date("2011-07-01"), by="month")
plot(0,axes=FALSE,ann=FALSE,xlim=c(as.Date("2010-01-01"),as.Date("2011-07-01")))
lines(c(as.Date("2010-01-01"),as.Date("2011-07-01")),c(.5,.5),lty=2)
lines(c(as.Date("2010-01-01"),as.Date("2010-01-01")),c(.55,.45))
lines(c(as.Date("2011-01-01"),as.Date("2011-01-01")),c(.55,.45))
text(as.Date("2010-01-01"),.7,"2010")
text(as.Date("2011-01-01"),.7,"2011")

for (i in 1:length(period)) lines(rep(period[i],2),c(.525,.475))
mn<-format(period,'%b')
mn[13]<-NA 
text(x=period,y=rep(.38,length(period)),mn,cex=.5)

t=8
tc=rgb(0,.1,0,alpha=.4)
lines(c(as.Date("2010-02-17"),as.Date("2010-02-26")),c(.5,.5),lwd=t,col=tc)
lines(c(as.Date("2010-03-26"),as.Date("2010-04-12")),c(.5,.5),lwd=t,col=tc)
lines(c(as.Date("2010-06-09"),as.Date("2010-06-21")),c(.5,.5),lwd=t,col=tc)
lines(c(as.Date("2011-01-05"),as.Date("2011-01-16")),c(.5,.5),lwd=t,col=tc)
lines(c(as.Date("2011-06-20"),as.Date("2011-06-30")),c(.5,.5),lwd=t,col=tc)
lines(c(as.Date("2010-05-19"),as.Date("2010-05-19")),c(.5,.1),lty=1)
text(as.Date("2010-05-19"),.1,"Liberals and DPP reverse policy position,\nproposing to cut the unemployment benefit\nperiod from four to two years",pos=1,cex=.8,xpd=T)
lines(c(as.Date("2011-01-03"),as.Date("2011-01-03")),c(.5,-.4),lty=1)
text(as.Date("2011-01-20"),-.4,"Liberals reverse policy position, proposing to\nabolish the early retirement program",pos=1,cex=.8)
text(as.Date("2010-03-05"),.8,"Fielding period of panel waves",cex=.8,pos=4)
segments(as.Date("2010-02-27"),0.55,as.Date("2010-03-15"),.75)

#dev.off()


#######################################################################
# Response rates reported in section "Panel Survey Data and Measures" #
#######################################################################

#the calculated response rate (55%) reported in text was obtained from the polling Company, Epinion.

#re-contact acceptance rate (reported in text)
prop.table(table(paneldat$recontact))[1]

#calculation of completion rate (reported in text)
prop.table(table(paneldat$completion))[2]


################################################################
# Descriptives on measures reported in Data and Design section #
################################################################

#how many partisans identify with a party that changes position?
table(dat$partyAll[dat$completion==1]%in%c("V"))/5	#ID with liberals
prop.table(table(dat$partyAll[dat$completion==1]%in%c("V"))) #percent

table(dat$partyAll[dat$completion==1]%in%c("DF"))/5	#ID with DPP
prop.table(table(dat$partyAll[dat$completion==1]%in%c("DF")))	#percent

#distribution on DVs (for whole sample)
mean(dat.balance$benefitClean,na.rm=T)
sd(dat.balance$benefitClean,na.rm=T)
table(dat$benefit)[6]/sum(table(dat$benefit))

mean(dat$retire_cutClean,na.rm=T)
sd(dat$retire_cutClean,na.rm=T)
table(dat$retire_cut)[6]/sum(table(dat$retire_cut))


########################
# Reproducing Figure 3 #
########################

#balancing panel
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$benefitClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

## matched control group ##
	covariates<-c("pressure","education","sex","unemployed","age","income","benefitW1","benefitW2")
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
	
	#summary(match)
	#plot(match)

	dta_m<-match.data(match)
	matchID<-dta_m$id[dta_m$treated==0] #store ID of matched control group

## calculate quantities of interest for plotting

dat.balance$partygroup<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=0')
dat.balance$partygroup<-factor(dat.balance$partygroup)
table(dat.balance$partygroup)/5


dat.balance$partygroup_matched<-recode(dat.balance$partyAll,'"V"=1;"DF"=1;else=NA')
dat.balance$partygroup_matched[dat.balance$id%in%matchID] <- 0

#calculate quantities of interest
SE=function(x)sd(x)/sqrt(length(x)-1) #function to calculate naive SEs
out<-aggregate(benefitClean~partygroup*time,data=dat.balance,mean)
out.se<-aggregate(benefitClean~partygroup*time,data=dat.balance,SE)

outm<-aggregate(benefitClean~partygroup_matched*time,data=dat.balance,mean)
outm.se<-aggregate(benefitClean~partygroup_matched*time,data=dat.balance,SE)


######
# PLOTTING

#sets correct spacing between waves
x<-c("2010-02-22","2010-04-05","2010-06-15","2011-01-11","2011-06-25") 
x<-as.Date(x)


c1="grey70"

#pdf('benefits_plot.pdf',width=6,height=4)
  par(omd=c(0.05,1,0,1),mai=c(1,1,.1,.1),mfrow=c(1,1))
  y=out[out$partygroup==1,3]
  se=out.se[out.se$partygroup==1,3]
  plot(x,y,ylim=c(.2,.8),xlim=c(x[1],as.Date("2011-08-01")),main="",xaxt="n",yaxt="n",ylab="",xlab="")
  segments(x,y+1.96*se,x,y-1.96*se)
  lines(x,y,lty=1)
  points(x,y,pch=21,bg="black")
  y=out[out$partygroup==0,3]
  se=out.se[out.se$partygroup==0,3]
  lines(x,y,lty=1,col=c1)
  points(x,y,pch=24,bg=c1,col=c1)
  segments(x,y+1.96*se,x,y-1.96*se,col=c1)
  y=outm[outm$partygroup==0,3]
  se=outm.se[outm.se$partygroup==0,3]
  lines(x,y,lty=1,col=c1)
  points(x,y,pch=21,bg=c1,col=c1)
  segments(x,y+1.96*se,x,y-1.96*se,col=c1)

#redraw CI's of treatment group (for overlap)
  y=out[out$partygroup==1,3]
  se=out.se[out.se$partygroup==1,3]
  segments(x,y+1.96*se,x,y-1.96*se)
  points(x,y,pch=21,bg="black")

  axis(2,las=2)
  axis(2,at=seq(0,1,.025),tck=-.015,labels=NA)
  mtext(2,text="Policy Support",line=3)
  axis.Date(1,at=seq(as.Date("2010-03-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=1)
  axis.Date(1,at=c(as.Date("2010-03-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=1,line=1,tick="FALSE")
  abline(v=as.Date("2010-05-21"),lty=2)

  legend('bottomright',pch=c(21,24,21),pt.bg=c("black",c1,c1),col=c("black",c1,c1),legend=c("Treated","Control (all)","Control (matched)"),cex=.8,pt.cex=1,bty='n')

#dev.off()


######
# Change among DPP and Liberals
# Note: Exact numbers are not referenced in text, but the analysis is referred to
#####

out_lib<-aggregate(benefitClean~time,data=dat.balance[dat.balance$partyAll=="V",],mean)
out_dpp<-aggregate(benefitClean~time,data=dat.balance[dat.balance$partyAll=="DF",],mean)

out_dpp[3,2]-out_dpp[2,2] #change among DPP
out_lib[3,2]-out_lib[2,2] #change among liberals
out[6,3]-out[4,3] 	  #aggregate 


####
# no. of obs
# reported in caption of figure

table(dat.balance$partygroup)
table(dat.balance$partygroup_matched)

(table(dat.balance$partygroup)/5)
(table(dat.balance$partygroup_matched)/5)

#####
# PLACEBO DID (reported in footnote)

pfit1<-plm(benefitClean~time*partygroup,data=dat.balance[dat.balance$time%in%c(1,2),],model="pooling",index=c("id","time"))
coeftest(pfit1, vcov=vcovHC(pfit1,cluster="group"))[4,]

####
# FD among Lib and DPP voters (reported in text)

pfit1<-plm(benefitClean~time,data=dat.balance[dat.balance$time%in%c(2,3)&dat.balance$partygroup==1,],model="pooling",index=c("id","time"))

coef(pfit1)[1] 	#level before change in policy position
sum(coef(pfit1))	#level after change in policy position
coeftest(pfit1, vcov=vcovHC(pfit1,cluster="group"))[2,] #change


###################################
# Reproducing results in Table 1  #
# DiD models			    #
###################################

pfit1<-plm(benefitClean~partygroup*time,data=dat.balance[dat.balance$time%in%c(2,3),],model="pooling",index=c("id","time"))
	pfit1_out<-coeftest(pfit1,vcov=vcovHC(pfit1,cluster="group"))["partygroup1:time3",]
	eff1<-pfit1_out[1]
	se1<-pfit1_out[2]

pfit2<-plm(benefitClean~partygroup*time+retire_cutClean+bb+ub,data=dat.balance[dat.balance$time%in%c(2,3),],model="pooling",index=c("id","time"))
	pfit2_out<-coeftest(pfit2,vcov=vcovHC(pfit2,cluster="group"))["partygroup1:time3",]
	eff2<-pfit2_out[1]
	se2<-pfit2_out[2]

pfit3<-plm(benefitClean~partygroup_matched*time,data=dat.balance[dat.balance$time%in%c(2,3),],model="pooling",index=c("id","time"))
	pfit3_out<-coeftest(pfit3,vcov=vcovHC(pfit3,cluster="group"))["partygroup_matched:time3",]
	eff3<-pfit3_out[1]
	se3<-pfit3_out[2]

pfit4<-plm(benefitClean~partygroup_matched*time+retire_cutClean+bb+ub,data=dat.balance[dat.balance$time%in%c(2,3),],model="pooling",index=c("id","time"))
	pfit4_out<-coeftest(pfit4,vcov=vcovHC(pfit4,cluster="group"))["partygroup_matched:time3",]
	eff4<-pfit4_out[1]
	se4<-pfit4_out[2]


#constructing table
parenth <- function(x){paste0(paste0("(",x),")")} #small function for putting parentheses around numbers

tab<-round(cbind(pfit1_out,pfit2_out,pfit3_out,pfit4_out),2)[1:2,]
tab[2,]<-parenth(tab[2,])
rownames(tab)<-c("DiD","")
colnames(tab)<-rep("",ncol(tab))

obs<-cbind(pdim(pfit1)$nT[-2],pdim(pfit2)$nT[-2],pdim(pfit3)$nT[-2],pdim(pfit4)$nT[-2])
rownames(obs)<-c("Units","Observations")

stargazer(rbind(tab,obs))
#note: appearance of table has been edited manually 


#p-values reported in text
pfit1_out
pfit3_out


########################
# Reproducing Figure 4 #
########################

#balancing panel for the issue
dat.balance<-dat
dat.balance<-dat.balance[!dat.balance$id %in% (dat.balance[is.na(dat.balance$retire_cutClean)|is.na(dat.balance$partyAll),]$id),]
dim(dat.balance)

## matched control group ##
	covariates<-c("unemployed","pressure","education","sex","age","income","retireW1","retireW2","retireW3")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~ factor(retireW1)+factor(retireW2)+factor(retireW3)+pressure+sex+unemployed+factor(income)+factor(education)+age,
                     method = "nearest", data = dat_match)

	#summary(match)
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
out.se<-aggregate(retire_cutClean~partygroup*time,data=dat.balance,SE)

outm<-aggregate(retire_cutClean~partygroup_matched*time,data=dat.balance,mean)
outm.se<-aggregate(retire_cutClean~partygroup_matched*time,data=dat.balance,SE)


##########
#PLOTTING

c1="grey70"

#pdf('retire_plot.pdf',width=6,height=4)

  par(omd=c(0.05,1,0,1),mai=c(1,1,.1,.1),mfrow=c(1,1))
  y=out[out$partygroup==1,3]
  se=out.se[out.se$partygroup==1,3]
  plot(x,y,ylim=c(.4,1),xlim=c(x[1],as.Date("2011-08-01")),main="",xaxt="n",yaxt="n",ylab="",xlab="")
  segments(x,y+1.96*se,x,y-1.96*se)
  lines(x,y,lty=1)
  points(x,y,pch=21,bg="black")
  y=out[out$partygroup==0,3]
  se=out.se[out.se$partygroup==0,3]
  lines(x,y,lty=1,col=c1)
  points(x,y,pch=24,bg=c1,col=c1)
  segments(x,y+1.96*se,x,y-1.96*se,col=c1)
  y=outm[outm$partygroup==0,3]
  se=outm.se[outm.se$partygroup==0,3]
  lines(x,y,lty=1,col=c1)
  points(x,y,pch=21,bg=c1,col=c1)
  segments(x,y+1.96*se,x,y-1.96*se,col=c1)

#redraw CI's of treatment group (for overlap)
  y=out[out$partygroup==1,3]
  se=out.se[out.se$partygroup==1,3]
  segments(x,y+1.96*se,x,y-1.96*se)
  points(x,y,pch=21,bg="black")

  axis(2,las=2)
  axis(2,at=seq(0,1,.025),tck=-.015,labels=NA)
  mtext(2,text="Policy Support",line=3)
  axis.Date(1,at=seq(as.Date("2010-03-01"),as.Date("2011-08-01"), by="month"),format="%b",las=1,cex.axis=1)
  axis.Date(1,at=c(as.Date("2010-03-01"),as.Date("2011-01-01")),format="%Y",las=1,cex.axis=1,line=1,tick="FALSE")
  abline(v=as.Date("2011-01-01"),lty=2)
  legend('bottomright',pch=c(21,24,21),pt.bg=c("black",c1,c1),col=c("black",c1,c1),legend=c("Treated","Control (all)","Control (matched)"),cex=.8,pt.cex=1,bty='n')

#dev.off()

####
# number of observations (reported in caption for figure 4)

table(dat.balance$partygroup)
table(dat.balance$partygroup_matched)

table(dat.balance$partygroup)/5
table(dat.balance$partygroup_matched)/5

####
# reported change

out<-aggregate(retire_cutClean~time,data=dat.balance[dat.balance$partyAll=="V",],mean)
out[3,2] #pre 
out[4,2] #post


##
# placebo DiD (reported in footnote)

pfit1<-plm(retire_cutClean~partygroup*time,data=dat.balance[dat.balance$time%in%c(1,2),],model="pooling",index=c("id","time"))
coeftest(pfit1,vcov=vcovHC(pfit1,cluster="group"))[4,]

pfit1<-plm(retire_cutClean~partygroup*time,data=dat.balance[dat.balance$time%in%c(2,3),],model="pooling",index=c("id","time"))
coeftest(pfit1,vcov=vcovHC(pfit1,cluster="group"))[4,]

#######################
# Reproducing Table 2 #
#######################

pfit1<-plm(retire_cutClean~partygroup*time,data=dat.balance[dat.balance$time%in%c(3,4),],model="pooling",index=c("id","time"))
	pfit1_out<-coeftest(pfit1,vcov=vcovHC(pfit1,cluster="group"))["partygroup1:time4",]
	eff1<-pfit1_out[1]
	se1<-pfit1_out[2]

pfit2<-plm(retire_cutClean~partygroup*time+benefitClean+bb+ub,data=dat.balance[dat.balance$time%in%c(3,4),],model="pooling",index=c("id","time"))
	pfit2_out<-coeftest(pfit2,vcov=vcovHC(pfit2,cluster="group"))["partygroup1:time4",]
	eff2<-pfit2_out[1]
	se2<-pfit2_out[2]

pfit3<-plm(retire_cutClean~partygroup_matched*time,data=dat.balance[dat.balance$time%in%c(3,4),],model="pooling",index=c("id","time"))
	pfit3_out<-coeftest(pfit3,vcov=vcovHC(pfit3,cluster="group"))["partygroup_matched:time4",]
	eff3<-pfit3_out[1]
	se3<-pfit3_out[2]

pfit4<-plm(retire_cutClean~partygroup_matched*time+benefitClean+bb+ub,data=dat.balance[dat.balance$time%in%c(3,4),],model="pooling",index=c("id","time"))
	pfit4_out<-coeftest(pfit4,vcov=vcovHC(pfit4,cluster="group"))["partygroup_matched:time4",]
	eff4<-pfit4_out[1]
	se4<-pfit4_out[2]


#constructing table
parenth <- function(x){paste0(paste0("(",x),")")} #small function for putting parentheses around numbers

tab<-round(cbind(pfit1_out,pfit2_out,pfit3_out,pfit4_out),2)[1:2,]
tab[2,]<-parenth(tab[2,])
rownames(tab)<-c("DiD","")
colnames(tab)<-rep("",ncol(tab))

obs<-cbind(pdim(pfit1)$nT[-2],pdim(pfit2)$nT[-2],pdim(pfit3)$nT[-2],pdim(pfit4)$nT[-2])
rownames(obs)<-c("Units","Observations")

stargazer(rbind(tab,obs)) #note: appearance of table has been edited manually

#p-values reported in text
pfit1_out

#############################################################
# PARTY CUE EFFECTS CONDITIONAL ON PRIOR POLICY SUPPORT	#
#############################################################

#distribution of policy opinons among Liberal and DPP voters in wave 2 (reported in text)

#ISSUE: UNEMPLOYMENT BENEFITS
dat.balance<-dat
tab1<-table(dat.balance$benefit[dat.balance$partyAll%in%c("V","DF")&dat.balance$time==2])

tab1	#frequencies
prop.table(tab1) #percentages
sum(tab1[4:5]) #how many opposes policy proposal
sum(tab1) 	# total
sum(tab1[4:5])/sum(tab1) # percentage

#ISSUE: EARLY RETIREMENT
tab1<-table(dat.balance$retire_cut[dat.balance$partyAll=="V"&dat.balance$time==3])
tab1	#frequencies

prop.table(tab1) #percentages
sum(tab1[4:5]) #how many opposes
sum(tab1) 	# total
sum(tab1[4:5])/sum(tab1) # percentage


##########
# Reproducing Figure 5
##########

dat.balance<-dat[!is.na(dat$partyAll),]
dim(dat.balance)
dat.balance$benefitW2<-recode(dat.balance$benefitW2,'6=NA')

## matched control group ##
	covariates<-c("pressure","education","sex","unemployed","age","income","benefitW1","benefitW2")
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

	#summary(match)
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

did_est<-c(cf[2],cf[2]+cf[7],cf[2]+cf[8],cf[2]+cf[9],cf[2]+cf[10])
did_se<-c(sqrt(vmat[2,2]),sqrt(vmat[2,2]+vmat[7,7]+2*vmat[2,7]),
	sqrt(vmat[2,2]+vmat[8,8]+2*vmat[2,8]),
	sqrt(vmat[2,2]+vmat[9,9]+2*vmat[2,9]),
	sqrt(vmat[2,2]+vmat[10,10]+2*vmat[10,2]))

#means for plotting (levels etc)
benefit<-aggregate(benefitClean~partygroup_matched*time*factor(benefitW2),data=dat.balance[dat.balance$time%in%2:3,],mean)


############
# Effect among voters who supported cutbacks (reported in text)

coeftest(fit,vcovHC(fit,cluster="group"))[2,]

###########
# Effect among voters who opposed cutbacks (reported in text)

dat.balance$benefitW2f<-as.factor(dat.balance$benefitW2)
dat.balance$benefitW2f<-relevel(dat.balance$benefitW2f,ref='5')

fit<-plm(benefitClean~partygroup_matched*time*benefitW2f,data=dat.balance[dat.balance$time%in%2:3,],index=c("id","time"),model="within")
coeftest(fit,vcovHC(fit,cluster="group"))[2,]




####
# ISSUE: EARLY RETIREMENT

dat.balance$retireW3<-recode(dat.balance$retireW3,'6=NA')

## matched control group ##
	covariates<-c("pressure","education","unemployed","sex","age","income","retireW1","retireW2","retireW3")
	dat_match<-dat.balance[,c(covariates,"partyAll","id","time")]
	dat_match<-dat_match[unique(dat_match$id)&dat_match$time==1,] #return to "wide" format
	dat_match<-na.omit(dat_match)
	dat_match$treated<-recode(dat_match$partyAll,'"V"=1;else=0') #code treatment group vs. control pool
	table(dat_match$treated)

	#get rid of danish letters on values on covariates
	dat_match$income<-as.numeric(as.factor(dat_match$income))
	dat_match$education<-as.factor(as.numeric(as.factor(dat_match$education)))

	match <- matchit(treated ~ factor(retireW1)+factor(retireW2)+factor(retireW3)+pressure+sex+unemployed+factor(income)+factor(education)+age,
                     method = "nearest", data = dat_match)


	#summary(match)
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

did_est2<-c(cf2[2],cf2[2]+cf2[7],cf2[2]+cf2[8],cf2[2]+cf2[9],cf2[2]+cf2[10])
did_se2<-c(sqrt(vmat2[2,2]),sqrt(vmat2[2,2]+vmat2[7,7]+2*vmat2[2,7]),
	sqrt(vmat2[2,2]+vmat2[8,8]+2*vmat2[2,8]),
	sqrt(vmat2[2,2]+vmat2[9,9]+2*vmat2[2,9]),
	sqrt(vmat2[2,2]+vmat2[10,10]+2*vmat2[10,2]))

#means for plotting (levels etc)
retire<-aggregate(retire_cutClean~partygroup_matched*time*factor(retireW3),data=dat.balance[dat.balance$time%in%3:4,],mean)

############
# Effect among voters who supported cutbacks (reported in text)

coeftest(fit2,vcovHC(fit2,cluster="group"))[2,]

###########
# Effect among voters who opposed cutbacks (reported in text)

dat.balance$retireW3f<-as.factor(dat.balance$retireW3)
dat.balance$retireW3f<-relevel(dat.balance$retireW3f,ref='5')
fit<-plm(retire_cutClean~partygroup_matched*time*retireW3f,data=dat.balance[dat.balance$time%in%3:4,],index=c("id","time"),model="within")
coeftest(fit,vcovHC(fit,cluster="group"))[2,]




###########
#PLOTTING 


m<-benefit[benefit$partygroup_matched==1,4]
c<-benefit[benefit$partygroup_matched==0,4]

m2<-retire[retire$partygroup_matched==1,4]
c2<-retire[retire$partygroup_matched==0,4]

#pdf('priorDV.pdf',width=11.25,height=5.25)

par(mfrow=c(2,5))
par(omd=c(0.15,.9,.1,1),mai=c(0.1,0.1,0.5,.1))
header<-c("Before: Support","Before: Partly sup.","Before: Neither","Before: Partly opp.","Before: Oppose")
select<-matrix(c(1,3,5,7,9,2,4,6,8,10),nc=2)

for(i in 1:5){
plot(x=1:2,m[select[i,]],xlim=c(.25,2.75),ylim=c(0,1.15),xaxt='n',yaxt='n',ylab='',xlab='')

lines(1:2,m[select[i,]])
points(2,c[select[i,2]],pch=4)
lines(1:2,c(m[select[i,1]],c[select[i,2]]),lty=2)
points(1:2,m[select[i,]],pch=21,bg="white")

rng<-par('usr')
rect(rng[1],1.05,rng[2],rng[4],col="grey90")
mtext(3,line=-1.4,text=header[i],cex=.8)
axis(2,at=seq(0,1,.25),labels=NA)
axis(2,at=seq(0,1,.05),labels=NA,tck=-0.025)
if(i==1){
	axis(2,at=seq(0,1,.25),las=2)
	mtext(2,text="Policy Support",line=3)
	legend("bottomleft",legend=c("Treated","Control (matched)"),bty='n',pch=c(1,4),cex=1)
	}
brackets(2.1,m[select[i,]][2],2.1,c[select[i,2]],type=4)

text(2.2,(m[select[i,]][2]+c[select[i,2]])/2,labels=format(round(did_est[i],2),nsmall=2),pos=4)
text(2.175,(m[select[i,]][2]+c[select[i,2]])/2-.075,labels=paste0(paste0("(",round(did_se[i],2)),")"),pos=4,cex=.85)
}

rect(rng[2],1.05,3.2,rng[3],col="grey90",xpd=NA)
mtext(4,las=3,text="Unemploy. Benefits",line=0.2,cex=0.8,adj=.25)

par(mai=c(.75,.1,0,.1))

for(i in 1:5){
plot(x=1:2,m2[select[i,]],xlim=c(.25,2.75),ylim=c(0,1),xaxt='n',yaxt='n',ylab='',xlab='')
lines(1:2,m2[select[i,]])
points(2,c2[select[i,2]],pch=4)
lines(1:2,c(m2[select[i,1]],c2[select[i,2]]),lty=2)
points(1:2,m2[select[i,]],pch=21,bg="white")

rng<-par('usr')
axis(2,at=seq(0,1,.25),labels=NA)
axis(2,at=seq(0,1,.05),labels=NA,tck=-0.025)
axis(1,at=1:2,labels=c("Before","After"),cex.axis=1.25)
if(i==1){
	axis(2,at=seq(0,1,.25),las=2)
	mtext(2,text="Policy Support",line=3)
	legend("bottomleft",legend=c("Treated","Control (matched)"),bty='n',pch=c(1,4),cex=1)
	}

brackets(2.1,m2[select[i,]][2],2.1,c2[select[i,2]],type=4)
text(2.2,(m2[select[i,]][2]+c2[select[i,2]])/2,labels=format(round(did_est2[i],2), nsmall = 2),pos=4)
text(2.175,(m2[select[i,]][2]+c2[select[i,2]])/2-.075,labels=paste0(paste0("(",format(round(did_se2[i],2),nsmall=2),")")),pos=4,cex=.85)
}

rect(rng[2],rng[4],3.2,rng[3],col="grey90",xpd=NA)
mtext(4,las=3,text="Early Retirement",line=0.2,cex=0.8,adj=.5)

#dev.off()


#######
# END #
#######

