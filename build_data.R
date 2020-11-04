##########################################
# HOW POLITICAL PARTIES SHAPE PUBLIC OPINION IN THE REAL WORLD
# AUTHORS: Martin Bisgaard & Rune Slothuus
# VERSION: MARCH 2020 
# THIS SCRIPT: BUILDS MAIN PANEL DATA SET (LONG AND WIDE FORMAT)
##########################################

rm(list=ls())

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
install.packages("reshape2")

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
require(reshape2)


#loading original panel data

dat<-read.dta("raw_paneldata.dta",convert.factors=FALSE)

#var.labels <- attr(dat,"var.labels")
#data.key <- data.frame(var.name=names(dat),var.labels)
#data.key

#rename and clean dataframe to focus on variables of interest
paneldat<-dat
paneldat$id<-1:length(paneldat$id) #generate new and more tractable ID variable

paneldat<-rename(dat,c(
	"q17"="retire.1","q17_efter"="retire.2","q17_iii"="retire.3","q17_iv"="retire.4","q17_v"="retire.5",
	"q22"="retire_cut.1","q22_efter"="retire_cut.2","q22_iii"="retire_cut.3","q22_iv"="retire_cut.4","q22_v"="retire_cut.5",
	"q18"="benefit.1","q18_efter"="benefit.2","q18_iii"="benefit.3","q18_iv"="benefit.4","q18_v"="benefit.5",
	"q26"="ub.1","q26_efter"="ub.2","q26_iii"="ub.3","q26_iv"="ub.4","q26_v"="ub.5",
	"q28"="bb.1","q28_efter"="bb.2","q28_iii"="bb.3","q28_iv"="bb.4","q28_v"="bb.5",

	"q42"="govResp","q43"="USResp","q44"="consumResp","q45"="dkBankResp","q46"="intBankResp",
	"q20"="wage","q23"="afford","q19"="deficitCut",
	"q60"="nfc_imp","q61"="nfc_most","q62"="nfc_others",
	"q50"="know_boedskov","q51"="know_imm","q52"="know_evakjer",
	"q100_iii"="trust_pol","q101_iii"="trust_decision",
	"q46_iv"="persimp_econ","q49_iv"="persimp_unemp","q50_iv"="persimp_efterloen",
	"q60_iv"="know_efterloenyear","q61_iv"="know_efterloenmany","q62_iv"="know_dagpenge","q63_iv"="know_whitta",
	"q64_iv"="partypos_S","q65_iv"="partypos_RV","q66_iv"="partypos_C","q67_iv"="partypos_SF","q68_iv"="partypos_LA","q69_iv"="partypos_DF","q70_iv"="partypos_V",

	"q5"="party.1","q5_efter"="party.2","q5_iii"="party.3","q5_iv"="party.4","q5_v"="party.5",
	"q1"="vote.1","q1_efter"="vote.2","q1_iii"="vote.3","q1_iv"="vote.4","q1_v"="vote.5",
	"q2"="vlean.1","q2_efter"="vlean.2","q2_iii"="vlean.3","q2_iv"="vlean.4","q2_v"="vlean.5",
	"q7"="partynear.1","q7_efter"="partynear.2","q7_iii"="partynear.3","q7_iv"="partynear.4","q7_v"="partynear.5",
	"q6"="partystr.1","q6_efter"="partystr.2","q6_iii"="partystr.3","q6_iv"="partystr.4","q6_v"="partystr.5","q67"="recontact"))


colnames(paneldat)[colnames(paneldat)%in%paste0("q49_a",1:11)] <- paste0("knowGov_",c("A","B","C","F","I","K","O","V","OE","Oth","DK"))


# Constructing timeinvariant PID and vote choice indictors - WAVE 1 (latter used for robustness)

#strength of PID
paneldat$partystrinvar<-paneldat$partystr.1
paneldat$partystrinvar<-recode(paneldat$partystrinvar,'3=NA')
table(paneldat$partystrinvar)

#ID measure with all categories (includes leaners)
paneldat$partyAll<-NA
paneldat$partyAll[paneldat$party.1 %in% c(1) | paneldat$partynear.1 %in% c(1)]<-"S"
paneldat$partyAll[paneldat$party.1 %in% c(2) | paneldat$partynear.1 %in% c(2)]<-"RV"
paneldat$partyAll[paneldat$party.1 %in% c(3) | paneldat$partynear.1 %in% c(3)]<-"C"
paneldat$partyAll[paneldat$party.1 %in% c(4) | paneldat$partynear.1 %in% c(4)]<-"SF"
paneldat$partyAll[paneldat$party.1 %in% c(5) | paneldat$partynear.1 %in% c(5)]<-"LA"
paneldat$partyAll[paneldat$party.1 %in% c(6) | paneldat$partynear.1 %in% c(6)]<-"Kr"
paneldat$partyAll[paneldat$party.1 %in% c(7) | paneldat$partynear.1 %in% c(7)]<-"DF"
paneldat$partyAll[paneldat$party.1 %in% c(8) | paneldat$partynear.1 %in% c(8)]<-"V"
paneldat$partyAll[paneldat$party.1 %in% c(9) | paneldat$partynear.1 %in% c(9)]<-"OE"
#paneldat$partyAll[paneldat$party.1 %in% c(10,11,12,13) | paneldat$partynear.1 %in% c(10,11,12,13)]<-"None/other"
table(paneldat$partyAll)


#prior position on issue
paneldat$retireW1<-paneldat$retire_cut.1
paneldat$retireW2<-paneldat$retire_cut.2
paneldat$retireW3<-paneldat$retire_cut.3
paneldat$benefitW1<-paneldat$benefit.1
paneldat$benefitW2<-paneldat$benefit.2

#completion status (did respondent participate in all waves?)
paneldat$completion<-0
paneldat$completion[!is.na(paneldat$retire.1)&!is.na(paneldat$retire.2)
	&!is.na(paneldat$retire.3)&!is.na(paneldat$retire.4)
	&!is.na(paneldat$retire.5)]<-1
table(paneldat$completion)

#Reshaping data frame
keep=c("id","completion","partystrinvar","partyAll","education","sex","age","occupation","income",
	"retireW1","retireW2","retireW3","benefitW1","benefitW2",
	names(paneldat)[which(names(dat)!=names(paneldat))])

#clean data frame of excess variables
new<-paneldat[,(names(paneldat) %in% keep)] 
names(new);dim(new);dim(paneldat)


#reshaping data frame to LONG/stacked version
dat<-reshape(new,direction="long",
	idvar="id",sep=".",varying=keep[grepl("[.]",keep)])
names(dat);dim(dat)


#treat wave as factor variable
dat$time<-as.factor(dat$time)
is.factor(dat$time)


###############################################
# RECODING and CONSTRUCTING VARIABLES FURTHER #

dat$unemp_facts<-recode(dat$ub,'11=NA')
dat$budget_facts<-recode(dat$bb,'11=NA')

dat$sex<-as.numeric(as.factor(dat$sex))-1 #male is 1
table(dat$sex)

#Dependent variable
table(dat$retire)
dat$retireClean<-recode(dat$retire,"1=5;2=4;3=3;4=2;5=1;6=NA")
dat$retireClean<-(dat$retireClean-1)/4
table(dat$retireClean)

table(dat$retire_cut)
dat$retire_cutClean<-recode(dat$retire_cut,"1=5;2=4;3=3;4=2;5=1;6=NA")
dat$retire_cutClean<-(dat$retire_cutClean-1)/4
table(dat$retire_cutClean)

table(dat$benefit)
dat$benefitClean<-recode(dat$benefit,"1=5;2=4;3=3;4=2;5=1;6=NA")
dat$benefitClean<-(dat$benefitClean-1)/4
table(dat$benefitClean)


#CLEAN UP MODERATORS

##
#PRESSURE on welfare state
dat$deficitCut<-recode(dat$deficitCut,'6=NA')
dat$afford<-recode(dat$afford,'6=NA')
dat$wage<-recode(dat$wage,'6=NA')

#index of variables
dat$pressure<-(dat$wage+dat$afford+dat$deficitCut-3)/15


##
#RESPONSIBILITY
dat$govRespClean<-recode(dat$govResp,'6=NA')
dat$USRespClean<-recode(dat$USResp,'6=NA')
dat$consumRespClean<-recode(dat$consumResp,'6=NA')
dat$dkBankRespClean<-recode(dat$dkBankResp,'6=NA')
dat$intBankRespClean<-recode(dat$intBankResp,'6=NA')

grand_blame<-apply(cbind(dat$USRespClean,dat$consumRespClean,dat$dkBankRespClean,dat$intBankRespClean),1,mean)

dat$govRespScale<-dat$govRespClean-grand_blame


##
#POLITICAL SOPHISTICATION (FACTS)

dat$knowGov_combined=0
dat$knowGov_combined[dat$knowGov_V==1 & dat$knowGov_C==1 & is.na(dat$knowGov_A) &is.na(dat$knowGov_B)
	& is.na(dat$knowGov_F)&is.na(dat$knowGov_I)&is.na(dat$knowGov_K)&is.na(dat$knowGov_O)&is.na(dat$knowGov_OE)
	&is.na(dat$knowGov_Oth)&is.na(dat$knowGov_DK)]=1
table(dat$knowGov_combined)

dat<-dat[,-c(9:19)] #clean up dataframe by removing excess variables

dat$know_boedskovClean<-recode(dat$know_boedskov,'1=1;else=0')
table(dat$know_boedskovClean)

dat$know_immClean<-recode(dat$know_imm,'1=1;else=0')
table(dat$know_immClean)

dat$know_evakjerClean<-recode(dat$know_evakjer,'8=1;else=0')
table(dat$know_evakjerClean)

#INDEX
dat$polsoph<-(dat$knowGov_combined+dat$know_boedskovClean+dat$know_evakjerClean+dat$know_immClean)/4
table(dat$polsoph)


##
#ISSUE specific knowledge (wave4)

names(dat)

dat$know_efterloenyearClean<-recode(dat$know_efterloenyear,'4=1;NA=NA;else=0')
dat$know_efterloenmanyClean<-recode(dat$know_efterloenmany,'4=1;5=1;NA=NA;else=0')
dat$know_dagpengeClean<-recode(dat$know_dagpenge,'3=1;NA=NA;else=0')
dat$know_whittaClean<-recode(dat$know_whitta,'5=1;NA=NA;else=0')

dat$issueknow<-(dat$know_efterloenyearClean+dat$know_efterloenmanyClean+dat$know_dagpengeClean+dat$know_whittaClean)/4
table(dat$issueknow)



###
# NEED FOR COGNITION

dat$nfc_imp<-recode(dat$nfc_imp,'8=NA')
dat$nfc_most<-recode(dat$nfc_most,'8=NA')
dat$nfc_others<-recode(dat$nfc_others,'8=NA')

r3<-cbind(dat$nfc_imp,dat$nfc_most,dat$nfc_others)
cor(na.omit(r3))

dat$nfc_index<-(dat$nfc_imp+dat$nfc_most+dat$nfc_others-3)/18
table(dat$nfc_index)


###
# TRUST IN POLITICIANS AND THEIR DECISIONS

dat$trust_pol<-recode(dat$trust_pol,'5=NA;1=4;2=3;3=2;4=1')
dat$trust_pol<-(dat$trust_pol-1)/3
table(dat$trust_pol)


dat$trust_decision<-recode(dat$trust_decision,'1=5;2=4;3=3;4=2;5=1;6=NA')
dat$trust_decision<-(dat$trust_decision-1)/4
table(dat$trust_decision)

dat$trust_index<-(dat$trust_pol + dat$trust_decision)/2
table(dat$trust_index)



###
# Unemployment

dat$unemployed<-recode(dat$occupation,'"Arbejdsløs"=1;else=0')
table(dat$unemployed)

###
# Importance measures

dat$persimp_econ<-recode(dat$persimp_econ,'8=NA')
dat$persimp_econ<-(dat$persimp_econ-1)/6

dat$persimp_efterloen<-recode(dat$persimp_efterloen,'8=NA')
dat$persimp_efterloen<-(dat$persimp_efterloen-1)/6

dat$persimp_unemp<-recode(dat$persimp_unemp,'8=NA')
dat$persimp_unemp<-(dat$persimp_unemp-1)/6

#coarsening moderators

quantile(dat$pressure,na.rm=T,probs=c(.3,.66))
dat$pressure3<-recode(dat$pressure,'0:0.34=2;0.35:0.534=1;0.55:1=0')
table(dat$pressure3)

quantile(dat$govRespScale,na.rm=T,probs=c(.3,.66))
dat$govRespScale3<-recode(dat$govRespScale,'-3.75:-.01=0;0:.76=1;.77:4=2')
table(dat$govRespScale3)

quantile(dat$persimp_econ,na.rm=T,probs=c(.3,.66))
dat$persimp_econ3<-recode(dat$persimp_econ,'0:.67=0;.68:.84=1;.85:1=2')
table(dat$persimp_econ3)

quantile(dat$persimp_unemp,na.rm=T,probs=c(.3,.66))
dat$persimp_unemp3<-recode(dat$persimp_unemp,'0:.5=0;.51:.82=1;.83:1=2')
table(dat$persimp_unemp3)

quantile(dat$persimp_efterloen,na.rm=T,probs=c(.3,.66))
dat$persimp_efterloen3<-recode(dat$persimp_efterloen,'0:.49=0;.5:.82=1;.83:1=2')
table(dat$persimp_efterloen3)

quantile(dat$trust_pol,na.rm=T,probs=c(.3,.66))
dat$trust_pol3<-recode(dat$trust_pol,'0:.32=0;.33:.65=1;.66:1=2')
table(dat$trust_pol3)

quantile(dat$trust_decision,na.rm=T,probs=c(.3,.66))
dat$trust_decision3<-recode(dat$trust_decision,'0:.25=0;.26:.74=1;.75:1=2')
table(dat$trust_decision3)

quantile(dat$trust_index,na.rm=T,probs=c(.3,.66))
dat$trust_index3<-recode(dat$trust_index,'0:.3=0;.31:.55=1;.56:1=2')
table(dat$trust_index3)

quantile(dat$polsoph,na.rm=T,probs=c(.5))
dat$polsoph3<-recode(dat$polsoph,'1=2;0.75=1;0:0.5=0')
table(dat$polsoph3)

quantile(dat$issueknow,na.rm=T,probs=c(.33,.66))
dat$issueknow3<-recode(dat$issueknow,'0:.25=0;.26:.5=1;.51:1=2')
table(dat$issueknow3)

dat$nfc_index<-(dat$nfc_others+dat$nfc_imp+dat$nfc_most-3)/18
quantile(dat$nfc_index,na.rm=T,probs=c(.33,.66))
dat$nfc_index3<-recode(dat$nfc_index,'0:.56=0;.561:.729=1;.73:1=2')
table(dat$nfc_index3)

###
# Translating danish value labels on occupation and education variables


dat$occupation<-as.factor(dat$occupation)
levels(dat$occupation)<-c("Other","Unemployed","Skilled worker","House-making wife / husband","White-collar worker (w/ managerial resp.)","White-collar worker (w/o managerial resp.)",
	"Assisting spouse","Retired","Self-employed","Unskilled worker","Student","Do not wish to report")
table(dat$occupation)

dat$education<-as.factor(dat$education)
levels(dat$education)<-c("Vocational school","Commercial college (HHX or HTX)","Elementary school","High school (STX)","Higher education, short","Higher education, long","Higher education, middle")
table(dat$education)	

###
# Store data files 
dat.balance<-dat

var<-c("vote","vlean","party","partystr","partynear","retire","benefit","retire_cut"
	,"ub","bb","unemp_facts","budget_facts","retireClean","retire_cutClean","benefitClean")
paneldat<-reshape(dat.balance,direction="wide",
	idvar="id",sep=".",v.names=var)

write.dta(paneldat,file="paneldata_wide.dta")
write.dta(dat.balance,file="paneldata_long.dta")



#######
# END	#
#######

