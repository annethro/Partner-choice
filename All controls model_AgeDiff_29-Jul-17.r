#################################################################
############ When to diversify, and with whom? Choosing partners among out-group strangers in lowland Bolivia ###########

#################### Model with ALL CANDIDATE CONTROLS, with age difference not age included #################### 

library(foreach);library(doParallel)
#setwd(...)

############# Set up parallel processing ##############
cl<-makeCluster(50)
registerDoParallel(cl) 
dirs<-list.dirs(path = ".", full.names = TRUE, recursive = TRUE)[-1]

####################################### Load Data

foreach(i=1:100) %dopar% {
library(rstan)
dire=dirs[i]
#Outcome, target's group affiliation with respect to donor, recipient ID for random effect
 Outcome <- as.matrix(read.csv(paste0(dire,"/Amount_Given.csv"),header=FALSE))
 Category <- as.matrix(read.csv(paste0(dire,"/OG.csv"),header=FALSE)) #Ingroup is 1, intermediate is 2 (see Methods section for explanation for its exclusion), and outgroup is 3.
 RecipID <- as.matrix(read.csv(paste0(dire,"/RecipID.csv"),header=FALSE))

#Donor's perceptions of recipients 
 Rich_DR <- as.matrix(read.csv(paste0(dire,"/Rich_DR.csv"),header=FALSE)) #0 is no money, 1 is some money, 2 is money/lots of money.
 Good_DR <- as.matrix(read.csv(paste0(dire,"/Good_DR.csv"),header=FALSE)) #0 not good, 1 a little good, 2 good person.
 Trust_DR <- as.matrix(read.csv(paste0(dire,"/Trust_DR.csv"),header=FALSE)) #0 not trustworthy, 1 a little trustworthy, 2 trustworthy.
 Friend_DR <- data.matrix(read.csv(paste0(dire,"/Friend_DR.csv"),header=FALSE)) #1 is specifically mentioned wanting to be friends with that person when asked if wanted to be friends with any of the six targets. 0 indicates was not mentioned.

#Target's group qualities
 Bens <- data.matrix(read.csv(paste0(dire,"/Perceived_Benefits.csv"),header=FALSE)) #1=BG, 2=C, 3=none (held at zero), 4=R, 5=WG
 Cost <- data.matrix(read.csv(paste0(dire,"/Perceived_Costs.csv"),header=FALSE)) #1=C, 2=BG, 3=none (held at zero), 4=R, 5=D, 6=WG
 
#Donor's qualities
 NumLive <- as.matrix(read.csv(paste0(dire,"/PlacesLived.csv"),header=FALSE))
 NumVis <- as.matrix(read.csv(paste0(dire,"/Visited_zscore.csv"),header=FALSE))
 Media <- as.matrix(read.csv(paste0(dire,"/Media_zscore.csv"),header=FALSE))
 
#Minimal controls
 Nony <- data.matrix(read.csv(paste0(dire,"/Nonanonymous_Play.csv"),header=FALSE))
 Vers <- data.matrix(read.csv(paste0(dire,"/Version.csv"),header=FALSE)) #In order, 1=A, 2=B, etc. (F is held at zero)
 Ethn <- data.matrix(read.csv(paste0(dire,"/GrpType_Recip.csv"),header=FALSE)) #1=ethnic, 2=religion (held at zero)
 Pop <- data.matrix(read.csv(paste0(dire,"/Population.csv"),header=FALSE))#1=intercultural, 2=moseten (held at zero), 3=tsimane

#Variables that had effects on out-group interest in Pisor & Gurven 2016
 SES <- as.matrix(read.csv(paste0(dire,"/log_Subj_SES.csv"),header=FALSE))
 Labor <- as.matrix(read.csv(paste0(dire,"/Coop_Labor.csv"),header=FALSE))
 Ill <- as.matrix(read.csv(paste0(dire,"/Ill_This_Mo.csv"),header=FALSE))
 
#Full set of additional variables, except age difference
 Inc <- as.matrix(read.csv(paste0(dire,"/Income_zscore.csv"),header=FALSE))
 MI <- as.matrix(read.csv(paste0(dire,"/Market_Items_zscore.csv"),header=FALSE))
 Stay <- data.matrix(read.csv(paste0(dire,"/Can_Stay.csv"),header=FALSE))
 Short <- as.matrix(read.csv(paste0(dire,"/Shortfall_Summary.csv"),header=FALSE))
 Extra <- as.matrix(read.csv(paste0(dire,"/Extraverted.csv"),header=FALSE))
 Agree <- as.matrix(read.csv(paste0(dire,"/Agreeable.csv"),header=FALSE))
 Risk <- as.matrix(read.csv(paste0(dire,"/Risk_Aversion.csv"),header=FALSE))
 AgeDiff <- as.matrix(read.csv(paste0(dire,"/AgeDiff.csv"),header=FALSE))
 Educ <- as.matrix(read.csv(paste0(dire,"/Years_Education.csv"),header=FALSE))
 Sex <- data.matrix(read.csv(paste0(dire,"/Sex_Male.csv"),header=FALSE)) #0=female, 1=male
 ChurX <- as.matrix(read.csv(paste0(dire,"/Times_Church_Mo.csv"),header=FALSE))

####################################### Code Data
#Replace NAs, which represent the self column... perceptions of a given recipient should not map onto money saved for the self.

 Category[is.na(Category)] <- 99999
 RecipID[is.na(RecipID)] <- 99999
 Rich_DR[is.na(Rich_DR)] <- 99999
 Good_DR[is.na(Good_DR)] <- 99999
 Trust_DR[is.na(Trust_DR)] <- 99999
 Friend_DR[is.na(Friend_DR)] <- 99999
 
 Bens[is.na(Bens)] <- 99999
 Cost[is.na(Cost)] <- 99999
 Ethn[is.na(Ethn)] <- 99999
 Pop[is.na(Pop)] <- 99999
 
#One individual has NAs for these THREE variables (also missing data for education); these three values will be imputed below. 
 NumLive[is.na(NumLive)] <- 99999
 NumVis[is.na(NumVis)] <- 99999
 Educ[is.na(Educ)] <- 99999

#Two donor-recipient pairs are missing age difference data. These will be imputed below.
 AgeDiff[is.na(AgeDiff)]<-99999

#Making Ethn an indicator variable for whether the target recipient is being identified to the donor on the basis of her ethnicity (or, if 0, her religion).
 Ethn[Ethn==2]<-0 
 
####################################### Define Indices
 N <- nrow(Outcome)
 K <- 7

####################################### Prep data for Stan
model_dat=list(
   N=N,
   K=K,
   Outcome=Outcome,
   Category=Category,
   RecipID=RecipID,
   
   Rich_DR=Rich_DR,
   Good_DR=Good_DR,
   Trust_DR=Trust_DR,
   Friend_DR=Friend_DR,
   
   Bens=Bens,
   Cost=Cost,
   
   NumLive=NumLive,
   NumVis=NumVis,
   Media=Media,
   
   Nony=Nony,
   Ethn=Ethn,
   Pop=Pop,
   Vers=Vers,
   
   SES=SES,
   Labor=Labor,
   Ill=Ill,
   
   AgeDiff=AgeDiff,
   Agree=Agree,
   ChurX=ChurX,
   Educ=Educ,
   Extra=Extra,
   Inc=Inc,
   MI=MI,
   Risk=Risk,
   Sex=Sex,
   Short=Short,
   Stay=Stay
   )

####################################### Stan Model
model_code='
data{              //# All the data the model will need.
 int N;
 int K;

 int Category[N,K];
 int Outcome[N,K];
 int RecipID[N,K];
 
 real Rich_DR[N,K];
 real Good_DR[N,K];
 real Trust_DR[N,K];
 real Friend_DR[N,K];
 
 real Bens[N,K];
 real Cost[N,K];
 
 real NumLive[N,K];
 real NumVis[N,K];
 real Media[N,K];
 
 real Nony[N,K];
 real Ethn[N,K];
 real Pop[N,K];
 real Vers[N,K];
 
 real SES[N,K];
 real Labor[N,K];
 real Ill[N,K];
 
 real AgeDiff[N,K];
 real Agree[N,K];
 real ChurX[N,K];
 real Educ[N,K];
 real Extra[N,K];
 real Inc[N,K];
 real MI[N,K];
 real Risk[N,K];
 real Sex[N,K];
 real Short[N,K];
 real Stay[N,K];
 }

transformed data {  //#Transform categorical variables into indicator variables.
 real Rich_DR1[N,K];
 real Rich_DR2[N,K];
 real Good_DR1[N,K];
 real Good_DR2[N,K];
 real Trust_DR1[N,K];
 real Trust_DR2[N,K];
 
 real BensBG[N,K];
 real BensWG[N,K];
 real BensC[N,K];
 real BensR[N,K];
 
 real CostBG[N,K];
 real CostWG[N,K];
 real CostC[N,K];
 real CostR[N,K];
 real CostD[N,K];
 
 real PopI[N,K];
 real PopT[N,K]; //# 2=Moseten held at zero.
 
 real VersA[N,K];
 real VersB[N,K];
 real VersC[N,K];
 real VersD[N,K];
 real VersE[N,K]; //# 6=F held at zero.
 
 for(n in 1:N){
 for(k in 1:K){
 Rich_DR1[n,k] = if_else(Rich_DR[n,k]==1,1,0);
 Rich_DR2[n,k] = if_else(Rich_DR[n,k]==2,1,0);
 Good_DR1[n,k] = if_else(Good_DR[n,k]==1,1,0);
 Good_DR2[n,k] = if_else(Good_DR[n,k]==2,1,0);
 Trust_DR1[n,k] = if_else(Trust_DR[n,k]==1,1,0);
 Trust_DR2[n,k] = if_else(Trust_DR[n,k]==2,1,0);
 
 BensBG[n,k] = if_else(Bens[n,k]==1,1,0); //#1=BG, 2=C, 3=none (held at zero), 4=R, 5=WG
 BensWG[n,k] = if_else(Bens[n,k]==5,1,0); 
 BensC[n,k] = if_else(Bens[n,k]==2,1,0);
 BensR[n,k] = if_else(Bens[n,k]==4,1,0); 

 CostBG[n,k] = if_else(Cost[n,k]==2,1,0); //#1=C, 2=BG, 3=none (held at zero), 4=R, 5=D, 6=WG
 CostWG[n,k] = if_else(Cost[n,k]==6,1,0); 
 CostC[n,k] = if_else(Cost[n,k]==1,1,0);
 CostR[n,k] = if_else(Cost[n,k]==4,1,0); 
 CostD[n,k] = if_else(Cost[n,k]==5,1,0); 
 
 PopI[n,k] = if_else(Pop[n,k]==1,1,0);
 PopT[n,k] = if_else(Pop[n,k]==3,1,0);
 
 VersA[n,k] = if_else(Vers[n,k]==1,1,0);
 VersB[n,k] = if_else(Vers[n,k]==2,1,0);
 VersC[n,k] = if_else(Vers[n,k]==3,1,0);
 VersD[n,k] = if_else(Vers[n,k]==4,1,0);
 VersE[n,k] = if_else(Vers[n,k]==5,1,0);
 }}

}

parameters {
//# Parameters will be unique to in- versus out-groups, indexed by 1 and 3 (2 is intermediate)

 vector[3] Alpha;          //# Intercepts
 real <lower=0> sigmaZeta; //# Place appropriate limits on sigma for random effects...
 real<lower=0,upper=8> imp_liv; //# ...and imputed values.
 real<lower=-1.0420,upper=3.1174> imp_vis;
 real<lower=0,upper=16> imp_educ;
 real<lower=-31,upper=18> imp_ad;

 vector[3] BetaRich1;
 vector[3] BetaRich2;
 vector[3] BetaGood1;
 vector[3] BetaGood2;
 vector[3] BetaTrust1;
 vector[3] BetaTrust2;
 vector[3] BetaFriend;

 vector[3] BetaBensBG;
 vector[3] BetaBensWG;
 vector[3] BetaBensC;
 vector[3] BetaBensR;
 vector[3] BetaCostBG;
 vector[3] BetaCostWG;
 vector[3] BetaCostC;
 vector[3] BetaCostR;
 vector[3] BetaCostD;
 
 vector[3] BetaLive;
 vector[3] BetaVis;
 vector[3] BetaMed;

 vector[3] BetaNony;
 vector[3] BetaEthn; 
 vector[3] BetaPopI;
 vector[3] BetaPopT; 
 vector[3] BetaVersA; 
 vector[3] BetaVersB; 
 vector[3] BetaVersC;
 vector[3] BetaVersD;
 vector[3] BetaVersE;
 
 vector[3] BetaSES;
 vector[3] BetaLabor;
 vector[3] BetaIll;
 
 vector[3] BetaAgeDiff; 
 vector[3] BetaAgree; 
 vector[3] BetaChurX; 
 vector[3] BetaEduc; 
 vector[3] BetaExtra; 
 vector[3] BetaInc; 
 vector[3] BetaMI; 
 vector[3] BetaRisk; 
 vector[3] BetaSex; 
 vector[3] BetaShort; 
 vector[3] BetaStay;
 
 vector[117] ZetaRecip; 
}

transformed parameters {  //# Impute values for participant 162 on where lived and visited.
//#Impute two missing AgeDiff values where missing.
 real NumLiveI[N,K];
 real NumVisI[N,K];
 real EducI[N,K]; 
 real AgeDiffI[N,K];
 NumLiveI=NumLive;
 NumVisI=NumVis;
 AgeDiffI=AgeDiff;
 EducI=Educ;
 for (k in 1:K){
 NumLiveI[162,k]=imp_liv;
 NumVisI[162,k]=imp_vis; 
 EducI[162,k]=imp_educ;
 AgeDiffI[22,4]=imp_ad;
 AgeDiffI[174,5]=imp_ad;
 }
 }

model{
// Local storage
 vector[K] Link;

 sigmaZeta ~ cauchy(0,2);        //# Specify all prior distributions incl those for imputation.
 imp_liv ~ uniform(0,8);
 imp_vis ~ uniform(-1.0420,3.1174);
 imp_educ ~ uniform(0,16);
 imp_ad ~ uniform(-31,18);
 
 Alpha ~ normal(0,5);           
 
 BetaRich1 ~ normal(0,5);
 BetaRich2 ~ normal(0,5);
 BetaGood1 ~ normal(0,5);
 BetaGood2 ~ normal(0,5);
 BetaTrust1 ~ normal(0,5);
 BetaTrust2 ~ normal(0,5);
 BetaFriend ~ normal(0,5);  
 
 BetaBensBG ~ normal(0,5);  
 BetaBensWG ~ normal(0,5);
 BetaBensC ~ normal(0,5);
 BetaBensR ~ normal(0,5);
 BetaCostBG ~ normal(0,5);  
 BetaCostWG ~ normal(0,5);
 BetaCostC ~ normal(0,5);
 BetaCostR ~ normal(0,5);
 BetaCostD ~ normal(0,5);
 
 BetaLive ~ normal(0,5);
 BetaVis ~ normal(0,5);
 BetaMed ~ normal(0,5);
 
 BetaNony ~ normal(0,5);
 BetaEthn ~ normal(0,5); 
 BetaPopI ~ normal(0,5);  
 BetaPopT ~ normal(0,5);  
 BetaVersA ~ normal(0,5);
 BetaVersB ~ normal(0,5);
 BetaVersC ~ normal(0,5);
 BetaVersD ~ normal(0,5);
 BetaVersE ~ normal(0,5);
 
 BetaSES ~ normal(0,5);
 BetaLabor ~ normal(0,5);
 BetaIll ~ normal(0,5);

 BetaAgeDiff ~ normal(0,5);
 BetaAgree ~ normal(0,5);
 BetaChurX ~ normal(0,5);
 BetaEduc ~ normal(0,5);
 BetaExtra ~ normal(0,5);
 BetaInc ~ normal(0,5);
 BetaMI ~ normal(0,5);
 BetaRisk ~ normal(0,5);
 BetaSex ~ normal(0,5);
 BetaShort ~ normal(0,5);
 BetaStay ~ normal(0,5);
 
 ZetaRecip ~ normal(0,sigmaZeta);   
 
//###################################################################### Model Full Data
 for(n in 1:N){
   for (k in 1:(K-1)){
    Link[k] = Alpha[Category[n,k]] 
	
	+ BetaRich1[Category[n,k]]*Rich_DR1[n,k] + BetaRich2[Category[n,k]]*Rich_DR2[n,k] + BetaGood1[Category[n,k]]*Good_DR1[n,k] + BetaGood2[Category[n,k]]*Good_DR2[n,k] + BetaTrust1[Category[n,k]]*Trust_DR1[n,k] + BetaTrust2[Category[n,k]]*Trust_DR2[n,k] + BetaFriend[Category[n,k]]*Friend_DR[n,k] 
	
	+ BetaBensBG[Category[n,k]]*BensBG[n,k] + BetaBensWG[Category[n,k]]*BensWG[n,k] + BetaBensC[Category[n,k]]*BensC[n,k] + BetaBensR[Category[n,k]]*BensR[n,k] + BetaCostBG[Category[n,k]]*CostBG[n,k] + BetaCostWG[Category[n,k]]*CostWG[n,k] + BetaCostC[Category[n,k]]*CostC[n,k] + BetaCostR[Category[n,k]]*CostR[n,k] + BetaCostD[Category[n,k]]*CostD[n,k] 
	
	+ BetaLive[Category[n,k]]*NumLiveI[n,k] + BetaVis[Category[n,k]]*NumVisI[n,k] + BetaMed[Category[n,k]]*Media[n,k]
	
	+ BetaNony[Category[n,k]]*Nony[n,k] + BetaEthn[Category[n,k]]*Ethn[n,k] + BetaPopI[Category[n,k]]*PopI[n,k] + BetaPopT[Category[n,k]]*PopT[n,k]
	
	+ BetaVersA[Category[n,k]]*VersA[n,k] + BetaVersB[Category[n,k]]*VersB[n,k] + BetaVersC[Category[n,k]]*VersC[n,k] + BetaVersD[Category[n,k]]*VersD[n,k] + BetaVersE[Category[n,k]]*VersE[n,k]
	
	+ BetaSES[Category[n,k]]*SES[n,k] + BetaLabor[Category[n,k]]*Labor[n,k] + BetaIll[Category[n,k]]*Ill[n,k]
	
	+ BetaAgeDiff[Category[n,k]]*AgeDiffI[n,k] + BetaAgree[Category[n,k]]*Agree[n,k] + BetaChurX[Category[n,k]]*ChurX[n,k] + BetaEduc[Category[n,k]]*EducI[n,k] + BetaExtra[Category[n,k]]*Extra[n,k] + BetaInc[Category[n,k]]*Inc[n,k] + BetaMI[Category[n,k]]*MI[n,k] + BetaRisk[Category[n,k]]*Risk[n,k] + BetaSex[Category[n,k]]*Sex[n,k] + BetaShort[Category[n,k]]*Short[n,k] +  BetaStay[Category[n,k]]*Stay[n,k] 
	
	+ ZetaRecip[RecipID[n,k]];
    
	
	}
	
  Link[K] = 0;

 Outcome[n] ~ multinomial(softmax(Link));
 }

}

'

param <- c('Alpha','BetaRich1','BetaRich2','BetaGood1','BetaGood2','BetaTrust1','BetaTrust2','BetaFriend','BetaBensBG','BetaBensWG','BetaBensC','BetaBensR','BetaCostBG','BetaCostWG','BetaCostC','BetaCostR','BetaCostD','BetaLive','BetaVis','BetaMed','BetaNony','BetaEthn','BetaPopI','BetaPopT','BetaVersA','BetaVersB','BetaVersC','BetaVersD','BetaVersE','sigmaZeta','BetaSES','BetaLabor','BetaIll','BetaAgeDiff','BetaAgree','BetaChurX','BetaEduc','BetaExtra','BetaInc','BetaMI','BetaRisk','BetaSex','BetaShort','BetaStay')

 mfit <- stan( model_code=model_code, data=model_dat,refresh=10,chains=4,iter=1000,warmup=500,seed=562941,par=param)
 summ_stat<-summary(mfit, probs = c(0.05, 0.95))$summary
 extr_sams<-as.data.frame(mfit)
 write.csv(extr_sams,paste0("All controls_Agediff_extracted samples_",gsub("^..","",dire),".csv"),na="",row.names=FALSE) #These will be necessary below to examine the posteriors.
 write.csv(summ_stat,paste0("All controls_Agediff_summary statistics_",gsub("^..","",dire),".csv"),na="") #These are useful checks to have for looking at R hats, effective sample sizes, etc. Keep rownames here: they're useful to see which variables have low effective sample sizes or high R hats below.
 
}

######### CHECK R HATS AND EFFECTIVE SAMPLE SIZES ##########
fils_ss<-grep("./All controls_Agediff_summary statistics_Iter",list.files(full.names = TRUE, recursive = TRUE)[-1],value=TRUE)

assig_leng<-names(read.csv("./All controls_Agediff_summary statistics_Iter1.csv",header=T)) #Doesn't matter which one is picked, just need the column names.
ss<-data.frame(matrix(ncol=length(assig_leng)))
names(ss)<-assig_leng
for (i in 1:length(fils_ss)) {ss<-rbind(ss,read.csv(fils_ss[i],header=TRUE))} #Append each results file, one to the other.

ss[order(ss$n_eff),][1:50,] #Examine effective sample sizes that are low...
ss[order(ss$Rhat,decreasing=TRUE),][1:50,] #...and R hats that are high.

######### PROCESS & EXAMINE POSTERIOR DISTRIBUTIONS #########
library(xtable);library(forestplot)

fils_es<-grep("./All controls_Agediff_extracted samples_Iter",list.files(full.names = TRUE, recursive = TRUE)[-1],value=TRUE)
assig_leng<-names(read.csv("./All controls_Agediff_extracted samples_Iter98.csv",header=T)) #Doesn't matter which one is picked, just need the column names.
summ<-data.frame(matrix(ncol=length(assig_leng)))
names(summ)<-assig_leng
for (i in 1:length(fils_es)) {summ<-rbind(summ,read.csv(fils_es[i],header=TRUE))} #Append each results file, one to the other.

stats<-data.frame("Variable"=rep(names(summ),length.out=length(summ)),"Mean"=rep(NA,length(summ)),"CI_5"=rep(NA,length(summ)),"CI_95"=rep(NA,length(summ)),stringsAsFactors=FALSE)
for (i in 1:nrow(stats)){
nam<-names(summ)[i]
focal<-summ[,nam]
focal<-sort(focal)
stats[stats$Variable==nam,"Mean"]<-mean(focal)
stats[stats$Variable==nam,"CI_5"]<-quantile(focal,0.05)
stats[stats$Variable==nam,"CI_95"]<-quantile(focal,0.95)
rm(focal,nam)
}

stats1<-stats[-grep("\\.2\\.",stats$Variable),] #Remove intermediate groups from consideration. (\\ is to indicate to R that the . in grep are not punctuation indicating a regular expression, but that we are actually looking for .2. and removing those rows.
stats1<-stats1[!(stats1$Variable %in% c("X","lp__","sigmaZeta",grep("BetaVers",stats1$Variable,value=TRUE))),] #Focus on predictors at hand.

#Take the exponential to transform the predictors to relative risks.
stats1t<-data.frame("Variable"=stats1$Variable,"Mean"=exp(stats1$Mean),"CI_5"=exp(stats1$CI_5),"CI_95"=exp(stats1$CI_95),stringsAsFactors=FALSE)


######## NEED TO HAVE ALL SAME VARIABLE ON SAME ROW #######
forestplot(labeltext=matrix(stats1t$Variable),mean=stats1t$Mean,lower=stats1t$CI_5,upper=stats1t$CI_95,zero=1)

############### TABLE ###############

# Rename variables for printing
stats1t$Group<-rep(c("IG","OG"),length.out=nrow(stats1t))
stats2<-stats1t[,c(5,1,2,3,4)]

stats2$Variable[grep("Alpha",stats2$Variable)]<-"Intercept"
stats2$Variable[grep("BetaRich1",stats2$Variable)]<-"A little wealthy"
stats2$Variable[grep("BetaRich2",stats2$Variable)]<-"Wealthy"
stats2$Variable[grep("BetaGood1",stats2$Variable)]<-"A little good"
stats2$Variable[grep("BetaGood2",stats2$Variable)]<-"Good"
stats2$Variable[grep("BetaTrust1",stats2$Variable)]<-"A little trustworthy"
stats2$Variable[grep("BetaTrust2",stats2$Variable)]<-"Trustworthy"
stats2$Variable[grep("BetaFriend",stats2$Variable)]<-"Want as friend"
stats2$Variable[grep("BetaBensBG",stats2$Variable)]<-"Benefits: Btwn-group"
stats2$Variable[grep("BetaBensWG",stats2$Variable)]<-"Benefits: Within-group"
stats2$Variable[grep("BetaBensC",stats2$Variable)]<-"Benefits: Character"
stats2$Variable[grep("BetaBensR",stats2$Variable)]<-"Benefits: Religious"
stats2$Variable[grep("BetaCostBG",stats2$Variable)]<-"Costs: Btwn-group"
stats2$Variable[grep("BetaCostWG",stats2$Variable)]<-"Costs: Within-group"
stats2$Variable[grep("BetaCostC",stats2$Variable)]<-"Costs: Character"
stats2$Variable[grep("BetaCostR",stats2$Variable)]<-"Costs: Religious"
stats2$Variable[grep("BetaCostD",stats2$Variable)]<-"Costs: Discrimination"
stats2$Variable[grep("BetaLive",stats2$Variable)]<-"Places lived"
stats2$Variable[grep("BetaVis",stats2$Variable)]<-"Places visited (z-score)"
stats2$Variable[grep("BetaMed",stats2$Variable)]<-"Hours of media (z-score)"
stats2$Variable[grep("BetaSES",stats2$Variable)]<-"Log subjective SES"
stats2$Variable[grep("BetaLabor",stats2$Variable)]<-"Cooperative labor"
stats2$Variable[grep("BetaIll",stats2$Variable)]<-"Ill this month"
stats2$Variable[grep("BetaAgeDiff",stats2$Variable)]<-"Donor-recipient age difference"
stats2$Variable[grep("BetaAgree",stats2$Variable)]<-"Agreeableness"
stats2$Variable[grep("BetaChurX",stats2$Variable)]<-"Times to church this month"
stats2$Variable[grep("BetaEduc",stats2$Variable)]<-"Years of education"
stats2$Variable[grep("BetaExtra",stats2$Variable)]<-"Extraversion"
stats2$Variable[grep("BetaInc",stats2$Variable)]<-"Income (z-scored)"
stats2$Variable[grep("BetaMI",stats2$Variable)]<-"Market items owned (z-score)"
stats2$Variable[grep("BetaRisk",stats2$Variable)]<-"Risk aversion"
stats2$Variable[grep("BetaSex",stats2$Variable)]<-"Sex"
stats2$Variable[grep("BetaShort",stats2$Variable)]<-"Shortfall summary measure"
stats2$Variable[grep("BetaStay",stats2$Variable)]<-"Can stay during flood"
stats2$Variable[grep("BetaNony",stats2$Variable)]<-"Non-anonymous giving"
stats2$Variable[grep("BetaEthn",stats2$Variable)]<-"Group type: Ethnic"
stats2$Variable[grep("BetaPopI",stats2$Variable)]<-"Donor pop.: Intercultural"
stats2$Variable[grep("BetaPopT",stats2$Variable)]<-"Donor pop.: Tsimane'"

# Reorder
stats2$Prediction<-c("","","P1.1",rep("",3),"P1.2",rep("",9),"P2.1",rep("",17),"P3.1",rep("",5),"Controls",rep("",7),"Additional variables",rep("",27))
stats3<-stats2[,c(6,1,2,3,4,5)]
names(stats3)[c(2,5,6)]<-c("Target group","Credible int. 5%","Credible int. 95%")

print(xtable(stats3),include.rownames=FALSE,type="html",file="All controls_AgeDiff_30-Jul-17.html")