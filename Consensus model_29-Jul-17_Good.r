#################################################################
############ When to diversify, and with whom? Choosing partners among out-group strangers in lowland Bolivia ###########

#################### Model with CONSENSUS MEASURES, with goodness included not trust #################### 

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

#Perceptions of recipients from out-group and intermediate group donors OTHER than the focal donor
 Rich_OthR <- as.matrix(read.csv(paste0(dire,"/Rich_OthR.csv"),header=FALSE)) #0 is no money, 1 is some money, 2 is money/lots of money.
 Good_OthR <- as.matrix(read.csv(paste0(dire,"/Good_OthR.csv"),header=FALSE)) #0 not good, 1 a little good, 2 good person.
 #Trust_OthR <- as.matrix(read.csv(paste0(dire,"/Trust_OthR.csv"),header=FALSE)) #0 not trustworthy, 1 a little trustworthy, 2 trustworthy.
 Friend_OthR <- data.matrix(read.csv(paste0(dire,"/Friend_OthR.csv"),header=FALSE)) #1 is specifically mentioned wanting to be friends with that person when asked if wanted to be friends with any of the six targets. 0 indicates was not mentioned.

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
 
####################################### Define Indices
 N <- nrow(Outcome)
 K <- 7
 
####################################### Preparing to impute missing data

# Rich
 Pres_Rich<-ifelse(is.na(Rich_OthR),0,1) # Mark present data as 1, missing data as 0.
 M_Rich<-sum(is.na(Rich_OthR)) # Number of missing data.
 Cumu_Rich<-matrix(cumsum(is.na(Rich_OthR)), nrow=N, ncol=K) # Cell by cell, cumulative number of missing values up to that point. Proceeds column by column.
 Cumu_Rich<-ifelse(Cumu_Rich==0,1,Cumu_Rich) #Make zeroes ones...
 Rich_Raw <-Rich_OthR
 Rich_Raw[is.na(Rich_Raw)]<-99999 # Store NAs as numeric.

# Good 
 Pres_Good<-ifelse(is.na(Good_OthR),0,1)
 M_Good<-sum(is.na(Good_OthR)) 
 Cumu_Good<-matrix(cumsum(is.na(Good_OthR)), nrow=N, ncol=K) 
 Cumu_Good<-ifelse(Cumu_Good==0,1,Cumu_Good) 
 Good_Raw <-Good_OthR
 Good_Raw[is.na(Good_Raw)]<-99999 
 
# Trust 
# Pres_Trust<-ifelse(is.na(Trust_OthR),0,1)
# M_Trust<-sum(is.na(Trust_OthR)) 
# Cumu_Trust<-matrix(cumsum(is.na(Trust_OthR)), nrow=N, ncol=K) 
# Cumu_Trust<-ifelse(Cumu_Trust==0,1,Cumu_Trust) 
# Trust_Raw <-Trust_OthR
# Trust_Raw[is.na(Trust_Raw)]<-99999 

# Friend
 Pres_Friend<-ifelse(is.na(Friend_OthR),0,1)
 M_Friend<-sum(is.na(Friend_OthR)) 
 Cumu_Friend<-matrix(cumsum(is.na(Friend_OthR)), nrow=N, ncol=K) 
 Cumu_Friend<-ifelse(Cumu_Friend==0,1,Cumu_Friend) 
 Friend_Raw <-Friend_OthR
 Friend_Raw[is.na(Friend_Raw)]<-99999 
 
####################################### Code Data
#Replace NAs, which represent the self column... perceptions of a given recipient should not map onto money saved for the self.

 Category[is.na(Category)] <- 99999
 RecipID[is.na(RecipID)] <- 99999
 
 Bens[is.na(Bens)] <- 99999
 Cost[is.na(Cost)] <- 99999
 Ethn[is.na(Ethn)] <- 99999
 Pop[is.na(Pop)] <- 99999
 
#One individual has NAs for these two variables; these two values will be imputed below. 
 NumLive[is.na(NumLive)] <- 99999
 NumVis[is.na(NumVis)] <- 99999

#Making Ethn an indicator variable for whether the target recipient is being identified to the donor on the basis of her ethnicity (or, if 0, her religion).
 Ethn[Ethn==2]<-0 
 
####################################### Prep data for Stan
model_dat=list(
   N=N,
   K=K,
   Outcome=Outcome,
   Category=Category,
   RecipID=RecipID,
   
   Bens=Bens,
   Cost=Cost,
   
   NumLive=NumLive,
   NumVis=NumVis,
   Media=Media,
   
   Nony=Nony,
   Ethn=Ethn,
   Pop=Pop,
   Vers=Vers,

# Everything needed for imputation of the missing values.   
   M_Rich=M_Rich,
   M_Good=M_Good,
   #M_Trust=M_Trust,
   M_Friend=M_Friend,
   
   Rich_Raw=Rich_Raw,
   Good_Raw=Good_Raw,
   #Trust_Raw=Trust_Raw,
   Friend_Raw=Friend_Raw,
   
   Pres_Rich=Pres_Rich,
   Pres_Good=Pres_Good,
   #Pres_Trust=Pres_Trust,
   Pres_Friend=Pres_Friend,
   
   Cumu_Rich=Cumu_Rich,
   Cumu_Good=Cumu_Good,
   #Cumu_Trust=Cumu_Trust,
   Cumu_Friend=Cumu_Friend
   )

####################################### Stan Model
model_code='
data{
 int N;
 int K;

 int Category[N,K];
 int Outcome[N,K];
 int RecipID[N,K];
 
 real Bens[N,K];
 real Cost[N,K];
 
 real NumLive[N,K];
 real NumVis[N,K];
 real Media[N,K];
 
 real Nony[N,K];
 real Ethn[N,K];
 real Pop[N,K];
 real Vers[N,K];
 
 int <lower=0> M_Rich;                //# number of missings wealth
 real Rich_Raw[N,K];                  //# Observed data for wealth 
 int Pres_Rich[N,K];                  //# Binary to declare non-missing
 int <lower=1> Cumu_Rich[N,K];        //# Cumulative sum of missing wealth data
 
 int <lower=0> M_Good;                
 real Good_Raw[N,K];                            
 int Pres_Good[N,K];                       
 int <lower=1> Cumu_Good[N,K];   

 //int <lower=0> M_Trust;                
 //real Trust_Raw[N,K];                            
 //int Pres_Trust[N,K];                       
 //int <lower=1> Cumu_Trust[N,K];   

 int <lower=0> M_Friend;                
 real Friend_Raw[N,K];                            
 int Pres_Friend[N,K];                       
 int <lower=1> Cumu_Friend[N,K];    
 }

transformed data { //#Transform categorical variables into indicator variables.
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
// Parameters will be unique to in- versus out-groups, index by 1 and 2

 vector[3] Alpha;          //# Intercepts

 real <lower=0> sigmaZeta; //# Place appropriate limits on sigma for random effects...

 real <lower=0,upper=8> imp_liv; //# ...and imputed values.
 real <lower=-1.0420,upper=3.1174> imp_vis; //# We are imputing for just one here...
 real <lower=0,upper=2> imp_richo[M_Rich]; //# ...and for the length of M_Rich here.
 real <lower=0,upper=2> imp_goodo[M_Good];
 //real <lower=0,upper=2> imp_trusto[M_Trust];
 real <lower=0,upper=1> imp_friendo[M_Friend];

 vector[3] BetaRicho;
 vector[3] BetaGoodo;
 //vector[3] BetaTrusto;
 vector[3] BetaFriendo;

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
 
 vector[117] ZetaRecip; 
}

transformed parameters {
 real NumLiveI[N,K];
 real NumVisI[N,K];
 matrix[N,K] RichoI;
 matrix[N,K] GoodoI;
 //matrix[N,K] TrustoI;
 matrix[N,K] FriendoI;
 
 NumLiveI=NumLive;
 NumVisI=NumVis;

 for (k in 1:K){
 NumLiveI[162,k]=imp_liv;
 NumVisI[162,k]=imp_vis;
 }
 
 //# Missing data matrix for rich. Missing data are supplied with a value, raw data are inserted where available.
 for(i in 1:N){
    for(j in 1:K){
        RichoI[i,j] = if_else(Pres_Rich[i,j], Rich_Raw[i,j], imp_richo[Cumu_Rich[i,j]]); //#Where data are missing, pulls the nth value (starting at 1) from the imputed distribution.
    }
 }
 for(i in 1:N){
    for(j in 1:K){
        GoodoI[i,j] = if_else(Pres_Good[i,j], Good_Raw[i,j], imp_goodo[Cumu_Good[i,j]]); //#Where data are missing, pulls the nth value (starting at 1) from the imputed distribution.
   }
 }							
// for(i in 1:N){
//    for(j in 1:K){
//        TrustoI[i,j] = if_else(Pres_Trust[i,j], Trust_Raw[i,j], imp_trusto[Cumu_Trust[i,j]]); //#Where data are missing, pulls the nth value (starting at 1) from the imputed distribution.
//    }
// }
 for(i in 1:N){
    for(j in 1:K){
        FriendoI[i,j] = if_else(Pres_Friend[i,j], Friend_Raw[i,j], imp_friendo[Cumu_Friend[i,j]]); //#Where data are missing, pulls the nth value (starting at 1) from the imputed distribution.
    }
 }
 
 }

model{
// Local storage
 vector[K] Link;

 sigmaZeta ~ cauchy(0,2);
 imp_liv ~ uniform(0,8);
 imp_vis ~ uniform(-1.0420,3.1174);
 imp_richo ~ uniform(0,2);
 imp_goodo ~ uniform(0,2);
 //imp_trusto ~ uniform(0,2);
 imp_friendo ~ uniform(0,1);
 
 Alpha ~ normal(0,5);           //# Priors
 
 BetaRicho ~ normal(0,5);
 BetaGoodo ~ normal(0,5);
 //BetaTrusto ~ normal(0,5);
 BetaFriendo ~ normal(0,5);  
 
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

 ZetaRecip ~ normal(0,sigmaZeta);   
 
//###################################################################### Model Full Data
 for(n in 1:N){
   for (k in 1:(K-1)){
    Link[k] = Alpha[Category[n,k]] 
	
	+ BetaRicho[Category[n,k]]*RichoI[n,k] + 
	BetaGoodo[Category[n,k]]*GoodoI[n,k] + 
	//BetaTrusto[Category[n,k]]*TrustoI[n,k] + 
	BetaFriendo[Category[n,k]]*FriendoI[n,k] 
	
	+ BetaBensBG[Category[n,k]]*BensBG[n,k] + BetaBensWG[Category[n,k]]*BensWG[n,k] + BetaBensC[Category[n,k]]*BensC[n,k] + BetaBensR[Category[n,k]]*BensR[n,k] + BetaCostBG[Category[n,k]]*CostBG[n,k] + BetaCostWG[Category[n,k]]*CostWG[n,k] + BetaCostC[Category[n,k]]*CostC[n,k] + BetaCostR[Category[n,k]]*CostR[n,k] + BetaCostD[Category[n,k]]*CostD[n,k] 
	
	+ BetaLive[Category[n,k]]*NumLiveI[n,k] + BetaVis[Category[n,k]]*NumVisI[n,k] + BetaMed[Category[n,k]]*Media[n,k]
	
	+ BetaNony[Category[n,k]]*Nony[n,k] + BetaEthn[Category[n,k]]*Ethn[n,k] + BetaPopI[Category[n,k]]*PopI[n,k] + BetaPopT[Category[n,k]]*PopT[n,k]
	
	+ BetaVersA[Category[n,k]]*VersA[n,k] + BetaVersB[Category[n,k]]*VersB[n,k] + BetaVersC[Category[n,k]]*VersC[n,k] + BetaVersD[Category[n,k]]*VersD[n,k] + BetaVersE[Category[n,k]]*VersE[n,k]
	
	+ ZetaRecip[RecipID[n,k]];
    
	}
	
  Link[K] = 0;

 Outcome[n] ~ multinomial(softmax(Link));
 }

}

'

param <- c('Alpha',
'BetaRicho',
'BetaGoodo',
#'BetaTrusto',
'BetaFriendo','BetaBensBG','BetaBensWG','BetaBensC','BetaBensR','BetaCostBG','BetaCostWG','BetaCostC','BetaCostR','BetaCostD','BetaLive','BetaVis','BetaMed','BetaNony','BetaEthn','BetaPopI','BetaPopT','BetaVersA','BetaVersB','BetaVersC','BetaVersD','BetaVersE','sigmaZeta')

 mfit <- stan( model_code=model_code, data=model_dat,refresh=10,chains=4,iter=1000,warmup=500,seed=562941,par=param,control=list(adapt_delta=0.99))
 summ_stat<-summary(mfit, probs = c(0.05, 0.95))$summary
 extr_sams<-as.data.frame(mfit)
 write.csv(extr_sams,paste0("Consensus_Good_extracted samples_",gsub("^..","",dire),".csv"),na="",row.names=FALSE) #These will be necessary below to examine the posteriors.
 write.csv(summ_stat,paste0("Consensus_Good_summary statistics_",gsub("^..","",dire),".csv"),na="") #These are useful checks to have for looking at R hats, effective sample sizes, etc. Keep rownames here: they're useful to see which variables have low effective sample sizes or high R hats below.
 
}

######### CHECK R HATS AND EFFECTIVE SAMPLE SIZES ##########
fils_ss<-grep("./Consensus_Good_summary statistics_Iter",list.files(full.names = TRUE, recursive = TRUE)[-1],value=TRUE)

assig_leng<-names(read.csv("./Consensus_Good_summary statistics_Iter1.csv",header=T)) #Doesn't matter which one is picked, just need the column names.
ss<-data.frame(matrix(ncol=length(assig_leng)))
names(ss)<-assig_leng
for (i in 1:length(fils_ss)) {ss<-rbind(ss,read.csv(fils_ss[i],header=TRUE))} #Append each results file, one to the other.

ss[order(ss$n_eff),][1:50,] #Examine effective sample sizes that are low...
ss[order(ss$Rhat,decreasing=TRUE),][1:50,] #...and R hats that are high.

######### PROCESS & EXAMINE POSTERIOR DISTRIBUTIONS #########
library(xtable);library(forestplot)

fils_es<-grep("./Consensus_Good_extracted samples_Iter",list.files(full.names = TRUE, recursive = TRUE)[-1],value=TRUE)
assig_leng<-names(read.csv("./Consensus_Good_extracted samples_Iter98.csv",header=T)) #Doesn't matter which one is picked, just need the column names.
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

forestplot(labeltext=matrix(stats1t$Variable),mean=stats1t$Mean,lower=stats1t$CI_5,upper=stats1t$CI_95,zero=1)

############### TABLE ###############

# Rename variables for printing
stats1t$Group<-rep(c("IG","OG"),length.out=nrow(stats1t))
stats2<-stats1t[,c(5,1,2,3,4)]

stats2$Variable[grep("Alpha",stats2$Variable)]<-"Intercept"
stats2$Variable[grep("BetaRicho",stats2$Variable)]<-"Consensus wealth"
stats2$Variable[grep("BetaGoodo",stats2$Variable)]<-"Consensus goodness"
#stats2$Variable[grep("BetaTrusto",stats2$Variable)]<-"Consensus trustworthiness"
stats2$Variable[grep("BetaFriendo",stats2$Variable)]<-"Consensus interest in friendship"
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
stats2$Variable[grep("BetaMed",stats2$Variable)]<-"Hours of media"
stats2$Variable[grep("BetaNony",stats2$Variable)]<-"Non-anonymous giving"
stats2$Variable[grep("BetaEthn",stats2$Variable)]<-"Group type: Ethnic"
stats2$Variable[grep("BetaPopI",stats2$Variable)]<-"Donor pop.: Intercultural"
stats2$Variable[grep("BetaPopT",stats2$Variable)]<-"Donor pop.: Tsimane'"

# Reorder
stats2$Prediction<-c("","","P1.1",rep("",1),"P1.2",rep("",3),"P2.1",rep("",17),"P3.1",rep("",5),"Controls",rep("",7))
stats3<-stats2[,c(6,1,2,3,4,5)]
names(stats3)[c(2,5,6)]<-c("Target group","Credible Int. 5%","Credible Int. 95%")

print(xtable(stats3),include.rownames=FALSE,type="html",file="Consensus good table_31-Jul-17.html")