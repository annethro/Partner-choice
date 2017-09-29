#################################################################
############ When to diversify, and with whom? Choosing partners among out-group strangers in lowland Bolivia #####

#################### Prepare raw data for analysis #################### 

library(rethinking);library(mice);library(reshape)

dat<-read.csv("./Partner choice dataset_31-Jul-17.csv",header=T)

#sample(.Machine$integer.max, size=1)
seed1<-914707130
set.seed(seed1)

##################### ADJUST VARIABLES ######################

### Round potentially influential outliers in a systematic way: the 97.5th %ile ###
upb<-0.975

qm<-quantile(dat$Media_zscore[dat$Population=="Tsimane"],upb,na.rm=TRUE)
dat$Media_zscore[dat$Population=="Tsimane" & dat$Media_zscore>qm]<-qm #Big outlier, could be driving results.

qi<-quantile(dat$Income_zscore[dat$Population=="Tsimane"],upb,na.rm=TRUE)
dat$Income_zscore[dat$Population=="Tsimane" & dat$Income_zscore>qi]<-qi #Big outlier for Tsimane.

qv<-quantile(dat$Visited_zscore[dat$Population=="Tsimane"],upb,na.rm=TRUE) 
dat$Visited_zscore[dat$Population=="Tsimane" & dat$Visited_zscore>qv]<-qv #Two big outliers, could be driving results.

qra<-quantile(dat$Risk_Aversion,upb,na.rm=TRUE)
dat$Risk_Aversion[dat$Risk_Aversion>qra]<-qra

# Also remove extreme low values for age difference.
qadu<-quantile(dat$AgeDiff,upb,na.rm=TRUE)
qadl<-quantile(dat$AgeDiff,0.025,na.rm=TRUE)
dat$AgeDiff[dat$AgeDiff>qadu]<-qadu
dat$AgeDiff[dat$AgeDiff<qadl]<-qadl

### Center age such that the intercept is interpretable ###
dat$Age<-dat$Age-20 #Participant ages have been rounded to the nearest decade (e.g., 18 -> 20, 24-> 20) to help protect participant anonymity.

### Few people were perceived as having "a lot" of money; round down to avoid the influence of outliers and avoid model convergence issues. ###
dat$Rich_DR[dat$Rich_DR==3]<-2

### Convert from true/false to numeric ###
dat$Friend_DR<-as.numeric(dat$Friend_DR)

########### SETTING APPROPRIATE NAs ###########
dats1<-dat

dats1$Friend_DR[dats1$Rich_DR==""]<-NA #An Access design issue lead to all those with missing data for this question being recorded as "false" rather than NA. Luckily easy to fix since these same people didn't answer questions about goodness, trust, or wealth either.
dats1$Good_DR[dats1$Good_DR==""]<-NA 
dats1$Trust_DR[dats1$Trust_DR==""]<-NA
dats1$Rich_DR[dats1$Rich_DR==""]<-NA

dats1$Perceived_Benefits[dats1$Perceived_Benefits==""]<-NA
dats1$Perceived_Costs[dats1$Perceived_Costs==""]<-NA

dats1$Friend_OthR[dats1$Rich_OthR==""]<-NA
dats1$Good_OthR[dats1$Good_OthR==""]<-NA 
dats1$Trust_OthR[dats1$Trust_OthR==""]<-NA
dats1$Rich_OthR[dats1$Rich_OthR==""]<-NA

#Since round down Rich_DR, be consistent by also rounding down Rich_OthR.
dats1$Rich_OthR[dats1$Rich_OthR>2]<-2

#NPD indicates "no puede decir": participants who felt they could not rate the photo. However, only 7 participants said this for at least one photo for goodness, only 5 for at least one photo for richness, and only 2 for at least one photo for trust. As such, I've set NPD to NA and imputed the values accordingly.
dats1$Good_DR[dats1$Good_DR=="NPD"]<-NA
dats1$Trust_DR[dats1$Trust_DR=="NPD"]<-NA
dats1$Rich_DR[dats1$Rich_DR=="NPD"]<-NA

### Create decision index representing position 1 through 6 of photo placement -- that is, whether a given target recipient appeared in position 1 for Donor X, position 2, etc.
dats1$Decision_Index<-with(dats1, ave(seq(DonorID), DonorID, FUN = function(x) seq(length(x))))

### OPTIONAL: save data set for use in making figures

write.csv(dats1,"./Partner choice figure dataset_31-Jul-17.csv",row.names=FALSE,na="")

##################### IMPUTATION (goodness, richness, trust, friendship, costs, benefits) #########################

rep('',length(dats1))->methd
grep('_DR',colnames(dats1))->cols_dr
grep('Perceived_',colnames(dats1))->cols_pe

## Don't use the following variables for imputation: consensus variables (because they already reflect all the other data and are the same for every recipient), group size data (because not involved in analysis), group known (not used in analysis), date/time of interview (irrelevant).
ncol(dats1)->how_many
pred_mat<-matrix(rep(1,how_many*how_many),ncol=how_many,nrow=how_many)
diag(pred_mat)<-0
c(grep("Size",colnames(dats1)),grep("OthR",colnames(dats1)),grep("GrpKnown",colnames(dats1)),grep("Date.Time2",colnames(dats1)),grep("Decision_Index",colnames(dats1)))->not_these

pred_mat[,not_these]<-0

methd[c(cols_dr,cols_pe)]<-'pmm' #Impute (via predictive mean matching) only goodness, trust, wealth, and friendship (all of which end with DR) and for perceived benefits and costs of out-group membership.
mice(dats1,method=methd,m=100,seed=seed1,predictorMatrix=pred_mat)->imputes
dat_i<-complete(imputes,"long")

#Remove unnecessary column, rename .id because stan doesn't do well with periods
dat_i<-subset(dat_i,select=-.id)
dat_i<-rename(dat_i,c(.imp="Impute_Iter"))
dat_i$Impute_Iter<-as.numeric(dat_i$Impute_Iter)

#Save each iteration into a separate set of folders.
revert<-getwd()
for (i in 1:100) {
curr.dir<-paste0("Iter",i)
dir.create(curr.dir)
setwd(paste0("./",curr.dir))
firs<-dat_i[dat_i$Impute_Iter==i,]
firs1<-subset(firs,select=c(-Impute_Iter,-Self))

firs1_dq<-firs[!duplicated(firs$DonorID),]
firs1_dq[,c("AgeDiff","Perceived_Benefits","Perceived_Costs","OG","Amount_Given","RecipID","Good_DR","Friend_DR","Good_OthR","Friend_OthR","GrpType_Recip","GrpKnown","Stereotypes","Rich_DR","Rich_OthR","Trust_DR","Trust_OthR")]<-NA
firs1_dq[,c("Decision_Index")]<-7
firs1_dq$Amount_Given<-firs1_dq$Self
firs1_dq1<-subset(firs1_dq,select=c(-Self,-Impute_Iter))

rbind(firs1,firs1_dq1)->firs2

notDID<-which(colnames(firs2)!="DonorID")
DID<-which(colnames(firs2)=="DonorID")
firs2<-firs2[,c(notDID,DID)]

firs2<-firs2[order(firs2$DonorID,firs2$OG),]

	for (j in 1:(length(firs2)-2)) {
		focal<-colnames(firs2)[j]
		subs<-firs2[,c(focal,"DonorID","Decision_Index")]
		expo<-reshape(subs,idvar="DonorID",timevar="Decision_Index",v.names=focal,direction="wide",sep="")
		expo1<-subset(expo,select=c(-DonorID))
		write.table(expo1,paste0(focal,".csv"),na="",row.names=FALSE,col.names=FALSE,sep=",")
	}

firs3<-firs2
focal<-"DonorID"
firs3$DonorIDalt<-coerce_index(firs3$DonorID)
subs<-firs3[,c(focal,"DonorIDalt","Decision_Index")]
expo<-reshape(subs,idvar="DonorIDalt",timevar="Decision_Index",v.names=focal,direction="wide",sep="")
expo1<-subset(expo,select=c(-DonorIDalt))
write.table(expo1,paste0(focal,".csv"),na="",row.names=FALSE,col.names=FALSE,sep=",")
setwd(revert)
}

####### OPTIONAL: use data that has not been imputed
#dat_i<-read.csv("./Partner choice figure dataset_25-Jul-17.csv",header=TRUE)

## Separte into separate csv.s

#firs<-dat_i
#firs1<-subset(dat_i,select=c(-Self))

#firs1_dq<-firs[!duplicated(firs$DonorID),]
#firs1_dq[,c("AgeDiff","Perceived_Benefits","Perceived_Costs","OG","Amount_Given","RecipID","Good_DR","Friend_DR","Good_OthR","Friend_OthR","GrpType_Recip","GrpKnown","Stereotypes","Recip_Size","Rich_DR","Rich_OthR","Trust_DR","Trust_OthR")]<-NA
#firs1_dq[,c("Decision_Index")]<-7
#firs1_dq$Amount_Given<-firs1_dq$Self
#firs1_dq1<-subset(firs1_dq,select=c(-Self))

#rbind(firs1,firs1_dq1)->firs2

#notDID<-which(colnames(firs2)!="DonorID")
#DID<-which(colnames(firs2)=="DonorID")
#firs2<-firs2[,c(notDID,DID)]

#firs2<-firs2[order(firs2$DonorID,firs2$OG),]

	#for (j in 1:(length(firs2)-2)) {
		#focal<-colnames(firs2)[j]
		#subs<-firs2[,c(focal,"DonorID","Decision_Index")]
		#expo<-reshape(subs,idvar="DonorID",timevar="Decision_Index",v.names=focal,direction="wide",sep="")
		#expo1<-subset(expo,select=c(-DonorID))
		#write.table(expo1,paste0(focal,".csv"),na="",#row.names=FALSE,col.names=FALSE,sep=",")
#	}

#firs3<-firs2
#focal<-"DonorID"
#firs3$DonorIDalt<-coerce_index(firs3$DonorID)
#subs<-firs3[,c(focal,"DonorIDalt","Decision_Index")]
#expo<-reshape(subs,idvar="DonorIDalt",timevar="Decision_Index",v.names=focal,direction="wide",sep="")
#expo1<-subset(expo,select=c(-DonorIDalt))
#write.table(expo1,paste0(focal,".csv"),na="",#row.names=FALSE,col.names=FALSE,sep=",")

######### Descriptive statistics ########

#### Table S1b ####
library(xtable)

nums<-subset(dat,select=c("Amount_Given","PlacesLived","Visited_zscore","Media_zscore","Income_zscore","Market_Items_zscore","log_Subj_SES","Shortfall_Summary","Agreeable","Extraverted","Risk_Aversion","Age","AgeDiff","Years_Education","Times_Church_Mo"))
cors<-round(cor(nums,nums,use="complete.obs"),2)
upper<-cors
upper[upper.tri(cors,diag=TRUE)]<-""
upper<-as.data.frame(upper)
cols<-c("Avg bolivianos","Places lived","Places visited","TV and movies","Income","Market items","Subjective SES","Shortfall","Agreeableness","Extraversion","Risk aversion","Age","Age difference","Education","Church")
colnames(upper)<-cols;rownames(upper)<-cols

print(xtable(upper),include.rownames=TRUE,type="html",file="Table S1b_29-Jul-17.html")

#### Table S1c ####
dieses<-c("Perceived_Benefits","Perceived_Costs","Good_DR","Rich_DR","Trust_DR","Friend_DR","OG")
rec_cat<-subset(dat,select=c(dieses))
rec_cat$Perceived_Benefits[rec_cat$Perceived_Benefits==""]<-NA
rec_cat$Perceived_Costs[rec_cat$Perceived_Costs==""]<-NA
rec_o_cat_summ<-vector("list", length(rec_cat)-1)
for (i in 1:6){as.factor(rec_cat[rec_cat$OG==3,i])->focal
rec_o_cat_summ[[i]]<-summary(focal)/length(focal)
rm(focal)}

rec_i_cat_summ<-vector("list", length(rec_cat)-1)
for (i in 1:6){as.factor(rec_cat[rec_cat$OG==1,i])->focal
rec_i_cat_summ[[i]]<-summary(focal)/length(focal)
rm(focal)}

#### Table S1d ####
these<-c("Can_Stay","Coop_Labor","Ill_This_Mo","Sex_Male","Nonanonymous_Play","Population")
don_cat<-subset(dat,select=c(these))
don_cat_summ<-vector("list", length(don_cat))
for (i in 1:length(don_cat)){don_cat_summ[[i]]<-summary(as.factor(don_cat[,i]))/nrow(don_cat)}

#### Table 7 ####
dat$GrpKnown[dat$GrpKnown==""]<-NA
aggregate(PlacesLived~GrpKnown,mean,data=dat) #"Yes" here indicates that groups were well-known to the donor.
aggregate(PlacesLived~GrpKnown,sd,data=dat)
aggregate(Visited_zscore~GrpKnown,mean,data=dat)
aggregate(Visited_zscore~GrpKnown,sd,data=dat)

#### Table 8a: descriptive statistics by anonymous vs non-anonymous ####
nums<-c("Amount_Given","PlacesLived","Visited_zscore","Media_zscore","Income_zscore","Market_Items_zscore","log_Subj_SES","Shortfall_Summary","Agreeable","Extraverted","Age","AgeDiff","Years_Education","Times_Church_Mo")
as.numeric(dat$Nonanonymous_Play)->dat$Nonanonymous_Play

num_mat<-matrix(ncol=7,nrow=length(nums)*4)
num_mat[,1]<-rep(nums,each=4)
num_mat[,2]<-rep(rep(c("IG","OG"),each=2),length.out=length(nums)*2)
num_mat[,3]<-rep(rep(c("Anony","Nonanony"),2),length.out=length(nums)*2)

ct<-1
for (i in 1:length(nums)){
	for (j in c(1,3)){
		for (k in 0:1) {
		here<-dat[dat$OG==j & dat$Nonanonymous_Play==k,]
		num_mat[ct,4]<-round(mean(here[,c(nums[i])],na.rm=TRUE),2)
		num_mat[ct,5]<-round(sd(here[,c(nums[i])],na.rm=TRUE),2)
		num_mat[ct,6]<-round(min(here[,c(nums[i])],na.rm=TRUE),2)
		num_mat[ct,7]<-round(max(here[,c(nums[i])],na.rm=TRUE),2)
		ct+1->ct
		rm(here)
}
}
}

num_mat1<-data.frame(num_mat,stringsAsFactors=FALSE)
colnames(num_mat1)<-c("Variable","Group type","Anonymous?","Mean","SD","Min","Max")
num_mat1$Variable->num_mat1$Variable1
num_mat1$Variable1[num_mat1$Variable=="Times_Church_Mo"]<-"Times to church";num_mat1$Variable1[num_mat1$Variable=="Years_Education"]<-"Years of education";num_mat1$Variable1[num_mat1$Variable=="AgeDiff"]<-"Donor-recip age diff.";num_mat1$Variable1[num_mat1$Variable=="Shortfall_Summary"]<-"Shortfall summary";num_mat1$Variable1[num_mat1$Variable=="log_Subj_SES"]<-"Log subjective SES";num_mat1$Variable1[num_mat1$Variable=="Market_Items_zscore"]<-"Market items (z-score)";num_mat1$Variable1[num_mat1$Variable=="Income_zscore"]<-"Income (z-score)";num_mat1$Variable1[num_mat1$Variable=="Media_zscore"]<-"Hours TV/movies (z-score)";num_mat1$Variable1[num_mat1$Variable=="Visited_zscore"]<-"Places visited (z-score)";num_mat1$Variable1[num_mat1$Variable=="PlacesLived"]<-"Places lived";num_mat1$Variable1[num_mat1$Variable=="Amount_Given"]<-"Amount given"

num_mat2<-num_mat1[,c(8,2,3,4,5,6,7)]

print(xtable(num_mat2),include.rownames=FALSE,type="html",file="Anonymous numeric descriptives_31-Jul-17.html")

#### Table 8b ####

cats<-c("Perceived_Benefits","Perceived_Costs","Good_DR","Rich_DR","Trust_DR","Friend_DR","Can_Stay","Coop_Labor","Ill_This_Mo","Sex_Male","Population")
don_cat<-subset(dat,select=c(cats,"OG","Nonanonymous_Play"))

cat_mat<-matrix(ncol=10,nrow=length(cats)*4)
cat_mat[,1]<-rep(cats,each=4)
cat_mat[,2]<-rep(rep(c("IG","OG"),each=2),length.out=length(cats)*2)
cat_mat[,3]<-rep(rep(c("Anony","Nonanony"),2),length.out=length(cats)*2)

ct<-1
for (i in 1:(length(don_cat)-2)){
	for (j in c(1,3)){
		for (k in 0:1){
		here<-don_cat[don_cat$OG==j & don_cat$Nonanonymous_Play==k,]
		focal<-factor(here[,c(cats[i])])
		lvls<-3+length(levels(focal))
		cat_mat[ct,4:lvls]<-round(summary(focal)/nrow(here),2)
		ct+1->ct
		rm(here,focal,lvls)
	}}}
	
cat_mat1<-data.frame(cat_mat,stringsAsFactors=FALSE)
colnames(cat_mat1)<-c("Variable","Group type","Anonymous?","Level 1","Level 2","Level 3","Level 4","Level 5","Level 6","Level 7")
cat_mat1$Variable->cat_mat1$Variable1
cat_mat1$Variable1[cat_mat1$Variable=="Can_Stay"]<-"Can stay during flood";cat_mat1$Variable1[cat_mat1$Variable=="Coop_Labor"]<-"Cooperative labor";cat_mat1$Variable1[cat_mat1$Variable=="Ill_This_Mo"]<-"Ill this month";cat_mat1$Variable1[cat_mat1$Variable=="Sex_Male"]<-"Sex: Male";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Benefits"]<-"Perceived benefits";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Costs"]<-"Perceived costs";cat_mat1$Variable1[cat_mat1$Variable=="Good_DR"]<-"Perceived goodness";cat_mat1$Variable1[cat_mat1$Variable=="Rich_DR"]<-"Perceived wealth";cat_mat1$Variable1[cat_mat1$Variable=="Trust_DR"]<-"Perceived trustworthiness";cat_mat1$Variable1[cat_mat1$Variable=="Friend_DR"]<-"Interest in friendship"

cat_mat2<-cat_mat1[,c(11,2:10)]
cat_mat2[is.na(cat_mat2)]<-""

print(xtable(cat_mat2),include.rownames=FALSE,type="html",file="Anonymous categorical descriptives_31-Jul-17.html")

#### Table 10 - Pop by pop descriptive statistics ####

### Tsimane continuous ###
nums<-c("Amount_Given","PlacesLived","Visited_zscore","Media_zscore","Income_zscore","Market_Items_zscore","log_Subj_SES","Shortfall_Summary","Agreeable","Extraverted","Age","AgeDiff","Years_Education","Times_Church_Mo")

tsim<-dat[dat$Population=="Tsimane",]

num_mat<-matrix(ncol=5,nrow=length(nums))
num_mat[,1]<-nums

for (i in 1:length(nums)){
		num_mat[i,2]<-round(mean(tsim[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,3]<-round(sd(tsim[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,4]<-round(min(tsim[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,5]<-round(max(tsim[,c(nums[i])],na.rm=TRUE),2)
}

num_mat1<-data.frame(num_mat,stringsAsFactors=FALSE)
colnames(num_mat1)<-c("Variable","Mean","SD","Min","Max")
num_mat1$Variable->num_mat1$Variable1
num_mat1$Variable1[num_mat1$Variable=="Times_Church_Mo"]<-"Times to church";num_mat1$Variable1[num_mat1$Variable=="Years_Education"]<-"Years of education";num_mat1$Variable1[num_mat1$Variable=="AgeDiff"]<-"Donor-recip age diff.";num_mat1$Variable1[num_mat1$Variable=="Shortfall_Summary"]<-"Shortfall summary";num_mat1$Variable1[num_mat1$Variable=="log_Subj_SES"]<-"Log subjective SES";num_mat1$Variable1[num_mat1$Variable=="Market_Items_zscore"]<-"Market items (z-score)";num_mat1$Variable1[num_mat1$Variable=="Income_zscore"]<-"Income (z-score)";num_mat1$Variable1[num_mat1$Variable=="Media_zscore"]<-"Hours TV/movies (z-score)";num_mat1$Variable1[num_mat1$Variable=="Visited_zscore"]<-"Places visited (z-score)";num_mat1$Variable1[num_mat1$Variable=="PlacesLived"]<-"Places lived";num_mat1$Variable1[num_mat1$Variable=="Amount_Given"]<-"Amount given"

num_mat2<-num_mat1[,c(6,2,3,4,5)]

print(xtable(num_mat2),include.rownames=FALSE,type="html",file="Tsimane continuous descriptives_31-Jul-17.html")

### Tsimane categorical ###
cats<-c("Perceived_Benefits","Perceived_Costs","Good_DR","Rich_DR","Trust_DR","Friend_DR","Can_Stay","Coop_Labor","Ill_This_Mo","Sex_Male","Nonanonymous_Play")
don_cat<-subset(tsim,select=c(cats))

cat_mat<-matrix(ncol=7,nrow=length(cats))
cat_mat[,1]<-rep(cats)

ct<-1
for (i in 1:(length(don_cat))){
		focal<-factor(don_cat[,i])
		lvls<-1+length(levels(focal))
		cat_mat[ct,2:lvls]<-round(summary(focal)/nrow(don_cat),2)
		ct+1->ct
		rm(focal,lvls)
}
	
cat_mat1<-data.frame(cat_mat,stringsAsFactors=FALSE)
colnames(cat_mat1)<-c("Variable","Level 1","Level 2","Level 3","Level 4","Level 5","Level 6")
cat_mat1$Variable->cat_mat1$Variable1
cat_mat1$Variable1[cat_mat1$Variable=="Can_Stay"]<-"Can stay during flood";cat_mat1$Variable1[cat_mat1$Variable=="Coop_Labor"]<-"Cooperative labor";cat_mat1$Variable1[cat_mat1$Variable=="Ill_This_Mo"]<-"Ill this month";cat_mat1$Variable1[cat_mat1$Variable=="Sex_Male"]<-"Sex: Male";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Benefits"]<-"Perceived benefits";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Costs"]<-"Perceived costs";cat_mat1$Variable1[cat_mat1$Variable=="Good_DR"]<-"Perceived goodness";cat_mat1$Variable1[cat_mat1$Variable=="Rich_DR"]<-"Perceived wealth";cat_mat1$Variable1[cat_mat1$Variable=="Trust_DR"]<-"Perceived trustworthiness";cat_mat1$Variable1[cat_mat1$Variable=="Friend_DR"]<-"Interest in friendship";cat_mat1$Variable1[cat_mat1$Variable=="Nonanonymous_Play"]<-"Shared name"

cat_mat2<-cat_mat1[,c(8,2:7)]
cat_mat2[is.na(cat_mat2)]<-""

print(xtable(cat_mat2),include.rownames=FALSE,type="html",file="Tsimane categorical descriptives_31-Jul-17.html")

### Moseten continuous ###
nums<-c("Amount_Given","PlacesLived","Visited_zscore","Media_zscore","Income_zscore","Market_Items_zscore","log_Subj_SES","Shortfall_Summary","Agreeable","Extraverted","Age","AgeDiff","Years_Education","Times_Church_Mo")

mos<-dat[dat$Population=="Moseten",]

num_mat<-matrix(ncol=5,nrow=length(nums))
num_mat[,1]<-nums

for (i in 1:length(nums)){
		num_mat[i,2]<-round(mean(mos[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,3]<-round(sd(mos[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,4]<-round(min(mos[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,5]<-round(max(mos[,c(nums[i])],na.rm=TRUE),2)
}

num_mat1<-data.frame(num_mat,stringsAsFactors=FALSE)
colnames(num_mat1)<-c("Variable","Mean","SD","Min","Max")
num_mat1$Variable->num_mat1$Variable1
num_mat1$Variable1[num_mat1$Variable=="Times_Church_Mo"]<-"Times to church";num_mat1$Variable1[num_mat1$Variable=="Years_Education"]<-"Years of education";num_mat1$Variable1[num_mat1$Variable=="AgeDiff"]<-"Donor-recip age diff.";num_mat1$Variable1[num_mat1$Variable=="Shortfall_Summary"]<-"Shortfall summary";num_mat1$Variable1[num_mat1$Variable=="log_Subj_SES"]<-"Log subjective SES";num_mat1$Variable1[num_mat1$Variable=="Market_Items_zscore"]<-"Market items (z-score)";num_mat1$Variable1[num_mat1$Variable=="Income_zscore"]<-"Income (z-score)";num_mat1$Variable1[num_mat1$Variable=="Media_zscore"]<-"Hours TV/movies (z-score)";num_mat1$Variable1[num_mat1$Variable=="Visited_zscore"]<-"Places visited (z-score)";num_mat1$Variable1[num_mat1$Variable=="PlacesLived"]<-"Places lived";num_mat1$Variable1[num_mat1$Variable=="Amount_Given"]<-"Amount given"

num_mat2<-num_mat1[,c(6,2,3,4,5)]

print(xtable(num_mat2),include.rownames=FALSE,type="html",file="Moseten continuous descriptives_31-Jul-17.html")

### Moseten categorical ###
cats<-c("Perceived_Benefits","Perceived_Costs","Good_DR","Rich_DR","Trust_DR","Friend_DR","Can_Stay","Coop_Labor","Ill_This_Mo","Sex_Male","Nonanonymous_Play")
don_cat<-subset(mos,select=c(cats))

cat_mat<-matrix(ncol=8,nrow=length(cats))
cat_mat[,1]<-rep(cats)

ct<-1
for (i in 1:(length(don_cat))){
		focal<-factor(don_cat[,i])
		lvls<-1+length(levels(focal))
		cat_mat[ct,2:lvls]<-round(summary(focal)/nrow(don_cat),2)
		ct+1->ct
		rm(focal,lvls)
}
	
cat_mat1<-data.frame(cat_mat,stringsAsFactors=FALSE)
colnames(cat_mat1)<-c("Variable","Level 1","Level 2","Level 3","Level 4","Level 5","Level 6","Level 7")
cat_mat1$Variable->cat_mat1$Variable1
cat_mat1$Variable1[cat_mat1$Variable=="Can_Stay"]<-"Can stay during flood";cat_mat1$Variable1[cat_mat1$Variable=="Coop_Labor"]<-"Cooperative labor";cat_mat1$Variable1[cat_mat1$Variable=="Ill_This_Mo"]<-"Ill this month";cat_mat1$Variable1[cat_mat1$Variable=="Sex_Male"]<-"Sex: Male";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Benefits"]<-"Perceived benefits";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Costs"]<-"Perceived costs";cat_mat1$Variable1[cat_mat1$Variable=="Good_DR"]<-"Perceived goodness";cat_mat1$Variable1[cat_mat1$Variable=="Rich_DR"]<-"Perceived wealth";cat_mat1$Variable1[cat_mat1$Variable=="Trust_DR"]<-"Perceived trustworthiness";cat_mat1$Variable1[cat_mat1$Variable=="Friend_DR"]<-"Interest in friendship";cat_mat1$Variable1[cat_mat1$Variable=="Nonanonymous_Play"]<-"Shared name"

cat_mat2<-cat_mat1[,c(9,2:8)]
cat_mat2[is.na(cat_mat2)]<-""

print(xtable(cat_mat2),include.rownames=FALSE,type="html",file="Moseten categorical descriptives_31-Jul-17.html")

### Intercultural continuous ###
nums<-c("Amount_Given","PlacesLived","Visited_zscore","Media_zscore","Income_zscore","Market_Items_zscore","log_Subj_SES","Shortfall_Summary","Agreeable","Extraverted","Age","AgeDiff","Years_Education","Times_Church_Mo")

int<-dat[dat$Population=="Intercultural",]

num_mat<-matrix(ncol=5,nrow=length(nums))
num_mat[,1]<-nums

for (i in 1:length(nums)){
		num_mat[i,2]<-round(mean(int[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,3]<-round(sd(int[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,4]<-round(min(int[,c(nums[i])],na.rm=TRUE),2)
		num_mat[i,5]<-round(max(int[,c(nums[i])],na.rm=TRUE),2)
}

num_mat1<-data.frame(num_mat,stringsAsFactors=FALSE)
colnames(num_mat1)<-c("Variable","Mean","SD","Min","Max")
num_mat1$Variable->num_mat1$Variable1
num_mat1$Variable1[num_mat1$Variable=="Times_Church_Mo"]<-"Times to church";num_mat1$Variable1[num_mat1$Variable=="Years_Education"]<-"Years of education";num_mat1$Variable1[num_mat1$Variable=="AgeDiff"]<-"Donor-recip age diff.";num_mat1$Variable1[num_mat1$Variable=="Shortfall_Summary"]<-"Shortfall summary";num_mat1$Variable1[num_mat1$Variable=="log_Subj_SES"]<-"Log subjective SES";num_mat1$Variable1[num_mat1$Variable=="Market_Items_zscore"]<-"Market items (z-score)";num_mat1$Variable1[num_mat1$Variable=="Income_zscore"]<-"Income (z-score)";num_mat1$Variable1[num_mat1$Variable=="Media_zscore"]<-"Hours TV/movies (z-score)";num_mat1$Variable1[num_mat1$Variable=="Visited_zscore"]<-"Places visited (z-score)";num_mat1$Variable1[num_mat1$Variable=="PlacesLived"]<-"Places lived";num_mat1$Variable1[num_mat1$Variable=="Amount_Given"]<-"Amount given"

num_mat2<-num_mat1[,c(6,2,3,4,5)]

print(xtable(num_mat2),include.rownames=FALSE,type="html",file="Intercultural continuous descriptives_31-Jul-17.html")

### Intercultural categorical ###
cats<-c("Perceived_Benefits","Perceived_Costs","Good_DR","Rich_DR","Trust_DR","Friend_DR","Can_Stay","Coop_Labor","Ill_This_Mo","Sex_Male","Nonanonymous_Play")
don_cat<-subset(int,select=c(cats))

cat_mat<-matrix(ncol=8,nrow=length(cats))
cat_mat[,1]<-rep(cats)

ct<-1
for (i in 1:(length(don_cat))){
		focal<-factor(don_cat[,i])
		lvls<-1+length(levels(focal))
		cat_mat[ct,2:lvls]<-round(summary(focal)/nrow(don_cat),2)
		ct+1->ct
		rm(focal,lvls)
}
	
cat_mat1<-data.frame(cat_mat,stringsAsFactors=FALSE)
colnames(cat_mat1)<-c("Variable","Level 1","Level 2","Level 3","Level 4","Level 5","Level 6","Level 7")
cat_mat1$Variable->cat_mat1$Variable1
cat_mat1$Variable1[cat_mat1$Variable=="Can_Stay"]<-"Can stay during flood";cat_mat1$Variable1[cat_mat1$Variable=="Coop_Labor"]<-"Cooperative labor";cat_mat1$Variable1[cat_mat1$Variable=="Ill_This_Mo"]<-"Ill this month";cat_mat1$Variable1[cat_mat1$Variable=="Sex_Male"]<-"Sex: Male";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Benefits"]<-"Perceived benefits";cat_mat1$Variable1[cat_mat1$Variable=="Perceived_Costs"]<-"Perceived costs";cat_mat1$Variable1[cat_mat1$Variable=="Good_DR"]<-"Perceived goodness";cat_mat1$Variable1[cat_mat1$Variable=="Rich_DR"]<-"Perceived wealth";cat_mat1$Variable1[cat_mat1$Variable=="Trust_DR"]<-"Perceived trustworthiness";cat_mat1$Variable1[cat_mat1$Variable=="Friend_DR"]<-"Interest in friendship";cat_mat1$Variable1[cat_mat1$Variable=="Nonanonymous_Play"]<-"Shared name"

cat_mat2<-cat_mat1[,c(9,2:8)]
cat_mat2[is.na(cat_mat2)]<-""

print(xtable(cat_mat2),include.rownames=FALSE,type="html",file="Intercultural categorical descriptives_31-Jul-17.html")