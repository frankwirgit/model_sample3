#update the predictors - keep those ones used for clustering only
reg_los <- glm(LOS ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

prob = predict(reg_los, newdata=dat.test, type="response")
aList = getROC_AUC(prob, dat.test$LOS)
auc_los = unlist(aList$auc)

> auc_los
[1] 0.6342564


reg_los <- glm(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_los
[1] 0.6344974


reg_los <- glm(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_los
[1] 0.6172506

#whole cohort
reg_los = glm(LOS ~ age + bmi_pre
+ factor(gender) + factor(antibiotics) + factor(ever_smoker)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Rheumatism),
data=data_los, family=binomial)

reg_los = glm(LOS ~ age + bmi_pre
+ factor(gender) + factor(antibiotics) + factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Rheumatism),
data=data_los, family=binomial)

summary(reg_los)

prob = predict(reg_los, type="response")

aList = getROC_AUC(prob, data_los$LOS)
auc_los = unlist(aList$auc)
> auc_los

#stay with full set of predictors
> auc_los
[1] 0.6446433

#stay with the 9 attributes used for cluster
> auc_los
[1] 0.642125

#stay with the 9 attributes used for cluster with race and insurance removed
> auc_los
[1] 0.6236503


#apply the support vector machine
library(e1071)

#factorize the output - can not be used to compute AUC
svm_los <- svm(as.factor(LOS) ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism, data=dat.train)

#wrong the predict will always be a factor based on the model with factorized output
psvm_los <- predict(svm_los, dat.test, probability=F)
t <- as.numeric(levels(psvm_los))[psvm_los]
aList = getROC_AUC(t, dat.test$LOS)
svm_auc_los = unlist(aList$auc)
> svm_auc_los
[1] 0.4989788

svm_los <- svm(LOS ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism, data=dat.train)

psvm_los <- predict(svm_los, dat.test, probability=F)
psvm_los <- predict(svm_los, dat.test, probability=T)
#psvm_los <- predict(svm_los, dat.test)
t <- data.frame(psvm_los)
aList = getROC_AUC(t$psvm_los, dat.test$LOS)
svm_auc_los = unlist(aList$auc)
> svm_auc_los
[1] 0.5137208



svm_los <- svm(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train)

> summary(svm_los)

Call:
svm(formula = LOS ~ age + bmi_pre + gender + antibiotics + ever_smoker + insurance_medicare + insurance_medicaid + 
    insurance_private + insurance_other + race_AfricanAmerican + race_Asian + race_Latino + race_Native + 
    pre_comor_Group3_Endocrine + pre_comor_Anemia + pre_comor_Group5_MentalDisorder + pre_comor_Group6_Nervous + 
    pre_comor_Hypertension + pre_comor_Group8_Respiratory + pre_comor_Rheumatism, data = dat.train)


Parameters:
   SVM-Type:  eps-regression 
 SVM-Kernel:  radial 
       cost:  1 
      gamma:  0.04761905 
    epsilon:  0.1 


Number of Support Vectors:  6135



> svm_auc_los
[1] 0.5188259


svm_los <- svm(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train)

> summary(svm_los)

Call:
svm(formula = LOS ~ age + bmi_pre + gender + antibiotics + ever_smoker + pre_comor_Group3_Endocrine + 
    pre_comor_Anemia + pre_comor_Group5_MentalDisorder + pre_comor_Group6_Nervous + pre_comor_Hypertension + 
    pre_comor_Group8_Respiratory + pre_comor_Rheumatism, data = dat.train)


Parameters:
   SVM-Type:  eps-regression 
 SVM-Kernel:  radial 
       cost:  1 
      gamma:  0.07692308 
    epsilon:  0.1 


Number of Support Vectors:  5935


> svm_auc_los
[1] 0.4858264



#apply cforest
library(party)
cf_los <- cforest(LOS ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism, data=dat.train, controls=cforest_unbiased(ntree=500, mtry=3))
pcf_los <- predict(cf_los, newdata=dat.test, type="prob")

> pcf_auc_los
[1] 0.6253709


cf_los <- cforest(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, controls=cforest_unbiased(ntree=500, mtry=3))

pcf_los <- predict(cf_los, newdata=dat.test, type="prob")
t <- as.matrix(pcf_los)
tt <- as.numeric(t[,1])
aList <- getROC_AUC(tt, dat.test$LOS)
pcf_auc_los = unlist(aList$auc)
> pcf_auc_los
[1] 0.6276193

cf_los <- cforest(LOS ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, controls=cforest_unbiased(ntree=500, mtry=3))

> pcf_auc_los
[1] 0.6127184




pcf_los <- predict(cf_los, newdata=dat.test, OOB=T, type="response")
pcf_los <- predict(cf_los, newdata=dat.test)

table(true=dat.test$LOS, pcf_los)

t <- as.numeric(levels(pcf_los))[pcf_los]
aList = getROC_AUC(t, dat.test$LOS)
pcf_auc_los = unlist(aList$auc)

#apply random forest for the model
#apply to the whole cohort
library(randomForest)


#LOS
#factorize the data
dat <- apply(data_los[,c(4:5, 7:9, 11:33)], 2, factor)
summary(dat)
length(rownames(dat))
dat = cbind(data_los[,c(34, 1:3, 6, 10)], dat)
names(dat)
dat=dat[,-c(2,5,11,16,34)]

df_auc <- data.frame(auc=numeric(0))

for(i in 21:30){
set.seed(i)
#set.seed(17)
ratio = 0.5
indx = sample(1:nrow(dat), size=ratio*nrow(dat))
dat.train = dat[indx,]
dat.test = dat[-indx,]

rf_los <- randomForest(as.factor(LOS) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

#rfp_los <- randomForest:::predict(rf_los, newdata=dat, type="response")
#rfp_los = predict(rf_los, newdata=dat, type="response")
#rfp_los = predict(rf_los, type="prob", newdata=dat)
#aList = getROC_AUC(rfp_los[,2], dat$LOS)
rfp_los = predict(rf_los, type="prob", newdata=dat.test)

aList = getROC_AUC(rfp_los[,2], dat.test$LOS)
rf_auc_los = unlist(aList$auc)
df_auc <- rbind(df_auc, data.frame(auc=rf_auc_los))
}


minrf.err <- min(data.frame(rf_los$err.rate)["OOB"])
minrf.err
[1] 0.07080986
minrf.index <- which(data.frame(rf_los$err.rate)["OOB"]==minrf.err)
length(minrf.index)
[1] 262
rfrn <- round(importance(rf_los),2)
rfrn[order(rfrn[,3], decreasing=T),]



#full set of predictors
rf_los <- randomForest(as.factor(LOS) ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism,
data=dat.train, importance=T)
> rf_auc_los
[1] 0.6128172

#keep the attributes for clustering only
rf_los <- randomForest(as.factor(LOS) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

> rf_auc_los
[1] 0.6013633

#keep the attributes for clustering only and remove insurance and race
rf_los <- randomForest(as.factor(LOS) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)
> rf_auc_los
[1] 0.5759988

#try a balanced tree
#divide 1 and 0
dat.one <- subset(dat, dat$LOS==1)
dat.zero <- subset(dat, dat$LOS==0)

set.seed(17)
ratio = 0.5
indx = sample(1:nrow(dat.one), size=ratio*nrow(dat.one))
datone.train = dat.one[indx,]
datone.test = dat.one[-indx,]
set.seed(18)
indx = sample(1:nrow(dat.zero), size=ratio*nrow(dat.one))
datzero.train = dat.zero[indx,]
datzero.test = dat.zero[-indx,]
#combine and shuffle
datq.train = rbind(datone.train, datzero.train)
datq.test = rbind(datone.test, datzero.test)
set.seed(19)
datq.train = datq.train[sample(nrow(datq.train)),]
datq.test = datq.test[sample(nrow(datq.test)),]

set.seed(10)
rf_losb <- randomForest(as.factor(LOS) ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism,
data=datq.train, importance=T)

> rf_auc_los
[1] 0.6146916

set.seed(10)
rf_losb <- randomForest(as.factor(LOS) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=datq.train, importance=T)

rfp_los = predict(rf_losb, type="prob", newdata=datq.test)
aList = getROC_AUC(rfp_los[,2], datq.test$LOS)
rf_auc_los = unlist(aList$auc)


rfp_los = predict(rf_losb, newdata=datq.test, type="class")
cm <- table(datq.test$LOS, rfp_los, dnn=c("Actual","Predicted"))
cmpct <- round(cm/length(rfp_los),2)

rfp_los = predict(rf_losb, type="prob", newdata=datq.test)

> rf_auc_los
[1] 0.6246337

set.seed(10)
rf_losb <- randomForest(as.factor(LOS) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=datq.train, importance=T)

> rf_auc_los
[1] 0.6018699


> df_auc
         auc
1  0.6012922
2  0.6060346
3  0.5953875
4  0.6100721
5  0.6026586
6  0.6048896
7  0.5978477
8  0.5946497
9  0.5938485
10 0.5989969



varImpPlot(rf_los, main="LOS", col="dark blue")
plot(rf_los, main="LOS")
legend("topright", c("OOB","No","Yes"), text.col=1:6, lty=1:3, col=1:3)

#ratio=10%
> rf_auc_los
[1] 0.5922786


#ratio=25%
> rf_auc_los
[1] 0.5981935


#ratio=50%
> rf_auc_los
[1] 0.6013633


#ratio=75%
> rf_auc_los
[1] 0.597819

#ratio=90%
> rf_auc_los
[1] 0.5962077


#ratio=100%
rfp_los = predict(rf_los, type="prob", newdata=dat)
aList = getROC_AUC(rfp_los[,2], dat$LOS)
rf_auc_los = unlist(aList$auc)

> rf_auc_los
[1] 0.9724798


rfp_los = predict(rf_los, type="response", newdata=dat)
t <- as.numeric(levels(rfp_los))[rfp_los]
aList = getROC_AUC(t, dat$LOS)
rf_auc_los = unlist(aList$auc)

> rf_auc_los
[1] 0.5366424



table(true=)

> t <- prediction(rfp_los[,2], dat$LOS)
> tt <- performance(t, "auc")
> tt

Slot "y.values":
[[1]]
[1] 0.9889726

> droc <- roc(LOS ~ rfp_los[,2], data=dat)
> droc$auc
Area under the curve: 0.989


#revision
dat <- apply(data_rev[,c(4:5, 7:9, 11:33)], 2, factor)
summary(dat)
length(rownames(dat))
dat = cbind(data_rev[,c(1:3, 6, 10)], dat)
names(dat)
dat=dat[,-c(4,10,15,33)]

set.seed(17)
ratio = 0.5
indx = sample(1:nrow(dat), size=ratio*nrow(dat))
dat.train = dat[indx,]
dat.test = dat[-indx,]

set.seed(10)
rf_rev <- randomForest(as.factor(revision) ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

rfp_rev = predict(rf_rev, type="prob", newdata=dat.test)
aList = getROC_AUC(rfp_rev[,2], dat.test$revision)
rf_auc_rev = unlist(aList$auc)
> rf_auc_rev
[1] 0.5733259

set.seed(10)
rf_rev <- randomForest(as.factor(revision) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

> rf_auc_rev
[1] 0.56986


#keep the attributes for clustering only and remove insurance and race
set.seed(10)
rf_rev <- randomForest(as.factor(revision) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

> rf_auc_rev
[1] 0.555211

#compare with the GLM
dat <- apply(data_rev[,c(4:5, 7:33)], 2, factor)
length(rownames(dat))
dat = cbind(data_rev[,c(1:3, 6)], dat)
names(dat)
dat=dat[,-c(4,9,15,33)]
set.seed(17)
ratio = 0.5
indx = sample(1:nrow(dat), size=ratio*nrow(dat))
dat.train = dat[indx,]
dat.test = dat[-indx,]

reg_rev <- glm(revision ~ age + bmi_pre
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

prob = predict(reg_rev, newdata=dat.test, type="response")
aList = getROC_AUC(prob, dat.test$revision)
auc_rev = unlist(aList$auc)

> auc_rev
[1] 0.6161524


reg_rev <- glm(revision ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_rev
[1] 0.612915


reg_rev <- glm(revision ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_rev
[1] 0.6150445


reg_rev <- glm(revision ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat, family=binomial)

prob = predict(reg_rev, type="response")
aList = getROC_AUC(prob, dat$revision)
auc_rev = unlist(aList$auc)
> auc_rev
[1] 0.6178434


#complication
dat <- apply(data_com[,c(4:5, 7:9, 11:33)], 2, factor)
summary(dat)
length(rownames(dat))
dat = cbind(data_com[,c(1:3, 6, 10)], dat)
names(dat)
dat=dat[,-c(4,10,15,33)]

set.seed(17)
ratio = 0.5
indx = sample(1:nrow(dat), size=ratio*nrow(dat))
dat.train = dat[indx,]
dat.test = dat[-indx,]

set.seed(10)
rf_com <- randomForest(as.factor(complication) ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

rfp_com = predict(rf_com, type="prob", newdata=dat.test)
aList = getROC_AUC(rfp_com[,2], dat.test$complication)
rf_auc_com = unlist(aList$auc)
> rf_auc_com
[1] 0.6051536


set.seed(10)
rf_com <- randomForest(as.factor(complication) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

> rf_auc_com
[1] 0.5971817



#keep the attributes for clustering only and remove insurance and race
set.seed(10)
rf_com <- randomForest(as.factor(complication) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

> rf_auc_com
[1] 0.5935633


#compare with the GLM
reg_com <- glm(complication ~ age + bmi_pre + urgent_pre_90
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

prob = predict(reg_com, newdata=dat.test, type="response")
aList = getROC_AUC(prob, dat.test$complication)
auc_com = unlist(aList$auc)

> auc_com
[1] 0.6304237


reg_com <- glm(complication ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_com
[1] 0.6258296

reg_com <- glm(complication ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_com
[1] 0.6198756

#post recovery
dat <- apply(data_porec[,c(4:5, 7:9, 11:33)], 2, factor)
summary(dat)
length(rownames(dat))
dat = cbind(data_porec[,c(1:3, 6, 10)], dat)
names(dat)
dat=dat[,-c(4,10,15,33)]

set.seed(17)
ratio = 0.5
indx = sample(1:nrow(dat), size=ratio*nrow(dat))
dat.train = dat[indx,]
dat.test = dat[-indx,]

set.seed(10)
rf_porec <- randomForest(as.factor(post_recovery) ~ age + bmi_pre + urgent_pre_90 
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

rfp_porec = predict(rf_porec, type="prob", newdata=dat.test)
aList = getROC_AUC(rfp_porec[,2], dat.test$post_recovery)
rf_auc_porec = unlist(aList$auc)
> rf_auc_porec
[1] 0.6938039

set.seed(10)
rf_porec <- randomForest(as.factor(post_recovery) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

> rf_auc_porec
[1] 0.683069


#keep the attributes for clustering only and remove insurance and race
set.seed(10)
rf_porec <- randomForest(as.factor(post_recovery) ~ age + bmi_pre + gender
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism,
data=dat.train, importance=T)

> rf_auc_porec
[1] 0.6574849


#compare with the GLM
reg_porec <- glm(post_recovery ~ age + bmi_pre + urgent_pre_90
+ antibiotics + gender + preop_prepare
+ ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group1_InfectiousParasitic
+ pre_comor_Group2_Neoplasm
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Group9_Digestive
+ pre_comor_Group10_Genitourinary
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

prob = predict(reg_porec, newdata=dat.test, type="response")
aList = getROC_AUC(prob, dat.test$post_recovery)
auc_porec = unlist(aList$auc)

> auc_porec
[1] 0.7175628

reg_porec <- glm(post_recovery ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ insurance_medicare + insurance_medicaid + insurance_private + insurance_other
+ race_AfricanAmerican + race_Asian + race_Latino + race_Native
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_porec
[1] 0.7076976

reg_porec <- glm(post_recovery ~ age + bmi_pre + gender 
+ antibiotics + ever_smoker
+ pre_comor_Group3_Endocrine
+ pre_comor_Anemia
+ pre_comor_Group5_MentalDisorder
+ pre_comor_Group6_Nervous
+ pre_comor_Hypertension
+ pre_comor_Group8_Respiratory
+ pre_comor_Rheumatism, data=dat.train, family=binomial)

> auc_porec
[1] 0.6914738


set.seed(7)
train <- sample(nrow(dat), 0.5*nrow(dat))
test <- setdiff(setdiff(seq_len(nrow(dat)), train), train)
rm(train, test)
set.seed(7)
bound <- floor(nrow(dat)*0.5)
df <- dat[sample(nrow(dat)), ]
df.train <- df[1:bound,]
df.test <- df[(bound+1):nrow(df), ]



rfp_porec = predict(tr_porec, type="prob", newdata=df.test)
aList = getROC_AUC(rfp_porec[,2], dat$post_recovery)
rf_auc_porec = unlist(aList$auc)

> rf_auc_porec
[1] 0.5039314
