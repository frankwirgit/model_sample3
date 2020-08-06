#Model the LOS, revision and complication based on multiple pre-op attributes (demographic and pre comorbidities)
#version 1 
# major changes: the comorbidities are chosen on the group level, instead of individual attributes
# add race, bmi_pre, urgent_pre_90 and all 4 insurance types, remove ortho_visit_pre60 and alcohol_pre_90
# add back on for the outside


#import odbc lib for query against explorys
library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdk5-6.1.0-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "lingtao.cao", "Hau37Eq6")

#import analysis data
setwd("H:/My Documents/workf/dataf")
list.files(pattern="*.csv")
mydata1 = read.csv("LOS_proc_variables.csv", header=TRUE)

#add mua patients
post_mua_patient = dbGetQuery(sm, "
select distinct a.explorys_patient_id from supermart_111.v_procedure a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.proc_date > b.proc_date
and a.cpt_concept = '27570'
and b.Length_of_Stay > 1
")
mydata1$mua = as.numeric(mydata1$explorys_patient_id %in% post_mua_patient$explorys_patient_id)

#adjust the preop_prepare
mydata1$preop_prepare = as.numeric(apply(mydata1[,c(8,12:15)], 1, sum) > 0)

#remove NA for bmi
mydata1.bmi = mydata1[(!is.na(mydata1$bmi_change_rate)),]

#extract LOS longer than 1 days
mydata1 <- mydata1[mydata1$Length_of_Stay > 1,]
#summary(mydata1$Length_of_Stay)

#adjust the revision and complication
mydata1$revision = as.numeric(apply(mydata1[,c("revision90","revision_18m")], 1, sum) > 0)
mydata1$complication = as.numeric(apply(mydata1[,c("complication_3m","complication_12m")], 1, sum) > 0)

#data check point
#length(which(mydata1$revision == 0))
#length(which(mydata1$revision == 1))
#length(which(mydata1$complication == 0))
#length(which(mydata1$complication == 1))

rownames(mydata1) = mydata1$explorys_patient_id
mydata1 <- mydata1[,-1]
#names(mydata1)

# form the data for LOS
data_all <- mydata1[, c("Length_of_Stay", "revision", "complication", 
"age", "bmi_pre", "antibiotics", "gender", 
"cci", "preop_prepare",                       
"ever_smoker", "smode_pre_90", "urgent_pre_90",                      
"insurance_medicare", "insurance_medicaid", "insurance_private", "insurance_other", 
"race_Caucasian", "race_AfricanAmerican", "race_Asian", "race_Latino", "race_Native",                
"pre_comor_Group1_InfectiousParasitic", 
"pre_comor_Group2_Neoplasm",   
"pre_comor_Group3_Endocrine", 
"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous", 
"pre_comor_Group7_Circularoty", 
"pre_comor_Group8_Respiratory", 
"pre_comor_Group9_Digestive", 
"pre_comor_Group10_Genitourinary", 
"pre_comor_Group13_Musculoskeletal"
)]

data_los <- data_all[,-c(2:3)]

#remove missing
nrow(data_los)
#sum(is.na(data_los))
data_los <- data_los[complete.cases(data_los),]
nrow(data_los)

#> nrow(data_los)
#[1] 58925
#> sum(is.na(data_los))
#[1] 11360
#> nrow(data_los)
#[1] 47565

#check outliers
#method used: outside of 1.5 times inter-quartile range
.f = function(){
> qnt <- quantile(data_los$Length_of_Stay, probs=c(.25,.75), na.rm=T)
> qnt
25% 75% 
  3   5 
> h <- 1.5*IQR(data_los$Length_of_Stay, na.rm=T)
> h
[1] 3
> table(subset(data_los, Length_of_Stay>8)$Length_of_Stay)

  9  10  11  12  13  14  15  16  17  18  19  20  21  23  24  26  27  29  34  36  40  56  65 
180  69  47  27  22  11  15   8   4   3   1   2   2   4   1   2   2   1   1   1   1   1   1 

> nrow(subset(data_los, Length_of_Stay>8))
[1] 406

data_los1 <- subset(data_los, Length_of_Stay<=8)

> nrow(data_los1)
[1] 47159

}



#histogram of LOS
.f <- function(){
hs <- hist(data_los$Length_of_Stay, main="", xlab="LOS", ylab="Freq", 
col="lightgreen", ylim=c(0,18000), breaks="FD", border=T)
dens <- density(data_los1$Length_of_Stay, na.rm=T)
rs <- max(hs$counts)/max(dens$y)
lines(dens$x, dens$y*rs, type="l", col="darkblue", lwd=2)
rug(data_los$Length_of_Stay)
title(main=paste("Distribution of LOS"))
#curve(dnorm(x, mean=mean(data_los$Length_of_Stay), sd=sd(data_los1$Length_of_Stay)), add=T, col="darkblue", lwd=2)
}

#correlation
library(lattice)
mycor <- cor(data_los[,2:29])
rgb.palette <- colorRampPalette(c("white","blue"), space="rgb")
levelplot(mycor, main="pre-op correlation matrix", xlab="", ylab="", scales=list(x=list(rot=45)),
col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))

#summarize the data
dat <- apply(data_los[,c(4:5, 7:9, 11:29)], 2, factor)
summary(dat)
.f = function(){
> summary(dat)
 antibiotics gender    preop_prepare ever_smoker smode_pre_90 insurance_medicare insurance_medicaid insurance_private
 0:30244     1:18387   0:11128       0:38581     0:44566      0:23756            0:46050            0:22110          
 1:17321     2:29178   1:36437       1: 8984     1: 2999      1:23809            1: 1515            1:25455          
 insurance_other race_Caucasian race_AfricanAmerican race_Asian race_Latino race_Native
 0:46582         0: 4610        0:45137              0:46463    0:47384     0:47392    
 1:  983         1:42955        1: 2428              1: 1102    1:  181     1:  173    
 pre_comor_Group1_InfectiousParasitic pre_comor_Group2_Neoplasm pre_comor_Group3_Endocrine pre_comor_Group5_MentalDisorder
 0:44156                              0:42346                   0:23455                    0:42335                        
 1: 3409                              1: 5219                   1:24110                    1: 5230                        
 pre_comor_Group6_Nervous pre_comor_Group7_Circularoty pre_comor_Group8_Respiratory pre_comor_Group9_Digestive
 0:38013                  0:21996                      0:37142                      0:34523                   
 1: 9552                  1:25569                      1:10423                      1:13042                   
 pre_comor_Group10_Genitourinary pre_comor_Group13_Musculoskeletal
 0:36175                         0: 9639                          
 1:11390                         1:37926                      
 1:37617                          
}

length(rownames(dat))
dat = cbind(data_los[,c(1:3, 6, 10)], dat)

library(foreign)
library(ggplot2)
library(boot)
library(MASS)
#library(VGAM)

#plot histograms
ggplot(dat, aes(Length_of_Stay)) + geom_histogram() + scale_x_log10() + 
facet_grid(ever_smoker ~ antibiotics, margins=T, scales="free_y") +
labs(x="ever_smoker", y="antibiotics")

.f <- function(){
ggplot(dat, aes(Length_of_Stay)) + geom_histogram() + scale_x_log10() + 
facet_grid(pre_comor_Diabetes ~ pre_comor_Anemia, margins=T, scales="free_y")

> dat2$agegrp <- toString(dat2$agegrp)
> dat2$agegrp = cut(dat$age, breaks=seq(10,90,by=10), labels=c(1:8))
> table(dat2$agegrp)

    1     2     3     4     5     6     7     8 
   67   231   463  2697 10953 16995 11909  3844 

ggplot(dat2, aes(factor(agegrp), Length_of_Stay)) + geom_violin() + geom_jitter(size=1.5) + scale_y_log10() 
+ stat_smooth(aes(x=agegrp, y=Length_of_Stay, group=1), method="loess")

ggplot(dat[dat$Length_of_Stay<=20,], aes(Length_of_Stay, fill=antibiotics)) +
geom_histogram(binwidth=1) + facet_grid(antibiotics ~ ., margins=T, scales="free")

ggplot(dat[dat$Length_of_Stay<=20,], aes(Length_of_Stay, fill=preop_prepare)) +
geom_histogram(binwidth=1) + facet_grid(preop_prepare ~ ., margins=T, scales="free")

ggplot(dat[dat$Length_of_Stay<=20,], aes(Length_of_Stay, fill=smode_pre_90)) +
geom_histogram(binwidth=1) + facet_grid(preop_prepare ~ ., margins=T, scales="free")

ggplot(dat[dat$Length_of_Stay<=20,], aes(Length_of_Stay, fill=pre_comor_Group13_Musculoskeletal)) +
geom_histogram(binwidth=1) + facet_grid(pre_comor_Group13_Musculoskeletal ~ ., margins=T, scales="free")

ggplot(data,aes(Length_of_Stay)) + geom_histogram(binwidth=1)

> output <- data.frame(resid=resid(reg_poi), fitted=fitted(reg_poi))
> ggplot(output, aes(fitted, resid))+
+ geom_jitter(position=position_jitter(width=.25), alpha=.5)+
+ stat_smooth(method="loess")
Error in predLoess(y, x, newx, s, weights, pars$robust, pars$span, pars$degree,  : 
  'Calloc' could not allocate memory (1697166815 of 4 bytes)
In addition: Warning messages:
1: In predLoess(y, x, newx, s, weights, pars$robust, pars$span, pars$degree,  :
  Reached total allocation of 2567Mb: see help(memory.size)
2: In predLoess(y, x, newx, s, weights, pars$robust, pars$span, pars$degree,  :
  Reached total allocation of 2567Mb: see help(memory.size)
> ggplot(output, aes(fitted, resid))+
+ geom_jitter(position=position_jitter(width=.25), alpha=.5)+
+ stat_quantile(method="rq")
Smoothing formula not specified. Using: y ~ x
> output <- within(output,{broken <- cut(fitted,hist(fitted,plot=F)$breaks)})
> ggplot(output,aes(broken,resid)) + geom_boxplot() + geom_jitter(alpha=.25)

}


#apply GLM with POISSON
reg_poi = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Group13_Musculoskeletal),
data=data_los, family=poisson)

summary(reg_poi)

f. <- function(){

require(ggplot2)
require(sandwich)
#require(msm)
with(dat, tapply(Length_of_Stay, antibiotics, function(x){sprintf("Mean(Std)=%1.1f(%1.2f)", mean(x), sd(x))}))
ggplot(dat, aes(Length_of_Stay, fill=antibiotics))+geom_histogram(binwidth=.5, position="dodge")

#robust stand error - package of sandwich
cov_poi <- vcovHC(reg_poi, type="HC0")
std.err <- sqrt(diag(cov_poi))
r.est <- cbind(Estimate=coef(reg_poi), "Robust SE"=std.err,
"Pr(>|z|)"=2*pnorm(abs(coef(reg_poi)/std.err), lower.tail=F),
LL=coef(reg_poi)-1.96*std.err, UL=coef(reg_poi)+1.96*std.err)

r.est

#test the goodness of fit - chi squared test is not statistically significant
> with(reg_poi, cbind(res.deviance=deviance, df=df.residual, p=pchisq(deviance, df.residual, lower.tail=F)))
     res.deviance    df p
[1,]     17866.73 47536 1

#test a single predictor if it's a statistically significant predictor
#select Group7 to test
reg_poi2 <- update(reg_poi, .~. - factor(pre_comor_Group7_Circularoty))
anova(reg_poi2, reg_poi, test="Chisq")
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
1     47537      17873                       
2     47536      17867  1   6.2506  0.01242 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#predict check
p <- data_los[,c("Length_of_Stay","age","gender","bmi_pre","cci", "ever_smoker","pre_comor_Group13_Musculoskeletal")]
p$phat <- predict(reg_poi, type="response")
p <- p[with(p, order(gender, cci)),]
p <- p[with(p, Length_of_Stay<=20),]
ggplot(p, aes(x=cci, y=phat, color=factor(gender))) +
geom_point(aes(y=Length_of_Stay), alpha=.5, position=position_jitter(h=.2)) +
geom_line(size=1) + 
labs(x="cci", y="Expected LOS")

}


#try log(LOS)
data_loglos <- data_los
data_loglos$Length_of_Stay =  log(data_loglos$Length_of_Stay)
colnames(data_loglos)[1] <- "LOGLOS"
head(data_loglos[,1], n=10)

reg_poi_log = glm(LOGLOS ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Group13_Musculoskeletal),
data=data_loglos, family=poisson)

summary(reg_poi_log)




#apply GLM with GAUSSIAN
reg_gau = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Group13_Musculoskeletal),
data=data_los, family=gaussian)

summary(reg_gau)


#GLM binomial for revision
data_rev <- data_all[,-c(1,3)]

#remove missing
nrow(data_rev)
#sum(is.na(data_los))
data_rev <- data_rev[complete.cases(data_rev),]
nrow(data_rev)

reg_rev = glm(revision ~ age + bmi_pre + cci + factor(urgent_pre_90) 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Group13_Musculoskeletal),
data=data_rev, family=binomial)

summary(reg_rev)

#GLM binomial for complication
data_com <- data_all[,-c(1,2)]

#remove missing
nrow(data_com)
#sum(is.na(data_los))
data_com <- data_com[complete.cases(data_com),]
nrow(data_com)

reg_com = glm(complication ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Group13_Musculoskeletal),
data=data_com, family=binomial)

summary(reg_com)

#replace group3, 7 & 13 with items in the groups
#"pre_comor_Group3_Endocrine", 
#"pre_comor_Group7_Circularoty", 
#"pre_comor_Group13_Musculoskeletal"

data_test <- mydata1[, c("Length_of_Stay", "revision", "complication", 
"age", "bmi_pre", "antibiotics", "gender", 
"cci", "preop_prepare",                       
"ever_smoker", "smode_pre_90", "urgent_pre_90",                      
"insurance_medicare", "insurance_medicaid", "insurance_private", "insurance_other", 
"race_Caucasian", "race_AfricanAmerican", "race_Asian", "race_Latino", "race_Native",                
"pre_comor_Group1_InfectiousParasitic", 
"pre_comor_Group2_Neoplasm",
   
"pre_comor_Thyroid",
"pre_comor_Diabetes",
"pre_comor_EndoGlands",
"pre_comor_Nutrition",
"pre_comor_Gout",
"pre_comor_Obesity",
"pre_comor_LipoidMetabolism",
"pre_comor_Metabolism",

"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous", 

"pre_comor_Rheumatic",
"pre_comor_Hypertension",
"pre_comor_Ischemic",            
"pre_comor_PulmonaryHearDisease",
"pre_comor_OtherHeart",
"pre_comor_Cerebrovascular",
"pre_comor_Arteries",
"pre_comor_Vein",

"pre_comor_Group8_Respiratory", 
"pre_comor_Group9_Digestive", 
"pre_comor_Group10_Genitourinary", 

"pre_comor_Arthropatheis",
"pre_comor_Dorsopathies",
"pre_comor_Rheumatism",
"pre_comor_Osteopathies" 
)]

data_los_test <- data_test[,-c(2:3)]

#remove missing
nrow(data_los_test)
data_los_test <- data_los_test[complete.cases(data_los_test),]
nrow(data_los_test)


#apply GLM with POISSON
reg_poi_test = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)

+ factor(pre_comor_Thyroid) + factor(pre_comor_Diabetes) + factor(pre_comor_EndoGlands) + factor(pre_comor_Nutrition)
+ factor(pre_comor_Gout) + factor(pre_comor_Obesity) + factor(pre_comor_LipoidMetabolism) + factor(pre_comor_Metabolism)

+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)

+ factor(pre_comor_Rheumatic) + factor(pre_comor_Hypertension) + factor(pre_comor_Ischemic)
+ factor(pre_comor_PulmonaryHearDisease) + factor(pre_comor_OtherHeart) + factor(pre_comor_Cerebrovascular)
+ factor(pre_comor_Arteries) + factor(pre_comor_Vein)

+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)

+ factor(pre_comor_Arthropatheis) + factor(pre_comor_Dorsopathies) + factor(pre_comor_Rheumatism) + factor(pre_comor_Osteopathies),

data=data_los_test, family=poisson)

summary(reg_poi_test)

reg_gau_test = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)

+ factor(pre_comor_Thyroid) + factor(pre_comor_Diabetes) + factor(pre_comor_EndoGlands) + factor(pre_comor_Nutrition)
+ factor(pre_comor_Gout) + factor(pre_comor_Obesity) + factor(pre_comor_LipoidMetabolism) + factor(pre_comor_Metabolism)

+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)

+ factor(pre_comor_Rheumatic) + factor(pre_comor_Hypertension) + factor(pre_comor_Ischemic)
+ factor(pre_comor_PulmonaryHearDisease) + factor(pre_comor_OtherHeart) + factor(pre_comor_Cerebrovascular)
+ factor(pre_comor_Arteries) + factor(pre_comor_Vein)

+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)

+ factor(pre_comor_Arthropatheis) + factor(pre_comor_Dorsopathies) + factor(pre_comor_Rheumatism) + factor(pre_comor_Osteopathies),

data=data_los_test, family=gaussian)

summary(reg_gau_test)


#test1 - modified predictors
#remove smode_pre_90, race_Caucasian, pre_comor_Group13_Musculoskeletal

reg_poi_t1 = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary),
data=data_los, family=poisson)

summary(reg_poi_t1)


#test2 - modified predictors
#remove preop_prepare??
#replace group3 with pre_comor_Diabetes, Nutrition, Gout, Metabolism
#replace group7 with Ischemic, Pulmonary Heart Disease, Cerbrovascular, Vein
#replace group13 with Dorsopathies, Rheumatism

data_t2 <- mydata1[, c("Length_of_Stay", "revision", "complication", 
"age", "bmi_pre", "antibiotics", "gender", 
"cci", "preop_prepare",                       
"ever_smoker", "smode_pre_90", "urgent_pre_90",                      
"insurance_medicare", "insurance_medicaid", "insurance_private", "insurance_other", 
"race_Caucasian", "race_AfricanAmerican", "race_Asian", "race_Latino", "race_Native",                
"pre_comor_Group1_InfectiousParasitic", 
"pre_comor_Group2_Neoplasm",

"pre_comor_Diabetes",
"pre_comor_Nutrition",
"pre_comor_Gout",
"pre_comor_Metabolism",

"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous", 

"pre_comor_Ischemic",            
"pre_comor_PulmonaryHearDisease",
"pre_comor_Cerebrovascular",
"pre_comor_Vein",

"pre_comor_Group8_Respiratory", 
"pre_comor_Group9_Digestive", 
"pre_comor_Group10_Genitourinary", 

"pre_comor_Dorsopathies",
"pre_comor_Rheumatism" 
)]

data_los_t2 <- data_t2[,-c(2:3)]

#remove missing
nrow(data_los_t2)
data_los_t2 <- data_los_t2[complete.cases(data_los_t2),]
nrow(data_los_t2)


#apply GLM with POISSON
reg_poi_t2 = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)

+ factor(pre_comor_Diabetes) + factor(pre_comor_Nutrition)
+ factor(pre_comor_Gout) + factor(pre_comor_Metabolism)

+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)

+ factor(pre_comor_Ischemic)
+ factor(pre_comor_PulmonaryHearDisease) + factor(pre_comor_Cerebrovascular)
+ factor(pre_comor_Vein)

+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)

+ factor(pre_comor_Dorsopathies) + factor(pre_comor_Rheumatism),

data=data_los_t2, family=poisson)

summary(reg_poi_t2)
 
#replace group 9 with individual items
 
data_t3 <- mydata1[, c("Length_of_Stay", "revision", "complication", 
"age", "bmi_pre", "antibiotics", "gender", 
"cci", "preop_prepare",                       
"ever_smoker", "smode_pre_90", "urgent_pre_90",                      
"insurance_medicare", "insurance_medicaid", "insurance_private", "insurance_other", 
"race_Caucasian", "race_AfricanAmerican", "race_Asian", "race_Latino", "race_Native",                
"pre_comor_Group1_InfectiousParasitic", 
"pre_comor_Group2_Neoplasm",

"pre_comor_Diabetes",
"pre_comor_Nutrition",
"pre_comor_Gout",
"pre_comor_Metabolism",

"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous", 

"pre_comor_Ischemic",            
"pre_comor_PulmonaryHearDisease",
"pre_comor_Cerebrovascular",
"pre_comor_Vein",

"pre_comor_Group8_Respiratory", 

"pre_comor_Oral","pre_comor_Esophagus","pre_comor_Appendicitis","pre_comor_Hernia",
"pre_comor_EnteritisColitis","pre_comor_Intestine","pre_comor_OtherDigestive",

"pre_comor_Group10_Genitourinary", 

"pre_comor_Dorsopathies",
"pre_comor_Rheumatism" 
)]

data_los_t3 <- data_t3[,-c(2:3)]

#remove missing
nrow(data_los_t3)
data_los_t3 <- data_los_t3[complete.cases(data_los_t3),]
nrow(data_los_t3)

reg_poi_t3 = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)

+ factor(pre_comor_Diabetes) + factor(pre_comor_Nutrition)
+ factor(pre_comor_Gout) + factor(pre_comor_Metabolism)

+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)

+ factor(pre_comor_Ischemic)
+ factor(pre_comor_PulmonaryHearDisease) + factor(pre_comor_Cerebrovascular)
+ factor(pre_comor_Vein)

+ factor(pre_comor_Group8_Respiratory)

+ factor(pre_comor_Appendicitis)+factor(pre_comor_Hernia)
+ factor(pre_comor_EnteritisColitis)+factor(pre_comor_Intestine)+factor(pre_comor_OtherDigestive)

+ factor(pre_comor_Group10_Genitourinary)

+ factor(pre_comor_Dorsopathies) + factor(pre_comor_Rheumatism),

data=data_los_t3, family=poisson)

summary(reg_poi_t3)

#grep("Group9",colnames(mydata1), fixed=T)





















f. <- function(){
#Analyze the outliers by using robust regression

opar <- par(mfrow=c(2, 2), oma=c(0, 0, 1.1, 0))
plot(reg_gau, las=1)
> par(opar)
> data_los[c("42557946","70561749","61503563"), 1:2]
         Length_of_Stay age
42557946             65  71
70561749             56  79
61503563             40  54

#records with large Cook's distance
codis <- cooks.distance(reg_gau)
r <- stdres(reg_gau)
a <- cbind(data_los[,1:2], codis, r)
nrow(a[codis> (4/nrow(a)),])
> nrow(a[codis> (4/nrow(a)),])
[1] 1646
> head(a[codis > 4/47565, ], n=10)
       Length_of_Stay age        codis          r
18401               8  60 3.890893e-04  2.8200162
290422              9  63 5.238338e-04  3.4969743
294083              3  58 1.153586e-04 -0.7091308
377780              7  59 1.164782e-04  2.1759385
409726              2  67 3.774819e-04 -1.3108290
454007             36  73 3.418349e-02 23.0995318
646866              6  48 1.007480e-04  1.6496452
674954              7  76 8.584162e-05  1.8398426
722201              7  61 9.368806e-05  1.9747225
770735              8  59 2.219523e-04  3.1085743

> asorted <- a[order(-abs(r)),]
> asorted[1:10,]
          Length_of_Stay age       codis        r
42557946              65  71 0.079317215 44.89932
70561749              56  79 0.022954919 38.60472
61503563              40  54 0.029424013 26.64898
454007                36  73 0.034183492 23.09953
50475031              34  24 0.015430966 22.86071
41234496              29  81 0.008744370 18.04696
54977309              27  25 0.015488550 17.09981
70183642              27  74 0.006323957 16.96527
468843386             26  47 0.008548507 16.33428
452212912             26  74 0.007482118 15.92835

}

library(foreign)
library(MASS)

#apply GLM with GAUSSIAN
los_rreg = rlm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Group13_Musculoskeletal),
data=data_los)

summary(los_rreg)

f. <- function(){
> b <- data.frame(los=data_los$Length_of_Stay, resid=los_rreg$resid, weight=los_rreg$w)
> b[order(b$weight),][1:10,]
          los    resid     weight
42557946   65 60.66928 0.02186991
70561749   56 52.04360 0.02549464
61503563   40 36.24665 0.03660571
454007     36 31.59468 0.04199576
50475031   34 30.96065 0.04285554
41234496   29 24.65927 0.05380691
54977309   27 23.29786 0.05695103
70183642   27 22.95748 0.05779541
468843386  26 22.17421 0.05983697
452212912  26 21.76025 0.06097548

> nrow(subset(b, weight<1))
[1] 9163

#compute the p value for RLM - can not trust
#p <- 2*pt(-abs(t), df=n-1)
> dd = data.frame(sumrlm$coefficients)
> dd$p.value = 2*pt(abs(dd$t.value), sumrlm$df[2], lower=F)

#test the ctree in party package
los_ctree = ctree(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Group13_Musculoskeletal),
data=data_los, controls=ctree_control(maxdepth=5))

plot(los_ctree, type="simple", inner_panel=node_inner(los_ctree, abbreviate=T, pval=F, id=F),
terminal_panel=node_terminal(los_ctree, abbreviate=T, digits=1, fill=c("white"),id=F))

}


























#predict the linear fitting
prob = predict(reg_grp1, type=c("response"))

#calculate the AUC
getROC_AUC = function(probs, true_Y) {
	probsSort = sort(probs, decreasing=TRUE, index.return=TRUE)
	val = unlist(probsSort$x)
	idx = unlist(probsSort$ix)

	roc_y = true_Y[idx];
	stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
	stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)

	auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
	return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}

aList = getROC_AUC(prob, data_los1$Length_of_Stay)
auc = unlist(aList$auc)
auc




res_grp3 <- resid(reg_grp3)
plot(data_los1$Length_of_Stay, res_grp3, ylab="Residuals", xlab="LOS", main="Linear Fitting")
abline(0,0)

#conduct clustering
mydata2 <- data_all[,c("bmi_pre","antibiotics","ever_smoker",
"pre_comor_Group1_InfectiousParasitic", "pre_comor_Group2_Neoplasm", "pre_comor_Group3_Endocrine",
"pre_comor_Group5_MentalDisorder","pre_comor_Group6_Nervous","pre_comor_Group7_Circularoty",
"pre_comor_Group8_Respiratory","pre_comor_Group9_Digestive","pre_comor_Group10_Genitourinary",
"pre_comor_Group13_Musculoskeletal")]


> nrow(mydata2)
[1] 58925
> mydata2 <- mydata2[complete.cases(mydata2),]
> nrow(mydata2)


#create bmi groups - normal, overweight, obese
idx = which(mydata2$bmi_pre < 30)
mydata2$bmi_30 = 0
mydata2$bmi_30[idx] = 1
idx = which(mydata2$bmi_pre >= 30 & mydata2$bmi_pre < 35)
mydata2$bmi_30_35 = 0
mydata2$bmi_30_35[idx] = 1
idx = which(mydata2$bmi_pre >= 35 & mydata2$bmi_pre < 40)
mydata2$bmi_35_40 = 0
mydata2$bmi_35_40[idx] = 1
idx = which(mydata2$bmi_pre >= 40)
mydata2$bmi_40 = 0
mydata2$bmi_40[idx] = 1


mydata2 <- mydata2[,-1]
mydata2[,13:16] = mydata2[,13:16] / 4

set.seed(7)
mykm = kmeans(mydata2, centers = 10)

> table(mykm$cluster)

   1    2    3    4    5    6    7    8    9   10 
4244 2080 3361 5828 5767 2822 5017 5566 3831 9060 
> mykm$size
 [1] 4244 2080 3361 5828 5767 2822 5017 5566 3831 9060
> ms <- cbind(mydata2, cluster=mykm$cluster)
> ms <- ms[sample(nrow(ms), 5000),]
> ms <- cbind(ms, id=seq(nrow(ms)))
> ms$idsort <- ms$id[order(ms$cluster)]

> ms$idsort <- order(ms$idsort)
> head(ms[,c("id","cluster","idsort")], n=20)
> msd <- melt(ms[,c(1:12,18:19)], id.vars=c("id","idsort"))
> ggplot(msd, aes(x=variable, y=idsort))+geom_tile(aes(fill=value)) + theme(axis.text.x=element_text(angle=90, hjust=1))
> 

t <- data.frame(cluster=mykm$cluster)
data_clust <- merge(data_los, t, by="row.names")
rownames(data_clust) = data_clust$Row.names
dclust1 <- subset(data_clust[,2:31], cluster==1)
dclust1 <- dclust1[,-30]
summary(apply(dclust1[,c(4:5, 7:9,11:29)],2,factor)

 
#cluster1
#check the grouping feature - 
#smode_pre_90, insurance_medicaid, insurance_other, race_Asian, race_Latino, race_Native
#pre_comor_Group8_Respiratory
 antibiotics gender   preop_prepare ever_smoker smode_pre_90 insurance_medicare insurance_medicaid insurance_private
 0:2515      1:1302   0: 682        0:4101      0:4229       0:1882             0:4137             0:1783           
 1:1729      2:2942   1:3562        1: 143      1:  15       1:2362             1: 107             1:2461           
 insurance_other race_Caucasian race_AfricanAmerican race_Asian race_Latino race_Native
 0:4166          0: 325         0:4015               0:4181     0:4225      0:4230     
 1:  78          1:3919         1: 229               1:  63     1:  19      1:  14     
 pre_comor_Group1_InfectiousParasitic pre_comor_Group2_Neoplasm pre_comor_Group3_Endocrine pre_comor_Group5_MentalDisorder
 0:3901                               0:3788                    0: 623                     0:3745                         
 1: 343                               1: 456                    1:3621                     1: 499                         
 pre_comor_Group6_Nervous pre_comor_Group7_Circularoty pre_comor_Group8_Respiratory pre_comor_Group9_Digestive
 0:3255                   0: 708                       1:4244                       0:2787                    
 1: 989                   1:3536                                                    1:1457                    
 pre_comor_Group10_Genitourinary pre_comor_Group13_Musculoskeletal
 0:3102                          0: 252                           
 1:1142                          1:3992     
 

reg_poi_c1 = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_Caucasian)+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary),
data=dclust1, family=poisson)

reg_poi_c1 = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_private)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary),
data=data_los, family=poisson)

summary(reg_poi_c1)


dclust2 <- subset(data_clust[,2:31], cluster==2)
dclust2 <- dclust2[,-30]

dclust3 <- subset(data_clust[,2:31], cluster==3)
dclust3 <- dclust3[,-30]

dclust4 <- subset(data_clust[,2:31], cluster==4)
dclust4 <- dclust4[,-30]




mykm.cluster=as.data.frame(mykm$cluster)
myclust1 <- merge(mykm.cluster, myclust, by="row.names")

#rownames(myclust1) = myclust12$Row.names
#myclust1 = myclust1[,-1]
> table(myclust1$clust_numb)

    1     2     3     4     5     6     7     8     9    10 
13001  4554  4132  2901  1879  5648  3372  4701  3736  6683 

> library(plyr)
Warning message:
package ‘plyr’ was built under R version 3.2.2 
> tt = count(myclust1, c('clust_numb'))
> tt
   clust_numb  freq
1           1 13001
2           2  4554
3           3  4132
4           4  2901
5           5  1879
6           6  5648
7           7  3372
8           8  4701
9           9  3736
10         10  6683

> tt = count(myclust1, c('clust_numb','pre_comor_Group5_MentalDisorder'))
> tt
   clust_numb pre_comor_Group5_MentalDisorder  freq
1           1                               0 12833
2           1                               1   168
3           2                               0  4208
4           2                               1   346
5           3                               0  3752
6           3                               1   380
7           4                               0  2572
8           4                               1   329
9           5                               0  1602
10          5                               1   277
11          6                               0  5223
12          6                               1   425
13          7                               0  1915
14          7                               1  1457
15          8                               0  3295
16          8                               1  1406
17          9                               0  3168
18          9                               1   568
19         10                               0  6590
20         10                               1    93

#assign the cluster to new data
cluster.in <- function(x){
	mydist <- apply(mykm$centers, 1, function(y) sqrt(sum((x-y)^2)))
	return(which.min(mydist)[1])
}
dftest <- myclust1[1:10, 2:(length(myclust1)-1)]
newcluster <- apply(dftest, 1, cluster.in)
> class(newcluster)
[1] "integer"
> head(newcluster)
1 2 3 4 5 6 
5 2 6 6 5 2 
> nrow(dftest)
[1] 10
> newcluster
 1  2  3  4  5  6  7  8  9 10 
 5  2  6  6  5  2  4  3  1  1 
> myclust1[1:10,length(myclust1)]
 [1] 5 2 6 6 5 2 4 3 1 1

 
