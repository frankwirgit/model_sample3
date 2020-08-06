#Model the LOS based on multiple pre-op attributes (demographic and pre comorbidities)
#version 2
# major changes: data is based on the segmentation results done by Zhaonan on 11/3/15
# the comorbidities are chosen on the group level, instead of individual attributes
# add race, bmi_pre, urgent_pre_90 and all 4 insurance types, remove ortho_visit_pre60 and alcohol_pre_90



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
mydata1$post_recovery = as.numeric(apply(mydata1[,c("Ortho_visit_90_360","pain","xray")], 1, sum) > 0)
#data check point
#length(which(mydata1$revision == 0))
#length(which(mydata1$revision == 1))
#length(which(mydata1$complication == 0))
#length(which(mydata1$complication == 1))

rownames(mydata1) = mydata1$explorys_patient_id
mydata1 <- mydata1[,-1]
#names(mydata1)


#extract the segmentation info
setwd("H:/My Documents/workf/dataf/segmentation_without_ortho_visit")
list.files(pattern="*.csv")
#centers ordered by cluster number
mykm.centers = read.csv("new_preop_kmeans_13_centers_8_binary_48K.csv", header=TRUE)

#size ordered by cluster number
mykm.size=read.csv("new_preop_kmeans_13_size_8_binary_48K.csv", header=TRUE)
colnames(mykm.size)[1]="size"
mykm.cluster = read.csv("new_preop_kmeans_13_cluster_binary_48K.csv", header=TRUE)
rownames(mykm.cluster) = mykm.cluster[,"X"]
mykm.cluster <- mykm.cluster[,"seg_8", drop=F]
colnames(mykm.cluster)[1]="cluster"


f. <- function(){

> nrow(mykm.cluster)
[1] 47565

> sum(mykm.size[,1])
[1] 47565

> names(mykm.centers)
 [1] "bmi_30"                          "bmi_30_35"                       "bmi_35_40"                      
 [4] "bmi_40"                          "antibiotics"                     "ever_smoker"                    
 [7] "pre_comor_Group3_Endocrine"      "pre_comor_Anemia"                "pre_comor_Group5_MentalDisorder"
[10] "pre_comor_Group6_Nervous"        "pre_comor_Hypertension"          "pre_comor_Group8_Respiratory"   
[13] "pre_comor_Rheumatism"
}



# select attributes form the data
# include outcomes of LOS, revision and complication
# include all attributes used for clustering except bmi subgroup
data_all <- mydata1[, c("Length_of_Stay", "revision", "complication", "post_recovery",
"age", "bmi_pre", "antibiotics", "gender", 
"cci", "preop_prepare",                       
"ever_smoker", "smode_pre_90", "urgent_pre_90",                      
"insurance_medicare", "insurance_medicaid", "insurance_private", "insurance_other", 
"race_Caucasian", "race_AfricanAmerican", "race_Asian", "race_Latino", "race_Native",                
"pre_comor_Group1_InfectiousParasitic", 
"pre_comor_Group2_Neoplasm",   
"pre_comor_Group3_Endocrine", 
"pre_comor_Anemia",
"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous",
"pre_comor_Hypertension", 
"pre_comor_Group7_Circularoty", 
"pre_comor_Group8_Respiratory", 
"pre_comor_Group9_Digestive", 
"pre_comor_Group10_Genitourinary", 
"pre_comor_Rheumatism",
"pre_comor_Group13_Musculoskeletal",
"alcohol_pre_90"
)]

data_los <- data_all[,-c(2:4)]

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



#correlation
library(lattice)
#remove group7 and group13 which have the sub-items
mycor <- cor(data_los[,2:26, 28:31])
rgb.palette <- colorRampPalette(c("white","blue"), space="rgb")
levelplot(mycor, main="pre-op correlation matrix", xlab="", ylab="", scales=list(x=list(rot=45)),
col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))

#summarize the data
dat <- apply(data_los[,c(4:5, 7:9, 11:33)], 2, factor)
summary(dat)
.f = function(){
> summary(dat)
 antibiotics gender    preop_prepare ever_smoker smode_pre_90 insurance_medicare insurance_medicaid insurance_private insurance_other
 0:30244     1:18387   0:11128       0:38581     0:44566      0:23756            0:46050            0:22110           0:46582        
 1:17321     2:29178   1:36437       1: 8984     1: 2999      1:23809            1: 1515            1:25455           1:  983        
 race_Caucasian race_AfricanAmerican race_Asian race_Latino race_Native pre_comor_Group1_InfectiousParasitic pre_comor_Group2_Neoplasm
 0: 4610        0:45137              0:46463    0:47384     0:47392     0:44156                              0:42346                  
 1:42955        1: 2428              1: 1102    1:  181     1:  173     1: 3409                              1: 5219                  
 pre_comor_Group3_Endocrine pre_comor_Anemia pre_comor_Group5_MentalDisorder pre_comor_Group6_Nervous pre_comor_Hypertension
 0:23455                    0:41283          0:42335                         0:38013                  0:25887               
 1:24110                    1: 6282          1: 5230                         1: 9552                  1:21678               
 pre_comor_Group7_Circularoty pre_comor_Group8_Respiratory pre_comor_Group9_Digestive pre_comor_Group10_Genitourinary
 0:21996                      0:37142                      0:34523                    0:36175                        
 1:25569                      1:10423                      1:13042                    1:11390                        
 pre_comor_Rheumatism pre_comor_Group13_Musculoskeletal alcohol_pre_90
 0:37117              0: 9639                           0:43929       
 1:10448              1:37926                           1: 3636                 
}

length(rownames(dat))
dat = cbind(data_los[,c(1:3, 6, 10)], dat)

library(foreign)
library(ggplot2)
library(boot)
library(MASS)
#library(VGAM)

.f <- function(){

#plot histograms
ggplot(dat, aes(Length_of_Stay)) + geom_histogram() + scale_x_log10() + 
facet_grid(ever_smoker ~ antibiotics, margins=T, scales="free_y") +
labs(x="ever_smoker", y="antibiotics")

ggplot(dat, aes(Length_of_Stay)) + geom_histogram() + scale_x_log10() + 
facet_grid(pre_comor_Diabetes ~ pre_comor_Anemia, margins=T, scales="free_y")

#view mean
with(dat, tapply(Length_of_Stay, antibiotics, function(x){sprintf("Mean(Std)=%1.1f(%1.2f)", mean(x), sd(x))}))
ggplot(dat, aes(Length_of_Stay, fill=antibiotics))+geom_histogram(binwidth=.5, position="dodge")


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
#remove alcohol_pre_90, smode_pre_90 and race_Caucasian
#note - if keep sub-items under certain group then the group needs to be removed
#in this case, they are group7 and group13

#apply for the whole population
reg_poi_all = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=data_los, family=poisson)

summary(reg_poi_all)

f. <- function(){

require(ggplot2)
require(sandwich)
#require(msm)

#test the goodness of fit - chi squared test is not statistically significant
> with(reg_poi_all, cbind(res.deviance=deviance, df=df.residual, p=pchisq(deviance, df.residual, lower.tail=F)))
     res.deviance    df p
[1,]     17922.89 47536 1

#test a single predictor if it's a statistically significant predictor
#select Group7 to test
reg_poi2 <- update(reg_poi_all, .~. - factor(pre_comor_Hypertension))
anova(reg_poi2, reg_poi_all, test="Chisq")
  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
1     47537      17945                          
2     47536      17923  1   21.709 3.172e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#predict check
p <- data_los[,c("Length_of_Stay","age","gender","bmi_pre","cci", "ever_smoker")]
p$phat <- predict(reg_poi_all, type="response")
p <- p[with(p, order(gender, cci)),]
p <- p[with(p, Length_of_Stay<=20),]
ggplot(p, aes(x=cci, y=phat, color=factor(gender))) +
geom_point(aes(y=Length_of_Stay), alpha=.5, position=position_jitter(h=.2)) +
geom_line(size=1) + 
labs(x="cci", y="Expected LOS")

}


#conduct clustering
> mykm.size
   size
1 13270
2  4053
3  4097
4  4630
5  4628
6  6214
7  7969
8  2704

#merge the cluster with the data
dclust_los <- merge(data_los, mykm.cluster, by="row.names")
rownames(dclust_los) = dclust_los$Row.names
dclust_los <- dclust_los[,2:35]


#check the merging result
head(rownames(dclust_los), n=5)
 [1] "1000191"  "10027705" "1003539"  "1003615"  "1015364"
> mykm.cluster[c("1000191",  "10027705", "1003539",  "1003615",  "1015364"), "cluster"]
[1] 2 4 4 4 3
> dclust_los[c(1:5), "cluster"]
[1] 2 4 4 4 3

#extract each cluster population into vector
counter <- 0
vclust <- list(NULL)
nlen <- 1

for(i in seq_len(nrow(mykm.size)))
{
	if(counter == nlen){
		length(vclust) <- nlen <- nlen*2
	}
	counter <- counter + 1
	dc <- subset(dclust_los, cluster==(counter-1))
	vclust[[counter]] <- dc[,-34]
}

#check the assignment
> lapply(vclust, nrow)
[[1]]
[1] 13270

[[2]]
[1] 4053

[[3]]
[1] 4097

[[4]]
[1] 4630

[[5]]
[1] 4628

[[6]]
[1] 6214

[[7]]
[1] 7969

[[8]]
[1] 2704

> colnames(vclust[[1]])
 [1] "Length_of_Stay"                       "age"                                  "bmi_pre"                             
 [4] "antibiotics"                          "gender"                               "cci"                                 
 [7] "preop_prepare"                        "ever_smoker"                          "smode_pre_90"                        
[10] "urgent_pre_90"                        "insurance_medicare"                   "insurance_medicaid"                  
[13] "insurance_private"                    "insurance_other"                      "race_Caucasian"                      
[16] "race_AfricanAmerican"                 "race_Asian"                           "race_Latino"                         
[19] "race_Native"                          "pre_comor_Group1_InfectiousParasitic" "pre_comor_Group2_Neoplasm"           
[22] "pre_comor_Group3_Endocrine"           "pre_comor_Anemia"                     "pre_comor_Group5_MentalDisorder"     
[25] "pre_comor_Group6_Nervous"             "pre_comor_Hypertension"               "pre_comor_Group7_Circularoty"        
[28] "pre_comor_Group8_Respiratory"         "pre_comor_Group9_Digestive"           "pre_comor_Group10_Genitourinary"     
[31] "pre_comor_Rheumatism"                 "pre_comor_Group13_Musculoskeletal"    "alcohol_pre_90"        

#apply GLM POISSON to each of the cluster
reg_poi <- vector("list",8)


#cluster 1
dat <- apply(vclust[[1]][,c(4, 8, 22:26, 28,31)], 2, factor)
summary(dat)


lapply(vclust[[1]][,c(4, 8, 22:26, 28,31)], unique)
$antibiotics
[1] 0

$ever_smoker
[1] 0 1

$pre_comor_Group3_Endocrine
[1] 0

$pre_comor_Anemia
[1] 0 1

$pre_comor_Group5_MentalDisorder
[1] 0 1

$pre_comor_Group6_Nervous
[1] 1 0

$pre_comor_Hypertension
[1] 0 1

$pre_comor_Group8_Respiratory
[1] 0 1

$pre_comor_Rheumatism
[1] 1 0


#u <- lapply(vclust[[1]][,c(4, 8, 22:26, 28,31)], unique)
#lapply(u, function(x) {if(length(x)==1) {paste(x)}})

#remove the antibiotics 0 and group3 0
reg_poi[[1]] = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=vclust[[1]], family=poisson)

summary(reg_poi[[1]])

#cluster 2
lapply(vclust[[2]][,c(4, 8, 22:26, 28,31)], unique)

#remove ever_smoker 1
reg_poi[[2]] = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=vclust[[2]], family=poisson)

summary(reg_poi[[2]])

#cluster 3
lapply(vclust[[3]][,c(4, 8, 22:26, 28,31)], unique)

#remove ever_smoker 0
reg_poi[[3]] = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=vclust[[3]], family=poisson)

summary(reg_poi[[3]])

#cluster 4
lapply(vclust[[4]][,c(4, 8, 22:26, 28,31)], unique)

dat <- apply(vclust[[4]][,c(4:5, 7:9, 11:33)], 2, factor)
summary(dat)

#remove antibiotics 1, pre_comor_Hypertension 1 and preop_prepare 1 and urgent_pre_90 0
reg_poi[[4]] = glm(Length_of_Stay ~ age + bmi_pre + cci
+ factor(gender)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=vclust[[4]], family=poisson)

summary(reg_poi[[4]])


#cluster 5

lapply(vclust[[5]][,c(4, 8, 22:26, 28,31)], unique)

#remove group3 1 and  pre_comor_Hypertension 0
reg_poi[[5]] = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=vclust[[5]], family=poisson)

summary(reg_poi[[5]])


#cluster 6
lapply(vclust[[6]][,c(4, 8, 22:26, 28,31)], unique)

#remove antibiotics 0, pre_comor_Hypertension 1 and pre_comor_Rheumatism 0
reg_poi[[6]] = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary),
data=vclust[[6]], family=poisson)

summary(reg_poi[[6]])

#cluster 7
lapply(vclust[[7]][,c(4, 8, 22:26, 28,31)], unique)
dat <- apply(vclust[[7]][,c(4:5, 7:9, 11:33)], 2, factor)
summary(dat)

#remove antibiotics 1, group3 0, preop_prepare 1 and urgent_pre_90 0
reg_poi[[7]] = glm(Length_of_Stay ~ age + bmi_pre + cci
+ factor(gender)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=vclust[[7]], family=poisson)

summary(reg_poi[[7]])

#cluster 8
lapply(vclust[[8]][,c(4, 8, 22:26, 28,31)], unique)

#remove antibiotics 0, group6 0 and pre_comor_Rheumatism 1
reg_poi[[8]] = glm(Length_of_Stay ~ age + bmi_pre + cci + urgent_pre_90 
+ factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary),
data=vclust[[8]], family=poisson)

summary(reg_poi[[8]])


#compute the R squared values
rm(reg_r2)
r2v <- 1- sum((reg_poi_all$y-reg_poi_all$fitted)^2)/sum((reg_poi_all$y-mean(reg_poi_all$y))^2)
reg_r2 <- data.frame(r2=r2v)
rownames(reg_r2)[1] = "0"
for(i in 1:8){
 r2v <- 1- sum((reg_poi[[i]]$y-reg_poi[[i]]$fitted)^2)/sum((reg_poi[[i]]$y-mean(reg_poi[[i]]$y))^2)
 reg_r2 <- rbind(reg_r2, r2v)
 rownames(reg_r2)[i+1] = as.character(i)
 }

> reg_r2
          r2
0 0.07794179
1 0.10066360
2 0.07608116
3 0.07289282
4 0.05681551
5 0.08188575
6 0.07946374
7 0.03960003
8 0.08474407

#use the median of LOS instead of the fitted LOS
r2v <- 1- sum((reg_poi_all$y-median(reg_poi_all$y))^2)/sum((reg_poi_all$y-mean(reg_poi_all$y))^2)
reg_r2 <- data.frame(r2=r2v)
rownames(reg_r2)[1] = "0"
for(i in 1:8){
 r2v <- 1- sum((reg_poi[[i]]$y-median(reg_poi[[i]]$y))^2)/sum((reg_poi[[i]]$y-mean(reg_poi[[i]]$y))^2)
 reg_r2 <- rbind(reg_r2, r2v)
 rownames(reg_r2)[i+1] = as.character(i)
 }
 
 
#compute the MSE
msev <- mean((reg_poi_all$y-reg_poi_all$fitted)^2)
reg_mse <- data.frame(mse=msev)
rownames(reg_mse)[1] = "0"
for(i in 1:8){
 msev <- mean((reg_poi[[i]]$y-reg_poi[[i]]$fitted)^2)
 reg_mse <- rbind(reg_mse, msev)
 rownames(reg_mse)[i+1] = as.character(i)
 }
> reg_mse
       mse
0 1.821479
1 1.834327
2 2.581900
3 2.018530
4 1.413416
5 1.503468
6 1.969111
7 1.557212
8 1.604684

#use the median of LOS instead of the fitted LOS
msev <- mean((reg_poi_all$y-median(reg_poi_all$y))^2)
reg_mse <- data.frame(mse=msev)
rownames(reg_mse)[1] = "0"
for(i in 1:8){
 msev <- mean((reg_poi[[i]]$y-median(reg_poi[[i]]$y))^2)
 reg_mse <- rbind(reg_mse, msev)
 rownames(reg_mse)[i+1] = as.character(i)
 }
> reg_mse


#baseline
#prediction1 mean(LOS), prediction2 median(LOS)
plos <- predict(reg_poi_all, type="response") #or reg_poi_all$fitted
p1 <- mean(plos)
p2 <- median(plos)
mse1 <- mean((reg_poi_all$y-p1)^2)
mse2 <- mean((reg_poi_all$y-p2)^2)
q1 <- quantile(plos, probs=c(.25, .75))
q2 <- quantile(plos, probs=c(.05, .95))


#segments
plos <- reg_poi_all$fitted
p1 <- data.frame(mean=mean(plos))
p2 <- data.frame(median=median(plos))
rownames(p1)[1] = "0"
rownames(p2)[1] = "0"
mse1 <- data.frame(mse_mean=mean((reg_poi_all$y-p1)^2))
mse2 <- data.frame(mse_media=mean((reg_poi_all$y-p2)^2))
rownames(mse1)[1] = "0"
rownames(mse2)[1] = "0"

q1 <- data.frame(t(quantile(plos, probs=c(0, .05, .25, .75, .95, 1))))
colnames(q1)[1]="0%"
colnames(q1)[2]="5%"
colnames(q1)[3]="25%"
colnames(q1)[4]="75%"
colnames(q1)[5]="95%"
colnames(q1)[6]="100%"
rownames(q1)[1] = "0"

for(i in 1:8){
plos <- reg_poi[[i]]$fitted #or reg_poi_all$fitted
p1 <- rbind(p1, mean(plos))
p2 <- rbind(p2, median(plos))
rownames(p1)[i+1] = as.character(i)
rownames(p2)[i+1] = as.character(i)
mse1 <- rbind(mse1, mean((reg_poi[[i]]$y-mean(plos))^2))
mse2 <- rbind(mse2, mean((reg_poi[[i]]$y-median(plos))^2))
rownames(mse1)[i+1] = as.character(i)
rownames(mse2)[i+1] = as.character(i)
q1 <- rbind(q1, t(quantile(plos, probs=c(0, .05, .25, .75, .95, 1))))
rownames(q1)[i+1] = as.character(i)
}

#prediction range
#no need to set up the new data option if it's the same as the training data

t1 <- qnorm(0.975)
t2 <- qnorm(0.75) 
#pr <- predict(reg_poi_all, type="response", se.fit=T) 
pr <- predict(reg_poi_all, link="log", se.fit=T) 
upr1 = pr$fit + t1*pr$se.fit
lwr1 = pr$fit - t1*pr$se.fit
upr2 = pr$fit + t2*pr$se.fit
lwr2 = pr$fit - t2*pr$se.fit

#coverage
b1 <- data.frame(los=data_los$Length_of_Stay, upper=upr1, lower=lwr1)
pct1 <- nrow(subset(b1, los<=upper & los>=lower))/nrow(data_los)*100
c1 <- data.frame(in_range1_pct=pct1)

b2 <- data.frame(los=data_los$Length_of_Stay, upper=upr2, lower=lwr2)
pct2 <- nrow(subset(b2, los<=upper & los>=lower))/nrow(data_los)*100
c2 <- data.frame(in_range2_pct=pct2)

#mean of width of prediction range
mw1 <- data.frame(mean_width1=mean(pr$se.fit)*2*t1)
mw2 <- data.frame(mean_width2=mean(pr$se.fit)*2*t2)

rownames(c1)[1] = "0"
rownames(c2)[1] = "0"
rownames(mw1)[1] = "0"
rownames(mw2)[1] = "0"

for(i in 1:8){

pr <- predict(reg_poi[[i]], link="log", se.fit=T) 
upr1 = pr$fit + t1*pr$se.fit
lwr1 = pr$fit - t1*pr$se.fit
upr2 = pr$fit + t2*pr$se.fit
lwr2 = pr$fit - t2*pr$se.fit

#coverage
b1 <- data.frame(los=vclust[[i]]$Length_of_Stay, upper=upr1, lower=lwr1)
pct1 <- nrow(subset(b1, los<=upper & los>=lower))/nrow(vclust[[i]])*100
c1 <- rbind(c1, pct1)


b2 <- data.frame(los=vclust[[i]]$Length_of_Stay, upper=upr2, lower=lwr2)
pct2 <- nrow(subset(b2, los<=upper & los>=lower))/nrow(vclust[[i]])*100
c2 <- rbind(c2, pct2)

rownames(c1)[i+1] = as.character(i)
rownames(c2)[i+1] = as.character(i)

#mean of width of prediction range
mw1 <- rbind(mw1, mean(pr$se.fit)*2*t1)
mw2 <- rbind(mw2, mean(pr$se.fit)*2*t2)
rownames(mw1)[i+1] = as.character(i)
rownames(mw2)[i+1] = as.character(i)

}

#compute the means across the segments
> mean(mse1[-1,])
[1] 2.00518
> mean(mse2[-1,])
[1] 2.005556


#mean, median, MSE of whole cohort and each segmentation

ms1 <- mean(data_los$Length_of_Stay)
ms2 <- median(data_los$Length_of_Stay)

me1 <- data.frame(mse_mean=mean((data_all$Length_of_Stay-ms1)^2))
me2 <- data.frame(mse_median=mean((data_all$Length_of_Stay-ms2)^2))
mlos <- data.frame(los_mean=ms1, los_median=ms2)
rownames(me1)[1] = "0"
rownames(me2)[1] = "0"
rownames(mlos)[1]="0"

t1 <- data.frame(t(quantile(data_los$Length_of_Stay, probs=c(0, .05, .25, .75, .95, 1))))
colnames(t1)[1]="0%"
colnames(t1)[2]="5%"
colnames(t1)[3]="25%"
colnames(t1)[4]="75%"
colnames(t1)[5]="95%"
colnames(t1)[6]="100%"
rownames(t1)[1] = "0"


for(i in 1:8){
ms1 <- mean(vclust[[i]]$Length_of_Stay)
ms2 <- median(vclust[[i]]$Length_of_Stay)
me1 <- rbind(me1, mean((vclust[[i]]$Length_of_Stay-ms1)^2))
me2 <- rbind(me2, mean((vclust[[i]]$Length_of_Stay-ms2)^2))
mlos <- rbind(mlos, c(ms1, ms2))
rownames(mlos)[i+1]= as.character(i)
rownames(me1)[i+1] = as.character(i)
rownames(me2)[i+1] = as.character(i)
t1 <- rbind(t1, t(quantile(vclust[[i]]$Length_of_Stay, probs=c(0, .05, .25, .75, .95, 1))))
rownames(t1)[i+1] = as.character(i)
}
 


#GLM binomial for revision
data_rev <- data_all[,-c(1,3,4)]

#remove missing
nrow(data_rev)
#sum(is.na(data_rev))
data_rev <- data_rev[complete.cases(data_rev),]
nrow(data_rev)

reg_rev = glm(revision ~ age + bmi_pre + factor(urgent_pre_90)
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=data_rev, family=binomial)

summary(reg_rev)


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

#predict the linear fitting
prob = predict(reg_rev, type="response")

aList = getROC_AUC(prob, data_rev$revision)
auc_rev = unlist(aList$auc)
auc_rev

#GLM binomial for complication
data_com <- data_all[,-c(1,2,4)]

#remove missing
nrow(data_com)
data_com <- data_com[complete.cases(data_com),]
nrow(data_com)

reg_com = glm(complication ~ age + bmi_pre + cci + urgent_pre_90
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=data_com, family=binomial)

summary(reg_com)

#predict the linear fitting
prob = predict(reg_com, type="response")

aList = getROC_AUC(prob, data_com$complication)
auc_com = unlist(aList$auc)
auc_com


#GLM binomial for post recovery
data_porec <- data_all[,-c(1:3)]

#remove missing
nrow(data_porec)
data_porec <- data_porec[complete.cases(data_porec),]
nrow(data_porec)

reg_com = glm(complication ~ age + bmi_pre + cci + urgent_pre_90
+ factor(antibiotics) + factor(gender) + factor(preop_prepare)
+ factor(ever_smoker)
+ factor(insurance_medicare) + factor(insurance_medicaid) + factor(insurance_private) + factor(insurance_other)
+ factor(race_AfricanAmerican) + factor(race_Asian) + factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_Group1_InfectiousParasitic)
+ factor(pre_comor_Group2_Neoplasm)
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Anemia)
+ factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous)
+ factor(pre_comor_Hypertension)
+ factor(pre_comor_Group8_Respiratory)
+ factor(pre_comor_Group9_Digestive)
+ factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Rheumatism),
data=data_com, family=binomial)

summary(reg_com)

#predict the linear fitting
prob = predict(reg_com, type="response")

aList = getROC_AUC(prob, data_com$complication)
auc_com = unlist(aList$auc)
auc_com


#run the wald test for the effects of comorbidities (significant)
library(aod)
> wald.test(b=coef(reg_rev), Sigma=vcov(reg_rev), Terms=24:34)
Wald test:
----------

Chi-squared test:
X2 = 30.6, df = 11, P(> X2) = 0.0013

#the effects of race (not significant)
> wald.test(b=coef(reg_rev), Sigma=vcov(reg_rev), Terms=20:23)
Wald test:
----------

Chi-squared test:
X2 = 6.9, df = 4, P(> X2) = 0.14





