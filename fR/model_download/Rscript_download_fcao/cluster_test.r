
#import libraries
library(cluster)
library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdk5-6.1.0-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "lingtao.cao", "Hau37Eq6")

#import data
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

rownames(mydata1) = mydata1$explorys_patient_id

#check quantile
myquantile = function(x){
  res = list()
  res$quantile = quantile(x, na.rm=TRUE, prob = c(seq(0, 0.1, 0.01), seq(0.1, 0.9, 0.1), seq(0.9, 1, 0.01)))
  res$mean = mean(x, na.rm=TRUE)
  res$sum = summary(x)
  return(res)
}


#select parameters
col.idx = c(1, 8, 9, 10, 11, 35, 36, 18:20, 21,24,26, 52, 65, 69, 72, 73, 76, 85, 87, 94, 101, 110, 114, 117:120, 121)

#sapply(mydata2, function(x) sum(is.na(x)))
#for matrix apply(is.na(mymatrix),2,sum) or colSums(is.na(mymatrix))
mydata2 = mydata1[,col.idx] # 0.602% missing values
rownames(mydata2) = mydata2$explorys_patient_id
mydata2 = mydata2[,-1]

#check the current parameters
colnames(mydata2)
# [1] "antibiotics"                          "gender"                               "age"                                 
# [4] "ortho_visit_pre60"                    "cci"                                  "preop_prepare"                       
# [7] "ever_smoker"                          "smode_pre_90"                         "alcohol_pre_90"                      
#[10] "bmi_pre"                              "insurance_medicare"                   "insurance_private"                   
#[13] "pre_comor_Group1_InfectiousParasitic" "pre_comor_Diabetes"                   "pre_comor_Obesity"                   
#[16] "pre_comor_Group3_Endocrine"           "pre_comor_Anemia"                     "pre_comor_Group5_MentalDisorder"     
#[19] "pre_comor_Group6_Nervous"             "pre_comor_Hypertension"               "pre_comor_Group7_Circularoty"        
#[22] "pre_comor_Group8_Respiratory"         "pre_comor_Kidney"                     "pre_comor_Group10_Genitourinary"     
#[25] "pre_comor_Arthropatheis"              "pre_comor_Dorsopathies"               "pre_comor_Rheumatism"                
#[28] "pre_comor_Osteopathies"               "pre_comor_Group13_Musculoskeletal"    "age_60"                              
#[31] "age_60_70"                            "age_70_80"                            "age_80"                              
#[34] "bmi_30"                               "bmi_30_35"                            "bmi_35_40"                           
#[37] "bmi_40"   

#factorize the gender
mydata2$gender = as.numeric(mydata2$gender == 2)

#create age groups
idx = which(mydata2$age < 60)
mydata2$age_60 = 0
mydata2$age_60[idx] = 1
idx = which(mydata2$age >= 60 & mydata2$age < 70)
mydata2$age_60_70 = 0
mydata2$age_60_70[idx] = 1
idx = which(mydata2$age >= 70 & mydata2$age < 80)
mydata2$age_70_80 = 0
mydata2$age_70_80[idx] = 1
idx = which(mydata2$age >= 80)
mydata2$age_80 = 0
mydata2$age_80[idx] = 1
idx = which(is.na(mydata2$age))
mydata2[idx, 30:33] = NA

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
idx = which(is.na(mydata2$bmi_pre))
mydata2[idx, 34:37] = NA

#check NA
any_na = function(x){
  if (any(is.na(x))) {return(TRUE)}
  else {return(FALSE)}
}

#prepare the clustering data 
#check and remove NA
row.del.idx = which(apply(mydata2, 1, any_na) > 0)
mydata5.nona = mydata2[-row.del.idx,c(34:37, 1, 4, 7,16:20, 22, 27)]
#normalize cci?
#mydata5.nona$cci = (mydata5.nona$cci - min(mydata5.nona$cci)) / (max(mydata5.nona$cci) - min(mydata5.nona$cci))

#adjust the bmi value
#BMI is inversely related to mortality in elderly subjects
#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2173925/
mydata5.nona[,1:4] = mydata5.nona[,1:4] / 4

colnames(mydata5.nona)
ncol(mydata5.nona)

#extract pre-op features used for clustering -
# "bmi_30"                          "bmi_30_35"                       "bmi_35_40"                       "bmi_40"                         
# "antibiotics"                     "ortho_visit_pre60"               "ever_smoker"                     "pre_comor_Group3_Endocrine"?
# "pre_comor_Anemia"                "pre_comor_Group5_MentalDisorder" "pre_comor_Group6_Nervous"        "pre_comor_Hypertension"         
# "pre_comor_Group8_Respiratory"    "pre_comor_Rheumatism"           
# grep("nfection", names(mydata1), value=T)
# "pre_comor_IntestinalInfection" "pre_comor_OtherInfection"     
# "insurance_medicare" "insurance_medicaid" "insurance_private"  "insurance_other"   
# "pre_comor_Diabetes"?

#check the correlation
mycor <- cor(mydata5.nona[,5:14])
library(lattice)
rgb.palette <- colorRampPalette(c("white","blue"), space="rgb")
levelplot(mycor, main="pre-op correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))

dfcor <- data.frame(mycor)
dfcor.sorted <- dfcor[order(dfcor$pre_comor_Group3_Endocrine, decreasing=T),]
rownames(dfcor.sorted)[2]


# apply kmeans without missing values
LK = 5  #lower bound of number of clusters
UK = 15 #upper bound of number of clusters
AA = 40 #iteration number
    

seed.list5 = NULL
mykm5.k = NULL
mykm5.centers = list()
mykm5.size = list()
mykm5.wss = NULL
mykm5.cluster = NULL
for (k in LK:UK)
{
  print(paste("===", k, "==="))
  wss = NULL
  for (aa in 1:AA){
    set.seed(aa)
    tmp = kmeans(mydata5.nona, centers = k)
    wss = c(wss, sum(tmp$withinss))
  }
  #plot(1:AA, wss, type = 'b', xlab = 'seed', ylab = 'Within Group Sum of Squares')

  seed.list5 = c(seed.list5, which(wss == min(wss)))
  set.seed(which(wss == min(wss)))
  mykm = kmeans(mydata5.nona, centers = k)
  mykm5.k = c(mykm5.k, k)
  mykm5.centers[[length(mykm5.centers) + 1]] = mykm$centers
  mykm5.size[[length(mykm5.size) + 1]] = mykm$size
  mykm5.cluster = cbind(mykm5.cluster, mykm$cluster - 1)
  mykm5.wss = c(mykm5.wss, sum(mykm$withinss))
}

#end of the clustering

#apply PCA
> col.idx = c(1, 8:11, 35:37, 18:21,24,26, 51:52, 65, 69, 72:73, 76, 85, 87, 94, 101, 110, 114, 117:121)
> rm(mydt)
> mydt = mydata1[,col.idx]
> rownames(mydt) = mydt$explorys_patient_id
> mydt = mydt[,-1]
> names(mydt)
 [1] "antibiotics"                          "gender"                               "age"                                 
 [4] "ortho_visit_pre60"                    "cci"                                  "preop_prepare"                       
 [7] "pre_comor_IntestinalInfection"        "ever_smoker"                          "smode_pre_90"                        
[10] "alcohol_pre_90"                       "bmi_pre"                              "insurance_medicare"                  
[13] "insurance_private"                    "pre_comor_OtherInfection"             "pre_comor_Group1_InfectiousParasitic"
[16] "pre_comor_Diabetes"                   "pre_comor_Obesity"                    "pre_comor_Group3_Endocrine"          
[19] "pre_comor_Anemia"                     "pre_comor_Group5_MentalDisorder"      "pre_comor_Group6_Nervous"            
[22] "pre_comor_Hypertension"               "pre_comor_Group7_Circularoty"         "pre_comor_Group8_Respiratory"        
[25] "pre_comor_Kidney"                     "pre_comor_Group10_Genitourinary"      "pre_comor_Arthropatheis"             
[28] "pre_comor_Dorsopathies"               "pre_comor_Rheumatism"                 "pre_comor_Osteopathies"              
[31] "pre_comor_Group13_Musculoskeletal"   

#head(mydt,n=10)

#>sum(is.na(mydt$age))
#[1] 10
#> sum(is.na(mydt$bmi))
#[1] 11697
#> length(mydt)
#[1] 31
#> nrow(mydt)
#[1] 62315
#> sum(is.na(mydt))
#[1] 11708

mydt2 = na.omit(mydt)
#> nrow(mydt2)
#[1] 50607
#> sum(is.na(mydt2))
#[1] 0

#level the age
mydt2["agegrp"] <- NA
mydt2$agegrp <- cut(mydt2$age, breaks=seq(10,90,by=10), labels=c(1:8))
mydt2$agegrp = as.numeric(mydt2$agegrp)

head(mydt2[,c("age","agegrp")], n=10)

#create bmi groups
idx = which(mydt2$bmi_pre < 30)
mydt2$bmi_30 = 0
mydt2$bmi_30[idx] = 1
idx = which(myt2$bmi_pre >= 30 & mydt2$bmi_pre < 35)
mydt2$bmi_30_35 = 0
mydt2$bmi_30_35[idx] = 1
idx = which(mydt2$bmi_pre >= 35 & mydt2$bmi_pre < 40)
mydt2$bmi_35_40 = 0
mydt2$bmi_35_40[idx] = 1
idx = which(mydt2$bmi_pre >= 40)
mydt2$bmi_40 = 0
mydt2$bmi_40[idx] = 1
mydt2[idx, 33:36] = NA


mydt5.nona <- mydt2[,c(33:36, 1, 8, 15:16, 18, 19:22, 24, 29)]
ncol(mydt5.nona)
.f = function(){
> names(mydt5.nona)
 [1] "bmi_30"                               "bmi_30_35"                            "bmi_35_40"                           
 [4] "bmi_40"                               "antibiotics"                          "ever_smoker"                         
 [7] "pre_comor_Group1_InfectiousParasitic" "pre_comor_Diabetes"                   "pre_comor_Group3_Endocrine"          
[10] "pre_comor_Anemia"                     "pre_comor_Group5_MentalDisorder"      "pre_comor_Group6_Nervous"            
[13] "pre_comor_Hypertension"               "pre_comor_Group8_Respiratory"         "pre_comor_Rheumatism"                
> 
}

> which(colnames(mydata5.nona)=="ortho_visit_pre60")
[1] 6
> sum(!complete.cases(myclust))
[1] 0
> sum(complete.cases(myclust))
[1] 50607

#adjust the weight for divided bmi subgroups
mydt5.nona[,1:4] = mydt5.nona[,1:4] / 4

#cor(mydata5.nona)


# sample matrix
set.seed(1)
mysample <- sample(length(mykm5.cluster), 10, replace=TRUE))
mykm5.cluster[mysample]

#sample df
mm1 <- mm[sample(nrow(mm),2000),]
plot(mm1[1:5], col=mm1[6])

> myclus <- data.frame(mykm5.cluster)
> class(myclus)
[1] "data.frame"
> nrow(myclus)
[1] 50607
> colnames(myclus)
 [1] "X1"  "X2"  "X3"  "X4"  "X5"  "X6"  "X7"  "X8"  "X9"  "X10" "X11"
> head(rownames(myclus), n=20)
 [1] "7225"   "9037"   "13980"  "18401"  "25506"  "28422"  "39004"  "41320"  "48004"  "54181"  "63162"  "65336"  "73991"  "75089" 
[15] "79112"  "85941"  "88435"  "94854"  "105924" "113837"
> myclust <- myclus$X1 #5 groups
> head(rownames(myclust), n=20)
NULL
> head(myclust, n=20)
 [1] 1 2 4 2 4 1 3 2 4 0 2 0 0 1 1 0 2 1 2 0
> rm(myclust)
> mm <- cbind(mdata, myclus$X1)
> colnames(mm)
[1] "antibiotics"                "ortho_visit_pre60"          "ever_smoker"                "pre_comor_Group3_Endocrine"
[5] "pre_comor_Anemia"           "myclus$X1"                 

> set.seed(1)
> mm1 <- mm[sample(nrow(mm),2000),]
> colnames(mm1)
[1] "antibiotics"                "ortho_visit_pre60"          "ever_smoker"                "pre_comor_Group3_Endocrine"
[5] "pre_comor_Anemia"           "myclus$X1"                 
> head(mm1[1:4], n=5)
         antibiotics ortho_visit_pre60 ever_smoker pre_comor_Group3_Endocrine
58186814           0                 1           0                          0
60585252           1                 1           0                          0
70717630           0                 1           1                          0
84432705           0                 1           0                          1
53282644           1                 1           0                          0
> head(mm1[5],n=5)
         pre_comor_Anemia
58186814                0
60585252                0
70717630                0
84432705                1
53282644                0
> head(mm1[6],n=5)
         myclus$X1
58186814         1
60585252         3
70717630         1
84432705         1
53282644         3
> plot(mm1[1:5], col=mm1[6])
There were 45 warnings (use warnings() to see them)
> warnings()
Warning messages:
1: In plot.xy(xy, type, ...) : supplied color is neither numeric nor character
2: In plot.xy(xy, type, ...) : supplied color is neither numeric nor character
3: In plot.xy(xy.coords(x, y), type = type, ...) :


mydata3.nona = mydata3[-row.del.idx,]

 plot(dn, horiz=F, nodePar=list(col=3:2,cex=c(2.0,0.75), 
 pch=21:22, bg=c("light blue","pink"), lab.cex=0.75,lab.col="tomato"),
 edgePar = list(col="gray",lvd=2),ylab="Height")

 > library()
> library(lattice)
> rgb.palette <- colorRampPalette(c("blue","red"), space="rgb")
> mycor <- cor(mydata5.nona)
> levelplot(mycor, main="pre-op correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))
> rgb.palette <- colorRampPalette(c("blue","yellow"), space="rgb")
> levelplot(mycor, main="pre-op correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))
> mycor <- cor(mydata5.nona[,-1:6])

#Error in .subset(x, j) : only 0's may be mixed with negative subscripts
> plot(mycl)
> save.image("H:\\My Documents\\workf\\R_code\\cluster_test")
install.packages(c("party"), repos = "http://repo.vdi.internal/r")
install.packages(c("randomForest"), repos = "http://repo.vdi.internal/r")
install.packages(c("caret"), repos = "http://repo.vdi.internal/r")
install.packages(c("e1071"), repos = "http://repo.vdi.internal/r")
install.packages(c("glmnet"), repos = "http://repo.vdi.internal/r")
install.packages(c("pROC"), repos = "http://repo.vdi.internal/r")
install.packages(c("ROCR"), repos = "http://repo.vdi.internal/r")
#install.packages(c("Deducer"), repos = "http://repo.vdi.internal/r")
install.packages(c("zoo"), repos = "http://repo.vdi.internal/r")
install.packages(c("ggplot2"), repos = "http://repo.vdi.internal/r")
install.packages(c("aod"), repos = "http://repo.vdi.internal/r")
install.packages(c("rms"), repos = "http://repo.vdi.internal/r")
install.packages(c("flexclust"), repos = "http://repo.vdi.internal/r")
#install.packages(c("ResourceSelection"), repos = "http://repo.vdi.internal/r")
install.packages(c("pscl"), repos = "http://repo.vdi.internal/r")
#install.packages(c("pheatmap"), repos = "http://repo.vdi.internal/r")
#install.packages(c("VGAM"), repos = "http://repo.vdi.internal/r")
#install.packages(c("msm"), repos = "http://repo.vdi.internal/r")
#install.packages(c("VGAM"), repos = "http://cran.r-project.org")
install.packages(c("plotrix"), repos = "http://repo.vdi.internal/r")

simpleCap <- function(x){
	s <- strsplit(x," ")[[1]]
	paste(toupper(substring(s,1,1)), substring(s,2), sep="", collapse=" ")
}

userinput1 <- function()
{
	uname <<- readline(prompt="Your last name? ")
	ugender <<- readline(prompt="And your gender(M/F)? ")
	utitle <<- ifelse(toupper(ugender)=="M", "Mr.", "Ms.")
	paste("Hello,", utitle, simpleCap(uname), sep=" ", collapse="\n")
}

print(userinput1())
userinput2 <- function()
{
	tt <- 1
	while(tt<2) {
		uage <<- readline(prompt="Your age? ")
		uconfirm <- readline(paste("You entered your age as ", uage, ", is this correct(Y/N)? ", sep=""))
		tt <- ifelse(toupper(uconfirm)=="Y", 2,1)
	}
	paste("Thank you for the input.", collapse="\n")
}
print(userinput2())
userfeed2 <- function(uage, ugender)
{
	tgender <- ifelse(toupper(ugender)=="M",1,2)
	tbmi <- ifelse(as.numeric(uage)<30, "bmi_30", "bmi_30_35")
	paste("The summary of your bmi group is", tbmi, collapse="\n")
	summary(mydt2[mydt2$gender==tgender,][, c(tbmi)])
}
print(userinput2())

userfeed2 <- function(uage, ugender)
{
	tgender <- ifelse(toupper(ugender)=="M",1,2)
	uage <- as.numeric(uage)
	tbmi <- ifelse(as.numeric(uage)<30, "bmi_30", "bmi_30_35")
	paste("Your BMI group is", tbmi, collapse="\n")
	paste("And the summary of this group is")
	summary(data_los[data_los$gender==tgender & data_los$age>=30 & data_los$age<35,][, c("bmi_pre")])
}
print(userfeed2(uage, ugender))


#glm - stats

data1 <- read.csv("V:\\data\\LOS_proc_variables.csv")
#colnames(data1)
#head(data1)
#summary(data1)
#sapply(data1, class)
data1 <- data1[data1$Length_of_Stay > 1,]
data1$revision <- ifelse((data1$revision90==1 | data1$revision_18m==1), 1, 0)

num_no_revision <- length(which(data1$revision == 0))
num_revision <- length(which(data1$revision == 1))

num_no_revision
num_revision

# select only columns for LR
data_lr <- data1[, c("revision", "gender", "age", "preop_prepare", 
"ortho_visit_pre60", "urgent_pre_90", "ever_smoker", 
"smode_pre_90", "alcohol_pre_90", "insurance_medicare", 
"insurance_medicaid", "insurance_private", "insurance_other", "race_Caucasian",
"race_AfricanAmerican", "race_Asian", "race_Latino", "race_Native", 
"pre_comor_IntestinalInfection",
"pre_comor_Tuberculosis",
"pre_comor_Zoonotic",      
"pre_comor_Bacterial",          
"pre_comor_HIV",         
"pre_comor_poliomyelitis", 
"pre_comor_exanthem",     
"pre_comor_ArthropodViral", 
"pre_comor_Chlamydiae",
"pre_comor_Rickettsioses", 
"pre_comor_Syphilis",     
"pre_comor_Spirochetal", 
"pre_comor_mycoses",       
"pre_comor_helminthiases", 
"pre_comor_OtherInfection", 
"pre_comor_Group2_Neoplasm",   
"pre_comor_Group3_Endocrine", 
"pre_comor_Group5_MentalDisorder", 
"pre_comor_Group6_Nervous", 
"pre_comor_Group7_Circularoty", 
"pre_comor_Group8_Respiratory", 
"pre_comor_Group9_Digestive", 
"pre_comor_Group10_Genitourinary", 
"pre_comor_Group13_Musculoskeletal")]

# remove rows with missing value
data_comp <- data_lr[complete.cases(data_lr),]

# run logistic regression on revision within 18 months
reg_group = glm(revision ~ factor(gender) + age + factor(preop_prepare) + 
				    factor(ortho_visit_pre60) + factor(urgent_pre_90) +
				    factor(ever_smoker) + factor(smode_pre_90) + factor(alcohol_pre_90) + 
				    factor(insurance_medicare) + factor(insurance_medicaid) +
				    factor(insurance_private) + factor(insurance_other) + 
				    factor(race_Caucasian) + factor(race_AfricanAmerican) + factor(race_Asian) + 
				    factor(race_Latino) + factor(race_Native)
+ factor(pre_comor_IntestinalInfection)
+ factor(pre_comor_Tuberculosis)       
+ factor(pre_comor_Zoonotic)           
+ factor(pre_comor_Bacterial)          
+ factor(pre_comor_HIV)                
+ factor(pre_comor_poliomyelitis)      
+ factor(pre_comor_exanthem)           
+ factor(pre_comor_ArthropodViral)     
+ factor(pre_comor_Chlamydiae)         
+ factor(pre_comor_Rickettsioses)      
+ factor(pre_comor_Syphilis)           
+ factor(pre_comor_Spirochetal)        
+ factor(pre_comor_mycoses)            
+ factor(pre_comor_helminthiases)      
+ factor(pre_comor_OtherInfection) 
+ factor(pre_comor_Group2_Neoplasm)   
+ factor(pre_comor_Group3_Endocrine)
+ factor(pre_comor_Group5_MentalDisorder)     
+ factor(pre_comor_Group6_Nervous) 
+ factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory) 
+ factor(pre_comor_Group9_Digestive)   
+ factor(pre_comor_Group10_Genitourinary)     
+ factor(pre_comor_Group13_Musculoskeletal), 
		family=binomial, data=data_comp)

summary(reg_group)
prob = predict(reg_group, type=c("response"))

# function for AUC
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

aList = getROC_AUC(prob, data_comp$revision)
auc = unlist(aList$auc)
auc


#glmnet
library(glmnet)

library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdk5-6.1.0-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "lingtao.cao", "Hau37Eq6")

#import data
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

#rownames(mydata1) = mydata1$explorys_patient_id

mydata1 <- mydata1[mydata1$Length_of_Stay > 1,]


mydata1$revision = as.numeric(apply(mydata1[,c("revision90","revision_18m")], 1, sum) > 0)
mydata1$complication = as.numeric(apply(mydata1[,c("complication_3m","complication_12m")], 1, sum) > 0)

num_no_revision <- length(which(mydata1$revision == 0))
num_revision <- length(which(mydata1$revision == 1))
> num_no_revision
[1] 57961
> num_revision
[1] 964

num_no_complication <- length(which(mydata1$complication == 0))
num_complication <- length(which(mydata1$complication == 1))
> num_no_complication
[1] 55573
> num_complication
[1] 3352

# pre-op features
# [1] "antibiotics"                          "gender"                               "age"                                 
# [4] "ortho_visit_pre60"                    "cci"                                  "preop_prepare"                       
# [7] "ever_smoker"                          "smode_pre_90"                         "alcohol_pre_90"                      
#[10] "bmi_pre"                              "insurance_medicare"                   "insurance_private"                   
#[13] "pre_comor_Group1_InfectiousParasitic" "pre_comor_Diabetes"                   "pre_comor_Obesity"                   
#[16] "pre_comor_Group3_Endocrine"           "pre_comor_Anemia"                     "pre_comor_Group5_MentalDisorder"     
#[19] "pre_comor_Group6_Nervous"             "pre_comor_Hypertension"               "pre_comor_Group7_Circularoty"        
#[22] "pre_comor_Group8_Respiratory"         "pre_comor_Kidney"                     "pre_comor_Group10_Genitourinary"     
#[25] "pre_comor_Arthropatheis"              "pre_comor_Dorsopathies"               "pre_comor_Rheumatism"                
#[28] "pre_comor_Osteopathies"               "pre_comor_Group13_Musculoskeletal"    "age_60"                              
#[31] "age_60_70"                            "age_70_80"                            "age_80"                              
#[34] "bmi_30"                               "bmi_30_35"                            "bmi_35_40"                           
#[37] "bmi_40"   

# select only columns for LR
data_net <- mydata1[, c("revision", "age", "bmi_pre", "antibiotics", "gender", 
"ortho_visit_pre60", "cci", "preop_prepare",                       
"ever_smoker", "smode_pre_90", "alcohol_pre_90",                      
"insurance_medicare", "insurance_private",                    
"pre_comor_Group1_InfectiousParasitic", "pre_comor_Diabetes", "pre_comor_Obesity",
"pre_comor_Group3_Endocrine", "pre_comor_Anemia", "pre_comor_Group5_MentalDisorder",
"pre_comor_Group6_Nervous", "pre_comor_Hypertension", "pre_comor_Group7_Circularoty",        
"pre_comor_Group8_Respiratory", "pre_comor_Kidney", "pre_comor_Group10_Genitourinary",     
"pre_comor_Arthropatheis", "pre_comor_Dorsopathies", "pre_comor_Rheumatism",                
"pre_comor_Osteopathies", "pre_comor_Group13_Musculoskeletal")]

# remove rows with missing value
> names(data_net)
 [1] "revision"                             "age"                                  "bmi_pre"                             
 [4] "antibiotics"                          "gender"                               "ortho_visit_pre60"                   
 [7] "cci"                                  "preop_prepare"                        "ever_smoker"                         
[10] "smode_pre_90"                         "alcohol_pre_90"                       "insurance_medicare"                  
[13] "insurance_private"                    "pre_comor_Group1_InfectiousParasitic" "pre_comor_Diabetes"                  
[16] "pre_comor_Obesity"                    "pre_comor_Group3_Endocrine"           "pre_comor_Anemia"                    
[19] "pre_comor_Group5_MentalDisorder"      "pre_comor_Group6_Nervous"             "pre_comor_Hypertension"              
[22] "pre_comor_Group7_Circularoty"         "pre_comor_Group8_Respiratory"         "pre_comor_Kidney"                    
[25] "pre_comor_Group10_Genitourinary"      "pre_comor_Arthropatheis"              "pre_comor_Dorsopathies"              
[28] "pre_comor_Rheumatism"                 "pre_comor_Osteopathies"               "pre_comor_Group13_Musculoskeletal"   


xfactors <- model.matrix(revision ~ age + bmi_pre + factor(antibiotics)+ factor(gender) 
+ factor(ortho_visit_pre60) + factor(cci) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90) + factor(alcohol_pre_90) 
+ factor(insurance_medicare) + factor(insurance_private)
+ factor(pre_comor_Group1_InfectiousParasitic) + factor(pre_comor_Diabetes) + factor(pre_comor_Obesity)
+ factor(pre_comor_Group3_Endocrine) + factor(pre_comor_Anemia) + factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous) + factor(pre_comor_Hypertension) + factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory) + factor(pre_comor_Kidney) + factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Arthropatheis) + factor(pre_comor_Dorsopathies) + factor(pre_comor_Rheumatism)
+ factor(pre_comor_Osteopathies) + factor(pre_comor_Group13_Musculoskeletal),
data=data_net)



yrev <- as.matrix(data_comp$revision, ncol=1)
reg_net <- glmnet(x=xfactors,y=yrev, alpha=1, family="binomial")

reg_net2 <- glmnet(x=xfactors,y=yrev, alpha=1, family="binomial", lambda=0)

> cv.glmmod <- cv.glmnet(xfactors, yrev, family="binomial")
> plot(cv.glmmod)
> best_lambda <- cv.glmmod$lambda.min
> best_lambda
[1] 0.0002843263
> log(best_lambda)
[1] -8.165388

prob_net = predict.glmnet(reg_net, xfactors, s=best_lambda, type=c("response"))
cv_net <- cv.glmnet(xfactors, yrev, family="binomial", type.measure="auc", nfold=5, alpha=1)



xfactors <- model.matrix(revision ~ factor(antibiotics)+ age + factor(gender) 
+ factor(ortho_visit_pre60) + factor(cci) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90) + factor(alcohol_pre_90) 
+ factor(insurance_medicare) + factor(insurance_private)
+ factor(pre_comor_Group1_InfectiousParasitic) + factor(pre_comor_Diabetes) + factor(pre_comor_Obesity)
+ factor(pre_comor_Group3_Endocrine) + factor(pre_comor_Anemia) + factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous) + factor(pre_comor_Hypertension) + factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory) + factor(pre_comor_Kidney) + factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Arthropatheis) + factor(pre_comor_Dorsopathies) + factor(pre_comor_Rheumatism)
+ factor(pre_comor_Osteopathies) + factor(pre_comor_Group13_Musculoskeletal),
data=data_net)

yrev <- as.matrix(data_net$revision, ncol=1)
net1 <- glmnet(x=xfactors,y=yrev, alpha=1, family="binomial")
> netcv1 <- cv.glmnet(x=xfactors,y=yrev, alpha=1, family="binomial")
> netcv1$lambda.min
[1] 0.0003267306
> print(coef(netcv1, s=netcv1$lambda.min))
45 x 1 sparse Matrix of class "dgCMatrix"
                                                        1
(Intercept)                                   -2.90163625
(Intercept)                                    .         
factor(antibiotics)1                          -0.26396350
age                                           -0.01917182
factor(gender)2                               -0.32011232
factor(ortho_visit_pre60)1                    -0.08479309
factor(cci)1                                   0.23659585
factor(cci)2                                   0.08058388
factor(cci)3                                   .         
factor(cci)4                                   .         
factor(cci)5                                   .         
factor(cci)6                                   .         
factor(cci)7                                   0.05851686
factor(cci)8                                   0.44397342
factor(cci)9                                  -0.06423889
factor(cci)10                                  0.33846016
factor(cci)11                                  0.82711127
factor(cci)12                                  0.27709800
factor(cci)13                                 -0.40286675
factor(cci)14                                  .         
factor(cci)15                                  1.40708967
factor(cci)16                                  .         
factor(preop_prepare)1                         0.05346585
factor(ever_smoker)1                           0.05803826
factor(smode_pre_90)1                          0.12467674
factor(alcohol_pre_90)1                       -0.12690181
factor(insurance_medicare)1                    0.28440384
factor(insurance_private)1                     0.06477565
factor(pre_comor_Group1_InfectiousParasitic)1  0.20335977
factor(pre_comor_Diabetes)1                    0.12267969
factor(pre_comor_Obesity)1                     0.17543133
factor(pre_comor_Group3_Endocrine)1           -0.10191777
factor(pre_comor_Anemia)1                      0.28329463
factor(pre_comor_Group5_MentalDisorder)1       0.19734284
factor(pre_comor_Group6_Nervous)1              0.00246596
factor(pre_comor_Hypertension)1                .         
factor(pre_comor_Group7_Circularoty)1         -0.03576167
factor(pre_comor_Group8_Respiratory)1          .         
factor(pre_comor_Kidney)1                      .         
factor(pre_comor_Group10_Genitourinary)1       .         
factor(pre_comor_Arthropatheis)1               .         
factor(pre_comor_Dorsopathies)1                0.14432224
factor(pre_comor_Rheumatism)1                  0.01246087
factor(pre_comor_Osteopathies)1                .         
factor(pre_comor_Group13_Musculoskeletal)1     .         
> 



net1 <- glmnet(x=xfactors,y=yrev, alpha=1, family="binomial", standardize=FALSE)
netcv1 <- cv.glmnet(x=xfactors,y=yrev, alpha=1, family="binomial", standardize=FALSE)

# run logistic regression on revision within 18 months
reg_grp1 = glm(revision ~ factor(antibiotics)+ age + bmi_pre + factor(gender) 
+ factor(ortho_visit_pre60) + factor(cci) + factor(preop_prepare)
+ factor(ever_smoker) + factor(smode_pre_90) + factor(alcohol_pre_90) 
+ factor(insurance_medicare) + factor(insurance_private)
+ factor(pre_comor_Group1_InfectiousParasitic) + factor(pre_comor_Diabetes) + factor(pre_comor_Obesity)
+ factor(pre_comor_Group3_Endocrine) + factor(pre_comor_Anemia) + factor(pre_comor_Group5_MentalDisorder)
+ factor(pre_comor_Group6_Nervous) + factor(pre_comor_Hypertension) + factor(pre_comor_Group7_Circularoty)
+ factor(pre_comor_Group8_Respiratory) + factor(pre_comor_Kidney) + factor(pre_comor_Group10_Genitourinary)
+ factor(pre_comor_Arthropatheis) + factor(pre_comor_Dorsopathies) + factor(pre_comor_Rheumatism)
+ factor(pre_comor_Osteopathies) + factor(pre_comor_Group13_Musculoskeletal),
data=data_net, family=binomial)

summary(reg_grp1)

Call:
glm(formula = revision ~ factor(antibiotics) + age + bmi_pre + 
    factor(gender) + factor(ortho_visit_pre60) + factor(cci) + 
    factor(preop_prepare) + factor(ever_smoker) + factor(smode_pre_90) + 
    factor(alcohol_pre_90) + factor(insurance_medicare) + factor(insurance_private) + 
    factor(pre_comor_Group1_InfectiousParasitic) + factor(pre_comor_Diabetes) + 
    factor(pre_comor_Obesity) + factor(pre_comor_Group3_Endocrine) + 
    factor(pre_comor_Anemia) + factor(pre_comor_Group5_MentalDisorder) + 
    factor(pre_comor_Group6_Nervous) + factor(pre_comor_Hypertension) + 
    factor(pre_comor_Group7_Circularoty) + factor(pre_comor_Group8_Respiratory) + 
    factor(pre_comor_Kidney) + factor(pre_comor_Group10_Genitourinary) + 
    factor(pre_comor_Arthropatheis) + factor(pre_comor_Dorsopathies) + 
    factor(pre_comor_Rheumatism) + factor(pre_comor_Osteopathies) + 
    factor(pre_comor_Group13_Musculoskeletal), family = binomial, 
    data = data_net)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6731  -0.1964  -0.1675  -0.1435   3.2570  

Coefficients:
                                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                                    -3.720675   0.434047  -8.572  < 2e-16 ***
factor(antibiotics)1                           -0.334146   0.091280  -3.661 0.000252 ***
age                                            -0.029849   0.006943  -4.299 1.72e-05 ***
bmi_pre                                         0.015764   0.005169   3.050 0.002289 ** 
factor(gender)2                                -0.395933   0.075996  -5.210 1.89e-07 ***
factor(ortho_visit_pre60)1                     -0.126069   0.086300  -1.461 0.144066    
factor(cci)1                                    1.030668   0.350961   2.937 0.003317 ** 
factor(cci)2                                    0.977395   0.362737   2.695 0.007049 ** 
factor(cci)3                                    0.963741   0.391090   2.464 0.013730 *  
factor(cci)4                                    1.055168   0.429380   2.457 0.013994 *  
factor(cci)5                                    1.062083   0.451720   2.351 0.018713 *  
factor(cci)6                                    1.049019   0.477652   2.196 0.028078 *  
factor(cci)7                                    1.330366   0.508218   2.618 0.008852 ** 
factor(cci)8                                    1.762684   0.544521   3.237 0.001207 ** 
factor(cci)9                                    0.871155   0.699896   1.245 0.213245    
factor(cci)10                                   1.724040   0.661290   2.607 0.009132 ** 
factor(cci)11                                   2.149271   0.696649   3.085 0.002034 ** 
factor(cci)12                                   1.778959   0.857858   2.074 0.038105 *  
factor(cci)13                                  -9.502639 168.184613  -0.057 0.954943    
factor(cci)14                                  -9.270212 252.229601  -0.037 0.970682    
factor(cci)15                                   2.888944   1.205838   2.396 0.016584 *  
factor(cci)16                                  -9.323145 611.112825  -0.015 0.987828    
factor(preop_prepare)1                          0.112761   0.097755   1.154 0.248703    
factor(ever_smoker)1                            0.082376   0.109566   0.752 0.452150    
factor(smode_pre_90)1                           0.168263   0.156065   1.078 0.280961    
factor(alcohol_pre_90)1                        -0.234143   0.144269  -1.623 0.104600    
factor(insurance_medicare)1                     0.412252   0.094241   4.374 1.22e-05 ***
factor(insurance_private)1                      0.148881   0.084863   1.754 0.079366 .  
factor(pre_comor_Group1_InfectiousParasitic)1   0.238611   0.128908   1.851 0.064166 .  
factor(pre_comor_Diabetes)1                     0.082968   0.122914   0.675 0.499671    
factor(pre_comor_Obesity)1                      0.133778   0.116019   1.153 0.248881    
factor(pre_comor_Group3_Endocrine)1            -0.214304   0.105767  -2.026 0.042746 *  
factor(pre_comor_Anemia)1                       0.353632   0.105987   3.337 0.000848 ***
factor(pre_comor_Group5_MentalDisorder)1        0.221730   0.114243   1.941 0.052275 .  
factor(pre_comor_Group6_Nervous)1               0.042501   0.101155   0.420 0.674372    
factor(pre_comor_Hypertension)1                 0.024228   0.146867   0.165 0.868974    
factor(pre_comor_Group7_Circularoty)1          -0.137367   0.149222  -0.921 0.357283    
factor(pre_comor_Group8_Respiratory)1          -0.086284   0.098222  -0.878 0.379696    
factor(pre_comor_Kidney)1                      -0.223956   0.197024  -1.137 0.255664    
factor(pre_comor_Group10_Genitourinary)1        0.034710   0.099656   0.348 0.727616    
factor(pre_comor_Arthropatheis)1               -0.064419   0.210611  -0.306 0.759705    
factor(pre_comor_Dorsopathies)1                 0.166524   0.102242   1.629 0.103372    
factor(pre_comor_Rheumatism)1                   0.036610   0.095621   0.383 0.701819    
factor(pre_comor_Osteopathies)1                -0.034450   0.117458  -0.293 0.769293    
factor(pre_comor_Group13_Musculoskeletal)1      0.103719   0.228184   0.455 0.649438    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 7852.9  on 47564  degrees of freedom
Residual deviance: 7658.4  on 47520  degrees of freedom
AIC: 7748.4

Number of Fisher Scoring iterations: 13



confint(reg_grp1)
> confint(reg_grp1)
Waiting for profiling to be done...
                                                      2.5 %        97.5 %
(Intercept)                                   -4.607252e+00 -2.898718e+00
factor(antibiotics)1                          -5.139009e-01 -1.559205e-01
age                                           -4.338715e-02 -1.616910e-02
bmi_pre                                        5.527549e-03  2.578700e-02
factor(gender)2                               -5.447793e-01 -2.467696e-01
factor(ortho_visit_pre60)1                    -2.939860e-01  4.445003e-02
factor(cci)1                                   3.793937e-01  1.766980e+00
factor(cci)2                                   3.024514e-01  1.734958e+00
factor(cci)3                                   2.281045e-01  1.770667e+00
factor(cci)4                                   2.379768e-01  1.929511e+00
factor(cci)5                                   1.965716e-01  1.975424e+00
factor(cci)6                                   1.287661e-01  2.008823e+00
factor(cci)7                                   3.449364e-01  2.344627e+00
factor(cci)8                                   6.981407e-01  2.840960e+00
factor(cci)9                                  -6.008989e-01  2.188776e+00
factor(cci)10                                  3.598982e-01  2.983618e+00
factor(cci)11                                  6.823991e-01  3.460007e+00
factor(cci)12                                 -2.055472e-01  3.306292e+00
factor(cci)13                                 -1.342877e+02 -1.277330e+02
factor(cci)14                                 -2.052806e+02 -1.917685e+02
factor(cci)15                                 -1.840238e-01  4.971951e+00
factor(cci)16                                            NA  8.627482e+01
factor(preop_prepare)1                        -7.720155e-02  3.062160e-01
factor(ever_smoker)1                          -1.364176e-01  2.934236e-01
factor(smode_pre_90)1                         -1.409084e-01  4.717903e-01
factor(alcohol_pre_90)1                       -5.275881e-01  3.900781e-02
factor(insurance_medicare)1                    2.278280e-01  5.972999e-01
factor(insurance_private)1                    -1.730046e-02  3.154305e-01
factor(pre_comor_Group1_InfectiousParasitic)1 -1.982955e-02  4.860162e-01
factor(pre_comor_Diabetes)1                   -1.589654e-01  3.231357e-01
factor(pre_comor_Obesity)1                    -9.573471e-02  3.593650e-01
factor(pre_comor_Group3_Endocrine)1           -4.224875e-01 -7.790404e-03
factor(pre_comor_Anemia)1                      1.430127e-01  5.587239e-01
factor(pre_comor_Group5_MentalDisorder)1      -5.578254e-03  4.425446e-01
factor(pre_comor_Group6_Nervous)1             -1.576677e-01  2.389973e-01
factor(pre_comor_Hypertension)1               -2.555506e-01  3.211871e-01
factor(pre_comor_Group7_Circularoty)1         -4.387225e-01  1.472132e-01
factor(pre_comor_Group8_Respiratory)1         -2.806660e-01  1.045137e-01
factor(pre_comor_Kidney)1                     -6.177177e-01  1.557439e-01
factor(pre_comor_Group10_Genitourinary)1      -1.630455e-01  2.278162e-01
factor(pre_comor_Arthropatheis)1              -4.558231e-01  3.732394e-01
factor(pre_comor_Dorsopathies)1               -3.588508e-02  3.650872e-01
factor(pre_comor_Rheumatism)1                 -1.523879e-01  2.226017e-01
factor(pre_comor_Osteopathies)1               -2.697071e-01  1.911824e-01
factor(pre_comor_Group13_Musculoskeletal)1    -3.644816e-01  5.335290e-01

prob = predict(reg_grp1, type=c("response"))

#correlation
library(lattice)
mycor <- cor(data_net)
levelplot(mycor, main="pre-op correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))
rgb.palette <- colorRampPalette(c("blue","yellow"), space="rgb")

aa <- as.list(data.frame(mycor))
bb <- data.frame(mycor)
cc <- bb[order(bb$pre_comor_Diabetes, decreasing=T),]
rownames(cc)[2]

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

aList = getROC_AUC(prob, data_net$revision)
auc = unlist(aList$auc)
auc

> .libPaths()
[1] "\\\\winfs-vdi-1.vdi.internal/home_folders/lingtao.cao/My Documents/R/win-library/3.2"
[2] "C:/Program Files/R/R-3.2.0/library"                                                  
> remove.packages("ggplot2", "\\\\winfs-vdi-1.vdi.internal/home_folders/lingtao.cao/My Documents/R/win-library/3.2")

