library(RJDBC)
smDriver = JDBC(driverClass = "com.vertica.jdbc.Driver", classPath = "C:\\Program Files\\Vertica Systems\\JDBC\\vertica-jdk5-6.1.0-0.jar")
sm = dbConnect(smDriver, "jdbc:vertica://vertica-als-1:5433/db1", "zhaonan.sun", "sT9o6n8x")
setwd("H:/")


myquantile = function(x){
  res = list()
  res$quantile = quantile(x, na.rm=TRUE, prob = c(seq(0, 0.1, 0.01), seq(0.1, 0.9, 0.1), seq(0.9, 1, 0.01)))
  res$mean = mean(x, na.rm=TRUE)
  res$sum = summary(x)
  return(res)
}

any_na = function(x){
  if (any(is.na(x))) {return(TRUE)}
  else {return(FALSE)}
}


preop = read.csv("LOS_proc_preop.csv", header=TRUE)
postop = read.csv("LOS_proc_postop.csv", header=TRUE)
preop.comor = read.csv("LOS_proc_preop_comorbidity_icd9.csv", header=TRUE)
mydata = read.csv("LOS_proc_variables.csv", header=TRUE)
mydata1 = read.csv("LOS_proc_variables_LOS1.csv", header=TRUE)


post_mua_patient = dbGetQuery(sm, "
select distinct a.explorys_patient_id from supermart_111.v_procedure a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.proc_date > b.proc_date
and a.cpt_concept = '27570'
and b.Length_of_Stay > 1
")


mydata1$mua = as.numeric(mydata1$explorys_patient_id %in% post_mua_patient$explorys_patient_id)


mydata1$preop_prepare = as.numeric(apply(mydata1[,c(8,12:15)], 1, sum) > 0)

mydata1.bmi = mydata1[(!is.na(mydata1$bmi_change_rate)),]

rownames(mydata1) = mydata1$explorys_patient_id
########################## clustering based on outcomes #########################


mydata3 = mydata1[,c(1,4, 136:142, 145, 146:155, 159)]
rownames(mydata3) = mydata3$explorys_patient_id
mydata3 = mydata3[,(2:ncol(mydata3))]


mydata3$Length_of_Stay = as.numeric(mydata3$Length_of_Stay > 4)

mydata3$Ortho_visit_90_360 = mydata3$Ortho_visit_90_360 / 20
mydata3$Post_num_surgery = mydata3$Post_num_surgery
mydata3$urgent_post_360 = as.numeric(mydata3$urgent_post_360 > 0)
mydata3$discharge_hospital = as.numeric(mydata3$discharge_hospital == 1)
mydata3$post_recovery = as.numeric(apply(mydata3[,c(2,4,5, 20)], 1, sum) > 0)
mydata3$revision = as.numeric(apply(mydata3[,c(6,7)], 1, sum) > 0)
mydata3$complication = as.numeric(apply(mydata3[,c(15,18)], 1, sum) > 0)
mydata3$bmi_change_rate = as.numeric(mydata3$bmi_change_rate <= 0)
#mydata3 = mydata3[,c(1, 14, 21:23)]
mydata3 = mydata3[,c(1, 23, 22, 14, 21)]


wss = NULL
for (aa in 0:40){
  set.seed(aa)
  tmp = kmeans(mydata3, centers = 6)
  wss = c(wss, sum(tmp$withinss))
}
plot(0:40, wss, type = 'b', xlab = 'seed', ylab = 'Within Group Sum of Squares')

which(wss == min(wss))
aa = 25

set.seed(aa)
postkm_5 = kmeans(mydata3, centers = 6)  


wss = NULL
for (aa in 1:40){
  set.seed(aa)
  tmp = kmeans(mydata3[,c(1,2,4,5)], centers = 6)
  wss = c(wss, sum(tmp$withinss))
}
plot(1:40, wss, type = 'b', xlab = 'seed', ylab = 'Within Group Sum of Squares')


aa = which(wss == min(wss))

set.seed(aa)
postkm_4 = kmeans(mydata3[,c(1,2,4,5)], centers = 6)
write.csv(postkm_4$centers, 'postkm_4_centers_6_58k.csv', row.names=FALSE)
write.csv(postkm_4$size, 'postkm_4_size_6_58k.csv', row.names=FALSE)
write.csv(postkm_5$centers, 'postkm_5_centers_6_58k.csv', row.names=FALSE)
write.csv(postkm_5$size, 'postkm_5_size_6_58k.csv', row.names=FALSE)



#mydata3$cluster = factor(postkm$cluster - 1)


########################## clustering based on pre-op features #########################
library(cluster)

col.idx = c(1, 8, 9, 10, 11, 35, 36, 18:20, 21,24,26, 52, 65, 69, 72, 73, 76, 85, 87, 94, 101, 110, 114, 117:120, 121)


mydata2 = mydata1[,col.idx] # 0.602% missing values
rownames(mydata2) = mydata2$explorys_patient_id
mydata2 = mydata2[,-1]

colnames(mydata2)

mydata2$gender = as.numeric(mydata2$gender == 2)

# age group
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
mydata2[idx, 28:31] = NA

# bmi group
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
mydata2[idx, 32:35] = NA



row.del.idx = which(apply(mydata2, 1, any_na) > 0)


mydata5.nona = mydata2[-row.del.idx,c(34:37, 1, 4, 7,16:20, 22, 27)]
#mydata5.nona$cci = (mydata5.nona$cci - min(mydata5.nona$cci)) / (max(mydata5.nona$cci) - min(mydata5.nona$cci))
mydata5.nona[,1:4] = mydata5.nona[,1:4] / 4
colnames(mydata5.nona)
ncol(mydata5.nona)

#cor(mydata5.nona)

mydata3.nona = mydata3[-row.del.idx,]


# kmeans without missing values
LK = 5
UK = 15

AA = 40



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

colnames(mykm5.cluster) = paste("seg", LK:UK, sep = "_")
mykm5.cluster = as.data.frame(mykm5.cluster)


#plot(LK:UK, mykm5.wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within Groups sum of Squares')


for (i in 1:length(mykm5.k))
{
  fname = paste("preop_kmeans_", ncol(mydata5.nona), "_centers_", mykm5.k[i], "_binary_48K.csv", sep="")
  write.csv(mykm5.centers[[i]], fname, row.names=FALSE)
  fname = paste("preop_kmeans_", ncol(mydata5.nona), "_size_", mykm5.k[i], "_binary_48K.csv", sep="")
  write.csv(mykm5.size[[i]], fname, row.names=FALSE)
}

ncol(mydata5.nona)


############### Connect preop and postop clusters ######################

### description of preop cluster with outcome measures

k = 10

full.proportion = apply(postop.data[,1:5], 2, mean)

idx = which(colnames(mykm5.cluster) == paste("seg", k, sep="_"))


tmp5 = cbind(mykm5.cluster[,idx], postop.data[,1:5])
tmp5.proportion = NULL
for (i in min(mykm5.cluster[,idx]):max(mykm5.cluster[,idx])){
  tmp5.proportion = rbind(tmp5.proportion, apply(tmp5[(tmp5[,1] == i), -1], 2, mean) / full.proportion)
}
rownames(tmp5.proportion) = min(mykm5.cluster[,idx]):max(mykm5.cluster[,idx])

round(tmp5.proportion, 3)









##################################3
#Orthopedic frequency

provider_frequency = dbGetQuery(sm, "
select provider_id_hash, count(provider_id_hash) as freq from
(
select distinct a.explorys_patient_id, a.specialty_name_1, a.provider_id_hash from supermart_111.v_encounter_provider a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.encounter_join_id = b.encounter_join_id
and b.Length_of_Stay > 1
and a.specialty_name_1 = 'Orthopaedics'
) c
group by provider_id_hash
order by freq desc
")


provider = dbGetQuery(sm, "
select distinct a.explorys_patient_id, a.specialty_name_1, a.provider_id_hash from supermart_111.v_encounter_provider a, sandbox_jj.LOS_proc_zsun b
where a.explorys_patient_id = b.explorys_patient_id
and a.encounter_join_id = b.encounter_join_id
and b.Length_of_Stay > 1
and a.specialty_name_1 = 'Orthopaedics'
")





####################### 2015-10-17 ############################

par(mfrow = c(2,5))

for(i in 0:(k-1)){
  tmp.idx = as.numeric(as.character(rownames(mykm5.cluster)[mykm5.cluster$seg_10 == i]))
  tmp.los = postop$Length_of_Stay[postop$explorys_patient_id %in% tmp.idx]
  hist(tmp.los, xlab = "Length of Stay", ylab = "Freq", main = paste("Segment", i+1, sep=" "))
  print(paste("segment ", i+1, sep=""))
  print(quantile(tmp.los, prob = c(0, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 1)))
}


#par(mfrow = c(2,5))

for(i in 0:(k-1)){
  tmp.idx = as.numeric(as.character(rownames(mykm5.cluster)[mykm5.cluster$seg_10 == i]))
  tmp.los = mydata3$discharge_hospital[rownames(mydata3) %in% rownames(mykm5.cluster)[mykm5.cluster$seg_10 == i]]
  print(paste("segment ", i+1, sep=""))
  print(mean(tmp.los))
}


for(i in 0:(k-1)){
  tmp.idx = as.numeric(as.character(rownames(mykm5.cluster)[mykm5.cluster$seg_10 == i]))
  tmp.los = mydata3$complication[rownames(mydata3) %in% rownames(mykm5.cluster)[mykm5.cluster$seg_10 == i]]
  print(paste("segment ", i+1, sep=""))
  print(mean(tmp.los))
}



for(i in 0:(k-1)){
  tmp.idx = as.numeric(as.character(rownames(mykm5.cluster)[mykm5.cluster$seg_10 == i]))
  tmp.los = postop$Length_of_Stay[postop$explorys_patient_id %in% tmp.idx]
  print(paste("segment ", i+1, sep=""))
  print(mean(tmp.los > 4))
}
























