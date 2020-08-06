#run the beginning part of model test for LOS
#to create the whole cohort - data
#run the glm model to create the model reg_rev

#2.1 Choose one patient
f. = function(){
set.seed(0)

test_patient_id = sample(intersect(data$explorys_patient_id, cluster_assignment$X), 1)
pid = toString(test_patient_id)
#print the patient ID
paste("test patient ID =", pid)
patient_org = data[data$explorys_patient_id %in% test_patient_id,]

#add row ID
rownames(patient_org) = patient_org$explorys_patient_id
}

#2.2 Choose multiple patients
num_test=1000
set.seed(0)
test_id_set = sample(intersect(data$explorys_patient_id, cluster_assignment$X), num_test)
patient_set = data[data$explorys_patient_id %in% test_id_set,]
rownames(patient_set) = patient_set$explorys_patient_id

# 3. Randomly pick up feature from the list as the one to ask
# to pick up null as the start and then one at a time for total of nine times

cr <<- qnorm(0.975)
coe.rev <- as.numeric(reg_rev$coefficients)
cov.rev <- vcov(reg_rev)

#check the sign of each coefficients
c <- reg_rev$coefficients[5:13]
names(c)=features
d <- names(which(c<0))

#prepare lists to store results
wmp_list <- list(NULL)
length(wmp_list)=num_test
wmb_list <- wmp_list
wmr_list <- wmp_list
wmf_list <- wmp_list


#choose a single patient from the patient set
for(j in 1:num_test){

#choose the seed for question order
#if(j==7) {k=7}
#else{ k = sample(c(1:10),1)}

patient_org=patient_set[j,]
test_patient_id = patient_org$explorys_patient_id
pid = toString(test_patient_id)

#used to store the results based on fixed probability
rmp <- data.frame(pid=character(0), 
pred_val=numeric(0), pred_upr=numeric(0), pred_lwr=numeric(0), pred_range=numeric(0),
range_pct=numeric(0), 
true_value=character(0), 
next_question=character(0), imputed_feature=character(0), 
diff=numeric(0), diff_pct=numeric(0))

#used to store the results based on binary and imputed range
rmb <- rmp
rmr <- rmp
rmf <- rmp

#initialize the question list
#feature_toask <- features
feature_toask <- feature_inorder

for(i in 1:length(features)){
#mark the rest of the features as missing ones
patient = patient_org
patient[feature_toask] <- NA
#print(str(patient))
#print(noquote("to impute the feature(s) of "))
#print(feature_toask)


#run the imputation process
# 4. Initialize the estimates from the conditional population mean
cond_pop <- findCondPop(patient)
init_est <- initEstimate(cond_pop, patient)
f_hat <- init_est$f_hat
f_range <- init_est$f_range

# 5. Infer missing features from clustering
support_cluster <- findSupportClusters(cluster_centroid, f_hat, f_range)
box <- findSupportBox(cluster_centroid, support_cluster, f_hat, f_range)
group_cluster <- findGroupCluster(cluster_centroid, box)

if (is.null(group_cluster)) {
  feature_est = f_hat
  feature_range = f_range
  feature_est_bin = f_hat

  for (name in names(f_range)) {
  if (f_range[[name]] != 0) {
      feature_est_bin[[name]] <- as.numeric(f_hat[[name]] >= pop_avg[[name]])
    }
  }
} else {
  new_est <- updateEstimates(cluster_centroid, cluster_size, group_cluster, f_hat, f_range)
  feature_est = new_est$f_hat
  feature_range = new_est$f_range
  feature_est_bin = new_est$f_hat_bin
}

#6. compute the predicted confidence interval

feature_est = data.frame(feature_est)
feature_range = data.frame(feature_range)

#6.1 using the imputed fixed probability
tmp <- computePredCI2(patient_org, feature_est, coe.rev, cov.rev)

#6.2 using the imputed binary rsult
tmb <- computePredCI2(patient_org, data.frame(feature_est_bin), coe.rev, cov.rev)

#6.3 using the imputed range to get max and min bounds - numeric value
feature_upr <- feature_est + feature_range
feature_lwr <- feature_est - feature_range
a <- feature_upr[d] 
feature_upr[d]=feature_lwr[d]
feature_lwr[d]=a
tmr <- computePredCI3(patient_org, feature_est, feature_upr, feature_lwr, coe.rev, cov.rev)

#6.4 using the imputed range plus confident interval
tmf <- computePredCI4(patient_org, feature_est, feature_range, coe.rev, cov.rev)


#pick up the next feature to ask
#use the random order
#set.seed(k)
#use the fixed order
#set.seed(7)

#pickup_idx = sample(1:length(feature_toask), 1, replace=F)
pickup_idx=1

#print the missing features
#print(noquote(paste("the coming up question to ask =", feature_toask[pickup_idx], "?")))
tmp$next_question=feature_toask[pickup_idx]
#print(tmp$next_question)
#add the imputed feature list
#tmp$imputed_feature=paste(feature_toask, collapse=" ")

tmb$next_question=tmp$next_question
tmr$next_question=tmp$next_question
tmf$next_question=tmp$next_question

#append the results
rmp <- rbind(rmp, data.frame(tmp))
rmb <- rbind(rmb, data.frame(tmb))
rmr <- rbind(rmr, data.frame(tmr))
rmf <- rbind(rmf, data.frame(tmf))

feature_toask <- feature_toask[-pickup_idx]
#print(feature_toask)

}

#use the original data to get the predicted confidence interval
td <- computePredCI2(patient_org, patient_org, coe.rev, cov.rev)
td$diff=0
td$diff_pct=0
td$true_value="***"
td$next_question=NA
#td$imputed_feature=NA

#compute the error distance and its percentage to prediction fitted value
rmp$diff=rmp$pred_val-td$pred_val
rmb$diff=rmb$pred_val-td$pred_val
rmr$diff=rmp$diff
rmf$diff=rmp$diff

if(td$pred_val!=0) {
rmp$diff_pct=rmp$diff/td$pred_val*100
rmb$diff_pct=rmb$diff/td$pred_val*100
rmr$diff_pct=rmp$diff_pct
rmf$diff_pct=rmp$diff_pct
} else {
rmp$range_pct=NA
rmb$range_pct=NA
rmr$range_pct=NA
rmf$range_pct=NA
}

#mark the step when reached to prediction fitted value
for(q in 1:9)
{
if(rmp$diff[q]==0) {rmp$true_value[q]="*"} else {rmp$true_value[q]=""}
if(rmb$diff[q]==0) {rmb$true_value[q]="*"} else {rmb$true_value[q]=""}
if(rmr$diff[q]==0) {rmr$true_value[q]="*"} else {rmr$true_value[q]=""}
if(rmf$diff[q]==0) {rmf$true_value[q]="*"} else {rmf$true_value[q]=""}
}

#combine the results
rmp <- rbind(rmp, data.frame(td))
rmb <- rbind(rmb, data.frame(td))
rmr <- rbind(rmr, data.frame(td))
rmf <- rbind(rmf, data.frame(td))

#prepare the data to test
wmp_list[[j]] <- rmp[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmb_list[[j]] <- rmb[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmr_list[[j]] <- rmr[,c("pred_range","diff","true_value","range_pct","diff_pct")]
wmf_list[[j]] <- rmf[,c("pred_range","diff","true_value","range_pct","diff_pct")]


names(wmp_list)[j] = pid
names(wmb_list)[j] = pid
names(wmr_list)[j] = pid
names(wmf_list)[j] = pid

#monitor the loop
if(j%%100==0) {print(j)}


} #end of j loop 


#test and plot

delist <- function(wlist){
  return (data.frame(x=rownames(wlist), y=wlist$pred_range, z=abs(wlist$diff), yy=wlist$range_pct, zz=abs(wlist$diff_pct)))
}

min_conv <- function(wlist){
	idx = which(wlist$true_value=="*")
	if(length(idx)>0) {
		step_numb=min(idx)
		} else {step_numb=NA}
	return (step_numb)
}

ewmp <- data.frame(x=numeric(0), y=numeric(0), z=numeric(0), yy=numeric(0), zz=numeric(0))
ewmb <- ewmp
ewmr <- ewmp
ewmf <- ewmp

for(i in 1:num_test){
	ewmp = rbind(ewmp, delist(wmp_list[[i]]))
	ewmb = rbind(ewmb, delist(wmb_list[[i]]))
	ewmr = rbind(ewmr, delist(wmr_list[[i]]))
	ewmf = rbind(ewmf, delist(wmf_list[[i]]))
}



r1 <- aggregate(ewmp[,2:5], by=list(as.numeric(as.character(ewmp$x))), mean)
colnames(r1)[1]="x"
r2 <- aggregate(ewmb[,2:5], by=list(as.numeric(as.character(ewmb$x))), mean)
colnames(r2)[1]="x"
r3 <- aggregate(ewmr[,2:5], by=list(as.numeric(as.character(ewmr$x))), mean)
colnames(r3)[1]="x"
r4 <- aggregate(ewmf[,2:5], by=list(as.numeric(as.character(ewmf$x))), mean)
colnames(r4)[1]="x"

par(mfrow=c(1,2))
matplot(r1$x, cbind(r1$y, r2$y, r4$y), axes=F, col=c("red","purple","black"), pch=19, 
main="Random order, Average CI comparison - \nfixed P(red) | binary(purple) | imputed range(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(r1$x, cbind(r1$yy, r2$yy, r4$yy), axes=F, col=c("red","purple","black"), pch=19, 
main="Random order, Average varies(%) comparison \nfixed P(red) | binary(purple) | imputed range(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(r1$x, cbind(r1$z, r2$z), axes=F, col=c("green","black"), pch=19, 
main="Random order, Average error distance comparison - \nfixed P(green) | binary(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(r1$x, cbind(r1$zz, r2$zz), axes=F, col=c("green","black"), pch=19, 
main="Random order, Average error distance(%) comparison \nfixed P(green) | binary(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

#for the fixed order
fr1 <- aggregate(ewmp[,2:5], by=list(as.numeric(as.character(ewmp$x))), mean)
colnames(fr1)[1]="x"


par(mfrow=c(1,2))
matplot(fr1$x, cbind(fr1$y, r1$y), axes=F, col=c("dark green","black"), pch=19, 
main="Average CI comparison - \nfixed order(green) | random order(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(fr1$x, cbind(fr1$yy, r1$yy), ylab="Range%", axes=F, col=c("dark green","black"), pch=19, 
main="Average varies(%) comparison \nfixed order(green) | random order(black)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

par(mfrow=c(1,2))
matplot(fr1$x, cbind(fr1$z, r1$z), axes=F, col=c("brown","blue"), pch=19, 
main="Average error distance comparison - \nfixed order(brown) | random order(blue)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(fr1$x, cbind(fr1$zz, r1$zz), ylab="Error%", axes=F, col=c("brown","blue"), pch=19, 
main="Average error distance(%) comparison \nfixed order(brown) | random order(blue)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)


emincv <- data.frame(ptid=character(0), p_numb=numeric(0), b_numb=numeric(0), r_numb=numeric(0))

for(i in 1:num_test){
	ptid = names(wmp_list[i])
	emincv <- rbind(emincv, data.frame(ptid=ptid, 
	p_numb=min_conv(wmp_list[[i]]), b_numb=min_conv(wmb_list[[i]]), r_numb=min_conv(wmr_list[[i]])) )
	#if(i%%10==0) {print(i)}
}






