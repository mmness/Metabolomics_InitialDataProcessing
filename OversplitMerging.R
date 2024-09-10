# Oversplit Merging -------------------------------------------------------
#this is another method of oversplit removal. It works very similar to the other oversplit removal code
#however, instead of removing the oversplit rows, it adds them all into the one row that remains. 
#this method needs to have stricter mz_tol and rt_tol. to prevent different peaks from being merged
#it also has a max_oversplit_num. "real" peaks probably are not going to be oversplit in 10+ peaks

#can be used BEFORE the other oversplit code.  

##UPDATE. after testing it out, wouldn't recommend using it. It would add oversplit peaks to good peaks

feat<-read.csv("quant_HILICpos_withDS.csv") #add in your quantfile name

mz_tol<-0.0002
rt_tol<-0.1  
oversplit_num<-2 
max_oversplit_num<-4 #maximum repeating number before its "junk"

feat$row.m.z<-round(feat$row.m.z, 4) 
feat$row.retention.time<-round(feat$row.retention.time, 2) 
feat <- feat[, names(feat) != "X"]
mz_table<-as.data.frame(  table(feat$row.m.z)  )
potential_oversplits<-mz_table[mz_table$Freq >= oversplit_num & mz_table$Freq <= max_oversplit_num,]
potential_mz_forremov<-as.vector(potential_oversplits$Var1)

CIs_to_drop<-numeric()
to_add_column<-numeric()
to_be_added_to_column<-numeric()

for (i in 1:length(potential_mz_forremov)) {
  print(i)
  mz<-subset(feat, feat$row.m.z<=as.numeric(potential_mz_forremov[i])+mz_tol & feat$row.m.z>=as.numeric(potential_mz_forremov[i])-mz_tol)
  rt_sorted<-mz[order(mz$row.retention.time),]
  rt_sorted <- rt_sorted[, names(rt_sorted) != "X"]
  for (j in nrow(rt_sorted):2){
    dif<-rt_sorted$row.retention.time[j]-rt_sorted$row.retention.time[j-1]
    
    if (dif < rt_tol){
      sum_rowj <- sum(rt_sorted[j, 4:ncol(rt_sorted)])
      sum_rowjmin1 <- sum(rt_sorted[j-1, 4:ncol(rt_sorted)])
      
      if (sum_rowj > sum_rowjmin1) {
        to_be_added_to <- rt_sorted$row.ID[j]
        to_add <-rt_sorted$row.ID[j-1]
      } else {
        to_be_added_to <- rt_sorted$row.ID[j-1]
        to_add <- rt_sorted$row.ID[j]}
      
      if (!(to_add %in% to_add_column)) {
        to_add_column <- c(to_add_column, to_add) 
        to_be_added_to_column<-c(to_be_added_to_column, to_be_added_to)  
      }
      
      feat[which(feat$row.ID==to_be_added_to), 4:ncol(feat)] <- feat[which(feat$row.ID==to_be_added_to), 4:ncol(feat)] + feat[which(feat$row.ID==to_add), 4:ncol(feat)]
      CIs_to_drop<-c(CIs_to_drop, to_add)
      num<-which(feat$row.ID==to_add)
      rt_sorted <- subset(rt_sorted, !(row.ID %in% feat$row.ID[num]))
    }}}  

data_export<-cbind(to_add_column, to_be_added_to_column)
colnames(data_export) <- c("this RowID was added to", "This RowID")
CIs_to_drop<-unique(CIs_to_drop)
cleaned_data<- subset(feat, !(row.ID %in% CIs_to_drop))
cleaned_data <- cleaned_data[, names(cleaned_data) != "X"]

cat(length(CIs_to_drop), "features were removed from the dataset", "\n")

#exporting data

#two files where you can check the CIs in MZmine
write.csv(data_export, "info_HILICpos_withDS_features_merged.csv", row.names = FALSE)
write.csv(CIs_to_drop, "info_HILICpos_withDS_deleted_rows.csv", row.names = FALSE)

write.csv(cleaned_data, "quant_HILICpos_withDS_OSmerged.csv", row.names = FALSE) #make sure to keep .csv at end



