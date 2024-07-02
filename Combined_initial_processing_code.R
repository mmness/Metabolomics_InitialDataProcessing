# OVERSPLIT REMOVAL -------------------------------------------------------
feat<-read.csv("quant.csv") #add in your quantfile name
mz_tol<-0.001
rt_tol<-0.2 #set to what you used in MZMine
oversplit_num<-6 #lower if you want more removed

#you dont have to change anything until line 35
feat$row.m.z<-round(feat$row.m.z, 4) 
feat$row.retention.time<-round(feat$row.retention.time, 2) 
mz_table<-as.data.frame(  table(feat$row.m.z)  )
potential_oversplits<-mz_table[mz_table$Freq >= oversplit_num,]
potential_mz_forremov<-as.vector(potential_oversplits$Var1)
#these lines ID the potential oversplit MZs and saves them into the "potential_mz_forremov" vector. 
#If an MZ repeats more than the "oversplit num" set above, it gets saved into the vector
CIs_to_drop<-numeric()
for (i in 1:length(potential_mz_forremov)) {
  mz<-subset(feat, feat$row.m.z<=as.numeric(potential_mz_forremov[i])+mz_tol & feat$row.m.z>=as.numeric(potential_mz_forremov[i])-mz_tol)
  rt_sorted<-mz[order(mz$row.retention.time),]
  for (j in 2:nrow(rt_sorted)){
    dif<-rt_sorted$row.retention.time[j]-rt_sorted$row.retention.time[j-1]
    if (dif < rt_tol){
      CIs_to_drop<-c(CIs_to_drop, rt_sorted$row.ID[j])}}}
CIs_to_drop<-unique(CIs_to_drop)
cleaned_data<- subset(feat, !(row.ID %in% CIs_to_drop))
cleaned_data <- cleaned_data[, names(cleaned_data) != "X"]

#explaination of the nested-for-loop:The outer loop subsets the dataset with just a "potential mz for oversplit" value +/- the mz tolerance
#the subsetted dataset is sorted from low to high by RT value
#the inner loop finds the difference between each RT value, if the difference between the two is less than the "rt_tol" set above, the feature CI is saved in the "CIs to drop" vector

#if it gives an error that says: (dif < rt_tol) { : missing value where TRUE/FALSE needed. lower the oversplit_num

#data output line. put whatever name you want in
length(CIs_to_drop) #how many values are removed
write.csv(cleaned_data, "OSremoved_quant.csv", row.names = FALSE) #make sure to keep .csv at end

# BLANK REMOVAL -----------------------------------------------------------
tab<-read.csv("quant.csv", header=TRUE) 
l<-3 #degree of removal wanted
blanknames<-"blank|Blank|BLANK|bnk|BNK" #Change depending on your blank names. Case sensitive

print(names(tab)[grep(blanknames, colnames(tab))]) #test to make sure your blanks are IDed

#dont change anything til line 58
tab <- tab[, names(tab) != "X"]
fcount <- c()
for(i in 1:nrow(tab)) {
  bmax <- max(tab[i, grep(blanknames, colnames(tab))])
  if(bmax==0) {
    fcount <- c(fcount, 1 )
  } else {
    fcount <- c(fcount, sum(tab[i, -grep(blanknames, colnames(tab))][,-1|-2|-3] > l*bmax) ) 
  }
}
num_removed<-nrow(tab)-nrow(tab[-which(fcount==0),])

cat("number of features removed:", num_removed, "\n") #prints how many are removed
write.csv(tab[-which(fcount==0),], "quant_blankremoved.csv", row.names=FALSE) #change name to what you want but keep the .csv

# TIC NORM ----------------------------------------------------------------
table<-read.csv("quant.csv")

norm_table<- apply(table[, -(1:3)], 2, function(x) x / sum(x) )
norm_table<-cbind(table[,1:3], norm_table)
norm_table <- norm_table[, names(norm_table) != "X"]                   

print(sum(is.na(norm_table))) # checks that no NA values popped up
print(min(colSums(norm_table[, -(1:3)]))) #should be 1 or theres a problem

write.csv(norm_table, "quant_norm.csv", row.names = FALSE) #name what you want but keep .csv
# QIIME FORMATTING --------------------------------------------------------
table<-read.csv("quant_3blankremoved_norm.csv")

#formatting. dont change things til like 94
table$row.m.z<-round(table$row.m.z, 4) 
table$row.retention.time<-round(table$row.retention.time, 2) 
sampleid <-numeric()
for(i in 1:nrow(table)){
  id<-c(table$row.m.z[i], table$row.retention.time[i], table$row.ID[i])
  nums<-c("X", paste(id, collapse = "_"))
  sampleid[i]<-paste(nums, collapse = "")
}
tab.w.id<-cbind(sampleid, table[, -(1:3)])
names(tab.w.id) <- sub(".mzML.Peak.area|.mzXML.Peak.area", "", names(tab.w.id))
trans<-t(tab.w.id)
rownames(trans)[1] <- "sampleid"
trans<-cbind(row.names(trans), trans)
colnames(trans)<-trans[1,]
trans<-trans[-1,]
trans[is.na(trans)] <- 0     #this replaces any "NA" output with zero 
trans <- trans[rownames(trans) != "X", ]
#removes .mzML Peak Area from file name, removes old feature columns, adds in mz_RT_CI format for feature, transposes

write.table(trans, "FeatureTable_QiimeFormat.txt", row.names = FALSE, sep = "\t", na="") #make sure its .txt. NOT .csv

#if you want a csv copy:
write.csv(trans, "FeatureTable_QiimeFormat.csv", row.names = FALSE, na="")

# QIIME FORMAT CHECKING ---------------------------------------------------
meta<-read.csv("demo_metadata.csv") #input metadata .csv
trans<-read.table("FeatureTable_QiimeFormat.txt", sep="\t", header = TRUE) #from previous step

##part 1##
names(meta)[1] <- "sampleid"
metasamps<-as.vector(meta$sampleid) 
featlist_samples<-trans[ ,1]
unique_to_meta <- setdiff(metasamps, featlist_samples)
cat("Unique to Metadata file:", unique_to_meta, "\n")
unique_to_featlist <- setdiff(featlist_samples, metasamps)
cat("Unique to Featlist file:", unique_to_featlist, "\n")
#prints the filenames that dont have matches in the other file. does not work if one file has an exact duplicate
#youll have to edit the excel file if names are printed

##part 2##
#making sure engths are equal
length(metasamps)
length(featlist_samples)

##part3##
#checking for duplicates in featlist
duplicatesf <- duplicated(featlist_samples)
duplicate_valuesf <- unique(featlist_samples[duplicatesf])
if (any(duplicatesf)) {
  cat("Duplicate values found in featlist:", duplicate_valuesf, "\n")
} else {cat("No duplicate values found in featlist\n")}

##part 4##
#checking for duplicates in metadata
duplicatesm <- duplicated(metasamps)
duplicate_valuesm <- unique(metasamps[duplicatesm])
if (any(duplicatesm)) {
  cat("Duplicate values found in metadata:", duplicate_valuesm, "\n")
} else {cat("No duplicate values found in metadata\n")}

##part 5##
#only works if cells above have found no issues
#Qiime comes up with errors if one of the samples has no features present.(sum of 0)
#This code IDs the samples with a total row sum of zero, then deletes that sample from both the feature list and the metadata
rowsums<-numeric()
for(i in 1:nrow(trans)){
  sum<-sum(as.numeric(trans[i, 2:ncol(trans)]))
  rowsums[i]<-round(sum)
}
dftrans<-as.data.frame(trans)
dfsums<-as.data.frame(cbind(rownames(dftrans), rowsums))
samp_to_remov<- dfsums$V1[dfsums$rowsums == 0]
featlist_filtered<- trans[!(dftrans$sampleid %in% samp_to_remov), ]
metadata_filtered<- meta[!(meta$sampleid %in% samp_to_remov), ]

##part 6##
#this checks the previous cell. it will print the sample names that have been removed(it will likely be blanks)
#Then the length() function tells you how many.
dfsums$V1[dfsums$rowsums == 0]
length(dfsums$V1[dfsums$rowsums == 0])

#export. keep the .txt
write.table(featlist_filtered, "correctedfeatlist.txt", row.names = FALSE, sep = "\t", na="")
write.table(metadata_filtered, "correctedmetadata.txt", row.names = FALSE, sep = "\t", na="NA")

#if you want a .csv file
write.csv(featlist_filtered, "correctedfeatlist.csv", row.names = FALSE, na="")
write.csv(metadata_filtered, "correctedmetadata.csv", row.names = FALSE, na="NA")

# QIIME SUBSETTING --------------------------------------------------------

#input the metadata and feature table from the format checking output
metadata<-read.table("correctedmetadata.txt", sep="\t", header = TRUE)
featureTable<-read.table("correctedfeatlist.txt", sep="\t", header = TRUE)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!Do not run everything..choose what you want to change about your table. then choose one of the pre-made codes run. 
#make sure to check your files to make sure 

#if you want to drop sections. #change to whatever pattern you want in the sample names. This line is case sensitive. 
to_drop <- "blank|Blank|BLANK|QC|Qc|qc|6mix" #change this line
featTable_filtered <- subset(featureTable, !grepl(to_drop, sampleid, ignore.case = TRUE))
metadata_filtered <- subset(metadata, metadata$sampleid %in% featTable_filtered$sampleid)

#if you only want to include 1 group from metadata
metadata_filtered <- metadata[metadata$batch == "A", ] #change this to your column name and group you want to keep
featTable_filtered <- subset(featureTable, featureTable$sampleid %in% metadata_filtered$sampleid )

###exporting
#for Qiime format             
write.table(featTable_filtered, "subset_featlist.txt", row.names = FALSE, sep = "\t", na="")
write.table(metadata_filtered, "subset_metadata.txt", row.names = FALSE, sep = "\t", na="")
#for regular .csv
write.csv(featTable_filtered, "subset_featlist.csv", row.names = FALSE) 
write.csv(metadata_filtered, "subset_metadata.csv", row.names = FALSE) 


# METADATA MERGING --------------------------------------------------------

#read in files from format checking step for this to work
metadata<-read.table("correctedmetadata.txt", sep="\t", header = TRUE)
featureTable<-read.table("correctedfeatlist.txt", sep="\t", header = TRUE)

merged_data <- merge(metadata, featureTable, by="sampleid", all=TRUE)

write.csv(merged_data, "mergeddata.csv", row.names = FALSE, na="NA")

# GNPS FORMAT OUTPUT ------------------------------------------------------

metadata<-read.csv("metadata.csv", header = TRUE) #orginal .csv metadata, not qiime format
featureTable<-read.csv("quant.csv", header=TRUE)

#correcting metadata
add_cor_names <- function(df) {
  colnames(df) <- ifelse(!grepl("^ATTRIBUTE_", colnames(df)), paste0("ATTRIBUTE_", colnames(df)), colnames(df))
  colnames(df)[1] <- "filename"
  return(df)
}
metadata_cor<-add_cor_names(metadata)
metadata_cor$filename <- ifelse(grepl("\\.mzML$", metadata_cor$filename), metadata_cor$filename, paste0(metadata_cor$filename, ".mzML")) #may need to change to .mzXML

#correcting featlist
names(featureTable)[1]<-"row ID"
names(featureTable)[2]<-"row m/z"
names(featureTable)[3]<- "row retention time"
names(featureTable)<-gsub(".mzML.Peak.area",".mzML Peak area", names(featureTable)) #potentially need to change to .mzXML                                                                    

write.csv(featureTable, "GNPSfeattable.csv", row.names = FALSE)
write.table(metadata_cor, "GNPSmetadata.txt", row.names = FALSE, sep = "\t", na="")
