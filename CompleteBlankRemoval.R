tab <- read.csv("demofeaturetable_quant.csv", header=TRUE)

blanknames <- "blank|Blank|BLANK|bnk|BNK" # Change depending on your blank names. Case sensitive
print(names(tab)[grep(blanknames, colnames(tab))]) #test to make sure your blanks are IDed

tab <- tab[, names(tab) != "X"]
fcount <- c()
for(i in 1:nrow(tab)) {
  bmax <- max(tab[i, grep(blanknames, colnames(tab))])
  if(bmax > 0) { 
    fcount <- c(fcount, 1 )
  } else {
    fcount <- c(fcount, 0) 
  }
}

cleaned_tab<-tab[fcount == 0,]
cleaned_tab <- cleaned_tab[, !grepl(blanknames, colnames(cleaned_tab))]


cat("number of features removed:", sum(fcount), "\n") #prints how many are removed
write.csv(cleaned_tab, "quant_blankremoved.csv", row.names=FALSE) #change n
