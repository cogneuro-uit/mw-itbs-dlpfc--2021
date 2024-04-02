library(ProjectTemplate)
load.project()
library(stringr)


getwd()
#fix AG12
list.files("./data/fix", pattern=".csv", full.names = T) -> a
a[1] -> b
read.csv(file = b, row.names = NULL, header = F) -> D 
D$V2[D$V2=="AG102"] <- "AG012"
D$V3 <- str_replace(D$V3,"AG102","AG012")
strsplit(b,split="/")[[1]][4] -> name
write.csv(D, file = name, row.names = F, col.names = NA, quote = F)
# Writing to CSV without column names adds an empty row (with names), this has been manually deleted. 


#fix PFC020
list.files("./data/fix", pattern=".csv", full.names = T) -> a
a[2] -> b
read.csv(file = b, row.names = NULL, header = F) -> D
D$V2[D$V2=="PFC0020"] <- "PFC020"
D$V3 <- str_replace(D$V3,"PFC0020","PFC020")
strsplit(b, split="/")[[1]][4] -> name
write.csv(D, file = name, row.names = F, col.names = NA,  quote = F)
# Writing to CSV without column names adds an empty row (with names), this has been manually deleted. 


# fix PFC028 to AG029
list.files("./data/fix", pattern = ".csv", full.names = T) -> a
a[3:6] -> b
for(x in 1:length(b)){
  read.csv(file = b[x], row.names = NULL, header = F) -> D
  D$V1[D$V1=="PFC"] <- "AG"
  D$V2[D$V2=="PFC028"] <- "AG029"
  D$V2 <- str_replace(D$V2,"PFC","AG") 
  D$V3 <- str_replace(D$V3,"PFC028","AG029")
  unlist(strsplit(b[x],split="/"))[4] -> name 
  str_remove(name, "PFC_PFC028") -> name
  paste0("AG_AG029", name) -> name
  write.csv(D, file = name, row.names = F, col.names = NA, quote = F)
}
# Writing to CSV without column names adds an empty row (with names), this has been manually deleted. 




#####
# fix PFC039 to AG039
list.files("data/fix", pattern = ".csv", full.names = T) -> a
a[2] -> b
read.csv(file = b, row.names = NULL, header = F) -> Dd
D$V1[D$V1=="PFC"] <- "AG"
D$V2[D$V2=="PFC038"] <- "AG039"
D$V2 <- str_replace(D$V2,"PFC","AG") 
D$V3 <- str_replace(D$V3,"PFC038","AG039")
unlist(strsplit(b,split="/"))[3] -> name 
write.csv(D, file = name, row.names = F, col.names = NA, quote = F)

# Writing to CSV without column names adds an empty row (with names), this has been manually deleted. 


# fis for AG039 - AG038 (in file) AG 039

