eldata <- read.table("gene_result.txt", sep="\t", header = T)
str(eldata)

library(reutils)
library(dplyr)

eldata_filtered <- eldata %>% 
  select(GeneID, 
         Org_name, 
         Symbol, 
         description, 
         genomic_nucleotide_accession.version, 
         start_position_on_the_genomic_accession, 
         end_position_on_the_genomic_accession) %>% 
  filter(genomic_nucleotide_accession.version != "") 

head(eldata_filtered, 10)

eldata_filtered2 <- eldata_filtered[1:20,]
acc <- eldata_filtered2$genomic_nucleotide_accession.version
acc2 <- as.character(acc)
acc2down <- acc2[!duplicated(acc2)]
acc_path_names <- paste("sequences/", acc2down, ".fasta", sep="")
for(i in 1:length(acc2down)){
  ef <- efetch(uid = acc2down[i], 
               db = "nuccore", 
               retmode = "text", 
               rettype = "fasta") 
  write(content(ef),file=acc_path_names[i])
  Sys.sleep(1)
  cat(i, "/", length(acc2down), "\n")
  flush.console()
}

head(eldata_filtered2)


for(i in 1:nrow(eldata_filtered2)){
  
}

