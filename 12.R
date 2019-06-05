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
library(Biostrings)

genomeseq <- readDNAStringSet(acc_path_names)

tmp <- strsplit(names(genomeseq), split=" ")
tmp2 <- lapply(tmp , function(x){x[1]})
names(genomeseq) <- unlist(tmp2)

acc_ids <- as.character(eldata_filtered2$genomic_nucleotide_accession.version)
startpos <- eldata_filtered2$start_position_on_the_genomic_accession
endpos <- eldata_filtered2$end_position_on_the_genomic_accession



###### exercise 12-1
myseq <- vector("list", length(acc_ids))
for(i in 1:length(acc_ids)){
  myseq[[i]] <- subseq(genomeseq[[acc_ids[i]]], startpos[i], endpos[i])    
}

myseq <- DNAStringSet(myseq)
names(myseq) <- as.character(eldata_filtered2$Symbol)

library(DECIPHER)
BrowseSeqs(myseq, htmlFile="myseq.html", colWidth=100)
dnacolors <- c("#1E90FF", "#32CD32", "#9400D3", "black", "#EE3300")
BrowseSeqs(myseq, htmlFile="myseq.html", colors=dnacolors, colWidth=100)

aln <- AlignSeqs(myseq) # output alignment
BrowseSeqs(aln, htmlFile="myaln.html", colWidth=100)
print(aln, width=20)

d <- DistanceMatrix(aln, correction="Jukes-Cantor", verbose=FALSE)
c <- IdClusters(d, method="ML", cutoff=.05, showPlot=TRUE, myXStringSet=aln, verbose=FALSE)


library(msa)
library(ape)
library(seqinr)
library(ggtree)

myaln<-msa(myseq, method="ClustalOmega", type="dna")
myaln2 <- msaConvert(myaln, type="seqinr::alignment")
d <- dist.alignment(myaln2, "identity")
mytree <- njs(d)

ggtree(mytree) +
  geom_tiplab() +
  xlim(-1, 15)

ggtree(mytree, branch.length="none") +
  geom_tiplab() +
  xlim(-1, 15)

ggtree(mytree, layout="circular") + 
  geom_tiplab2(color='blue', size=3) 

ggtree(mytree, layout="circular", branch.length="none") + 
  geom_tiplab2(aes(angle=angle), color='blue', size=3) 


##3=============

library(reshape2)
library(ggplot2)
d <- DistanceMatrix(aln, correction="Jukes-Cantor", verbose=FALSE)

d_melt <- melt(d)
ggplot(d_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() 

ggplot(d_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



