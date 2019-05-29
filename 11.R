
library(reutils)

acc <- c("NC_001477", "NC_001474", "NC_001475", "NC_002640")
for(i in 1:length(acc)){
  ef <- efetch(acc, "nuccore", retmode = "text", rettype = "gb")  
  write(content(ef),file=paste(acc[i], ".gb", sep=""))
  Sys.sleep(1)
  cat(i, "/", length(acc), "\n");flush.console()
}

library(genbankr)

acc <- c("NC_001477", "NC_001474", "NC_001475", "NC_002640")
acc_files <- paste(acc, ".gb", sep="")

dg_list <- vector("list", length(acc_files))
for(i in 1:length(acc_files)){
  dg_list[[i]] <- parseGenBank(file = acc_files[i])
  cat(i, "/", length(acc_files), "\n");flush.console()
}
names(dg_list) <- acc
dg_list
dg_list[[1]]
dg_list$NC_001477



library(GenomicRanges)
library(ggbio)
library(ggplot2)


i <- 1
sel_dg <-





