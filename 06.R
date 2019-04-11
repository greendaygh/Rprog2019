setwd("C:\\Rprog\\06")


source("read_plate.R")

design_file_name <- "exp_design.xlsx"
data_file_name <- "Rprog04-fl.xls"

mydata <- multiple_plate_excel_reader(design_file_name, data_file_name)

args(multiple_plate_excel_reader)



args(multiple_plate_excel_reader2)

mydata <- multiple_plate_excel_reader2(design_file_name, data_file_name, sheet4design=2)


s <- "This is the sixth lecture of R programming"
substr(s, 0, 11)
nchar(s)
toupper(s)
tolower(s)
strsplit(s, split=" ")
paste(s, " at UST", sep="")
sub("This", "That", s)
## regular expression
sub("This is.+of ", "", s)
grep("This", s)
regexpr("This", s)

s <- "aatgctgtaga"
nchar(s)
ss <- toupper(s)
regexpr("ATG", ss)

regexpr("ATG", x)

source("read_plate.R")

design_file_name <- "exp_design2.xlsx"
data_file_names <- c("20171012-phenol-1.xls", 
                     "20171012-phenol-2.xls", 
                     "20171227-phenol-1.xls", 
                     "20171227-phenol-2.xls")

mydata1 <- multiple_plate_excel_reader2(design_file_name, data_file_names[1], sheet4design=1)
mydata2 <- multiple_plate_excel_reader2(design_file_name, data_file_names[2], sheet4design=2)
mydata3 <- multiple_plate_excel_reader2(design_file_name, data_file_names[3], sheet4design=3)
mydata4 <- multiple_plate_excel_reader2(design_file_name, data_file_names[4], sheet4design=4)

mydata <- rbind(mydata1, mydata2, mydata3, mydata4)

tmp1 <- subset(mydata, (sample_names==1 & concentration==0))
tmp2 <- subset(mydata, (sample_names==1 & concentration==0.1))
tmp3 <- subset(mydata, (sample_names==1 & concentration==1))

barplot(c(mean(tmp1$GFP), mean(tmp2$GFP), mean(tmp3$GFP)))


library(ggplot2)

ggplot(data=mydata, aes(x=sample_names, y=GFP))

ggplot(data=mydata, aes(x=sample_names, y=GFP)) +
  geom_bar()

?geom_bar

ggplot(data=mydata, aes(x=sample_names, y=GFP)) +
  geom_bar(stat="identity")

ggplot(data=mydata, aes(x=sample_names, y=GFP, fill=concentration)) +
  geom_bar(stat="identity", position="dodge")

mydata2 <- mydata
mydata2$concentration <- as.factor(mydata2$concentration)

ggplot(data=mydata2, aes(x=sample_names, y=GFP, fill=concentration)) +
  geom_bar(stat="identity", position="dodge")

ggplot(data=mydata2, aes(x=sample_names, y=GFP, fill=concentration)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values = rainbow(11))
  
ggplot(data=mydata2, aes(x=sample_names, y=GFP, fill=concentration)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values = heat.colors(11))


ggplot(data=mydata2, aes(x=sample_names, y=GFP, fill=concentration, color="black")) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values = heat.colors(11))



library(plyr)
df <- ddply(mydata2, c("GFP", "replication"), summarize, Mean = mean(GFP), SD = sd(GFP))  

  
## change color
RColorBrewer::display.brewer.all()




