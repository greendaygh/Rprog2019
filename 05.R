setwd("C:\\Rprog\\05")

matrix(0, nrow=2, ncol=2)
matrix(c(1,2,3,4), nrow=2, ncol=2)
matrix(c(1,2,3,4), 2, 2)
matrix(c(1,2,3,"4"), 2, 2)
matrix(c(1,2,3,TRUE), 2, 2)


x1 <- c(1,2,5,7)
y1 <- c(1,6,7,8)
xy<-data.frame(x1, y1)
write.table(xy, file="table_write.txt", quote=F)
myxy <- read.table(file="table_write.txt")
myxy
class(myxy)
names(myxy)
plot(myxy)
plot(myxy$x1, myxy$y1)
plot(x=myxy$x1, y=myxy$y1)
plot(y=myxy$x1, x=myxy$y1)
plot(y1~x1, data=myxy)


a <- y~x
class(a)

dat <- data.frame(x1, y1)
dat.mlt <- melt(dat)
lm(y1~x1, myxy)


x <- rnorm(100)
?rnorm
hist(x)
hist(x, br=20)
hist(x, br=20, xlim=c(-3,3))
hist(x, br=20, xlim=c(-3,3), main="Main text", xlab="X label")



boxplot(x)
y <- rnorm(100, 1, 1)
boxplot(y)
xy <- data.frame(x, y)
boxplot(xy)
class(xy)


x <- sample(1:12, 200, replace = T)
tab_x <- table(x)
y <- sample(1:12, 200, replace = T)
tab_y <- table(y)
tab_xy <- rbind(tab_x, tab_y)
tab_xy
barplot(tab_xy)
barplot(tab_xy, beside = T)
barplot(tab_xy, beside = T, col=c("darkblue","red"))
barplot(tab_xy, beside = T, col=c("darkblue","red"), xlab="Month")
barplot(tab_xy, beside = T, col=c("darkblue","red"), xlab="Month", horiz=TRUE)


x <- c(1,2,3,4,5,6,7,8)
count <- 0
for(i in x){
  if(i%%2==0){
    count <- count + 1
  }
}

x <- rnorm(500)
hist(x, 100)
y <- rnorm(500, mean=5, sd=2)
hist(c(x,y), br=100)

plot(x,y, xlim=c(-5, 5), ylim=c(-5, 10))
plot(x,y, xlim=c(-5, 5), ylim=c(-5, 10), pch=3)
idx <- which(x<0)
points(x[idx], y[idx], col="red")
abline(lm(y~x))



## readxl package
library(readxl)
setwd("C:/Rprog/05")
dir()

library(readxl)
design_file_name <- "exp_design.xlsx"
data_file_name <- "Rprog04-fl.xls"
mydesign <- read_excel(design_file_name, sheet=1)
mydesign <- as.data.frame(read_excel(design_file_name, sheet=1, range="A1:L8", skip = 0, col_names=F))
mydata <- as.data.frame(read_excel(data_file_name, sheet=1))

head(mydesign)
head(mydata)

# make a position matrix
pos1 <- rep(LETTERS[1:8], time=12)
pos2 <- rep(sprintf("%02d", 1:12), each=8)
well_position_labels <- paste(pos1, pos2, sep="")
well_position_matrix <- matrix(well_position_labels, nrow=8, ncol=12)

## vector
tmpi <- mydesign[, 1]
tmpv <- well_position_matrix[, 1]
!is.na(tmpi)
which(!is.na(tmpi))
tmpi[!is.na(tmpi)]
tmpv[!is.na(tmpi)]


extract_values <- function(x){
  flag <- which(!is.na(x))
  return(x[flag])
}
extract_values(tmpi)

extract_values2 <- function(x){
  flag <- which(!is.na(x[1:8]))
  return(x[9:16][flag])
}
extract_values2(c(tmpi, tmpv))

tmpdata <- rbind(mydesign, well_position_matrix)

colnames(mydesign) <- as.character(1:12)
colnames(well_position_matrix) <- as.character(1:12)
tmpdata <- rbind(mydesign, well_position_matrix)

tmpv <- lapply(tmpdata, extract_values2)
well_names <- unlist(tmpv)

tmpv <- lapply(mydesign, extract_values)
well_conditions <- unlist(tmpv)

well_info <- data.frame(well_names, well_conditions, stringsAsFactors = F)

mydata[1:10,]

dim(mydata)
match(mydata$Well, well_info$well_names)
tmpidx <- match(mydata$Well, well_info$well_names)
mydata_subset <- subset(mydata, !is.na(tmpidx))[,c(3,6,8)]

final_data <- merge(well_info, mydata_subset, by.x="well_names", by.y="Well")


strsplit("1;1;0", ";")
unlist(strsplit("1;1;0", ";"))
strsplit(final_data$well_conditions, ";")
myparse <- function(x){
  tmp <- unlist(strsplit(x, ";"))
  names(tmp) <- c("sample_names", "replication", "concentration")
  return(tmp)
}
tmpcond <- sapply(final_data$well_conditions, myparse)
t_tmpcond <- t(sapply(final_data$well_conditions, myparse))
t_tmpcond2 <- cbind(t_tmpcond, rownames(t_tmpcond))
t_tmpcond2 <- cbind(t_tmpcond, well_conditions=rownames(t_tmpcond))
final_data <- merge(final_data, t_tmpcond2, by="well_conditions")
final_data <- final_data[,-1]




