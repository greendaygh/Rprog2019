## exercise 3-1 matrix
mymat <- matrix(0, nrow=100, ncol=3) # 1
mymat[,1] <- 1:100 # 2
mymat[,2] <- seq(1,200,2) # 3
mymat[,3] <- seq(2,200,2) # 4
mymat[c(2,3,4,5),2] # 5
mymat-1 # 6
mysub <- mymat[,2] - mymat[,1] #7
sum(mysub) #8
sum(mysub^2) #8


## data.frame
ids <- 1:10
ids
idnames <- paste("Name", ids, sep="")
idnames
students <- data.frame(ids, idnames)
students
class(students$ids)
class(students$idnames)
students$idnames

students <- data.frame(ids, idnames, stringsAsFactors = F)
class(students$idnames)
students$idnames
students[1,]

## data frame indexing 
students$ids
students[,1]
students[,"ids"]


## list
parent_names <- c("Fred", "Mary")
number_of_children <- 2
child_ages <- c(4, 7, 9)
data.frame(parent_names, number_of_children, child_ages)
lst <- list(parent_names, number_of_children, child_ages)
lst[1]
lst[[1]]
class(lst[1])
class(lst[[1]])
lst[[1]][1]
lst[[1]][c(1,2)]




## text file write / read
getwd()
dir()

x <- c(1,2,3,4)
y <- c(5,6,7,8)
xy<-data.frame(x=x, y=y)

write.table(xy, file="table_write.txt")
write.table(xy, file="table_write.txt", quote=F)
write.table(xy, file="table_write.txt", quote=F, row.names=F)
write.table(xy, file="table_write.txt", quote=F, row.names=F, sep=",")
write.table(xy, file="table_write.csv", quote=F, row.names=F, sep=",")


mydata<-read.table(file="table_write.csv")
mydata<-read.table(file="table_write.csv", sep=",")
mydata<-read.table(file="table_write.csv", sep=",", header=T)
plot(mydata$x, mydata$z)




## readxl package
install.packages(readxl)
library(readxl)
setwd("C:/Rprog/03")
dir()

mydata <- read_excel("Rprog04-fl.xls", sheet=2, skip = 6, col_names=F)
#tibble 
# never changes type of the inputs
# never changes name of variables
# never create rownames

myod <- as.data.frame(mydata[1:8, ])
mygfp <- as.data.frame(mydata[12:21, ])

class(myod[,2])

## 
myod[,1] <- as.numeric(myod[,1])
mygfp[,1] <- as.numeric(mygfp[,1])


# OD
myod_treat <- myod[2:4,]
myod_control <- myod[5:7,]

sample_names <- paste("Sample", c(1:12), sep="")
replicate_labels <- paste("Rep", c(1:3), sep="")

rownames(myod_treat) <- replicate_labels
colnames(myod_treat) <- sample_names
rownames(myod_control) <- replicate_labels 
colnames(myod_control) <- sample_names


mean_treat <- colMeans(myod_treat)
mean_control <- colMeans(myod_control)

plot(mean_treat, type="h")
barplot(mean_treat, ylim=c(0,1))

mean_test <- data.frame(mean_treat, mean_control)
barplot(t(mean_test), ylim=c(0,1), beside=T)



## exercise 3-2 
mysd <- function(x){
  xmean <- sum(x)/length(x)
  tmpdif <- x-xmean
  xvar <- sum(tmpdif^2)/(length(x)-1)
  xsd <- sqrt(xvar)
  return(xsd)
}

x <- sample(1:100, 10)
mysd(x)
sd(x)


## apply
apply(myod_control, 1, mean)
apply(myod_control, 2, mean)

apply(myod_control, 2, function(x){ 
  xmean <- mean(x) 
  return(xmean)
  })

apply(myod_control, 2, sd)
apply(myod_control, 2, mysd)



## Exercise 3-3) barplot with sd

control_mean <- apply(myod_control, 2, function(x){mean(x)})
control_sd <- apply(myod_control, 2, mysd)

barplot(control_mean, width=0.83, space=0.2, col="gray", ylim=c(0,1))

arrows(0.5, control_mean[1], 0.5, control_mean[1]+control_sd[1], length=0.1, angle=90)
arrows(0.5, control_mean[1], 0.5, control_mean[1]-control_sd[1], length=0.1, angle=90)
lab <- paste("SD:", round(control_mean[1]+control_sd[1],1))
text(0.5, control_mean[1]+control_sd[1]+0.05, labels = lab)

arrows(1.5, control_mean[2], 1.5, control_mean[2]+control_sd[2], length=0.1, angle=90)
arrows(1.5, control_mean[2], 1.5, control_mean[2]-control_sd[2], length=0.1, angle=90)
lab <- paste("SD:", round(control_mean[2]+control_sd[2],1))
text(1.5, control_mean[2]+control_sd[2]+0.05, labels = lab)



## Exercise 3-4) barplot with sd using for
barplot(control_mean, width=0.83, space=0.2, col="gray", ylim=c(0,1))
for(i in 1:length(control_mean)){
  xpos <- 0.5+i-1
  arrows(xpos, control_mean[i], xpos, control_mean[i]+control_sd[i], length=0.1, angle=90)
  arrows(xpos, control_mean[i], xpos, control_mean[i]-control_sd[i], length=0.1, angle=90)
  lab <- paste("SD:", round(control_mean[i]+control_sd[i],1))
  text(xpos, control_mean[i]+control_sd[i]+0.05, labels = lab)
}


mean_comparison <- function(filename, plot_flag){
  # read excel file
  # get case control matrix
  # compute mean, sd 
  # barplot
  # return difference
}











plot(0, xlim=c(0, 6), ylim=c(0, 6), cex=5, pch=0, type="n")
polygon(x=c(1,1,2,2), y=c(1,2,2,1), col = "black")
points(1,1,pch=0)
#box(lty = '1373', col = 'red')


mymat <- matrix(0, 5, 5)
plot(x=c(1,2),y=c(1,1), xlim=c(0, 6), ylim=c(0, 6), cex=5, pch=0)
x_coord <- c(1,1,1)
y_coord <- c(1,2,3)
plot(x=x_coord,y=y_coord, xlim=c(0, 11), ylim=c(0, 11), cex=3, pch=0)


run_fun <- function(n){
  for(i in 1:n){
    plot(x=i, y=10, ylim=c(0,20), xlim=c(0,n))
    Sys.sleep(0.1)
  }
}

run_fun(100)



### 