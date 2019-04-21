


##
str(mydata2)

x <- rnorm(100)
hist(x, br=10)

x <- sample(1:3, 100, replace = T)
y <- table(x)
barplot(y)

x <- rnorm(10)
y <- rnorm(10)
plot(x, y, type="h")

x <- 1:3
y <- table(sample(x, 100, replace = T))
barplot(y)


## =======
dat <- data.frame(x1=rnorm(100))
ggplot(dat, aes(x=x1)) +
  geom_bar(stat="bin")


x <- sample(1:3, 100, replace = T)
dat <- data.frame(x=factor(x))
ggplot(dat, aes(x=x)) +
  geom_bar(stat="count")


x <- rnorm(10)
y <- rnorm(10)
dat <- data.frame(x, y)
ggplot(dat, aes(x=x, y=y)) +
  geom_bar(stat="identity")


x1 <- as.factor(1:3)
y1 <- tabulate(sample(x1, 100, replace=T))
dat <- data.frame(x1, y1)
ggplot(dat, aes(x=x1, y=y1)) +
  geom_bar(stat="identity") 

ggplot(dat, aes(x=x1, y=y1, fill=x1)) +
  geom_bar(stat="identity") 

ggplot(dat, aes(x=x, y=y, fill=x)) +
  geom_bar(stat="identity") +
  guides(fill=FALSE)


ggplot(dat, aes(x=x, y=y, fill=x)) +
  geom_bar(stat="identity") +
  guides(fill=FALSE) +
  xlab("Discrete cases") + 
  ylab("Value") +
  ggtitle("Barplot for x:discrete and y:value") 


ggplot(dat, aes(x=x, y=y, fill=x)) +
  geom_bar(stat="identity") +
  guides(fill=FALSE) +
  xlab("Discrete cases") + 
  ylab("Value") +
  ylim(c(0,50))+
  ggtitle("Barplot for x:discrete and y:value")

## line graph

ggplot(dat, aes(x=x, y=y, fill=x, group=1)) +
  geom_line(stat="identity") +
  guides(fill=FALSE) +
  xlab("Discrete cases") + ylab("Value") +
  ylim(c(0,50))+
  ggtitle("Barplot for x:discrete and y:value")



ggplot(dat, aes(x=x, y=y, group=1)) +
  geom_line(size=2) +
  geom_point(size=4, pch=21, fill="white") +
  guides(fill=FALSE) +
  xlab("Discrete cases") + ylab("Value") +
  ylim(c(0,50))+
  ggtitle("Barplot for x:discrete and y:value")


#### ====================


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

library(ggplot2)

ggplot(data=mydata, aes(x=sample_names, y=GFP))

ggplot(data=mydata, aes(x=sample_names, y=GFP)) +
  geom_bar()

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


ggplot(data=mydata2, aes(x=sample_names, y=GFP, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_manual(values = heat.colors(11))


tmp <- mydata2 %>% group_by(sample_names, replication, concentration) %>%  summarize(m=mean(GFP))
ggplot(tmp, aes(x=sample_names, y=m, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black")  

##


###
library(dplyr)
#library(plyr)

##
pi %>% sin
sin(pi)
pi %>% sin %>% cos
cos(sin(pi))

iris %>% head(10)
iris %>% str

round(pi, digits=6)
6 %>% round(pi, digits=.)




x <- 1:5
paste("1", letters[x], sep="")

x %>% paste("1", letters[.], sep="")


df <- data.frame(x=c(1:100), y=c(201:300))
df %>%  colMeans


df <- data.frame(x=c(1:100), y=c(201:300), )
colMeans(df)




iris %>% head(10)
iris %>% str

iris_split <- split(iris, iris$Species)
iris_means <- lapply(iris_split, function(x){colMeans(x[,1:4])})
iris_means_df <- data.frame(iris_means)

barplot(iris_means_df)
iris_means_df_sepal <- as.matrix(iris_means_df[1:2,])
barplot(iris_means_df_sepal)
barplot(iris_means_df_sepal, beside = T)
?barplot
barplot(iris_means_df_sepal, beside = T, legend.text=rownames(iris_means_df_sepal))



iris %>% 
  group_by(Species) %>% 
  summarise(sepal_mean=mean(Sepal.Length), sepal_width=mean(Sepal.Width))



iris_sepal <- iris %>% 
  group_by(Species) %>% 
  summarise(sepal_mean=mean(Sepal.Length), sepal_width=mean(Sepal.Width))

iris %>%  class
ggplot(iris_sepal, aes(x=Species, y=sepal_mean)) + 
  geom_bar(stat="identity")

iris %>% 
  group_by(Species) %>% 
  summarise(sepal_mean=mean(Sepal.Length), sepal_width=mean(Sepal.Width)) %>% 
  ggplot(aes(x=Species, y=sepal_mean)) + 
  geom_bar(stat="identity")



df <- data.frame(x=c(1:10), y=c(201:210))

x <- c(1:10)
y <- c(201:210)
value <- c(x, y)
variable <- c(rep("x", length(x)), rep("y", length(y)))
df <- data.frame(value, variable)

df %>% group_by(variable) %>% summarise(m=mean(value)) 



iris %>% 
  group_by(Species) %>% 
  summarise(sepal_mean=mean(Sepal.Length), sepal_width=mean(Sepal.Width)) %>%  
  melt %>% 
  ggplot(aes(x=Species, y=value, fill=variable)) + 
  geom_bar(stat="identity", position="dodge")



df <- ddply(mydata2, c("GFP", "replication"), summarize, Mean = mean(GFP), SD = sd(GFP))  

## change color
RColorBrewer::display.brewer.all()




