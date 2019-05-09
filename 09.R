library(dplyr)
library(ggplot2)
head(iris)

str(iris)
tmp <- select(iris, Sepal.Length)
head(tmp)

# to select variables
iris_sepal <- select(iris, Sepal.Length, Sepal.Width)
head(iris_sepal)

# to select a variable and divide into groups
iris_sepal <- select(iris, Sepal.Length, Sepal.Width, Species)
iris_group <- group_by(iris_sepal, Species)
iris_group

# to get means 
iris_mean <- summarize(iris_group, 
                       mean(Sepal.Length), 
                       mean(Sepal.Width))
iris_mean

# to get means for all columns 
iris_mean <- summarize_all(iris_group, mean)
iris_mean

# to get standard deviations for all columns 
iris_sd <- summarize_all(iris_group, sd)
iris_sd

# join iris_mean and iris_sd with the same species 
iris_join <- inner_join(iris_mean, iris_sd, by="Species")
iris_join

mutate(iris_join, Sepal.Length.x+2)
colnames(iris_join) <- c("Species", 
                         "Sepal.Length.mean", 
                         "Sepal.Width.mean", 
                         "Sepal.Length.sd", 
                         "Sepal.Width.sd")

## use pipe 
iris_mean <- iris %>% 
  select(Sepal.Length, Sepal.Width, Species) %>% 
  group_by(Species) %>% 
  summarize_all(mean)



## example 9-1
barplot(iris_join$Sepal.Length.mean)
ggplot(iris_join, aes(x=Species, y=Sepal.Length.mean)) +
  geom_bar(stat="identity")

iris_mean <- iris %>% group_by(Species) %>% summarize_all(mean) 
iris_sd <- iris %>% group_by(Species) %>% summarize_all(sd) 



library(reshape2)
iris_melt <- melt(iris_join)

iris_mean <- iris %>% group_by(Species) %>% summarize_all(mean) %>% melt
iris_sd <- iris %>% group_by(Species) %>% summarize_all(sd) %>% melt
iris_join <- inner_join(iris_mean, iris_sd, by=c("Species", "variable"))
colnames(iris_join)[c(3,4)] <- c("mean", "sd")
iris_join

iris_mean <- iris %>% group_by(Species) %>% summarize_all(mean) %>% melt(value.name=c("mean"))
iris_sd <- iris %>% group_by(Species) %>% summarize_all(sd) %>% melt(value.name=c("sd"))
iris_join <- inner_join(iris_mean, iris_sd, by=c("Species", "variable"))
iris_join


## mean
ggplot(iris_join, aes(x=Species, y=mean, fill=variable)) + 
  geom_bar(stat="identity", position="dodge")

## errorbars
ggplot(iris_join, aes(x=Species, y=mean, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(min=mean-sd, max=mean+sd),
                position="dodge")


p1 <- ggplot(iris_join, aes(x=Species, y=mean, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.9)) 

## scale  
## aes 에서는 data를 어떤 변수로 맵핑해줌 
## 그러나 어떤 color로 어떤 모양으로 할지는 지시하지 않음 대부분 디폴트로 
## scale에서는  

p1 + scale_fill_brewer(palette = "Greens")

p1 + scale_fill_hue(h = c(0, 360))

p2 <- p1 + scale_fill_hue(h = c(0, 360)) +
  scale_y_continuous(name="Length/width Mean") +
  scale_x_discrete(name="Iris species") 
p2

p2 <- p1 + scale_fill_hue(h = c(0, 360)) +
  ylab("Length/width Mean2") +
  xlab("Iris species2") +
  labs(title = "IRIS Comparison", subtitle="Three types of iris", caption="Data source: xxx", fill="Types")
p2

?scale_fill_hue

## theme
p2 + theme_bw()  
p2 + theme_gray()
p2 + theme_bw() +
  theme(
    text=element_text(size=14),
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=20),
    axis.title.x = element_text(size=20), 
    #legend.position = "bottom",
    legend.position = c(0.1,0.85),
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5)
  )






## save data
setwd("C:\\Rprog\\07")
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
mydata2 <- mydata
mydata2$concentration <- as.factor(mydata2$concentration)
str(mydata2)
head(mydata2)

#save(mydata2, file="mydata2.Rdata")
#load(mydata2)

ggplot(data=mydata2, aes(x=sample_names, y=GFP, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_manual(values = heat.colors(11))


grouped_data <- group_by(mydata2, sample_names, drugname, concentration)
data_mean <- summarize(grouped_data, gfp_mean=mean(GFP))
ggplot(data_mean, aes(x=sample_names, y=gfp_mean, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black") + 
  scale_fill_manual(values = heat.colors(11))



grouped_data <- group_by(mydata2, sample_names, drugname, concentration)
data_mean <- summarize(grouped_data, mean=mean(GFP))
data_sd <- summarize(grouped_data, sd=sd(GFP))
data_join <- inner_join(data_mean, data_sd, by=c("sample_names", "drugname", "concentration"))

ggplot(data_join, aes(x=sample_names, y=mean, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black") + 
  scale_fill_manual(values = heat.colors(11)) +
  geom_errorbar(aes(min=mean-sd, max=mean+sd), width=.2, position=position_dodge(0.9))
  

p1 <- ggplot(data_join, aes(x=sample_names, y=mean, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black") + 
  geom_errorbar(aes(min=mean-sd, max=mean+sd), width=.2, position=position_dodge(0.9))


p1 + scale_fill_hue(h = c(0, 360)) +
  ylab("GFP") +
  xlab("Concentration") +
  labs(title = "GFP profiles for drug xxx", 
       subtitle="6 samples and 11 concentrations", 
       caption="Data source: xxx", fill="Conc.") +
  theme_bw() +
  theme(
    text=element_text(size=14),
    axis.text.y = element_text(size=10),
    axis.title.y = element_text(size=20),
    axis.title.x = element_text(size=20), 
    plot.title=element_text(hjust=0.5),
    plot.subtitle=element_text(hjust=0.5)
  )



### ===========================

head(mydata2)

ggplot(mydata2, aes(x=concentration, y=GFP)) +
  geom_point()

ggplot(mydata2, aes(x=concentration, y=GFP, color=sample_names)) +
  geom_point()

ggplot(mydata2, aes(x=concentration, y=GFP, color=sample_names)) +
  geom_point()

ggplot(mydata2, aes(x=concentration, y=GFP, color=sample_names)) +
  geom_point()


