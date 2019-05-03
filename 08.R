

library(dplyr)
library(ggplot2)
head(iris)
str(iris)


df <- data.frame(x=rnorm(100)+1, y=rnorm(100)+3)
hist(df$x, br=50, xlim=c(-4,8))
hist(df$y, br=50, xlim=c(-4,8))
hist(c(df$x, df$y), br=100, xlim=c(-4,8))



hist(df[,1], br=50, xlim=c(-4,8))
hist(df[,2], br=50, xlim=c(-4,8), add=T)

hist(df[,1], br=50, xlim=c(-4,8), col="blue")
hist(df[,2], br=50, xlim=c(-4,8), col="red", add=T)

hist(df[,1], br=50, xlim=c(-4,8), col="#0000ff55")
hist(df[,2], br=50, xlim=c(-4,8), col="#ff000055", add=T)


std_x <- (df$x-mean(df$x))/sd(df$x)
std_y <- (df$y-mean(df$y))/sd(df$y)
std_df <- data.frame(std_x, std_y)

dfall <- mutate(df, 
                std_x=(x-mean(x))/sd(x), 
                std_y=(y-mean(y))/sd(y))
std_df <- select(dfall, std_x, std_y)

std_df <- df %>% 
  mutate(std_x=(x-mean(x))/sd(x), std_y=(y-mean(y))/sd(y)) %>%
  select(std_x, std_y)

library(ggplot2)


ggplot(df, aes(x=x)) +
  geom_bar(stat="bin")





ggplot(df, aes(x=x))+
  geom_bar(stat="bin") +
  scale_x_continuous(limits=c(-4,8))

ggplot(df, aes(x=y))+
  geom_bar(stat="bin") +
  scale_x_continuous(limits=c(-4,8))




ggplot(df)+
  geom_bar(aes(x=x), fill="blue", colour="black", stat="bin") +
  geom_bar(aes(x=y), fill="red", colour="black", stat="bin")

p1 <- ggplot(df)
p1 <- p1 + geom_bar(aes(x=x), fill="blue", colour="black", stat="bin")
p1 <- p1 + geom_bar(aes(x=y), fill="red", colour="black", stat="bin")

##

head(iris)
group_by(iris, Species)

?summarize




ggplot(df)+
  geom_bar(aes(x=x), stat="bin", fill="blue") +
  geom_bar(aes(x=y), stat="bin", fill="red") +
  scale_x_continuous(limits=c(-4,8))


ggplot(df)+
  geom_bar(aes(x=x), stat="bin", fill="#0000ff55") +
  geom_bar(aes(x=y), stat="bin", fill="#ff000055") +
  scale_x_continuous(limits=c(-4,8))


ggplot(df)+
  geom_bar(aes(x=x), stat="bin", fill="#0000ff55") +
  geom_bar(aes(x=y), stat="bin", fill="#ff000055") +
  scale_x_continuous(limits=c(-4,8))


## sepal length
iris_grouped <- group_by(iris, Species) 
iris_grouped_summary <- summarize(iris_grouped, 
                                  length_mean=mean(Sepal.Length), 
                                  length_sd=sd(Sepal.Length))

#iris_grouped <- iris %>% group_by(Species) 
iris_grouped_summary <- iris %>% 
  group_by(Species) %>% 
  summarize(length_mean=mean(Sepal.Length), 
            length_sd=sd(Sepal.Length))


ggplot(iris_grouped_summary, aes(x=Species, y=length_mean)) +
  geom_bar(stat="identity")



## melt 
library(reshape2)

class(df)
?melt

df_melt <- melt(df)
ggplot(df_melt, aes(x=value, fill=variable)) + 
  geom_bar(stat="bin")


## all
library(reshape2)


tmp1 <- group_by(iris, Species) %>%  summarise_all(mean) %>% melt
ggplot(tmp, aes(x=Species, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge")


tmp1 <- group_by(iris, Species) %>%  summarise_all(mean) %>% melt(value.name=c("mean"))
tmp2 <- group_by(iris, Species) %>%  summarise_all(sd) %>% melt(value.name = c("sd"))
tmp <- inner_join(tmp1, tmp2)

ggplot(tmp, aes(x=Species, y=mean, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(min=mean-sd, max=mean+sd),
                position="dodge")

##


iris_grouped <- group_by(iris, Species) 
iris_grouped_summary <- summarize_all(iris_grouped, mean) # summarize_at summarize_if ...

iris_grouped <- iris %>% group_by(Species) 
iris_grouped_summary <- iris %>% group_by(Species) %>% summarize_all(mean)


ggplot(iris_grouped_summary, aes(x=Species)) +
  geom_bar(aes(y=Sepal.Length), stat="identity")


#iris_grouped_summary2 <- iris_grouped_summary %>% cbind(group_id=rep("1", 3))


iris_grouped_summary2 <- melt(iris_grouped_summary)
?melt.data.frame

ggplot(iris_grouped_summary2, aes(x=Species, y=value, group=variable)) +
  geom_bar(stat="identity", position="dodge") 

ggplot(iris_grouped_summary2, aes(x=Species, y=value, group=variable, fill=variable)) +
  geom_bar(stat="identity", position="dodge") 


## sd
iris_grouped <- iris %>% group_by(Species) 
iris_grouped_mean <- iris_grouped %>% summarize_all(mean) %>% melt(value.name="mean")
iris_grouped_sd <- iris_grouped %>% summarize_all(sd) %>% melt(value.name="sd")
iris_final <- inner_join(iris_grouped_mean, iris_grouped_sd, by=c("Species", "variable"))

ggplot(iris_final, aes(x=Species, y=mean, fill=variable)) +
  geom_bar(stat="identity", position="dodge")
  
ggplot(iris_final, aes(x=Species, y=mean, fill=variable)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(0.9)) 

p1 <- ggplot(iris_final, aes(x=Species, y=mean, fill=variable)) +
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

p2 <- p1 + scale_fill_hue(h = c(0, 360)) +
  ylab("Length/width mean") +
  xlab("Iris species") +
  labs(title = "IRIS Comparison", subtitle="Three types of iris", caption="Data source: xxx")

p2 <- p1 + scale_fill_hue(h = c(0, 360)) +
  ylab("Length/width mean") +
  xlab("Iris species") +
  labs(title = "IRIS Comparison", subtitle="Three types of iris", caption="Data source: xxx", fill="")

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

save(mydata2, file="mydata2.Rdata")
load(mydata2)

ggplot(data=mydata2, aes(x=sample_names, y=GFP, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  scale_fill_manual(values = heat.colors(11))


head(mydata2)
grouped_data <- group_by(mydata2, sample_names, drugname, concentration)
grouped_data_mean <- summarize(grouped_data, gfp_mean=mean(GFP))
ggplot(grouped_data_mean, aes(x=sample_names, y=gfp_mean, fill=concentration)) +
  geom_bar(stat="identity", position="dodge", color="black") + 
  scale_fill_manual(values = heat.colors(11))




