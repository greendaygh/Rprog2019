
## exercise 1
ages <- c(21, 55, 23, 53)
names(ages) <- c("John","James","Sara", "Lilly")

who <- function(input){
  greater_than_fifty <- which(input > 50)
  z <- names(greater_than_fifty)
  return(z)
}

who(ages)


## exercise 2
ages <- sample(1:100, 100)
names(ages) <- paste("X", 1:100, sep="_")

who <- function(input, cri){
  y <- which(input>cri)
  z <- names(y)
  return(z)
} 
who(ages, 50)
who(ages, 20)
who(ages, 30)


##
who_between <- function(input, cri1, cri2){
  y <- which(input>cri1 & input<cri2)
  z <- names(y)
  return(z)
} 
who_between(ages, 20, 30)


a20 <- who(ages, 20)
a30 <- who(ages, 30)

length(a20)
length(a30)



## exercise 3
group1 <- sample(1:100, 10)
group2 <- sample(1:100, 10)
group3 <- sample(1:100, 10)
group4 <- sample(1:100, 10)
group5 <- sample(1:100, 10)

group1+group2+group3
groupmat <- cbind(group1, group2, group3, group4, group5)




### 
x <- matrix(1:20, nrow=4, ncol=5)
x[1,3]
x[2,2]
x[3,1]

x[1,3] <- 0
x[2,2] <- 0
x[3,1] <- 0






