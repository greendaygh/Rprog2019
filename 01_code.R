# 현재 디렉토리 설정 
setwd("C:\\Rprog\\01")
getwd()   

# 계산기
2+2; 2/2 
log((2+2)*10)
a <- 2
a*a

# 변수에 숫자형 값 저
x <- 2
x <- 2 > 12
x <- c(1:10)
x <- 1:10

# 합수 백터연산 
mean(x)
mean(x + 1)
y <- c("20190306", "control", "rep1")
paste(y, collapse="-")

# 인덱스 이용한 값 사용
y[1]


# 실습  
# 1. generate x with values of 1 to 20
# 2. extract 7 to 20 from x and assign them y variable
# 3. mean of y (all values in y variable)


# 인덱스 대신 값 이름 사용
fruit <- c(5, 10, 15, 20)
names(fruit) <- c("A", "B" , "C", "D")
fruit[c("A", "B")]
fruit[c(1,2)]

# 전역변수 vs 지역변
# gloal variable x
x <- 1:5
print(x)
my_mean <- function(x){
  # local variable x
  x <- x + 1
  print(x)
  y <- sum(x)/length(x)
  return(y)
}
z <- my_mean(x)










