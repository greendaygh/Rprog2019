# ���� ���丮 ���� 
setwd("C:\\Rprog\\01")
getwd()   

# ����
2+2; 2/2 
log((2+2)*10)
a <- 2
a*a

# ������ ������ �� ��
x <- 2
x <- 2 > 12
x <- c(1:10)
x <- 1:10

# �ռ� ���Ϳ��� 
mean(x)
mean(x + 1)
y <- c("20190306", "control", "rep1")
paste(y, collapse="-")

# �ε��� �̿��� �� ���
y[1]


# �ǽ�  
# 1. generate x with values of 1 to 20
# 2. extract 7 to 20 from x and assign them y variable
# 3. mean of y (all values in y variable)


# �ε��� ��� �� �̸� ���
fruit <- c(5, 10, 15, 20)
names(fruit) <- c("A", "B" , "C", "D")
fruit[c("A", "B")]
fruit[c(1,2)]

# �������� vs ������
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









