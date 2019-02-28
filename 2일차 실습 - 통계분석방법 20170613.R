# 디렉토리 체크
getwd()
dir() # data file이 있는지 체크

# 데이터 입력
# install.packages("readxl") # 패키지가 없는 경우 먼저 실해
library("readxl")
data <- as.matrix(read_excel("movie_dataset.xls"))

# 데이터 확인 
head(data)
tail(data)
summary(data)

##Exercise 1
##Q1 What percentage of all respondents attended at least 1 movie in the past year?
x=data[,2]
n = nrow(data)
(sum(x)/n ) * 100

##Q2 What percentage of all respondents never buy food items at a movie
x=data[,5]
head(x)
table(x)
n = nrow(data)
l=length(subset(x,x==0))
(l/n) * 100


##Q3 What percentage of respondents considers the "newspaper" very important source of information about movies playing at movie theaters?
x=data[,16]
head(x)
#NA를 지워주는 명령문
x=na.omit(x) 
n=length(x)                     
result = length(subset(x,x==4))
(result/n)*100

##Q4 What percentage of respondents considers the "internet" very unimportant source of information about movies playing at movie theaters?
x=data[,17]
head(x)
x=na.omit(x)
n=length(x)                     
result = length(subset(x,x==1))
(result/n)*100

#---------------------------------------------------------------------------------------------------------#

##Exercise 2

##Q1 What % of males does not attend movies at movie theaters?

n_male = sum(data[,'Q12']==0);
n_male_not_moive = sum(data[,'Q12']==0 & data[,'Q1']==0)

n_male_not_moive/n_male*100

##Q2 What % of all respondents are African American and do not attend movies at movie theaters?
n_af_not_movie = sum(data[,'Q11']==1 & data[,'Q1']==0)
n_af_not_movie/ nrow(data)*100


##Q3 What % of respondents not attending movies at movie theaters is in the 19 - 20 age category?
n_not_movie = sum(data[,'Q1']==0)
n_not_movie_20 = sum(data[,'Q14']==2 & data[,'Q1']==0)
n_not_movie_20/n_not_movie*100



#---------------------------------------------------------------------------------------------------------#

##Exercise 3
#성별에 따른 각각의 정보 source에 유의미한 차이가 발생하는가?
#null hypothesis = 두집단의 평균차이가 없다.
#alternative hypothesis = 두집단의 평균차이가 있다.
#p-value가 0.05보다 작다면 null hypothesis를 기각한다. = 두집단의 평균차이가 존재한다.


##Q1 The newspaper (Q7a)?

gender <- data[,'Q12']
response7a <- data[,'Q7a']
t.test(response7a~gender)


##Q2 The Internet (Q7b)?

response7b <- data[,'Q7b']
t.test(response7b~gender)


##Q3 Phoning in to the movie theater for information (Q7c)?

response7c <- data[,'Q7c']
t.test(response7c~gender)


##Q4 The television (Q7d)?

response7d <- data[,'Q7d']
t.test(response7d~gender)


##Q5 Friends or family (Q7e)?

response7e <- data[,'Q7e']
t.test(response7e~gender)


#---------------------------------------------------------------------------------------------------------#

##Exercise 4: ANOVA
#학년별로 차이가 발생하는지 아닌지를 가설검증한다. 
#null hypothesis는 Q13간의 차이가 없다는 것이다. p-value값이 작으면 기각이 가능하다.

##Q1 Video arcade at the movie theater (Q5a)?       

y1 <- data[,'Q5a']
group <- data[,'Q13']
group <- factor(group)
summary(aov(y1~group))
aggregate(y1~group,FUN=mean)

##Q2 Soft drinks and food items (Q5b)?   

y2 <- data[,'Q5b']
group <- factor(group)
summary(aov(y2~group))
aggregate(y2~group,FUN=mean)


##Q3 Plentiful restrooms (Q5c)?   

y3 <- data[,'Q5c']
group <- factor(group)
summary(aov(y3~group))
aggregate(y3~group,FUN=mean)


##Q4 Comfortable chairs (Q5d)?   

y4 <- data[,'Q5d']
group <- factor(group)
summary(aov(y4~group))
aggregate(y4~group,FUN=mean)


##Q5 Auditorium-type seating (Q5e)?  

y5 <- data[,'Q5e']
group <- factor(group)
summary(aov(y5~group))
aggregate(y5~group,FUN=mean)


##Q6 Size of the movie theater screen (Q5f)? 

y6 <- data[,'Q5f']
group <- factor(group)
summary(aov(y6~group))
aggregate(y6~group,FUN=mean)


##Q7 Quality of the sound system (Q5g)?    

y7 <- data[,'Q5g']
group <- factor(group)
summary(aov(y7~group))
aggregate(y7~group,FUN=mean)


##Q8 Number of screens at the movie theater (Q5h)? 

y8 <- data[,'Q5h']
group <- factor(group)
summary(aov(y8~group))
aggregate(y8~group,FUN=mean)



##Q9 Clean restrooms (Q5i)? 

y9 <- data[,'Q5i']
group <- factor(group)
summary(aov(y9~group))
aggregate(y9~group,FUN=mean)


#---------------------------------------------------------------------------------------------------------#

##Exercise 5
# Bivariate Regression

# Model 1: Q3 and Q5d

y <- data[,'Q3']
x <- data[,'Q5d']
summary(lm(y~x))

# Model 2: Q3 and Q5e

y <- data[,'Q3']
x <- data[,'Q5e']
summary(lm(y~x))

# Model 3: Q3 and Q7a

y <- data[,'Q3']
x <- data[,'Q7a']
summary(lm(y~x))

# Model 4: Q3 and Q7b

y <- data[,'Q3']
x <- data[,'Q7b']
summary(lm(y~x))

# Model 5: Q3 and Q7c

y <- data[,'Q3']
x <- data[,'Q7c']
summary(lm(y~x))

# Model 6: Q3 and Q9

y <- data[,'Q3']
x <- data[,'Q9']
summary(lm(y~x))

# Model 7: Q3 and Q10

y <- data[,'Q3']
x <- data[,'Q10']
summary(lm(y~x))





