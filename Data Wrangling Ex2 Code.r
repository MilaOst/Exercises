#import file
t=read.csv('C:\\Users\\lyudm\\Documents\\Springboard\\Ex\\titanic_original.csv')

t=tbl_df(t)
View(t)

#summarise
t %>%
 group_by(embarked) %>%
 summarise(embarked_count=n())
 
#or
t %>%
 count(embarked)

#1 
#replace missing value in port of embarkation with S
t<-mutate(t,embarked=replace(embarked, embarked=="","S"))

#2
#calculate the mean of Age 
mean_age <- t %>%
summarise(mean=mean(!is.na(age)))
mean_age<-as.numeric(mean_age)

#substitue missing values with the mean
t<-t %>%
mutate(age=replace(age, is.na(age),mean_age)) 

#check if missing values exist
filter(t, is.na(age))

#calculate the new mean
t %>%
summarise(mean=mean(age))
#Other ways to fill the missing values: 1) a regression model with Age being a dependant variable and other variables (gender, pclass, fare, embarked, etc.) being independent varibales; 2)use median Age becasue it's less impacted by extreme values as compared to mean.

#3
#count missing values
t%>% 
count(boat=="")

#replace missing values with NA
t<-t %>%
mutate(boat=replace(boat, boat=="", NA)) 

#4
#It doesn't make sense to fill the missing cabin values here becasue not all passengers had a cabin. 
#count missing cabin values
t%>% 
count(cabin=="")

#replace missing values with 0 and not missing with 1
t<-t %>%
mutate(has_cabin_number=ifelse(cabin!="",1,0))

#save results into file
write.csv(t,file='C:\\Users\\lyudm\\Documents\\Springboard\\Ex\\titanic_clean.csv')
