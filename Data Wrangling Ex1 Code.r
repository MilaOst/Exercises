#import file
r=read.csv('C:\\Users\\lyudm\\Documents\\Springboard\\Ex\\refine_original.csv')

install.packages("dplyr")
library(dplyr)

toys=tbl_df(r)

#summarise distinct company names
toys %>%
 group_by(company) %>%
 summarise(company_count=n())

#1
#clean up company names
toys_new=toys %>%
mutate(company=replace(company,company %in% c("Akzo","AKZO","akz0","ak zo"),"akzo")) %>%
mutate(company=replace(company,company %in% c("fillips","phillips","phillipS","Phillips","phillps","phlips","phllips"),"philips")) %>%
mutate(company=replace(company,company %in% c("Unilever","unilver"),"unilever")) %>%
mutate(company=replace(company,company %in% c("van Houten","Van Houten"),"van houten")) %>%
as.data.frame()

#summarise cleaned company names
toys_new %>%
 group_by(company) %>%
 summarise(company_count=n())
 
install.packages("tidyr")
library(tidyr) 

#2 
#separate product code and number 
toys_new=separate(toys_new, Product.code...number, c("product_code", "product_number"), sep = "-")


#3
#create new column: product_category
toys_new=mutate(toys_new,product_category=product_code)


#replace product codes with categories
toys_new=toys_new %>%
mutate(product_category=replace(product_category, product_category=="p","Smartphone")) %>%
mutate(product_category=replace(product_category, product_category=="v","TV")) %>%
mutate(product_category=replace(product_category, product_category=="x","Laptop")) %>%
mutate(product_category=replace(product_category, product_category=="q","Tablet")) %>%
as.data.frame()

#4
#add full address
toys_new=toys_new %>%
unite(full_address,address,city,country,sep=",", remove=FALSE)

#5
#create dummy variables for company
toys_new=toys_new %>%
mutate(company_philips=ifelse(company=="philips",1,0)) %>%
mutate(company_akzo=ifelse(company=="akzo",1,0)) %>%
mutate(company_van_houten=ifelse(company=="van houten",1,0)) %>%
mutate(company_unilever=ifelse(company=="unilever",1,0)) 

#create dummy variables for product category
toys_new=toys_new %>%
mutate(product_smartphone=ifelse(product_category=="Smartphone",1,0)) %>%
mutate(product_tv=ifelse(product_category=="TV",1,0)) %>%
mutate(product_laptop=ifelse(product_category=="Laptop",1,0)) %>%
mutate(product_tablet=ifelse(product_category=="Tablet",1,0))

#save results into file
write.csv(toys_new,file='C:\\Users\\lyudm\\Documents\\Springboard\\Ex\\refine_clean.csv')
