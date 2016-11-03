#install.packages("dplyr")
#install.packages("tidyr")
library(dplyr)
library(tidyr)

#part 0

data <- read.csv('/Users/Randy_Linder/Google Drive/Springboard/Foundations-of-Data-Science/refine_original.csv', header = T, sep = ",")
data <- data.frame(data)

# part 1

#Clean up the 'company' column, so all of the misspellings of the brand 
#names are standardized. For example, you can transform 
#the values in the column to be: philips, akzo, van houten and unilever (all lowercase).
#data %>% glimpse()

data <- data %>% mutate_each(funs(replace(., grepl('ill', .) | grepl('hlip', .) , 'philips')), company)
data <- data %>% mutate_each(funs(replace(., grepl('k z', .) | grepl('0', .) | grepl('A', .)  , 'akzo')), company)
data <- data %>% mutate_each(funs(replace(., grepl('V', .) | grepl('H', .) , 'van houten')), company)
data <- data %>% mutate_each(funs(replace(., grepl('ver', .) , 'unilever')), company)

# part 2

#Separate the product code and product number into separate columns i.e. 
#add two new columns called product_code and product_number, 
#containing the product code and number respectively

data <- data %>% separate(Product.code...number, c('product_code', 'product_number'), sep = '-')

# part 3
#You learn that the product codes actually represent the following product categories:
# p = Smartphone
# v = TV
# x = Laptop
# q = Tablet
#In order to make the data more readable, add a column with the product category 
#for each record.

prod_cat <- function(x) if(x == 'p') {'Smartphone'} else if(x == 'v'){'TV'} else if(x== 'x') {'Laptop'} else if(x =='q') {'Tablet'} 
data <- data %>% mutate(product_category = sapply(product_code, prod_cat))

# part 4

#You'd like to view the customer information on a map. In order to do that, 
#the addresses need to be in a form that can be easily geocoded. Create a new column 
#full_address that concatenates the three address fields (address, city, country), separated by commas.

data <- data %>% unite("full_address", address, city, country, sep = ',')

# part 5

#Both the company name and product category are categorical variables i.e. they take only a fixed set of values. 
#In order to use them in further analysis you need to create dummy variables. Create dummy binary variables for each of them with the 
#prefix company_ and product_ i.e.
#Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
#Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet


#binary_col_add <- function(data, col, val) {
# col_name <- paste0(col, '_binary')
#  data %>% mutate(col_name = as.numeric(col == val))
#}

data <- data %>% mutate(company_philips = as.numeric(company == 'philips'))
data <- data %>% mutate(company_akzo = as.numeric(company == 'akzo'))
data <- data %>% mutate(company_van_houten = as.numeric(company == 'van houten'))
data <- data %>% mutate(company_unilever = as.numeric(company == 'unilever'))
data <- data %>% mutate(product_smartphone = as.numeric(product_category == 'Smartphone'))
data <- data %>% mutate(product_tv = as.numeric(product_category == 'TV'))
data <- data %>% mutate(product_laptop = as.numeric(product_category == 'Laptop'))
data <- data %>% mutate(product_tablet = as.numeric(product_category == 'Tablet'))

#data %>% head()

# part 6 
#Include your code, the original data as a CSV file refine_original.csv, and the cleaned up data as a CSV file called refine_clean.csv

write.csv(data, 'refine_clean.csv')
