
library(dplyr)
#1. Which store has maximum sales?

#Approach:
----------
#a. Group data by store name
#b. Sum up the weekly sales for each store
#c. Arrange the data in desc order so as to get the highest sales store
#d. Fetch the first column first row as it will hold the store having max sales.

# R Code:
---------
Walmart.data<-read.csv('Walmart_Store_sales.csv')
colnames(Walmart.data)
max_sales.data<-Walmart.data %>%
  group_by(Store) %>%
  summarize(totalSales = sum(Weekly_Sales),) %>%
  arrange(desc(totalSales))
max_sales.data
Store_with_max_sales<-max_sales.data[,c(1)][1,1]
Store_with_max_sales

# #Output of code: 
# ----------------
# > Store_with_max_sales
# # A tibble: 1 x 1
# Store
# <int>
#   1    20
#   > max_sales.data
# # A tibble: 45 x 2
# Store totalSales
# <int>      <dbl>
#   1    20 301397792.
# 2     4 299543953.
# 3    14 288999911.
# 4    13 286517704.
# 5     2 275382441.
# 6    10 271617714.
# 7    27 253855917.
# 8     6 223756131.
# 9     1 222402809.
# 10    39 207445542.
# # ... with 35 more rows

#Conclusion:
------------
  #Store 20 has max sales in Walmart Store
  
#==========================================================================

#2. Which store has maximum standard deviation i.e., the sales vary a lot.
#Also, find out the coefficient of mean to standard deviation

#2.a - max std deviation

#Approach:
----------
  # a.Group the Walmart data by Store
  # b.Summarise the SD for Weekly Sales across Stores
  # c.Arrange this data in descencding order to get the store with max SD
  
#Code with R:
#------------
Walmart.data<-read.csv('Walmart_Store_sales.csv')
max_SDStores.data<-Walmart.data %>%
  group_by(Store) %>%
  summarize(max_SDdata = sd(Weekly_Sales),) %>%
  arrange(desc(max_SDdata))
max_SDStores.data
Store_with_max_SDdata<-max_SDStores.data[,c(1)][1,1]
Store_with_max_SDdata


#Output with R:
# --------------
#   > Store_with_max_SDdata
# # A tibble: 1 x 1
# Store
# <int>
#   1    14
# 
# > max_SDStores.data
# # A tibble: 45 x 2
# Store max_SDdata
# <int>      <dbl>
#   1    14    317570.
# 2    10    302262.
# 3    20    275901.
# 4     4    266201.
# 5    13    265507.
# 6    23    249788.
# 7    27    239930.
# 8     2    237684.
# 9    39    217466.
# 10     6    212526.
# # ... with 35 more rows

#2.b - coefficient of mean to standard deviation

# Approach:
# ---------
# a.Group the Walmart data by Store
# b.Summarise the SD to mean for Weekly Sales across Stores
# c.Display this data across stores

# Code with R:
# ------------
Walmart.data<-read.csv('Walmart_Store_sales.csv')
coeff.data<-Walmart.data %>%
  group_by(Store) %>%
  summarize(coeff_data = sd(Weekly_Sales)/mean(Weekly_Sales),)
coeff.data

#Output with R:
# ---------------
#   > coeff.data
# # A tibble: 45 x 2
# Store coeff_data
# <int>      <dbl>
#   1     1      0.100
# 2     2      0.123
# 3     3      0.115
# 4     4      0.127
# 5     5      0.119
# 6     6      0.136
# 7     7      0.197
# 8     8      0.117
# 9     9      0.127
# 10    10      0.159
# # ... with 35 more rows
#========================================================================================

#3. Which store/s has good quarterly growth rate in Q3'2012

# Approach:
--------
  # a.Format Date column of Walmart Data for easier manipulation
  # b.Filter data for third quarter for 2012 year...ie period from "2012-07-06" & 
  # upto "2012-09-28" (includes July, August and September)
  # c.Group this filtered data across Stores and summarise the sum of weekly sales.
  # d. Arrange them in descending order to get the store with max Quarterly sales
  
  # Code with R:
  # ------------
Walmart.data<-read.csv('Walmart_Store_sales.csv')
Walmart.data$Date<-as.Date(Walmart.data$Date,format="%d-%m-%Y")
third_Q.data<-filter(Walmart.data, Walmart.data$Date>="2012-07-06" & Walmart.data$Date<= "2012-09-28")
max_sales_Q.data<-third_Q.data %>%
  group_by(Store) %>%
  summarize(quarterly_sales = sum(Weekly_Sales),)  %>%
  arrange(desc(quarterly_sales))
max_sales_Q.data
max_sales_Q.data[1,1]

# 
# #Output :
# ---------
# > max_sales_Q.data[1,1]
# # A tibble: 1 x 1
# Store
# <int>
#   1     4

#   > max_sales_Q.data
# # A tibble: 45 x 2
# Store quarterly_sales
# <int>           <dbl>
#   1     4       27796792.
# 2    20       26891527.
# 3    13       26421259.
# 4     2       24303355.
# 5    10       23037259.
# 6    27       22307711.
# 7    14       21187561.
# 8    39       20715116.
# 9     1       20253948.
# 10     6       20167312.
# # ... with 35 more rows


#==========================================================================================

#4.Some holidays have a negative impact on sales. Find out holidays which have higher sales 
#than the mean sales in non-holiday season for all stores together
# 
# Approach:
# ---------
#   a.Filter Walmart data during non holidays
#   b.Group by store and calculate the mean of Weekly Sales for this filtered data
#   c.Find the mean value of the above non-holiday data
#   d.Filter Walmart data during Holidays.
#   e.Filter this holiday.data where the sales are more than mean val calculated above
#   f.Group this data by Date and lets consider only unique values
#   g. Create holiday vector and Date vector representing the Holiday Calender
#   h. For every unique date obtained in Step f, find the corresponding holiday
#      tagged to it from mapping of Holiday Calender
# 
# Code with R:
# ------------
Walmart.data<-read.csv('Walmart_Store_sales.csv')
non_holiday.data<-Walmart.data[Walmart.data$Holiday_Flag==0,]
group_NH.data<-non_holiday.data %>%
  group_by(Store) %>%
  summarize(mean_Sales = mean(Weekly_Sales),)
mean(group_NH.data$mean_Sales)
#OR
mean_val<-mean(non_holiday.data$Weekly_Sales)
mean_val
holiday.data<-Walmart.data[Walmart.data$Holiday_Flag==1,]
filtered_data<-holiday.data[holiday.data$Weekly_Sales >mean_val,]
filtered_data
arranged_data<-arrange(filtered_data,filtered_data$Weekly_Sales)
aranged_data
uniq_data<-unique(group_by(arranged_data,arranged_data$Date)[,c(2)])
uniq_data
uniq_data

uniq_data$Date<-as.Date(uniq_data$Date,format= "%Y-%m-%d")



Holiday_vec<-c(rep("SuperBowl",4),rep("LabourDay",4),rep("Thanksgiving",4) , rep("Christmas",4))
date_vec<-c("2010-02-12","2011-02-11","2012-02-10","2013-02-08","2010-09-10","2011-09-09","2012-09-07","2013-09-06","2010-09-26","2011-11-25","2012-11-23","2013-11-29","2010-12-31","2011-12-30","2012-12-28","2013-12-27")

comb_f<-data.frame(Holiday_vec, Date_vec)
colnames(comb_f)

for (value in as.numeric(uniq_data$Date)) 
{
  if (value %in% comb_f$Date_vec)
    print (value)
  print (comb_f$Holiday_vec[comb_f$Date_vec==value])
  
}
# Output:
# -------
# 
# Levels: Christmas LabourDay SuperBowl Thanksgiving

#==========================================================================================
#5.Provide a monthly and semester view of sales in units and give insights
# 
# Approach:
# ---------
#    a.Manipulate Date Objects in Walmart Data
#    b.Restructure the Date columns in terms of year value
#    c.Group the data by year and summarise the Sales and arrange in desc order
#    

#Yearly data

Walmart.data<-read.csv('Walmart_Store_sales.csv')
a<-as.Date(Walmart.data$Date,format="%d-%m-%Y")
Walmart.data$Date<-substring(a,1,4)
Yearly_data<-Walmart.data %>% group_by(Walmart.data$Date) %>% summarise(YearlySales=sum(Weekly_Sales))
arrange(Yearly_data, desc(Yearly_data$YearlySales))

#Conclusion: 2011 recorded max sales from Walmart Store

#Output with R:
# ---------------
#   `Walmart.data$Date` YearlySales
# <chr>                     <dbl>
#   1 2011                2448200007.
# 2 2010                2288886120.
# 3 2012                2000132859.
#Monthly Data

# 
# Approach:
# ---------
#    a.Manipulate Date Objects in Walmart Data
#    b.Restructure the Date columns in terms of year value
#    c.Group the data by year and summarise the Sales and arrange in desc order
#    
# 
# Code with R:
# ------------
Walmart.data<-read.csv('Walmart_Store_sales.csv')
a<-as.Date(Walmart.data$Date,format="%d-%m-%Y")
Walmart.data$Date<-substring(a,6,7)
Monthly_Data<-Walmart.data %>% group_by(Walmart.data$Date) %>% summarise(Monthly_sales=sum(Weekly_Sales))
arrange(Monthly_Data, desc(Monthly_Data$Monthly_sales))

#Conclusion: July Month always records the maximum sales in Walmart Store.
# 
# #Output with R:
# --------------
#   # A tibble: 12 x 2
#   `Walmart.data$Date` Monthly_sales
# <chr>                       <dbl>
#   1 07                     650000977.
# 2 04                     646859785.
# 3 06                     622629887.
# 4 08                     613090209.
# 5 03                     592785901.
# 6 10                     584784788.
# 7 09                     578761179.
# 8 12                     576838635.
# 9 02                     568727890.
# 10 05                     557125572.
# 11 11                     413015725.
# 12 01                     332598438.

#==========================================================================================     
#6. Statistical Model:
#For Store 1 - Build  prediction models to forecast demand

# Case a:
#Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010 
#(starting from the earliest date in order). 
#Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# 
# Approach:
# ---------
# a.Drop the columns having Date and Store (first two cols)
# b.Formulate the hypothesis taking CPI, Fuel Price and Unemployment against Sales
#- Model1
# c.Compute Linear regression model considering all variables against Sales -model2
# d.Compute Linear regression model considering CPI and Unemployment variables against Sales -model3
# e.Compute Linear regression model considering CPI .Unemployment ,Days against Sales -model4
# 
# Conclusion:
# -----------
#   Model 4 is the best as they are the most significant variables (CPI , Unemployment and Days)
# 
#   
###############################################################################
### Multiple Linear Regression Hypothesis testing with CPI , FuelPrice Unemployment
### against Weekly Sales  MODEL 1
###############################################################################

# y = b0 + b1x1 + b2x2 + b3x3

# Ho: There is no linear relationship between dependent and independent variables
# Ha: There is linear relationship between dependent and independent variables

# Ho: b1 = b2 = b3 = 0 or # Ho: b1 = b2 = b3 ====== bp = 0
# Ha: at least one b != 0
# if p-value = 2.2e-16 < alpha = 0.05 # Rej Ho
Walmart.data<-read.csv('Walmart_Store_sales.csv')
colnames(Walmart.data)
Walmart.data[,c("Store","Date")] <-NULL
colnames(Walmart.data)

model1 <- lm(Weekly_Sales ~ CPI+Unemployment+ Fuel_Price, data = Walmart.data)
summary(model1)

#Output with R:
# ---------------
#   > summary(model1)
# 
# Call:
#   lm(formula = Weekly_Sales ~ CPI + Unemployment + Fuel_Price, 
#      data = Walmart.data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -940435 -482015 -116582  392807 2817204 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1745656.7    79572.5  21.938   <2e-16 ***
#   CPI            -1696.9      188.8  -8.988   <2e-16 ***
#   Unemployment  -42859.2     3905.2 -10.975   <2e-16 ***
#   Fuel_Price    -19266.1    15440.6  -1.248    0.212    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 557800 on 6431 degrees of freedom
# Multiple R-squared:  0.02357,	Adjusted R-squared:  0.02312 
# F-statistic: 51.75 on 3 and 6431 DF,  p-value: < 2.2e-16

pvalue <- 2.2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej the null hypothesis and there is linear relationship between dependent and independent variables

###############################################################

# Ho: b1 = 0 ; CPI has no effect on Sales
# Ha: b1 != 0 ; CPI has effect on Sales

pvalue <- 2.2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej Ho, CPI has effect on Sales

###############################################################

# Ho: b2 = 0 ; Unemployment has no effect on Sales
# Ha: b2 != 0 ; Unemployment has effect on Sales

pvalue <- 2.2e-16
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Rej Ho, Unemployment has effect on Sales

###############################################################

# Ho: b3 = 0 ; Fuel Price has no effect on Sales
# Ha: b3 != 0 ; Fuel Price has effect on Sales

pvalue <- 0.212
alpha <-  0.05

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

#Conclusion : Do not Rej Ho, Fuel Price has no effect on Sales

###############################################################

# Model Evaluation Technique - R-Squared Value

Rsqd <- summary(model1)$r.squared # explained variation of the model
Rsqd

# 0.023 = 2%

# Model Evaluation Technique - RMSE
predicted_sales1 <- predict(model1, Walmart.data)
predicted_sales1[1:10]

Walmart.data$Weekly_Sales[1:20]

(Walmart.data$Weekly_Sales - predicted_sales1)
RMSE = sqrt(mean((Walmart.data$Weekly_Sales - predicted_sales1)^2)) # root mean squared error
RMSE # 557631 
###############################################################################
# Correlation - is the measure for linear relationships
###############################################################################

cor(Walmart.data$CPI, Walmart.data$Weekly_Sales) # -0.07263416
cor(Walmart.data$Unemployment, Walmart.data$Weekly_Sales) # -0.1061761
cor(Walmart.data$Fuel_Price, Walmart.data$Weekly_Sales) # 0.009463786


#============================MODEL 2- having all variables against Sales============================================
colnames(Walmart.data)
model2 <- lm(Weekly_Sales ~ ., data = Walmart.data)
summary(model2)
Rsqd2 <- summary(model2)$r.squared # explained variation of the model
Rsqd2 # 0.025
# 
# Output with R:
# -------------
#   > # > summary(model2)
#   > # 
#   > # Call:
#   > #   lm(formula = Weekly_Sales ~ ., data = Walmart.data)
#   > # 
#   > # Residuals:
#   > #   Min       1Q   Median       3Q      Max 
#   > # -1022429  -478555  -117266   397246  2800620 
#   > # 
#   > # Coefficients:
#   > #   Estimate Std. Error t value Pr(>|t|)    
#   > # (Intercept)  1726523.4    79763.5  21.646  < 2e-16 ***
#   > #   Holiday_Flag   74891.7    27639.3   2.710  0.00675 ** 
#   > #   Temperature     -724.2      400.5  -1.808  0.07060 .  
#   > # Fuel_Price    -10167.9    15762.8  -0.645  0.51891    
#   > # CPI            -1598.9      195.1  -8.194 3.02e-16 ***
#   > #   Unemployment  -41552.3     3972.7 -10.460  < 2e-16 ***
#   > #   ---
#   > #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#   > # 
#   > # Residual standard error: 557400 on 6429 degrees of freedom
#   > # Multiple R-squared:  0.02544,	Adjusted R-squared:  0.02469 
#   > # F-statistic: 33.57 on 5 and 6429 DF,  p-value: < 2.2e-16

#============================MODEL 3 having Significant variables against Sales============================================
colnames(Walmart.data)
model3 <- lm(Weekly_Sales ~ CPI+Unemployment, data = Walmart.data) # dependent variable ~ independent variables
summary(model3)

Rsqd3 <- summary(model3)$r.squared # explained variation of the model
Rsqd3 # 0.023
# 
# Output with R:
# -------------
#   > summary(model3)
# 
# Call:
#   lm(formula = Weekly_Sales ~ CPI + Unemployment, data = Walmart.data)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -933101 -482122 -114908  393270 2820581 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1669687.8    51235.8  32.588   <2e-16 ***
#   CPI            -1652.1      185.4  -8.913   <2e-16 ***
#   Unemployment  -42411.9     3888.9 -10.906   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 557800 on 6432 degrees of freedom
# Multiple R-squared:  0.02334,	Adjusted R-squared:  0.02303 
# F-statistic: 76.84 on 2 and 6432 DF,  p-value: < 2.2e-16
#============================MODEL 4 having Significant variables and Days against Sales============================================
install.packages('lubridate', dependencies = T)
library(lubridate)
Walmart.data<-read.csv('Walmart_Store_sales.csv')
colnames(Walmart.data)
date <- dmy(Walmart.data$Date)
date
days <- yday(date) - 35# 
days
head(days)
Walmart1.Data<-mutate(Walmart.data, days=days)
head(Walmart1.Data)
colnames(Walmart1.Data)
Walmart1.Data[,c("Store","Date")] <-NULL
model4 <- lm(Weekly_Sales ~ CPI + Unemployment + days, data = Walmart1.Data) # dependent variable ~ independent variables
summary(model4)

#Output with R:
# #--------------
# > summary(model4)
# 
# Call:
#   lm(formula = Weekly_Sales ~ CPI + Unemployment + days, data = Walmart1.Data)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1008726  -473481  -113306   398978  2746739 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1606993.25   52181.17  30.796  < 2e-16 ***
#   CPI            -1653.50     184.87  -8.944  < 2e-16 ***
#   Unemployment  -42115.41    3878.90 -10.858  < 2e-16 ***
#   days             415.50      70.03   5.933 3.13e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 556400 on 6431 degrees of freedom
# Multiple R-squared:  0.02865,	Adjusted R-squared:  0.0282 
# F-statistic: 63.23 on 3 and 6431 DF,  p-value: < 2.2e-16
#Conclusion : model 4 is the best ---> CPI Unemployment and days are significant
#=========================================================================================

#Case b:
#Change dates into days by creating new variable. 
# 
# #Approach:
# ----------
#   a.USe lubridate library 
#   b. Set start date since there are 35 days in all up till Feb 5th 2010(this is start date)
#   c. Add a new column holding days value

# Code with R:
# ------------
install.packages('lubridate', dependencies = T)
library(lubridate)
Walmart.data<-read.csv('Walmart_Store_sales.csv')
colnames(Walmart.data)
date <- dmy(Walmart.data$Date)
date
days <- yday(date) - 35# 
days
head(days)
Walmart1.Data<-mutate(Walmart.data, days=days)
head(Walmart1.Data)

# 
# Output with R:
# --------------

# > head(Walmart1.Data)
# Weekly_Sales Holiday_Flag Temperature Fuel_Price      CPI Unemployment days
# 1      1643691            0       42.31      2.572 211.0964        8.106    1
# 2      1641957            1       38.51      2.548 211.2422        8.106    8
# 3      1611968            0       39.93      2.514 211.2891        8.106   15
# 4      1409728            0       46.63      2.561 211.3196        8.106   22
# 5      1554807            0       46.50      2.625 211.3501        8.106   29
# 6      1439542            0       57.79      2.667 211.3806        8.106   36

#=========================================================================================
