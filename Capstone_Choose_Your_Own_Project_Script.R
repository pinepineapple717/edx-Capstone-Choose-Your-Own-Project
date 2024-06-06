##########################################################
# Load libraries you need or may need at once
##########################################################
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(lubridate)) install.packages("lubridate")
if(!require(zoo)) install.packages("zoo")
if(!require(scales)) install.packages("scales")
if(!require(patchwork)) install.packages("patchwork")
if(!require(editData)) install.packages("editData")
if(!require(corrplot)) install.packages("corrplot")
if(!require(car)) install.packages("car")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(caret)) install.packages("caret")
if(!require(randomForest)) install.packages("randomForest")
if(!require(rsample)) install.packages("rsample")
if(!require(imputeTS)) install.packages("imputeTS")
if(!require(Boruta)) install.packages("Boruta")
if(!require(knitr)) install.packages("knitr")



library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(patchwork)
library(editData)
library(corrplot)
library(car)
library(ggrepel)
library(caret)
library(Boruta)
library(imputeTS)
library(randomForest)
library(rsample)
library(knitr)


################################
# Loading Data Sets
################################
df <- read.csv("Purchase_dataset.csv")


###############################
# Data Wrangling
###############################
# Confirmation of data types in each column
dim(df)
str(df)
summary(df)

# Identify more detailed data characteristics of the columns of interest
unique(df$Education)
unique(df$Marital_Status)
unique(df$Marital_Status)
unique(df$Z_CostContact)
unique(df$Z_Revenue)

# Rename columns for easier understanding
df <- df %>% rename("Education_Level" = Education,
                    "Kids_in_Family" = Kidhome,
                    "Teenager_in_Family" = Teenhome,
                    "Registration_Date" = Dt_Customer,
                    "Elapse_Date_from_Last_Purchase" = Recency,
                    "Purchase_Wines_past2years" = MntWines,
                    "Purchase_Fruits_past2years" = MntFruits,
                    "Purchase_Meat_past2years" = MntMeatProducts,
                    "Purchase_Fish_past2years" = MntFishProducts,
                    "Purchase_Snacks_past2years" = MntSweetProducts,
                    "Purchase_Golds_past2years" = MntGoldProds,
                    "Purchase_Number_in_Sales" = NumDealsPurchases,
                    "Web_Purchase" = NumWebPurchases,
                    "Store_Purchase" = NumStorePurchases,
                    "Catalog_Purchase" = NumCatalogPurchases,
                    "Web_visits_LastMonth" = NumWebVisitsMonth,
                    "Complains_past2years" = Complain)

# Remove columns that were not well understood by looking at the column descriptions on the dataset source page in Kaggle
df <- df %>% select(-Z_CostContact,-Z_Revenue)


# Convert the Complains past 2 years column to a logical type
df <- df %>% mutate(Complains_past2years = case_when(Complains_past2years == 1 ~ TRUE,
                                                     Complains_past2years == 0 ~ FALSE))


str_length(df$Registration_Date) %>% unique()
df$Registration_Year <- str_sub(df$Registration_Date,start = 7)

class(df$Registration_Year)
unique(df$Registration_Year)

df$Registration_Month <- str_sub(df$Registration_Date,start = 4, end = 5)

class(df$Registration_Month)
unique(df$Registration_Month)

df$Registration_Day <- str_sub(df$Registration_Date,start = 1, end = 2)

class(df$Registration_Day)
unique(df$Registration_Day)

df$Registration_Date <- str_c(df$Registration_Year,df$Registration_Month,df$Registration_Month, sep = "-") %>% 
  as.Date()

# Identify the base date in this data set
summary(df$Registration_Date) # I found that the most recent registration date is 06/06/2014.

df %>% filter(Registration_Date >= "2014-06-06") %>% 
  arrange(desc(Elapse_Date_from_Last_Purchase)) %>%
  summary(Elapse_Date_from_Last_Purchase) # I found that the maximum number of days is 99.

# Add the latest base date column in this data set
df_Latest <- df %>% filter(Registration_Date >= "2014-06-06")
as.integer(df_Latest$Registration_Date) + 99 %>% 
  as.Date() # I found that the Reference Date is 09/13/2014.

df$Reference_Date_on_Dataset <- as.Date("2014-09-13")

# Sort Columns
df <- df %>% select(1:8,Reference_Date_on_Dataset,everything())


# Add an age column
df$Reference_Year_on_Dataset <- year(df$Reference_Date_on_Dataset)
df$Reference_Year_on_Dataset <- as.integer(df$Reference_Year_on_Dataset)
df$Age <- df$Reference_Year_on_Dataset - df$Year_Birth
df <- df %>% select(ID,Year_Birth,Age,everything())

# Add registration month column for aggregate 
df <- df %>% select(-Registration_Year,-Registration_Month,-Registration_Day)
df$Registration_Month <- floor_date(df$Registration_Date,"month")

# Add Total Children in Family column
df$Total_Children_in_Family <- df$Kids_in_Family + df$Teenager_in_Family
df <- df %>% select(1:8,Total_Children_in_Family,everything())

# Missing value of Income: NA is complemented by the mean value.
summary(df$Income)
Avg_Income <- mean(df$Income, na.rm = T)
Avg_Income
df <- df %>% replace_na(list(Income = 52247))

# Education LevelのLabel encoding　#################
# Basic:1 Graduation:2 2n Cycle:3 Master:3 PhD:4

df <- df %>% mutate(Education_Level_Label = case_when(Education_Level == "Basic" ~ 1,
                                                      Education_Level == "Graduation" ~ 2,
                                                      Education_Level == "2n Cycle" ~ 3,
                                                      Education_Level == "Master" ~ 3,
                                                      Education_Level == "PhD" ~ 4))
df <- df %>% select(1:4,Education_Level_Label,everything())

# Marital StatusのLabel encoding　#################
# Basic:1 Graduation:2 2n Cycle:3 Master:3 PhD:4
unique(df$Marital_Status)
df <- df %>% mutate(Current_Marital_Label = case_when(Marital_Status == "Single" ~ FALSE,
                                                      Marital_Status == "Together" ~ TRUE,
                                                      Marital_Status == "Married" ~ TRUE,
                                                      Marital_Status == "Divorced" ~ FALSE,
                                                      Marital_Status == "Widow" ~ FALSE,
                                                      Marital_Status == "Alone" ~ FALSE,
                                                      Marital_Status == "Absurd" ~ FALSE,
                                                      Marital_Status == "YOLO" ~ FALSE)) %>% 
  select(1:6,Current_Marital_Label,everything())



###################################
# Exploratory data analysis (EDA)
###################################
# Confirmation the number of registrants by month
df_Monthly_Registrations <- df %>% distinct(ID,.keep_all = T) %>% 
  group_by(Registration_Month) %>% 
  summarise(Monthly_Registrations = n())

df_Monthly_Registrations %>% 
  ggplot(aes(Registration_Month,Monthly_Registrations))+
  geom_line(linewidth = 1)+
  geom_point(size = 3,alpha = 0.5)+
  scale_y_continuous(breaks = seq(0,120,10),limits = c(0,120))+
  scale_x_date(date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Monthly Registrations In the Past 2 years")

# Confirmation the age distribution of Customers
summary(df$Age)
df %>% group_by(Age) %>% 
  summarise(Customers_by_Age = n()) %>% 
  ggplot(aes(Age,Customers_by_Age))+
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8,colour="green3")+
  scale_y_continuous(breaks = seq(0,100,10),limits = c(0,100))+
  scale_x_continuous(breaks = seq(15,125,5),limits = c(15,125),labels = label_comma())+
  labs(title = "Distribution of Customers by Age", y = "Customers")

# Confirmation the number of products purchased in the past two years
df_Sold_Qty <- df %>% select(16:21) %>% apply(2,sum) %>% as.data.frame()
df_Sold_Qty <- rownames_to_column(df_Sold_Qty)
str(df_Sold_Qty)

df_Sold_Qty <- df_Sold_Qty %>%  rename("Item_Category" = rowname)
df_Sold_Qty$Sold_Qty <- df_Sold_Qty$.
df_Sold_Qty <- df_Sold_Qty %>% select(1,3)
df_Sold_Qty <- df_Sold_Qty %>% arrange(desc(Sold_Qty))

df_Sold_Qty %>% 
  ggplot(aes(Item_Category,Sold_Qty))+
  geom_bar(stat = "identity", position = "dodge",width = 0.7,alpha = 0.8, colour = "orange1")+
  scale_y_continuous(breaks = seq(0,700000,50000),limits = c(0,700000),labels = label_comma())+
  scale_x_discrete(labels = c("Fish","Fruits","Golds","Meat","Snacks","Wines"))+
  labs(x = "Item Category", y= "Sold Qty",title = "Sold Qty by Item Category Past 2 Years")

# Confirmation the number of purchases by sales channel over the past two years
df_Sold_Channel <- df %>% select(23:25) %>% apply(2,sum) %>% as.data.frame()
df_Sold_Channel <- rownames_to_column(df_Sold_Channel)
str(df_Sold_Channel)

df_Sold_Channel <- df_Sold_Channel %>%  rename("Sold_Channel" = rowname)
df_Sold_Channel$Sold_Qty <- df_Sold_Channel$.
df_Sold_Channel <- df_Sold_Channel %>% select(1,3)
df_Sold_Channel <- df_Sold_Channel %>% arrange(desc(Sold_Qty))

df_Sold_Channel %>% 
  ggplot(aes(Sold_Channel,Sold_Qty))+
  geom_bar(stat = "identity", position = "dodge",width = 0.5,alpha = 0.8, colour = "yellow1")+
  scale_y_continuous(breaks = seq(0,15000,2500),limits = c(0,15000),labels = label_comma())+
  scale_x_discrete(labels = c("Catalog","Store","Web"))+
  labs(x = "Sold Channel", y= "Sold Qty",title = "Sold Qty by Sold Channel Past 2 Years")

# Confirmation of correlation coefficients
corr_df <- df %>% select(-Kids_in_Family,-ID,-Year_Birth,-Education_Level,-Marital_Status,-Teenager_in_Family,-Registration_Date,-Registration_Month,
                         -Elapse_Date_from_Last_Purchase,-Response,-Reference_Year_on_Dataset,-Registration_Month,-Reference_Date_on_Dataset) %>%
  select(-18,-19,-20,-21,-22)


corr <- cor(corr_df, method = "pearson")
corrplot(corr, method = "square")


# Add Age Generation Column
df$Age_Group <- round(df$Age,digits = -1)


##################################################################
# Build Linear regression Models & Accuracy evaluation
##################################################################

######################
# Data Splitting
######################
# Split Train data to 80% and Test data to 20%.
# Let's review the purpose of each data.
# Train data: data used to train the model
# Test data: data used to check the generalization performance of the final selected model

nrow(df) # Number of data sets = 2,240

set.seed(2000)
df_split_dataset <- initial_split(df, prop = 0.8)
df_train <- training(df_split_dataset)
df_test <- testing(df_split_dataset)


##########################################################
# Train a Linear Regression model using Train data.
##########################################################
# Model 1
Model_lr_1 <- lm(df_train$Income ~ df_train$Age + df_train$Purchase_Meat_past2years
                 + df_train$Purchase_Wines_past2years + df_train$Purchase_Snacks_past2years
                 + df_train$Catalog_Purchase + df_train$Store_Purchase + df_train$Web_Purchase
                 + df_train$Web_visits_LastMonth
                 + df_train$Total_Children_in_Family + df_train$Education_Level_Label,
                 data = df_train)

summary(Model_lr_1)
Model_1_R2 <- summary(Model_lr_1)$adj.r.squared

# Check the multicollinearity
# Generally, if the Multicollinearity is less than or equal to 10, there is no Multicollinearity problem among variables.
vif(Model_lr_1)

# Model 2
Model_lr_2 <- lm(df_train$Income ~ df_train$Purchase_Meat_past2years
                 + df_train$Purchase_Wines_past2years + df_train$Purchase_Snacks_past2years
                 + df_train$Catalog_Purchase + df_train$Store_Purchase + df_train$Web_Purchase
                 + df_train$Web_visits_LastMonth
                 + df_train$Total_Children_in_Family,
                 data = df_train)

summary(Model_lr_2)
Model_2_R2 <- summary(Model_lr_2)$adj.r.squared

# Check the multicollinearity
vif(Model_lr_2)

# Model 3
Model_lr_3 <- lm(df_train$Income ~ df_train$Purchase_Meat_past2years
                 + df_train$Purchase_Wines_past2years + df_train$Catalog_Purchase
                 + df_train$Store_Purchase + df_train$Web_Purchase
                 + df_train$Web_visits_LastMonth
                 + df_train$Total_Children_in_Family,
                 data = df_train)

summary(Model_lr_3)
Model_3_R2 <- summary(Model_lr_3)$adj.r.squared
# Check the multicollinearity
vif(Model_lr_3)


######################
# Create RMSE function
######################
RMSE <- function(m, o) {
  tmp <- sqrt(mean((m-o)^2))
  return(tmp)
}


#####################################################################################
# Calculate predictions for each model, calculate RMSE, and select the optimal model.
#####################################################################################
pred_Model_1 <- predict(Model_lr_1,newdata = df_test)
pred_Model_2 <- predict(Model_lr_2,newdata = df_test)
pred_Model_3 <- predict(Model_lr_3,newdata = df_test)

RMSE_Model1 <- RMSE(pred_Model_1,df_test$Income)
RMSE_Model2 <- RMSE(pred_Model_2,df_test$Income)
RMSE_Model3 <- RMSE(pred_Model_3,df_test$Income)


###################################
# Summarize the results in a table
###################################
Model <- c("Model 1", "Model 2", "Model 3")
R2 <- c(Model_1_R2,Model_2_R2,Model_3_R2)
RMSE <- c(RMSE_Model1,RMSE_Model2,RMSE_Model3)

report_table <- data.frame(Model = Model ,R2 = R2, RMSE = RMSE)

kable(report_table, caption = "Summary of Results for each Models")


##################################################################
# Build Random Forest Models & Accuracy evaluation
##################################################################
##########################################################
# Build a Random Forest model using Train data.
##########################################################

df_test$Age <- as.numeric(df_test$Age)
num_trees <- 1000

# Model 1
set.seed(3000)
Model_rf_1 <- randomForest(Age_Group ~ Income + Purchase_Meat_past2years + Purchase_Wines_past2years + Purchase_Snacks_past2years
                           + Catalog_Purchase + Store_Purchase + Web_Purchase + Web_visits_LastMonth + Total_Children_in_Family,
                           data = df_train, ntree = num_trees,
                           importance = TRUE, proximity = TRUE)

pred_rf_1 <- predict(Model_rf_1, df_test) %>% round(.,digits = 0)
pred_rf_1 <- round(pred_rf_1,digits = -1)
plot(Model_rf_1)
varImpPlot(Model_rf_1)

confusionMatrix(as.factor(pred_rf_1),as.factor(df_test$Age_Group))


# Model 2
set.seed(4000)
Model_rf_2 <- randomForest(Age_Group ~ Income + Purchase_Meat_past2years + Purchase_Wines_past2years + Purchase_Snacks_past2years
                           + Catalog_Purchase + Store_Purchase + Web_Purchase + Web_visits_LastMonth + Total_Children_in_Family
                           + Education_Level_Label,
                           data = df_train, ntree = num_trees,
                           importance = TRUE, proximity = TRUE)

pred_rf_2 <- predict(Model_rf_2, df_test) %>% round(.,digits = 0)
pred_rf_2 <- round(pred_rf_2,digits = -1)
plot(Model_rf_2)
varImpPlot(Model_rf_2)

confusionMatrix(as.factor(pred_rf_2),as.factor(df_test$Age_Group))


# Model 3
set.seed(5000)
Model_rf_3 <- randomForest(Age_Group ~ Income + Purchase_Meat_past2years + Purchase_Wines_past2years
                           + Catalog_Purchase + Store_Purchase + Web_Purchase + Web_visits_LastMonth + Total_Children_in_Family,
                           data = df_train, ntree = num_trees,
                           importance = TRUE, proximity = TRUE)

pred_rf_3 <- predict(Model_rf_3, df_test) %>% round(.,digits = 0)
pred_rf_3 <- round(pred_rf_3,digits = -1)
plot(Model_rf_3)
varImpPlot(Model_rf_3)

confusionMatrix(as.factor(pred_rf_3),as.factor(df_test$Age_Group))








