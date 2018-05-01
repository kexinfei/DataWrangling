---
title: "Cyence_Kexin Fei"
author: "Kexin Fei"
date: "3/21/2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(kableExtra)
library(caret)
library(pROC)
library(randomForest)
library(xgboost)
library(knitr)
library(glmnet)
library(rpart)
library(stringr)
library(ggpubr)
library(data.table)
library(ModelMetrics)
```

# Part I: Data Sanity Checking
1. Inaccurate data (missing data) 

2. Noisy data (erroneous data and outliers)

3. Inconsistent data
```{r,warning=FALSE}
df <- read.csv("/Users/Starryfifi/Desktop/Jobs/Data Challenge/Cyence/cyence_data_exercise_take_home/data_perm_take_home.csv")

length(unique(df$case_number)) == length(df$case_number) 

count = data.frame(missing = sapply(df,function(x) sum(is.na(x))),
                   unique = sapply(df,function(x) length(unique(x))))
kable(count, caption = "Missing value and unique values number for each column")

```


```{r,warning=FALSE}
df1 = df %>%
  filter(!is.na(wage_offer))%>%
  filter(!is.na(employer_num_employees))%>%
  filter(!is.na(employer_yr_established))%>%
  mutate(decision_date = as.character(decision_date))%>%
  mutate(case_received_date = as.character(case_received_date))%>%
  mutate(decision_date = as.Date(decision_date, "%m/%d/%y"))%>%
  mutate(case_received_date = as.Date(case_received_date, "%m/%d/%y"))%>%
  filter(!is.na(case_received_date))%>%
  mutate(duration = as.numeric(difftime(decision_date, case_received_date, units = c("days"))))%>%
  mutate(wage_unit = ifelse(as.character(wage_unit) == "", "Year", as.character(wage_unit)))%>%
  mutate(year_length=as.numeric(2018-employer_yr_established))%>%
  mutate(job_level = as.character(job_level))%>%
  mutate(job_level = ifelse(is.na(job_level),"other",job_level))

df1[df1$job_education == "", "job_education"] = "NA"
df1[df1$wage_unit == "Hour",'wage_offer']=40*52*df1[df1$wage_unit == "Hour",'wage_offer']
df1[df1$wage_unit == "Week",'wage_offer']=52*df1[df1$wage_unit == "Week",'wage_offer']
df1[df1$wage_unit == "Month",'wage_offer']=12*df1[df1$wage_unit == "Month",'wage_offer']
```

#Part II: Exploratory Data Analysis / Warm Up
1. Which employer has the most entries in the dataset?
```{r,warning=FALSE}
df1 %>% 
  group_by(employer_name) %>% 
  summarise(num_entry = n()) %>%
  arrange(desc(num_entry))%>%
  head(n=5)%>%
  kable(caption = "Top 5 Number of Entries by Employer")

```

2. Which employer has the most Certified-Expired cases?
```{r,warning=FALSE}
df1[df1$case_status=="Certified-Expired",] %>%
  group_by(employer_name, ) %>% 
  summarise(num_entry = n()) %>%
  arrange(desc(num_entry))%>%
  head(n=5)%>%
  kable(caption = "Top 5 Number of Entries by Employer(Certified-Expired)")
#MICROSOFT CORPORATION Certified-Expired
```

3.
```{r,warning=FALSE}
df1 %>%
  select(employer_name)%>%
  mutate(employer_name = as.character(employer_name))%>%
  filter(!is.na(str_match(employer_name,"APPLE")) & 
           is.na(str_match(employer_name,"APPLE INC.")))%>%
  distinct()
#The number of 6 
```


4.
```{r,warning=FALSE}
plot_duration = function(df, title){
  ggplot(df)+
    geom_histogram(aes(duration), bins = 50)+
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10))+
    ggtitle(paste0("Duration of ", title))
}
# APPLE INC.
df_apple = df1 %>%
  filter(!is.na(str_match(employer_name,"APPLE INC.")))
plot_duration(df_apple,"APPLE INC.")

# GOOGLE INC.
df_google = df1 %>%
  filter(!is.na(str_match(employer_name,"GOOGLE INC.")))
plot_duration(df_google,"GOOGLE INC.")
```

# Part III: Data Modeling
## Data Preprocessing
* Based on data sanity check, I did the follwing data preprocessing:
* Removed data points with missing wage_offer,missing employer_num_employees, and missing employer_yr_established
* Convered hourly, weekly, and monthly paid wages into annual wages
* Created new variable year_length which represents how long does the employer estabilished
* Created new variable duration which representes how many days past between case recieved and decition made
* Create a new level for NA values of job_level


#Plot distributioons of response and predictors
```{r,warning=FALSE}
plot_hist = function(df, coln, continuous){ 
  if (continuous == "Y"){
  ggplot(df)+
  geom_histogram(aes_string(coln), bins = 40)+ 
      theme(axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 4, angle = 45, hjust = 1), 
      axis.text.y = element_text(size = 6))
  }
  else{ ggplot(df)+
geom_histogram(aes_string(coln), stat = "count")+ 
      theme(axis.title.x = element_text(size = 10), 
            axis.title.y = element_text(size = 10),
            axis.text.x = element_text(size = 4, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 6))
  } 
}


p1 = plot_hist(df1,"case_status","N")
p2 = plot_hist(df1, "employer_num_employees","Y") 
p3 = plot_hist(df1, "job_education","N") 
p4 = plot_hist(df1, "job_experience_num_months","Y")
p5 = plot_hist(df1, "job_foreign_lang_req","N")
p6 = plot_hist(df1, "job_level","N")
p7 = plot_hist(df1, "wage_offer","Y")
p8 = plot_hist(df1,'wage_unit', "N" )
p9 = plot_hist(df1,'year_length',"Y")
p10 = plot_hist(df1,'duration','N')

ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,ncol = 2, nrow = 5)
#Missing value and unique levels
count = data.frame(missing = sapply(df1,function(x) sum(is.na(x))),
                   unique = sapply(df1,function(x) length(unique(x))))
kable(count, caption = "Missing value and unique values number for each column")

#column types
col_type = sapply(colnames(df1), function(x) class(df1[[x]]))

# list of colum names for numerical features
num_features = c()
features = colnames(data) 

for (f in colnames(df1)){
  if( !((class(df1[[f]]) == "character") || (class(df1[[f]]) == "factor"))) 
  num_features = c(num_features, f)
}
```
The plots above helped to understand the dataset and the the distribution for each varaible. Especially for the response variable wage_offer, this raw data is highly skewed, so I used log transformation for the model.

## Check Correlation: a sense of variable importance and possible collinearity
```{r,warning=FALSE}

drops = c('case_number','case_status','job_education','job_state','case_received_date','decision_date','employer_name','employer_yr_established','job_level','wage_unit','employee_citizenship','job_foreign_lang_req')
df2 = df1[ , !(names(df1) %in% drops)]


# absolute correlation matrix and heatmap
cormat = abs(round(cor(df1[, names(df2)]),2)) 
print(cormat)

```


Assumptions:
1: The parameters of the linear regression model must be numeric and linear in nature. 
2: The mean of the residuals is Zero.  # mean(mod$residuals)
3: Homoscedasticity of residuals or equal variance # par(mfrow = c(2,2)); plot(fit)
4: No autocorrelation of residuals
5: No perfect multicollinearity between the predictors

```{r,warning=F,eval=F}

# Initial Model
lm_fit = function(df){
  lm(log(wage_offer) ~ case_status + duration + job_foreign_lang_req + log(employer_num_employees+1) + 
       year_length + job_education + job_level, data=df)
}


#cross-validation function
cv = function(df, model, nfold){
  acc = rep(0,nfold)
  acc_t = rep(0,nfold)
  p = list()
  fit_summary = list()
  i = 1
  folds = createFolds(1:nrow(df),nfold)
  for (idx in folds){
    my_train = df[-idx,]
    my_test = df[idx,]
    fit = model(my_train)
    pred = predict(fit, my_test)
    pred_t = predict(fit, my_train)
    acc[i] = mse(exp(pred), my_test$wage_offer)
    acc_t[i] = mse(exp(pred_t), my_train$wage_offer)
    fit_summary[[i]] = summary(fit)
    p[[i]] = data.frame(p = exp(pred), t = my_test$wage_offer)
    i = i+1
  }
  return (list(acc,acc_t,fit_summary,p))
}

results_lm = cv(df1, lm_fit, 10)
data.frame(CV = c("Mean mse", "Standard Deviation"),
           Test = c(mean(results_lm[[1]]),sd(results_lm[[1]])),
           Train = c(mean(results_lm[[2]]),sd(results_lm[[2]])))%>%
     kable(caption = "Linear Regression Result")

plot(lm_fit(df1))
summary(lm_fit(df1))
```


#Random Forest feature importance
```{r,warning=FALSE, eval=F}
library(randomForest)
df2$employer_num_employees = log(df2$employer_num_employees+1)
df2$log_wage = log(df2$wage_offer)
fit.rf = randomForest(log_wage ~ case_status + duration + employer_num_employees + year_length + job_education + job_level, data = df2)
varImpPlot(fit.rf, main = "Variable Importance")
```

# Conclusions and Next Steps

1. Divide dataset into two parts, with and without 'job_experience_num_months'
2. The most important 5 variables are job_levels, case_status, job_foreign_lang_req, employer_num_employees,job_education
3. Variables with high variance are worth exploring 
4. Continuous data almost have no correlation so categorical data may be more important.


