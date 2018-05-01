---
title: "Indeed Product Science Assessment"
author: "Kexin Fei"
date: "3/12/2018"
output: pdf_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)
library(corrplot)
library(Hmisc)

data_indeed = read.csv("~/Desktop/Data Challenge/Indeed Product Science Assessment/homework_data_set.csv")
data_indeed = data_indeed[,-1]
```

## Data Preprocessing & Data Issues Checking

```{r, warning=FALSE, results = "hide"}
#Check percentage of missing values and distribution of each variable
summary(data_indeed)

#Step 1: Log transformation on revenue(right skewed distribution->normal distribution)
data_indeed$log_revenue = log(data_indeed$revenue)

#Step 2: Replace revenue=NA with revenue=0
data_indeed$revenue[is.na(data_indeed$revenue)] = 0

#Step 3: Convert other type date variables(factor/POSIXct) to Date type
data_indeed$date_assignment_ends = as.Date(data_indeed$date_assignment_ends,"%Y-%m-%d")
data_indeed$date_assignment_starts = as.Date(data_indeed$date_assignment_starts)
data_indeed$date_created = as.Date(data_indeed$date_created)
data_indeed$first_revenue_date = as.Date(data_indeed$first_revenue_date,"%Y-%m-%d")

#Step 4: check calculation of assign_days is correct 
real_assign_days = 
  difftime(data_indeed$date_assignment_ends ,
           data_indeed$date_assignment_starts, units = c("days"))
#Correct assign_days
data_indeed$assign_days = as.numeric(real_assign_days)

#Step 5: check calculation of age is correct 
real_age = difftime(data_indeed$date_assignment_starts, 
                    data_indeed$date_created, units = c("days"))
sum(data_indeed$age != real_age)

#Step 6: check whether first revenue happened after date_created
table(difftime(data_indeed$first_revenue_date, 
               data_indeed$date_created , units = c("day")))

#Step 7: Check whether advertiser_id is unique in the whole dataset
length(unique(data_indeed$advertiser_id))==length(data_indeed$advertiser_id)
```

***Data Issues Conclusions***

* In Step 4: *date_assignment_starts* is later than *date_assignment_ends*.
    After calculating *assign_days* by *date_assignment_ends* minus *date_assignment_starts*, 19438 out of n=77891 observations have different values from column *assign_days*. All the differences between original *assign_days* and correct *assign_days* are only 1 day. Because the specific reasons are unknown, then the wrong *assign_days* should be corrected and the two incorrect rows should be removed from the original data.
    
  + ***Field***: *date_assignment_starts*, *date_assignment_ends*
  + ***Oddity***:  *date_assignment_starts* should be earlier than *date_assignment_ends*. However, in this dataset, certain
*date_assignment_starts* is later than *date_assignment_ends*, which are given in Table 1.
  + ***Solution***: It is crucial to find out why these dates are in a wrong order and try to locate the original records. If it is because two dates were swapped, then we can switch them back. If it is because values under *date_assignment_ends* column were recorded incorrectly, then we need to check and record dates later than the date value under *date_assignment_starts* column.
  
```{r,warning=FALSE}
wrong = data_indeed %>%
  select(advertiser_id, date_assignment_starts, date_assignment_ends, assign_days, revenue) %>%
  slice(which(data_indeed$assign_days<0)) %>%
  distinct()

wrong %>% 
  head() %>% 
  kable(caption = "Data Points with Wrong Dates")

#Remove incorrect observations
data_indeed = data_indeed[data_indeed$assign_days>=0, ]
```
  

* In Step 5: No issue. The calculation of *age* is correct. The negative *age* is reasonable because leads can create the account after sales intervention.

* In Step 6: No issue. All the first revenue records happened after *date_created*.



# Question 1
### How many leads are represented in this dataset? Please describe both the assigned and unassigned populations.What is the average revenue of each group?

```{r,warning=FALSE}
df=data_indeed %>% 
   group_by (assigned) %>% 
   summarise ("Ave Revenue" = mean(revenue), num_leads=n()) %>% 
   mutate(rel_percentage = paste0(round(100 * num_leads/sum(num_leads), 1), "%")) %>% 
   ungroup() %>% 
   mutate("Total #Leads" = sum(num_leads))

colnames(df) = c("Group","Average Revenue","#Leads", "%Leads", "Total #Leads")
df[,1] = c("Unassigned Group","Assigned Group")
kable(df, caption = "Statistics on Leads and Revenue in Assigned/Unassigned Groups")
```



* The average revenue in the unassigned group is $1039001, while the average revenue in the assigned group is $3239021.

* 77891 leads are represented in this dataset(after removing 2 wrong rows, 77889 leads left). 40812(52.4%) leads are in the unassigned group, while 37077(47.6%) leads are in the assigned group. That indicates that it is a balanced sampling design.



# Question 2
### What are the most important metrics to consider in this data set? Why?

* **Revenue**(mean,total): The purpose of the project is to estimate the incremental impact the sales reps had on revenue. In other words, the most important metric is how much these leads would have spent with/without sales intervention. 

```{r,warning=FALSE,fig.height=5, fig.width=9}
#Check if any features(continuous) may correlate with revenue
res =cor(data_indeed[,c("revenue","age","assign_days")])
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

From the correlation plot, no strong correlation between *revenue*,*age* and *assign_days* is observed.


* **Age**: It could be one of the customer loyalty metrics with high variance, which can be put into models.
```{r,warning=FALSE}
ggplot(data_indeed, aes(x=age)) + 
  stat_density(kernel="biweight") + 
  ggtitle("Age Density")
```

* **First revenue date and date created**: It is possible to combine *first_revenue_date* with *date_created* to infer the customer habits, loyalty degree, and predict sales success probability.

* **Time Spent Selling**(How much time the reps actually spend selling, *assign_days*): It can measure the efficiency of sales performance and sales productivity.

* **Conversion rate**(Number of orders with revenue divided by total number of orders): It can measure sales productivity and performance. Historical conversion rates also show whether sales reps are becoming more effective. If the average conversion rate is increasing with the same or greater quantity of orders, it can be inferred that sales performance is improving.

* **Average order size**(Divide total money gained from orders by the number of deals during a chosen period of time): It is recommended to check this metric on a monthly or quarterly basis, and know whether the orders are getting larger or smaller, or staying the same. More importantly, this metric can spot opportunities that fall outside normal order size and flag them as cases that tend to have smaller conversion rates, longer sales cycles and higher sales cost.



# Question 3
### Analyze any existing relationship between account age and revenue, through visualization and/or other means.

Because there are a large amount of unprofitable leads, all leads without revenue are removed. And the impact of age on revenue(continuous) is investigated, both in the assigned and unassigned groups.

```{r,warning=FALSE, fig.height=5, fig.width=5}
revenue_df = data_indeed[data_indeed$revenue!=0,]
# Method 1: Basic scatter plot
ggplot(revenue_df, aes(x=age, y=log(revenue))) + 
  geom_point() +
  ggtitle("Relationship between age&revenue")

ggplot(revenue_df[revenue_df$assigned==1,], aes(x=age, y=log(revenue))) + 
  geom_point() +
  ggtitle("Relationship between age&revenue(ASSIGNED)")

ggplot(revenue_df[revenue_df$assigned==0,], aes(x=age, y=log(revenue))) + 
  geom_point() +
  ggtitle("Relationship between age&revenue(UNASSIGNED)")

# Method 2: Correlation Coefficient
cof = c(round(cor(revenue_df$age, revenue_df$revenue),4),
round(cor(revenue_df[revenue_df$assigned==1,]$age, revenue_df[revenue_df$assigned==1,]$revenue),4),
round(cor(revenue_df[revenue_df$assigned==0,]$age, revenue_df[revenue_df$assigned==0,]$revenue),4))

cof_df = data.frame(Group=c("Overall","Assigned","Unassigned"), "Correlation Coefficients"=cof)

kable(cof_df, caption = "Correlation Coefficients in Assigned/Unassigned Groups")

```

```{r,warning=FALSE}
#Method 3: Cut Age into chunks and Analyze the relationship by Table and Bar Plot 
data_indeed$age_cate = cut(data_indeed$age, c(min(data_indeed$age),0,100,200,300,400,500,600,700,900,1100,1400,1600,2000,2500,3000,max(data_indeed$age)))
                                                  
res = data_indeed %>% 
  group_by(age_cate) %>% 
  summarise(avg_rev = round(mean(revenue),2), 
            ave_order_rev = round(sum(revenue)/sum(revenue!=0),2),
            tot_rev = round(sum(revenue),2),
            tot_rev_precent = paste0(round(100*sum(revenue)/sum(data_indeed$revenue), 1), "%"))

colnames(res) = c("Group","Ave Revenue","Ave Order Revenue","Total Revenue", "Total Revenue%")

kable(res[-length(res$Group),], caption = "Revenue Data within Age Groups")


barplot_df = data.frame(age_cate =
  factor(res$Group,
    levels = as.vector(res$Group)),
  tot_rev_precent = as.vector(res$`Total Revenue`))

ggplot(barplot_df, aes(age_cate, tot_rev_precent)) + 
  geom_bar(stat = "identity") + 
  xlab("Age Group") +
  ylab("Total Revenue") +
  ggtitle("Total Revenue for Different Age Groups")

```


***Conclusions:***

* From both plots(scatter plot and bar plot) and quantitative analysis, it is observed that age and revenue shows no apparent relationship. 

* Within the assigned group, the trend is slightly more indicative of the positive relationship. 

* However, within the unassigned group, the positive trend is not observed and most profitable customers are new to Indeed(small age).


# Question 4
### What is the incremental value from assigning a lead to the sales team?
```{r,warning=FALSE}
not_assign_revenue = data_indeed[data_indeed$assigned==0,]$revenue
assign_revenue = data_indeed[-data_indeed$assigned==0,]$revenue

incremental_df = 
data_indeed %>% 
  group_by(assigned) %>% 
  summarise(avg_rev = round(mean(revenue),2), 
            ave_order_rev = round(sum(revenue)/sum(revenue!=0),2),
            n_orders = sum(revenue!=0), 
            convert_rate = paste0(round(100*sum(revenue!=0)/n(), 1), "%"),
            tot_rev = round(sum(revenue),2),
            tot_rev_precent = paste0(round(100*sum(revenue)/sum(data_indeed$revenue), 1), "%")) 

incremental_df[nrow(incremental_df) + 1,] = 
  c("Incremental Value", 
  diff(incremental_df$avg_rev), 
  diff(incremental_df$ave_order_rev),
  diff(incremental_df$n_orders), 
  paste0(round(diff(as.numeric(sub("%", "", incremental_df$convert_rate))),2),"%"),
  diff(incremental_df$tot_rev),
  paste0(diff(as.numeric(sub("%", "", incremental_df$tot_rev_precent))),"%"))
incremental_df[,1] = c("Unassigned Group","Assigned Group","Incremental Value")
colnames(incremental_df) = c("Group","Ave Revenue",
                             "Ave Order Revenue","#Orders",
                             "Conversion Rate", "Total Revenue", "Total Revenue%")
kable(incremental_df, caption = "Incremental Value from Sale Intervention")

```



***Conclusions:***

* The incremental value of average revenue(including 0 revenue orders) is $2199845.34, while the incremental value of average revenue per order(excluding 0 revenue orders) is $52847444.92. 

* Even if the total number of orders slightly decreases after sales intervention is introduced and the conversion rates are similar, the assigned total revenue is over 3 times as much as the unassigned total revenue and the incremental value of total revenue is about $77689473939.


# Bonus Question
### Investigate the data however you like and discuss any interesting insights you can find in the data.

***Idea:*** 
Check the impact severity of sales intervention on new business and existing customers.
Then provide different sales strategies for specific customers. 


```{r,warning=FALSE}
#Divide dataset into new business and existing customers
assigned_leads = data_indeed[data_indeed$assigned==1,]
assigned_leads$exist_customer = ifelse(assigned_leads$age>0,1,0)

assigned_res = assigned_leads%>% 
  group_by(exist_customer) %>% 
  summarise(n_leads = n(),
            n_order = sum(revenue!=0), 
            convert_rate = paste0(round(100*sum(revenue!=0)/n(), 1), "%"),
            avg_rev = mean(revenue), 
            avg_order_rev = round(sum(revenue)/sum(revenue!=0),2),
            tot_rev_precent = 
              paste0(round(100*sum(revenue)/sum(assigned_leads$revenue), 1), "%")) 
colnames(assigned_res) = c("Group","#Leads","#Orders","Conversion Rate",
                           "Avg Revenue","Avg Order Revenue","%Total Revenue")
assigned_res[,1] = c("New Customers","Existing Customers")
kable(assigned_res, caption = "Sales Metrics Comparison for New and Existing Customers")

```


***Conclusions:***

* *Conversion rate* for new customers is much higher than that of existing customers, which uncovers that sales intervention has a stronger impact on new customers compared to existing customers. If the customer acquisition cost is not high, it is recommended to assign more new leads to sales reps to acquire new profitable customers. However, it also can be caused by a much higher new customers commission rate for sales reps than that of existing customers, which make sales reps prefer to pay more attention to new customers. 

* *Average revenue* of new customers is higher than that of the existing customers group. It can be explained by the higher *conversion rate* for new customers group, which means a larger proportion of new customers has contributed to the group revenue and makes average revenue of new customers group higher than that of the existing customers group.

* Even if sales intervention works better for new customers, it can be observed that most revenue is still obtained from existing customers with much larger *average revenue per order* and *total revenue*. 


***Suggestion:***

* It is possible to have unfair new customers commission rate and existing customers commission rate for sales reps. However, a large proportion of revenue is obtained from existing customers. If this happens, it is important to adjust commission rate of different customers for sales reps.

* Normally, customer acquisition cost is much higher than the retention of an existing customer. Therefore, retaining and developing existing customers are necessary. It is recommended to use other strategies such as offering a reward program or loyalty points, and creating exclusive discounts for loyal customers.

* According to the Pareto principle, 20% of customers represent 80% of total sales. It is possible to obtain first 20% *advertiser_id* list with the highest revenue, and mark them as "Key Customers" in this sales assignment and focus on them more in the future.


