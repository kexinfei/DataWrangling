---
title: "Data Wrangling"
author: "Kexin Fei"
date: "2/9/2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(knitr)
```


```{r}
setwd("~/Desktop/Data Challenge/Cyence/cyence_data_exercise_take_home")
df = read.csv("data_perm_take_home.csv")
dff = as.tibble(df)

df1 = dff %>%
  select(-case_number) %>%
  filter(!is.na(wage_offer))%>%
  filter(country %in% c("Mexico", "Afghanistan")) %>%
  mutate(decision_date = as.character(decision_date))%>%
  mutate(decision_date = as.Date(decision_date, "%m/%d/%y"))%>%
  filter(!is.na(case_received_date))%>%
  mutate(duration = as.numeric(difftime(decision_date, case_received_date, units = c("days"))))%>%
  mutate(wage_unit = ifelse(as.character(wage_unit) == "", "Year", as.character(wage_unit)))%>%
  mutate(year_length = as.numeric(2018-employer_yr_established))%>%
  mutate(job_level = ifelse(is.na(job_level),"other",job_level))

df1[df1$job_education == "", "job_education"] = "NA"
df1[df1$wage_unit == "Hour",'wage_offer']=40*52*df1[df1$wage_unit == "Hour",'wage_offer']
df1[df1$wage_unit == "Week",'wage_offer']=52*df1[df1$wage_unit == "Week",'wage_offer']
df1[df1$wage_unit == "Month",'wage_offer']=12*df1[df1$wage_unit == "Month",'wage_offer']
```

select()
filter()
group_by()  
summarize()
mutate()
arrange(desc(year), continent) 
#sort the rows in our data frame according to values in a certain column. We can use the arrange() function to do this. For instance, let's organize our rows by year (recent first), and then by  continent.


```{r dplyr}
df %>% 
  group_by(id) %>% 
  mutate(cumsum = cumsum(value))

wrong = data_indeed %>%
  select(advertiser_id, date_assignment_starts, date_assignment_ends, assign_days, revenue) %>%
  slice(which(data_indeed$assign_days<0)) %>%
  distinct()

wrong %>% 
  head() %>% 
  kable(caption = "Data Points with Wrong Dates")

```



```{r Date conversion, echo=FALSE}

df1 %>% 
  select(decision_date) %>% 
  mutate(decision_date = as.Date(decision_date, "%m/%d/%y"))%>%
  mutate(week = week(decision_date)) %>%
  mutate(month = month(decision_date)) %>%
  mutate(day_of_week = weekdays(decision_date)) %>%
  arrange(desc(month))
```

```{r summarize, echo=FALSE}
starwars %>%
  group_by(species) %>%
  summarise(n = n(),
            mass = mean(mass, na.rm = TRUE)) %>%
  filter(n > 1)
```


```{r as function, echo=FALSE}
#https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

library(tibble)
as.tibble(iris)

my_summarise <- function(df, expr1, expr2) {
  expr1 <- enquo(expr1) #turns an argument into a string
  expr2 <- enquo(expr2)
  
  df %>%
  group_by(!!expr1) %>%
  summarise(
    mean = mean(!!expr2),
    sum = sum(!!expr2),
    n = n()
  )
}

x = my_summarise(iris, Species, Sepal.Width)

# To removing grouping, use ungroup
x %>% 
  ungroup() %>% 
  summarise(n = sum(n))




#Multiple variables in argument=================================================================
my_summarise <- function(df, ...) {
  group_var <- quos(...)

  df %>%
    group_by(!!!group_var) %>% # takes a list of elements and splices them into to the current call
    summarise(a = mean(a))
}

my_summarise(df, g1, g2)
```


```{r Consecutive/Rolling sums, echo=FALSE}
library(zoo)
x = c(1, 2, 3, 4, 5, 6)
rollapply(x, 3, sum)
```




