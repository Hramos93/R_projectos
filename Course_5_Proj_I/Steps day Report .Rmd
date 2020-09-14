# Steps day Report

Libraries neccesary to deploy report
``` {r load-packages, include = FALSE}
#Libraries
library(dplyr)
library(data.table)
library(magrittr)
library(ggplot2)
library(hrbrthemes)
```

Firts load data from repository local 


```{r echo= TRUE} 
path = "~/Project/R/Data Science/R/Projectos/R_projectos/repdata_data_activity/activity.csv"
data <- read.csv(path,header=TRUE,sep=",")

summary(data)
```
#### Observations:
```
- It can be seen that data on column steps have 2036 NA, this prevent that it can compute basic measures.
- The column date is characters, so this will be change to datatime
- The last column it can be seen that there is a minimun steps that is 0, the median is 1177 and max is 2235 steps. It posible to found that distribution steps would be balanced.
```

Build table with steps grouped by day to see how it is distributed

```{r  echo = TRUE, message=FALSE, warning=FALSE}
Measures <- data %>% 
  select(date,steps) %>% 
  group_by(date) %>%
  summarize(stepsbyday = sum(steps , na.rm=TRUE)) 

summary(Measures)

```

#### Observations:

```
- It can be seen that firts,second and thirs percentils are less that 12.811 but Max values is 21.194 so there are special day as {2012-11-22 and 2012-11-23}
```

Plot frequency steps by day to see better.

```{r  echo = TRUE, message=FALSE, warning=FALSE}
ggplot(Measures, aes(x=stepsbyday, fill = cut(x=stepsbyday, 100)))+
  geom_histogram(show.legend=FALSE)+
  ggtitle("Frequency Steps")
```
#### Observations 

```
- Can be seen that the impact of values NA in column steps is relevant because this could disarrange the correct distribution data.

So is necessary search strategy to impute NA 
```

Have to hand measure as mean and median before impute NA.

```{r echo = TRUE}
meanPreNa <- round(mean(Measures$stepsbyday),digits = 2)
medianPreNA <- round(median(Measures$stepsbyday),digits = 2)

meanPreNa
medianPreNA
```

Plot intervals by steps to see as see distribution 

```{r echo = TRUE}

intervals <- data %>% select(interval,steps) %>%  na.omit() %>%  group_by(interval) %>% summarize(steps = mean(steps))

```
```{r echo = TRUE, message=FALSE, warning=FALSE} 
ggplot(intervals, aes(x=interval, y=steps)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
   ggtitle("steps by intervals ")

 
intervals[which(intervals$steps == max (intervals$steps)),]
```

### Impute missing 

The strategy used is to replace to NA with values mean in each row.

``` {r echo = TRUE}
missing <-sum(is.na(data))
missing
```

```{r echo = TRUE }
replacewithmean <- function(x)
  replace(x, is.na(x), mean(x, na.rm=TRUE))

meandata <- data %>% group_by(interval) %>%
  mutate(steps = replacewithmean(steps))

summary(meandata)

```

### Compute sum steps by day 

```{r echo = TRUE}

FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)

summary(FullSummedDataByDay)  
```

Now Plot it frequency to see better

```{r echo = TRUE}
ggplot(FullSummedDataByDay, aes(x=totalsteps, fill = cut(x=totalsteps, 100)))+
  geom_histogram(show.legend=FALSE)+
  ggtitle("Total Daily Steps")
```


 
