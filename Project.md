---
title: "Economic and Health Impacts of Storm Events in the USA"
author: "Mario Fiamenghi"
date: "Friday, September 19, 2014"
output:
  html_document:
    keep_md: yes
---


##Synopsis

This analysis uses historical raw data from the USA department of National Oceanic and Atmospheric Administration's (NOAA). This work uses only data from 1996 onward, since prior to 1996 no all the events types have been recorded.The aim of this project is to determine the most relevant events affecting  public health and financial losses. The total number of injuries and fatalities for events have been used to determine the two most important events with impact in each public health. Analogously, the total Dollar amount in damages to properties and crops have been used to determine the two most important events impacting in financial losses for each category.Finally,two times-series graphic emphasize the importance of this events in recent years

##Data Processing

- Obtaining the data from the coursera Reproducible Research website and saving in the working directory

```{r,cache=TRUE}
 setwd("C:/BI/R/coursera/reprod") #(set directory only for cache stormdataorigina )
file<-"../stormdataoriginal.csv.bz2"
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists(file))
        {        
        library("downloader")
        download(url,file,mode="wb")
        }
data <- read.csv(file)

```
```{r}
setwd("C:/BI/R/coursera/reprod/RepData_PeerAssessment2") #(set working directory)
```

- The following data cleaning and transformations were performed bellow to guarantee:  
1 - Analysis in year basis of events  
2 - Calculation of the full cost associated with damages  

```{r,,cache=TRUE}
library(lubridate)
data$year<-year(as.Date(data$BGN_DATE,"%m/%d/%Y"))#(create variable year)

data=data[data$year>1995,]#uses only data from 1996 onwards

#(function to convert characters to numbers)
#(adapted from [Jean-Robert](http://stackoverflow.com/questions/6954017/r-replace-characters-using-gsub-how-to-create-a-function))
from<-c("","h","k","m","b")
to<-c("0","2","3","6","9")
sub2 <- function(pattern, replacement, x, ...)
        {
        x<-gsub("[[:punct:]]", "", x) # (removes punct characters)
        x<-tolower(x) # (converts all characters to lower case)
        
        for(i in 1:length(pattern))
        x <- sub(pattern[i], replacement[i], x, ...)
        x
        }
data$CROPDMGEXP1<-as.numeric(sub2(from,to,data$CROPDMGEXP))
data$PROPDMGEXP1<-as.numeric(sub2(from,to,data$PROPDMGEXP))#(replacing all character to numbers using the function above)
data$PROPDM1<-with(data,PROPDMG*10^PROPDMGEXP1)#(full property damage costs calculation)
data$CROPDMG1<-with(data,CROPDMG*10^CROPDMGEXP1)#(full crop damage costs calculation)

```
- Promote words presentation homogenization since the variable EVTYPE will be used to summarize the results
```{r,,cache=TRUE}
library(stringr)
data$EVTYPE<-str_trim(tolower(data$EVTYPE))
data$EVTYPE<-gsub("[[:punct:]]", " ", data$EVTYPE) #(replace punctuation characters by space character)
data$EVTYPE<-sub("s$", "", data$EVTYPE) #(eliminate character "s" at word ending)

```

- This part is performing the following:  
1 - Summarizes the total of occurrences for each of the category   
2 - Introduce the variable to name the category in the data  
3 - Organise the data set in order of importance  
4 - Limits the data set for only the two most important events 
```{r}
inj<-with(data,aggregate(INJURIES,by=list(EVTYPE),sum))
fat<-with(data,aggregate(FATALITIES,by=list(EVTYPE),sum))
prop<-with(data,aggregate(PROPDM1,by=list(EVTYPE),sum))
crop<-with(data,aggregate(CROPDMG1,by=list(EVTYPE),sum))
inj$meas<-"INJURIES"
fat$meas<-"FATALITIES"
prop$meas<-"PROPERTIES DAMAGES"
crop$meas<-"CROP DAMAGES"

inj<-inj[order(inj$x,decreasing=TRUE),]
fat<-fat[order(fat$x,decreasing=TRUE),]
prop<-prop[order(prop$x,decreasing=TRUE),]
crop<-crop[order(crop$x,decreasing=TRUE),]

#Renaming variables 
names(inj)<-c("EVTYPE","MEASURE","DESCRIPTION")
names(fat)<-c("EVTYPE","MEASURE","DESCRIPTION")
names(prop)<-c("EVTYPE","MEASURE","DESCRIPTION")
names(crop)<-c("EVTYPE","MEASURE","DESCRIPTION")


# Reducing to the first the  greatest event impact in health and damages
inj1<-inj[1:2,]
fat1<-fat[1:2,]
prop1<-prop[1:2,]
crop1<-crop[1:2,]


health<-rbind(inj1,fat1)
damage<-rbind(prop1,crop1)

```
- The script below perform the following:  
1 - Summarize the  data in events on a yearly basis  
2 - Subset the data for only the two top events in each of the category  
3 - Insert category descriptions
```{r}
inj2<-aggregate(INJURIES~EVTYPE+year,data,sum) 
fat2<-aggregate(FATALITIES~EVTYPE+year,data,sum)
prop2<-aggregate(PROPDM1~EVTYPE+year,data,sum)
crop2<-aggregate(CROPDMG1~EVTYPE+year,data,sum)

inj2<-inj2[inj2$EVTYPE==inj1$EVTYPE,]
fat2<-fat2[fat2$EVTYPE==fat1$EVTYPE,]
prop2<-prop2[prop2$EVTYPE==prop1$EVTYPE,]
crop2<-crop2[crop2$EVTYPE==crop1$EVTYPE,]

# insert description variable
inj2$meas<-"INJURIES"
fat2$meas<-"FATALITIES"
prop2$meas<-"PROPERTIES DAMAGES"
crop2$meas<-"CROP DAMAGES"



#Renaming variables 
names(inj2)<-c("EVTYPE","YEAR","MEASURE","DESCRIPTION")
names(fat2)<-c("EVTYPE","YEAR","MEASURE","DESCRIPTION")
names(prop2)<-c("EVTYPE","YEAR","MEASURE","DESCRIPTION")
names(crop2)<-c("EVTYPE","YEAR","MEASURE","DESCRIPTION")

health<-rbind(inj2,fat2)
damage<-rbind(prop2,crop2)

```



#Results

- This Graphic demonstrate the yearly impact behavior for the two most import weather basis events for public health in the USA
```{r,warning=FALSE}
library(ggplot2)

# graphic for health factors
p<-qplot(YEAR,log(MEASURE),col=EVTYPE,data=health,geom="line",ylab=" Log (Number Occurences)")
p + facet_wrap(~ DESCRIPTION)+ geom_point(data=health)

```

- This Graphic demonstrate the yearly impact behavior for the two most import weather basis events for financial damages in the USA
```{r, warning=FALSE}
# graphic for damages factors
q<-qplot(YEAR,log(MEASURE),col=EVTYPE,data=damage,geom="line",ylab=" Log (Dollar Amount)")
q+ facet_wrap(~ DESCRIPTION)+ geom_point(data=damage)

```




## Conclusion 

Tornadoes, floods and excessive heat are the most import weather events impacting on public health in the USA. By analyzing the injuries graphic, it can be observed that tornadoes are by far the most important event associate with this category. By analyzing the fatalities graphic, it can be observed that excessive heat is also very important in recent years.  
In relation to financial damages, it can be noted that floods have been the biggest responsible for properties damages along the years. It can also been noted the impact of droughts in crops before 2006.
