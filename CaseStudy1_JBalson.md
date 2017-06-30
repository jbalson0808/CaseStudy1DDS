# JBalson_CaseStudy1
Josh Balson  
June 30, 2017  


#Project Introduction:
###  The purpose of this analysis is to gain an understanding of the attributes and geographic locations of some of the most successful craft breweries in      the United States.
###  First, we will compile and organize the beer data provided to us by a third-party vendor who specializes in market analysis.
###  Next, we will look at the main characteristics of the data - namely geographic frequency, alcohol content, and the bitterness of the beers brewed.  
###  Finally, we will examine the question of whether or not there is a statistical correlation between a beer's alcohol content (ABV) and its bitterness (IBU).
###  We will then use this analysis to provide a framework for further discussions to identify pockets of opportunity in the craft beer market in the United States.




```r
# General workspace prep to begin new project
#  You will need to set your working directory to your local drive 
#  Be sure the two datasets provided (beers.csv and breweries.csv) are saved to the same location 
#  as your working directory.
setwd("C:/Users/emily/Documents/Summer2017/DoingDataScience/CaseStudy1")
rm(list=ls()) # remove all previous variables in workspace prior to starting this new project
#Some packages you may need to install for this project
# install.packages(c("brew", "countrycode","devtools", "dplyr", "plyr",
#                    "ggplot2", "googleVis","knitr", "MCMCpack",
#                    "repmis", "RCurl","rmarkdown", "texreg",
#                    "tidyr", "WDI","xtable", "Zelig","gridExtra"))

# import beers.csv from working directory
beers <- read.csv("beers.csv",header=TRUE)

names(beers) <- tolower(names(beers)) # make all variable names lower case first
# quick initial check of beers data set.  Uncomment the five lines below to view
#head(beers)
#tail(beers)
#dim(beers)
#summary(beers)
#str(beers)


# import breweries.csv from working directory
breweries <- read.csv("breweries.csv",header=TRUE)

names(breweries) <- tolower(names(breweries)) # make all variable names lower case first
# quick initial check of beers data set.  Uncomment the five lines below to view
#head(breweries)
#tail(breweries)
#dim(breweries)
#summary(breweries)
#str(breweries)


# load plyr library to get counts
library(plyr)
```
# 1. How many breweries are present in each state?
## Let's view this in descending order

```r
CountByState<-count(breweries,"state")
CountByStateDesc<-arrange(CountByState,desc(freq))
head(CountByStateDesc,5) #full results limited to top and bottom five states in order to save space.  
```

```
##   state freq
## 1    CO   47
## 2    CA   39
## 3    MI   32
## 4    OR   29
## 5    TX   28
```

```r
tail(CountByStateDesc,5)
```

```
##    state freq
## 47    NV    2
## 48    DC    1
## 49    ND    1
## 50    SD    1
## 51    WV    1
```

```r
#print(CountByStateDesc) #To see full results you can activate this line
```

# 2. Merge beer data with breweries data by brewery id. Print the first 6 observations and the
# last six observations to check the merged file.

```r
# The variable name for brewery id is not the same in the two data frames.
# So we'll change the variable name in breweries to match the variable name in beers
colnames(breweries)[1]<-"brewery_id"
#head(breweries) #Uncomment this line to check that the column name was changed correctly

#Merge the two datasets on 'brewery_id'
BeersBrew<-merge(beers,breweries,by="brewery_id")
# quick initial check of new data set.  Uncomment the two lines below to view.
#head(BeersBrew)
#str(BeersBrew) #this should show same number of observations as the beers data set with 10 (7+4-1) variables 

# Initial check of new data set looks good, 
# but let's change variable 'name.x' to 'beer_name' 
# and variable 'name.y' to 'brewery_name' to avoid confusion later.
colnames(BeersBrew)[2]<-"beer_name"
colnames(BeersBrew)[8]<-"brewery_name"
#head(BeersBrew) #Uncomment this line to check that the column names were changed correctly
```
# 3. Report the number of N/A (null) values in each column.

```r
colSums(is.na(BeersBrew)) #gives count of N/A values for each column
```

```
##   brewery_id    beer_name      beer_id          abv          ibu 
##            0            0            0           62         1005 
##        style       ounces brewery_name         city        state 
##            0            0            0            0            0
```

```r
colMeans(is.na(BeersBrew)) #not required, but good to see frequencies for each column also
```

```
##   brewery_id    beer_name      beer_id          abv          ibu 
##   0.00000000   0.00000000   0.00000000   0.02572614   0.41701245 
##        style       ounces brewery_name         city        state 
##   0.00000000   0.00000000   0.00000000   0.00000000   0.00000000
```

```r
#  The above serves as a health check for our two main variables, ABV and IBU.
#  ABV is missing less than 3% of its values
#  IBU, however, is missing nearly 42% of its values!
#  We will keep this mind later and in general we should proceed with caution when 
#  attempting to draw conclusions to questions that center around IBU.
```
# 4. Compute the median alcohol content and international bitterness unit for each state. Plot a bar chart to compare.

```r
#First create two dataframes for each (ABV and IBU)
AggAbv<-aggregate(abv~state, data=BeersBrew, median) #median alcohol content by state
#arrange(AggAbv,desc(abv)) #sort median alcohol content by state in descending order
AggIbu<-aggregate(ibu~state, data=BeersBrew, median)
#arrange(AggIbu,desc(ibu)) #sort median international bitterness unit in descending order

AbvIbu<-merge(AggAbv,AggIbu,by="state")
#AbvIbu
#str(AbvIbu)
#summary(AbvIbu)

library(ggplot2)

state<-factor(AbvIbu$state)
abv<-c(AbvIbu$abv)
ibu<-c(AbvIbu$ibu)
values<-c(ibu,abv*1000)
Key<-c(rep("ABV",50),rep("IBU",50))
mydata<-data.frame(state,values)

pAbv<-ggplot(mydata, aes(x=reorder(state,-values),values))+geom_bar(stat="identity", aes(fill=Key), position="dodge")
pAbv+labs(y="Median (ABVx1,000)")+labs(x="State")+labs(title="Median ABV and IBU by State (ABV desc)")
```

![](CaseStudy1_JBalson_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
values<-c(abv*1000,ibu)
pIbu<-ggplot(mydata, aes(x=reorder(state,-values),values))+geom_bar(stat="identity", aes(fill=Key), position="dodge")
pIbu+labs(y="Median (ABVx1,000)")+labs(x="State")+labs(title="Median ABV and IBU by State (IBU desc)")
```

![](CaseStudy1_JBalson_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

# 5. Which state has the maximum alcoholic beer? Which state has the most bitter beer?

```r
#First we'll find the state(s) with the highest alcohol content (ABV):
StateMaxAbu<-arrange(AbvIbu,desc(abv)) #arrange all states in descending order by abv
head(StateMaxAbu,5) #Looks at top 5
```

```
##   state    abv  ibu
## 1    DC 0.0625 47.5
## 2    KY 0.0625 31.5
## 3    MI 0.0620 35.0
## 4    NM 0.0620 51.0
## 5    WV 0.0620 57.5
```

```r
#Kentucky and The Distric of Columbia are tied for brewing beers with the highest alcohol content, at 6.25% 


#Next we'll find the state(s) with the bitterest beer (IBU):
StateMaxIbu<-arrange(AbvIbu,desc(ibu)) #arrange all states in descending order by IBU
head(StateMaxIbu,5) #Looks at top 5
```

```
##   state   abv  ibu
## 1    ME 0.051 61.0
## 2    WV 0.062 57.5
## 3    FL 0.057 55.0
## 4    GA 0.055 55.0
## 5    DE 0.055 52.0
```

```r
#Maine has the bitterest beer at 61 IBU 
```
# 6. Summary statistics for ABV (Alcohol by volume) variable.

```r
#Summary stats below suggest some left skew and small variance, i.e. - the craft brews tend to be centered 
#around a higher ABV
mean(abv) 
```

```
## [1] 0.05577
```

```r
quantile(abv) #0% = min, 25% = Q1, 50% = median (equivalent to Q2), 75% = Q3, and 100% = max
```

```
##     0%    25%    50%    75%   100% 
## 0.0400 0.0550 0.0560 0.0580 0.0625
```

```r
sd(abv) #standard deviation of the sampled ABV
```

```
## [1] 0.00422361
```

```r
#Below we provide two visual representations of the summary statistics
#The histogram or the boxplot can be used here, or both.  Pick your poison.
par(mfrow = c(2,2))  #2x2 matrix of plots
hist(abv, main="Histogram of ABV")
boxplot(abv, data=AbvIbu, ylab="Alcohol by Volume (ABV)", xlab="All States", main="Summary statistics for ABV", horizontal = TRUE)
```

![](CaseStudy1_JBalson_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## 7. Is there a relationship between the bitterness of the beer and its alcoholic content? 

```r
#Fist we'll plot ABV vs. IBU and add a trend line to get a visual representation
plot(abv,ibu,pch=19,col="blue", main="Scatter Plot of IBU vs. ABV")	
#pch changes point shape, col changes the color 
abline(lm(ibu~abv),col="darkgray",lwd=2) #adds a trend line to the plot, lwd changes line thickness
```

![](CaseStudy1_JBalson_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#Next, we'll run a Pearson's correlation and give a full analysis

#We wish to know whether there is a correlation between bitterness of the beer and its alcohol content.
#Ho: There is no correlation
#Ha: There is a correlation

cor.test(abv,ibu)  #for test for whether correlation is 0 (H0) or not 0 (Ha) and CI.
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  abv and ibu
## t = 2.0371, df = 48, p-value = 0.04717
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.004066385 0.519640151
## sample estimates:
##       cor 
## 0.2820948
```

```r
#From the correlation test above we get a t-value of 2.04 and p-value of 0.047 using a 95% confidence interval
#Therefore, we reject the null hypothesis that there is no correlation
#Conclusion:  Although we rejected the null hypothesis, the results are not overwhelmingly convincing.  
#The sample correlation is only 0.28, and it is likely that breweries make stronger beer more bitter because 
#that is the taste that the breweries believe consumers are looking for in stronger beers.  
#Because this is an observational study, the results only apply to the samples selected in the dataset and no 
#causal relationship can be made to infer that stronger beers are inherently more bitter.  Rather, this only 
#shows that (from the beers chosen in this dataset) beers with higher alcoholcontent tend to be more bitter.
```
 
#Project Conclusion:

###  Geographic frequency:  As we'd expect, some of the states with larger populations (CA, CO, TX) have the most breweries and some of the states with smaller populations (ND, SD, NV, WV) have the fewest amount of breweries.  States like Michigan and Oregon stick out because the higher frequency of breweries compared to population are not necessarily 'in-line'.  This hints that consumers in these states have a higher demand for craft beer than the national average.  Further analysis should be done in neighboring states to find pockets of market share opportunity keeping in mind that even if there is a higher demand in a state such as, say, Indiana or Ohio - there may be prohibitive state laws in these neighboring states that make them unfriendly to beer production and distribution.

###  Alcohol content and the bitterness of beer:  Kentucky and D.C. brew the beers with the highest alcohol content, followed by Michigan, New Mexico, and West Virginia.  Maine and West Virginia brew the beers with the highest bitterness, followed by Florida, Georgia, and Delaware.  Consumers in West Virginia apparently like thier beer to be strong and bitter.  This is another mid-western state (somewhat close to Michigan), where there may be market opportunity for brewing craft beer.  This begs the question, why only one brewery in West virgina?  If the state laws there are friendly to the brewery business this may point to market opportunity.

###  Correlation between ABV and IBU:  We performed a full analysis of this question in #6.  What we are really asking is do consumers prefer their craft brews to be strong and bitter at the same time?  The results of our analysis say yes, but this is not a strong statistical conclusion.  We should also remember from #3 that nearly 42% of all data points were missing for the IBU variable in our data set. Further market analysis should be performed to gauge true consumer sentiment.  The recent phenomenon of the 'all day', or 'session' IPAs is anecdotal evidence that there is a market base and perhaps further opportunity to buck the current market trend and capture the consumer who enjoys drinking beer but does not enjoy the side-effects of drinking craft beer in large quantities.



