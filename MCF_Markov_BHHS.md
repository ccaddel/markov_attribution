MCF\_Markov
================

=========================================================================================================

Using the Google Analytics API to run the Markov Model on Multi-Channel Funnel Groupings
----------------------------------------------------------------------------------------

Markov makes predictions based on movement through the states of a stochastic process. A visitor’s propensity to convert changes as he/she is exposed to various marketing channels over time. Those differences in propensity provide a great way to measure the influence each marketing channel has on overall conversion. \* Probabilities of different states, rates of transitions among them. Used to recognize and study patterns, the ever-changing market scenario and the statistics of sequential data. \* Pros: \* Channel sequence as fundamental - more closely aligned to customer’s journey \* Can better scale to larger number of channels \* Can be applied to not only marketing channel but potentially to the individual marketing campaign level - more actionable for personalization

We'll compare the Markov model to a Heuristic model to see the difference in attribution

### We'll use the RGA package query the API and run the model

``` r
# install packages
#install.packages("devtools", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(devtools)
```

    ## Loading required package: usethis

``` r
#install_github("artemklevtsov/RGA")
library(RGA)
```

    ## Please use predefined Credentials only for the testing requests. To obtain your own Credentials see help(authorize).

``` r
authorize(client.id = getOption("rga.client.id"), getOption("rga.client.secret"))
```

    ## Access token will be stored in the '.wlFmhluHqTdZw6UG22h5A2nr@gmail.com-token.rds' file.

Build the function and query the data
-------------------------------------

``` r
# Set report start and end dates
start_date <- "2019-01-01"
end_date <- "2019-05-31"

# Query MCF API to gather conversion path data
mcf_data <- get_mcf(profileId = profile_id, 
                    start.date = start_date, end.date = end_date, 
                    metrics = "mcf:totalConversions, mcf:totalConversionValue", 
                    dimensions = "mcf:conversionDate, mcf:basicChannelGroupingPath", 
                    sort = NULL,
                    filters = NULL, 
                    samplingLevel = NULL, 
                    start.index = NULL, max.results = NULL, fetch.by = NULL)
```

    ## 
      |                                                                       
      |======================                                           |  33%
      |                                                                       
      |===========================================                      |  67%
      |                                                                       
      |=================================================================| 100%

``` r
head(mcf_data)
```

    ##   conversionDate
    ## 1       20190101
    ## 2       20190101
    ## 3       20190101
    ## 4       20190101
    ## 5       20190101
    ## 6       20190101
    ##                                                                                                  basicChannelGroupingPath
    ## 1                                                                                                             Paid Search
    ## 2                                                                                                    Paid Search > Direct
    ## 3                                                                                           Paid Search > Direct > Direct
    ## 4                                                                Paid Search > Direct > Direct > Direct > Direct > Direct
    ## 5 Paid Search > Direct > Direct > Direct > Direct > Direct > Direct > Direct > Direct > Direct > Direct > Direct > Direct
    ## 6                                                                                                  Paid Search > Referral
    ##   totalConversions totalConversionValue
    ## 1               44                    0
    ## 2                2                    0
    ## 3                3                    0
    ## 4                1                    0
    ## 5                1                    0
    ## 6                1                    0

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code due to sensitive GA view id information.

``` r
# install attribution packages
#install.packages("ChannelAttribution", repos = "http://cran.us.r-project.org")
library(ChannelAttribution)

#install.packages("reshape", repos = "http://cran.us.r-project.org")
library(reshape)
```

### Build Attribution Models

``` r
# build heuristic models i.e. First Touch, Last Touch

H <- heuristic_models(mcf_data, 'basicChannelGroupingPath', 'totalConversions', var_value='totalConversionValue')

head(H)
```

    ##     channel_name first_touch_conversions first_touch_value
    ## 1    Paid Search                   16596                 0
    ## 2         Direct                   87557                 0
    ## 3       Referral                   10495                 0
    ## 4 Organic Search                  190757                 0
    ## 5 Social Network                    3005                 0
    ## 6  (unavailable)                      44                 0
    ##   last_touch_conversions last_touch_value linear_touch_conversions
    ## 1                  14031                0              15091.73998
    ## 2                 132232                0             115073.20722
    ## 3                  15056                0              14017.68060
    ## 4                 143032                0             160685.36574
    ## 5                   4071                0               3547.23203
    ## 6                     38                0                 41.00204
    ##   linear_touch_value
    ## 1                  0
    ## 2                  0
    ## 3                  0
    ## 4                  0
    ## 5                  0
    ## 6                  0

``` r
# build Markov Model now

markov_model <- markov_model(mcf_data, 'basicChannelGroupingPath', 
                             'totalConversions', 
                             var_value='totalConversionValue',
                             out_more = TRUE,
                             order = 2)

model_results <- markov_model$result

#rename total_conversions to markov_conversions
colnames(model_results)[colnames(model_results)=="total_conversions"] <- "markov_conversions"

model_results
```

    ##     channel_name markov_conversions total_conversion_value
    ## 1    Paid Search             308503                      0
    ## 2         Direct                  0                      0
    ## 3       Referral                  0                      0
    ## 4 Organic Search                  0                      0
    ## 5 Social Network                  0                      0
    ## 6  (unavailable)                  0                      0
    ## 7          Email                  0                      0
    ## 8        Display                  0                      0

``` r
# merge the heuristic and markov data frames, select relevant columns, rename columns, and transform into a dataframe that ggplot2 can use to graph the outcome comparisons

all_attribution <- merge(H, model_results, by='channel_name') 

all_attribution1 <- all_attribution[, (colnames(all_attribution)%in%c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 'linear_touch_conversions', 'markov_conversions'))]

all_attribution1 <- melt(all_attribution1, id='channel_name')

head(all_attribution1)
```

    ##     channel_name                variable  value
    ## 1  (unavailable) first_touch_conversions     44
    ## 2         Direct first_touch_conversions  87557
    ## 3        Display first_touch_conversions     31
    ## 4          Email first_touch_conversions     18
    ## 5 Organic Search first_touch_conversions 190757
    ## 6    Paid Search first_touch_conversions  16596

### Plot Attribution Comparisons

``` r
# install ggplot package
#install.packages("ggplot2")
library(ggplot2)
```

``` r
# plot comparison

ggplot(all_attribution1, aes(x = factor(channel_name, level = c('Direct','Paid Search','Organic Search','Other Advertising','Referral','Social Network','Display','Email','(unavailable)')), value, fill = variable)) +
  geom_bar(stat='identity', position='dodge') +
  ggtitle('TOTAL CONVERSIONS') + 
  theme(axis.title.x = element_text(vjust = -2)) +
  theme(axis.title.y = element_text(vjust = +2)) +
  theme(title = element_text(size = 16)) +
  theme(plot.title=element_text(size = 20)) +
  theme(text = element_text(size=9)) +
  ylab("") +
  xlab(" Marketing Channel")
```

![](MCF_Markov_BHHS_files/figure-markdown_github/unnamed-chunk-10-1.png)

As you can see, based on which model we use, the conversion allocations to marketing channels can really vary. This can significantly impact budget distribution and media planning. Selecting the one that accomodates the business model and marketing objectives best to achieve desired KPIs is key.
