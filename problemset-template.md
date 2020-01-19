Exploring NYC flights data in R
================
Shree Priya

## Setup:

Do whatever setup you do here, such as loading libraries

``` r
# Load standard libraries
library("tidyverse")
library("nycflights13")
data(flights)
library(plotly)
```

## Problem 1: Exploring the NYC Flights Data

### (a) Importing and Inspecting Data:

``` r
# Load standard libraries
library("tidyverse")
library("nycflights13")
library(dplyr)
mydata = filter(flights, year == 2013)
#Removing all the null values in the arrival and departure delay
data1 = mydata %>% filter(is.na(arr_delay) == FALSE) %>% filter(is.na(dep_delay) == FALSE)

#Checking the number of total number of flights by each carrier
cat(paste("The total number of flights by each carrier are:"))
```

    ## The total number of flights by each carrier are:

``` r
table(data1$carrier)
```

    ## 
    ##    9E    AA    AS    B6    DL    EV    F9    FL    HA    MQ    OO    UA 
    ## 17294 31947   709 54049 47658 51108   681  3175   342 25037    29 57782 
    ##    US    VX    WN    YV 
    ## 19831  5116 12044   544

``` r
#Checking the total number of carriers
cat(paste("The total number of carriers are: \n"))
```

    ## The total number of carriers are:

``` r
dim(table(data1$carrier))
```

    ## [1] 16

``` r
cat(paste("The total number of rows and columns are: \n"))
```

    ## The total number of rows and columns are:

``` r
dim(data1)
```

    ## [1] 327346     19

``` r
cat(paste("The total number of flights each month are"))
```

    ## The total number of flights each month are

``` r
ggplot(data1, aes(month)) + geom_histogram(stat = "count", color="Red", fill="Pink") + scale_x_continuous(breaks = c(1:12)) + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/Setup2-1.png)<!-- -->

This data set consists of all on-time data for all flights that departed
NYC(i.e JFK, LGA, EWR) in 2013. For the date of departure there is –\>
year, month, day For the actual departure and arrival times there is –\>
dep\_time, arr\_time For the schedule dep and arr time there is –\>
sched\_dep\_time, sched\_arr\_time for the departure and arrival delays
there is –\> dep\_delay, arr\_delay There is carrier for the carrier of
the flight There is the flight number, tail number, origin, dest, air
time, distance of the flight.

### (b) Formulating Questions:

1.  Exploring arrival delay
    
    1.  Which are the top 3 months with the highest arrival delay?
    2.  Why do you think these were the months with most delay?
    3.  Which airline carrier has the most number of delays in those
        months?
    4.  Choose a month and check if the airline had the most number of
        arrival delays that month

2.  Exploring departure delay
    
    1.  Which are the top 3 months with the highest departure delay?
    2.  Why do you think these were the months with most delay?
    3.  Which airline carrier has the most number of delays in those
        months?
    4.  Choose a month and check if the airline had the most number of
        departure delays that month

3.  Was there any correlation between 1 and 2

4.  Which airports have the highest departure delay?

5.  Did more number of flights that departed from 6am to 6pm get
    delayed?

### (c) Exploring Data:

### Question 1:Exploring arrival delay

### Part a :Which are the top 3 months with the highest arrival delay?

``` r
#data1 no missing values
head(data1)
```

    ## # A tibble: 6 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time
    ##   <int> <int> <int>    <int>          <int>     <dbl>    <int>
    ## 1  2013     1     1      517            515         2      830
    ## 2  2013     1     1      533            529         4      850
    ## 3  2013     1     1      542            540         2      923
    ## 4  2013     1     1      544            545        -1     1004
    ## 5  2013     1     1      554            600        -6      812
    ## 6  2013     1     1      554            558        -4      740
    ## # ... with 12 more variables: sched_arr_time <int>, arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>,
    ## #   time_hour <dttm>

``` r
#Mutating the data add a column arr which is 1 if the arrival delay is greater than 0 and 0 if the arrival delay is less than 0
arrival_flights <- data1 %>% group_by(month) %>% mutate( arr = ifelse(arr_delay >0 , 1, 0))

#Plotting the histogram with a factor of whether it was delayed or not.
ggplot(arrival_flights, aes(month, fill = factor(arr)), labels = TRUE) + 
   geom_histogram() + scale_x_continuous(breaks = c(1:12))
```

![](problemset-template_files/figure-gfm/chunk1-1.png)<!-- -->

According to this plot we can see that the maximum delays happen in
month 6, 7 and 12.

### Part b: Why do you think these were the months with most delay?

June, July and December tend to be the holidays in the US. Most of the
flights are overbooked and there are too many flights. This usually
causes delaying of flights.

### Part c: Which airline carrier has the most number of delays in those months?

``` r
#Filtering data of month 6 with positive arrival delay
df1 = data1 %>% filter(month == 6 & arr_delay >0)
dim(df1)
```

    ## [1] 12490    19

``` r
#Plotting the data
ggplot(df1, aes(carrier, fill = arr_delay)) + geom_histogram(stat="count", fill="pink") + xlab("MONTH 6(carrier)") + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk2-1.png)<!-- -->

``` r
#Filtering data of month 7 with positive arrival delay
df2 = data1 %>% filter(month == 7 & arr_delay >0)
dim(df2)
```

    ## [1] 13304    19

``` r
#Plotting the data
ggplot(df2, aes(carrier, fill = arr_delay)) + geom_histogram(stat="count", fill="red") + xlab("MONTH 7(carrier)") + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk2-2.png)<!-- -->

``` r
#Filtering data of month 12 with positive arrival delay
df3 = data1 %>% filter(month == 12 & arr_delay >0)
dim(df3)
```

    ## [1] 14394    19

``` r
#Plotting the data
ggplot(df3, aes(carrier, fill = arr_delay)) + geom_histogram(stat="count", fill="blue") + xlab("MONTH 12(carrier)") + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk2-3.png)<!-- -->

We can clearly see that most of the delays happen in carrier B6 and UA

### Part d: Choose a month and check if the airline had the most number of arrival delays that month.

``` r
#Choosing month 1 for checking the arrival delays that month
df4 = data1 %>% filter(month == 1 & arr_delay >0)
dim(df4)
```

    ## [1] 11150    19

``` r
#Plotting the data
ggplot(df4, aes(carrier, fill = arr_delay)) + geom_histogram(stat="count", fill="purple") + xlab("MONTH 1(carrier)")+ geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk3-1.png)<!-- -->

We can see that neither UA nor B6 has the highest arrival delays in
month 1.

### Question 2: Exploring departure delay

### a. Which are the top 3 months with the highest departure delay?

``` r
#Mutating the data add a column dep which is 1 if the departure delay is greater than 0 and 0 if the departure delay is less than 0
dep_flights <- data1 %>% group_by(month) %>% mutate( dep = ifelse(dep_delay >0 , 1, 0))

#Plotting the data
ggplot(dep_flights, aes(month, fill = factor(dep)), labels = TRUE) + 
   geom_histogram() + scale_x_continuous(breaks = c(1:12))
```

![](problemset-template_files/figure-gfm/chunk4-1.png)<!-- -->

We can see that months 6, 7 and 12 again have the most number of
departure delays.

### Part b: Why do you think these were the months with most delay?

June, July and December are holiday months, therefore, like the arrival
delay even the departure delay is the most in these months.

### Part c: Which airline carrier has the most number of delays in those months?

``` r
#Filtering data of month 6 with positive departure delay
df1 = data1 %>% filter(month == 6 & dep_delay >0)
dim(df1)
```

    ## [1] 12558    19

``` r
#Plotting the data
ggplot(df1, aes(carrier, fill = dep_delay)) + geom_histogram(stat="count", fill="blue") + xlab("MONTH 6(carrier)") + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk5-1.png)<!-- -->

``` r
#Filtering data of month 7 with positive arrival delay
df2 = data1 %>% filter(month == 7 & dep_delay >0)
dim(df2)
```

    ## [1] 13773    19

``` r
#Plotting the data
ggplot(df2, aes(carrier, fill = dep_delay)) + geom_histogram(stat="count", fill="orange") + xlab("MONTH 7(carrier)") + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk5-2.png)<!-- -->

``` r
#Filtering data of month 12 with positive arrival delay
df3 = data1 %>% filter(month == 12 & dep_delay >0)
dim(df3)
```

    ## [1] 13490    19

``` r
#Plotting the data
ggplot(df3, aes(carrier, fill = dep_delay)) + geom_histogram(stat="count", fill="brown") + xlab("MONTH 12(carrier)") + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk5-3.png)<!-- -->

UA flights have the highest departure delays.

### Part d: Choose a month and check if the airline had the most number of departure delays that month

``` r
#Choosing month 3 to check the departure delays
df4 = data1 %>% filter(month == 3 & dep_delay >0)
dim(df4)
```

    ## [1] 11166    19

``` r
#Plotting the data
ggplot(df4, aes(carrier, fill = dep_delay)) + geom_histogram(stat="count", fill="darkorchid4") + xlab("MONTH 3(carrier)") + geom_text(stat = "count", aes(label=..count..), vjust = -0.2)
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk%206-1.png)<!-- -->

No, EV had the highest delay in other months as well.

### Question 3: Was there any correlation between 1 and 2

Since months 6, 7 and 12 are holiday months, the arrival and departure
delays are the highest in these months. Other than that, there is no
correlation between the other two parts.

### Question 4: Which airports have the highest departure delay?

``` r
#Filtering the data for positive departure delay
df6 = data1 %>% filter(dep_delay >0)

#Getting the top 20 values for the plot
df6 = head(arrange(df6, desc(dep_delay)),20)

#Plotting the data
ggplot(df6,aes(x=origin, y=dep_delay)) + geom_histogram(stat="identity", fill="blue")
```

    ## Warning: Ignoring unknown parameters: binwidth, bins, pad

![](problemset-template_files/figure-gfm/chunk6-1.png)<!-- -->

From the above graph we can see that JFK, EWR and LGA have the highest
departure delay. All these airport are in NYC.

### Question 5: Did more number of flights that departed from 6am to 6pm get delayed?

``` r
#Filtering out all the flights that had a delay in departure
data1 = data1 %>% filter(dep_delay>0)

#Grouping by month and factor whether it departed between 6am to 6pm.
dep_times <- data1 %>% group_by(month) %>% mutate( dep = ifelse(dep_time>600& dep_time<1800 , 1, 0))

#Plotting the data
p <- ggplot(dep_times, aes(month, fill = factor(dep)), labels = TRUE) + 
   geom_histogram() + scale_x_continuous(breaks = c(1:12))

#Using plotly to make it an interactive plot!
ggplotly(p)
```

<!--html_preserve-->

<div id="htmlwidget-5fc6799fba81fd6a6672" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-5fc6799fba81fd6a6672">{"x":{"data":[{"orientation":"v","width":[0.379310344827586,0.379310344827586,0.379310344827586,0.379310344827586,0.379310344827586,0.379310344827585,0.379310344827586,0.379310344827586,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827585,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587],"base":[6573,0,6246,0,0,7239,0,0,6547,0,7070,0,0,7752,0,8411,0,0,7525,0,0,5124,0,5701,0,0,5454,0,0,9088],"x":[1.13793103448276,1.51724137931034,1.89655172413793,2.27586206896552,2.6551724137931,3.03448275862069,3.41379310344828,3.79310344827586,4.17241379310345,4.55172413793103,4.93103448275862,5.31034482758621,5.68965517241379,6.06896551724138,6.44827586206896,6.82758620689655,7.20689655172414,7.58620689655172,7.96551724137931,8.3448275862069,8.72413793103448,9.10344827586207,9.48275862068965,9.86206896551724,10.2413793103448,10.6206896551724,11,11.3793103448276,11.7586206896552,12.1379310344828],"y":[3047,0,2842,0,0,3927,0,0,3937,0,4157,0,0,4806,0,5362,0,0,4140,0,0,2628,0,3003,0,0,2764,0,0,4402],"text":["count: 3047<br />month:  1.137931<br />factor(dep): 0","count:    0<br />month:  1.517241<br />factor(dep): 0","count: 2842<br />month:  1.896552<br />factor(dep): 0","count:    0<br />month:  2.275862<br />factor(dep): 0","count:    0<br />month:  2.655172<br />factor(dep): 0","count: 3927<br />month:  3.034483<br />factor(dep): 0","count:    0<br />month:  3.413793<br />factor(dep): 0","count:    0<br />month:  3.793103<br />factor(dep): 0","count: 3937<br />month:  4.172414<br />factor(dep): 0","count:    0<br />month:  4.551724<br />factor(dep): 0","count: 4157<br />month:  4.931034<br />factor(dep): 0","count:    0<br />month:  5.310345<br />factor(dep): 0","count:    0<br />month:  5.689655<br />factor(dep): 0","count: 4806<br />month:  6.068966<br />factor(dep): 0","count:    0<br />month:  6.448276<br />factor(dep): 0","count: 5362<br />month:  6.827586<br />factor(dep): 0","count:    0<br />month:  7.206897<br />factor(dep): 0","count:    0<br />month:  7.586207<br />factor(dep): 0","count: 4140<br />month:  7.965517<br />factor(dep): 0","count:    0<br />month:  8.344828<br />factor(dep): 0","count:    0<br />month:  8.724138<br />factor(dep): 0","count: 2628<br />month:  9.103448<br />factor(dep): 0","count:    0<br />month:  9.482759<br />factor(dep): 0","count: 3003<br />month:  9.862069<br />factor(dep): 0","count:    0<br />month: 10.241379<br />factor(dep): 0","count:    0<br />month: 10.620690<br />factor(dep): 0","count: 2764<br />month: 11.000000<br />factor(dep): 0","count:    0<br />month: 11.379310<br />factor(dep): 0","count:    0<br />month: 11.758621<br />factor(dep): 0","count: 4402<br />month: 12.137931<br />factor(dep): 0"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0","legendgroup":"0","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":[0.379310344827586,0.379310344827586,0.379310344827586,0.379310344827586,0.379310344827586,0.379310344827585,0.379310344827586,0.379310344827586,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827585,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587,0.379310344827587],"base":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"x":[1.13793103448276,1.51724137931034,1.89655172413793,2.27586206896552,2.6551724137931,3.03448275862069,3.41379310344828,3.79310344827586,4.17241379310345,4.55172413793103,4.93103448275862,5.31034482758621,5.68965517241379,6.06896551724138,6.44827586206896,6.82758620689655,7.20689655172414,7.58620689655172,7.96551724137931,8.3448275862069,8.72413793103448,9.10344827586207,9.48275862068965,9.86206896551724,10.2413793103448,10.6206896551724,11,11.3793103448276,11.7586206896552,12.1379310344828],"y":[6573,0,6246,0,0,7239,0,0,6547,0,7070,0,0,7752,0,8411,0,0,7525,0,0,5124,0,5701,0,0,5454,0,0,9088],"text":["count: 6573<br />month:  1.137931<br />factor(dep): 1","count:    0<br />month:  1.517241<br />factor(dep): 1","count: 6246<br />month:  1.896552<br />factor(dep): 1","count:    0<br />month:  2.275862<br />factor(dep): 1","count:    0<br />month:  2.655172<br />factor(dep): 1","count: 7239<br />month:  3.034483<br />factor(dep): 1","count:    0<br />month:  3.413793<br />factor(dep): 1","count:    0<br />month:  3.793103<br />factor(dep): 1","count: 6547<br />month:  4.172414<br />factor(dep): 1","count:    0<br />month:  4.551724<br />factor(dep): 1","count: 7070<br />month:  4.931034<br />factor(dep): 1","count:    0<br />month:  5.310345<br />factor(dep): 1","count:    0<br />month:  5.689655<br />factor(dep): 1","count: 7752<br />month:  6.068966<br />factor(dep): 1","count:    0<br />month:  6.448276<br />factor(dep): 1","count: 8411<br />month:  6.827586<br />factor(dep): 1","count:    0<br />month:  7.206897<br />factor(dep): 1","count:    0<br />month:  7.586207<br />factor(dep): 1","count: 7525<br />month:  7.965517<br />factor(dep): 1","count:    0<br />month:  8.344828<br />factor(dep): 1","count:    0<br />month:  8.724138<br />factor(dep): 1","count: 5124<br />month:  9.103448<br />factor(dep): 1","count:    0<br />month:  9.482759<br />factor(dep): 1","count: 5701<br />month:  9.862069<br />factor(dep): 1","count:    0<br />month: 10.241379<br />factor(dep): 1","count:    0<br />month: 10.620690<br />factor(dep): 1","count: 5454<br />month: 11.000000<br />factor(dep): 1","count:    0<br />month: 11.379310<br />factor(dep): 1","count:    0<br />month: 11.758621<br />factor(dep): 1","count: 9088<br />month: 12.137931<br />factor(dep): 1"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":54.7945205479452},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.379310344827586,12.8965517241379],"tickmode":"array","ticktext":["1","2","3","4","5","6","7","8","9","10","11","12"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12],"categoryorder":"array","categoryarray":["1","2","3","4","5","6","7","8","9","10","11","12"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"month","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-688.65,14461.65],"tickmode":"array","ticktext":["0","5000","10000"],"tickvals":[0,5000,10000],"categoryorder":"array","categoryarray":["0","5000","10000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.93503937007874},"annotations":[{"text":"factor(dep)","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"4b0025ff4705":{"x":{},"fill":{},"type":"bar"}},"cur_data":"4b0025ff4705","visdat":{"4b0025ff4705":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

Therefore, from the above data we can see that most number of flights
that departed between 6am and 6pm got delayed.

### (d) Challenge Your Results:

The challenges with this data set are :

1.  It is not well structured
2.  The data is not sufficient to reach thorough conclusions.
3.  Few columns in the data set are not useful.
4.  There is no data if we want to explore the delay due to stop-overs.
