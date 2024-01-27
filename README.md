R Indego Bike-Share Case Study
================
Andy Kwon
2024-01-26

## Business Scenario

You are a junior data analyst working on the marketing analyst team at
Indego, a bike-share company in Philadelphia. The director of marketing
believes the company’s future success depends on maximizing the number
of annual memberships. Therefore, your team wants to understand how
casual riders and annual members use Indego bikes differently. From
these insights, your team will design a new marketing strategy to
convert casual riders into annual members. But first, Indego executives
must approve your recommendations, so they must be backed up with
compelling data insights and professional data visualizations.

## ASK

Two questions will guide the future marketing program:

1.  How do annual members and casual riders use Indego bikes
    differently?
2.  How can Indego use digital media to influence casual riders to
    become members?

## PREPARE

Dataset: <https://www.rideindego.com/about/data>

-For this purposes of this analysis, I will use Q1 2023 to Q4 2023 data

``` r
#install packages
install.packages("tidyverse")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/dx/z26zyhcs0lz2nmgy5_8pk3t40000gn/T//RtmpMsl7pj/downloaded_packages

``` r
install.packages("lubridate")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/dx/z26zyhcs0lz2nmgy5_8pk3t40000gn/T//RtmpMsl7pj/downloaded_packages

``` r
install.packages("ggplot2")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/dx/z26zyhcs0lz2nmgy5_8pk3t40000gn/T//RtmpMsl7pj/downloaded_packages

``` r
install.packages("dplyr")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/dx/z26zyhcs0lz2nmgy5_8pk3t40000gn/T//RtmpMsl7pj/downloaded_packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(lubridate)
library(ggplot2)
library(dplyr)
```

``` r
#import data sets
q1_2023 <- read.csv("/Users/andyokwon/workspace/csv_indego/indego-trips-2023-q1.csv")
q2_2023 <- read.csv("/Users/andyokwon/workspace/csv_indego/indego-trips-2023-q2.csv")
q3_2023 <- read.csv("/Users/andyokwon/workspace/csv_indego/indego-trips-2023-q3.csv")
q4_2023 <- read.csv("/Users/andyokwon/workspace/csv_indego/indego-trips-2023-q4.csv")
```

## PROCESS

First, we need to check the column names first before merging the four
datasets.

``` r
colnames(q1_2023)
```

    ##  [1] "trip_id"             "duration"            "start_time"         
    ##  [4] "start_station"       "start_lat"           "start_lon"          
    ##  [7] "end_station"         "end_lat"             "end_lon"            
    ## [10] "bike_id"             "plan_duration"       "trip_route_category"
    ## [13] "passholder_type"     "bike_type"

``` r
colnames(q2_2023)
```

    ##  [1] "trip_id"             "duration"            "start_time"         
    ##  [4] "start_station"       "start_lat"           "start_lon"          
    ##  [7] "end_station"         "end_lat"             "end_lon"            
    ## [10] "bike_id"             "plan_duration"       "trip_route_category"
    ## [13] "passholder_type"     "bike_type"

``` r
colnames(q3_2023)
```

    ##  [1] "trip_id"             "duration"            "start_time"         
    ##  [4] "start_station"       "start_lat"           "start_lon"          
    ##  [7] "end_station"         "end_lat"             "end_lon"            
    ## [10] "bike_id"             "plan_duration"       "trip_route_category"
    ## [13] "passholder_type"     "bike_type"

``` r
colnames(q4_2023)
```

    ##  [1] "trip_id"             "duration"            "start_time"         
    ##  [4] "start_station"       "start_lat"           "start_lon"          
    ##  [7] "end_station"         "end_lat"             "end_lon"            
    ## [10] "bike_id"             "plan_duration"       "trip_route_category"
    ## [13] "passholder_type"     "bike_type"

Great, all column names from the datasets match. Now, we will check the
datatypes of each column to make sure that all data is formatted
properly.

``` r
str(q1_2023)
```

    ## 'data.frame':    166858 obs. of  14 variables:
    ##  $ trip_id            : int  579507686 579499091 579499089 579499087 579499085 579499083 579499081 579499079 579834968 579499077 ...
    ##  $ duration           : int  70 17 11 1 27 13 5 27 20 22 ...
    ##  $ start_time         : chr  "2023-01-01" "2023-01-01" "2023-01-01" "2023-01-01" ...
    ##  $ start_station      : int  3063 3190 3182 3026 3046 3026 3046 3070 3164 3154 ...
    ##  $ start_lat          : num  39.9 39.9 40 39.9 40 ...
    ##  $ start_lon          : num  -75.2 -75.2 -75.2 -75.1 -75.1 ...
    ##  $ end_station        : int  3063 3207 3102 3026 3112 3041 3295 3205 3000 3028 ...
    ##  $ end_lat            : num  39.9 40 40 39.9 40 ...
    ##  $ end_lon            : num  -75.2 -75.2 -75.2 -75.1 -75.2 ...
    ##  $ bike_id            : int  22584 5281 22366 17780 16897 21897 19836 22769 16557 5257 ...
    ##  $ plan_duration      : int  365 30 30 30 30 30 30 1 30 365 ...
    ##  $ trip_route_category: chr  "Round Trip" "One Way" "One Way" "Round Trip" ...
    ##  $ passholder_type    : chr  "Indego365" "Indego30" "Indego30" "Indego30" ...
    ##  $ bike_type          : chr  "electric" "standard" "electric" "electric" ...

``` r
str(q2_2023)
```

    ## 'data.frame':    291894 obs. of  14 variables:
    ##  $ trip_id            : int  614756806 614756975 614752194 614754717 614754719 614752944 614757459 614754723 614757365 614777079 ...
    ##  $ duration           : int  9 12 4 5 5 1 19 1 14 141 ...
    ##  $ start_time         : chr  "2023-04-01" "2023-04-01" "2023-04-01" "2023-04-01" ...
    ##  $ start_station      : int  3255 3045 3254 3010 3010 3041 3124 3041 3056 3156 ...
    ##  $ start_lat          : num  40 39.9 40 39.9 39.9 ...
    ##  $ start_lon          : num  -75.2 -75.2 -75.2 -75.2 -75.2 ...
    ##  $ end_station        : int  3285 3157 3320 3244 3244 3041 3124 3041 3010 3205 ...
    ##  $ end_lat            : num  40 39.9 40 39.9 39.9 ...
    ##  $ end_lon            : num  -75.1 -75.2 -75.2 -75.2 -75.2 ...
    ##  $ bike_id            : chr  "27958" "27940" "24523" "3636" ...
    ##  $ plan_duration      : int  365 30 30 365 365 30 1 30 30 30 ...
    ##  $ trip_route_category: chr  "One Way" "One Way" "One Way" "One Way" ...
    ##  $ passholder_type    : chr  "Indego365" "Indego30" "Indego30" "Indego365" ...
    ##  $ bike_type          : chr  "electric" "electric" "electric" "standard" ...

``` r
str(q3_2023)
```

    ## 'data.frame':    353256 obs. of  14 variables:
    ##  $ trip_id            : int  677293140 677304406 677304584 677302282 677304444 677303703 677303750 677304589 677305488 677305762 ...
    ##  $ duration           : int  2 27 32 6 27 9 9 30 48 51 ...
    ##  $ start_time         : chr  "2023-07-01" "2023-07-01" "2023-07-01" "2023-07-01" ...
    ##  $ start_station      : int  3271 3060 3057 3038 3060 3158 3066 3047 3053 3296 ...
    ##  $ start_lat          : num  39.9 40 40 39.9 40 ...
    ##  $ start_lon          : num  -75.2 -75.2 -75.2 -75.2 -75.2 ...
    ##  $ end_station        : int  3246 3255 3165 3256 3255 3253 3150 3035 3152 3273 ...
    ##  $ end_lat            : num  39.9 40 40 40 40 ...
    ##  $ end_lon            : num  -75.2 -75.2 -75.2 -75.2 -75.2 ...
    ##  $ bike_id            : chr  "25775" "14583" "5191" "19170" ...
    ##  $ plan_duration      : int  30 30 1 365 30 30 30 30 365 365 ...
    ##  $ trip_route_category: chr  "One Way" "One Way" "One Way" "One Way" ...
    ##  $ passholder_type    : chr  "Indego30" "Indego30" "Day Pass" "Indego365" ...
    ##  $ bike_type          : chr  "electric" "standard" "standard" "electric" ...

``` r
str(q4_2023)
```

    ## 'data.frame':    272539 obs. of  14 variables:
    ##  $ trip_id            : int  753629944 753630821 753634977 753623433 753632036 753633911 753627972 753634323 753635528 753636170 ...
    ##  $ duration           : int  9 8 10 6 9 10 7 8 11 21 ...
    ##  $ start_time         : chr  "2023-10-01" "2023-10-01" "2023-10-01" "2023-10-01" ...
    ##  $ start_station      : int  3166 3058 3010 3046 3342 3005 3333 3187 3346 3294 ...
    ##  $ start_lat          : num  40 40 39.9 40 40 ...
    ##  $ start_lon          : num  -75.1 -75.2 -75.2 -75.1 -75.2 ...
    ##  $ end_station        : int  3243 3039 3314 3295 3059 3050 3032 3203 3292 3165 ...
    ##  $ end_lat            : num  40 40 39.9 40 40 ...
    ##  $ end_lon            : num  -75.1 -75.2 -75.1 -75.2 -75.2 ...
    ##  $ bike_id            : chr  "22288" "25464" "25174" "29414" ...
    ##  $ plan_duration      : int  30 30 30 30 30 30 30 30 365 30 ...
    ##  $ trip_route_category: chr  "One Way" "One Way" "One Way" "One Way" ...
    ##  $ passholder_type    : chr  "Indego30" "Indego30" "Indego30" "Indego30" ...
    ##  $ bike_type          : chr  "electric" "electric" "electric" "electric" ...

We need to change the bike id in datasets q2, q3, and q4 to be integers
instead of char.

``` r
suppressWarnings({
  q2_2023 <- mutate(q2_2023, bike_id = as.integer(bike_id))
  q3_2023 <- mutate(q3_2023, bike_id = as.integer(bike_id))
  q4_2023 <- mutate(q4_2023, bike_id = as.integer(bike_id))
})
```

The datasets are now ready to be merged into a single dataframe.

``` r
indego_trips = bind_rows(q1_2023, q2_2023, q3_2023, q4_2023)
```

Next, we will add columns that list the day, month, and year of each
trip. This will allow us to aggregate trip data for each day, month or
year.

``` r
indego_trips$date <- as.Date(indego_trips$start_time)
indego_trips$month <- format(as.Date(indego_trips$date), "%m")
indego_trips$day <- format(as.Date(indego_trips$date), "%d")
indego_trips$year <- format(as.Date(indego_trips$date), "%Y")
indego_trips$day_of_week <- format(as.Date(indego_trips$date), "%A")
colnames(indego_trips)
```

    ##  [1] "trip_id"             "duration"            "start_time"         
    ##  [4] "start_station"       "start_lat"           "start_lon"          
    ##  [7] "end_station"         "end_lat"             "end_lon"            
    ## [10] "bike_id"             "plan_duration"       "trip_route_category"
    ## [13] "passholder_type"     "bike_type"           "date"               
    ## [16] "month"               "day"                 "year"               
    ## [19] "day_of_week"

Before performing analysis, let’s look at the final dataframe

``` r
glimpse(indego_trips)
```

    ## Rows: 1,084,547
    ## Columns: 19
    ## $ trip_id             <int> 579507686, 579499091, 579499089, 579499087, 579499…
    ## $ duration            <int> 70, 17, 11, 1, 27, 13, 5, 27, 20, 22, 9, 7, 13, 25…
    ## $ start_time          <chr> "2023-01-01", "2023-01-01", "2023-01-01", "2023-01…
    ## $ start_station       <int> 3063, 3190, 3182, 3026, 3046, 3026, 3046, 3070, 31…
    ## $ start_lat           <dbl> 39.94633, 39.94892, 39.95081, 39.94182, 39.95012, …
    ## $ start_lon           <dbl> -75.16980, -75.16991, -75.16953, -75.14550, -75.14…
    ## $ end_station         <int> 3063, 3207, 3102, 3026, 3112, 3041, 3295, 3205, 30…
    ## $ end_lat             <dbl> 39.94633, 39.95441, 39.96759, 39.94182, 39.95373, …
    ## $ end_lon             <dbl> -75.16980, -75.19200, -75.17952, -75.14550, -75.21…
    ## $ bike_id             <int> 22584, 5281, 22366, 17780, 16897, 21897, 19836, 22…
    ## $ plan_duration       <int> 365, 30, 30, 30, 30, 30, 30, 1, 30, 365, 365, 365,…
    ## $ trip_route_category <chr> "Round Trip", "One Way", "One Way", "Round Trip", …
    ## $ passholder_type     <chr> "Indego365", "Indego30", "Indego30", "Indego30", "…
    ## $ bike_type           <chr> "electric", "standard", "electric", "electric", "e…
    ## $ date                <date> 2023-01-01, 2023-01-01, 2023-01-01, 2023-01-01, 2…
    ## $ month               <chr> "01", "01", "01", "01", "01", "01", "01", "01", "0…
    ## $ day                 <chr> "01", "01", "01", "01", "01", "01", "01", "01", "0…
    ## $ year                <chr> "2023", "2023", "2023", "2023", "2023", "2023", "2…
    ## $ day_of_week         <chr> "Sunday", "Sunday", "Sunday", "Sunday", "Sunday", …

## ANALYZE

We will now perform a descriptive analysis of the data and see if we can
find any trends/patterns between daily, monthly, and yearly riders.
First, we will look at some basic descriptive statistics of the duration
of bike trips in minutes.

``` r
mean(indego_trips$duration)
```

    ## [1] 16.1975

``` r
median(indego_trips$duration)
```

    ## [1] 10

``` r
max(indego_trips$duration)
```

    ## [1] 1440

``` r
min(indego_trips$duration)
```

    ## [1] 1

Now, we will compare day(daily pass), indego30(monthly pass), and indego
365(yearly pass) riders.

``` r
aggregate(indego_trips$duration ~ indego_trips$passholder_type, FUN = mean)
```

    ##   indego_trips$passholder_type indego_trips$duration
    ## 1                                           15.20588
    ## 2                     Day Pass              41.11176
    ## 3                     Indego30              15.65044
    ## 4                    Indego365              11.96385
    ## 5                         NULL              16.75652

``` r
aggregate(indego_trips$duration ~ indego_trips$passholder_type, FUN = median)
```

    ##   indego_trips$passholder_type indego_trips$duration
    ## 1                                                8.5
    ## 2                     Day Pass                  23.0
    ## 3                     Indego30                  10.0
    ## 4                    Indego365                   9.0
    ## 5                         NULL                  10.0

Before continuing, arrange the day_of_week column in the correct order.

``` r
indego_trips$day_of_week <- ordered(indego_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

Next, we will check the average ride time per day and the total number
of trips for daily, monthly, and yearly subscribers.

``` r
plot <- indego_trips %>% 
  group_by(passholder_type, day_of_week) %>%  #groups by type of customer
  summarise(number_of_rides = n() #calculates the number of rides and average duration 
  ,average_ride_length = mean(duration),.groups="drop") %>% # calculates the average duration
  arrange(passholder_type, day_of_week) #sort
```

## SHARE

Next, we will create visualizations to share with stakeholders as well
as give us a better idea of what insights to share.

``` r
 ggplot(plot,aes(x = day_of_week, y = number_of_rides, fill = passholder_type)) + 
 labs(title ="Total rides of Customers vs. Day of the Week") +
 geom_col(width=0.5, position = position_dodge(width=0.5))+
 scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

![](https://raw.githubusercontent.com/akwon191/indego_project/49c93890dc5d61c44051dc679c2fc25e6120b486/IMG_1.png)

This chart indicates that monthly customers use Indego bikes the most,
followed by yearly customers, and then lastly daily customers. It also
shows that monthly and yearly customers ride bikes more often during the
weekdays, while daily customers ride more often during the weekends.

``` r
indego_trips %>%  
  group_by(passholder_type, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(passholder_type, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = passholder_type)) +
  labs(title ="Total rides of Customers by Month") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

![](https://raw.githubusercontent.com/akwon191/indego_project/49c93890dc5d61c44051dc679c2fc25e6120b486/IMG_2.png)

The above chart indicates that all customers tended to cycle more during
April-October, which is understandable due to the warmer weather.

``` r
ggplot(plot,aes(x = day_of_week, y = average_ride_length, fill = passholder_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time of Customers Vs. Day of the week")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
```

![](https://raw.githubusercontent.com/akwon191/indego_project/49c93890dc5d61c44051dc679c2fc25e6120b486/IMG_3.png)

We can observe that daily customers cycled for much longer periods of
time throughout the week than monthly and yearly customers.

## ACT

For the last step in the data analysis process, we will make three
recommendations to increase the number of Subscribers every year. First,
we will review insights that were made during the share process.

### Key Insights:

1.  Monthly customers used Indego bikes than both yearly and daily
    customers combined. Yearly customers used bikes more frequently than
    daily customers also by a wide margin.
2.  All customers used Indego bikes more frequently during the warmer
    months of the year (April-October)
3.  Daily customers rode Indego bikes for longer periods of time than
    monthly and yearly customers.

### Recommendations:

1.  Target new or daily customers on the weekends and provide discounts
    for these groups to switch to monthly and/or yearly subscriptions.
2.  Create a Indego bike share campaign during the summer months to
    attract more potential customers.
