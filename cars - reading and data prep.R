library(tidyr)
library(forcats)
library(tidyverse)
library(stringr)

df.car_torque <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_torque_DATA.txt"))
df.car_0_60_times  <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
df.car_engine_size <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_engine-size_DATA.txt"))
df.car_horsepower  <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_horsepower_DATA.txt"))
df.car_top_speed   <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_top-speed_DATA.txt"))
df.car_power_to_weight <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_power-to-weight_DATA.txt"))

#Inspecting data: Glimpse, Head, View, names, table, level 
glimpse(df.car_torque)
glimpse(df.car_0_60_times)
glimpse(df.car_engine_size)
glimpse(df.car_horsepower)
glimpse(df.car_top_speed)
glimpse(df.car_power_to_weight)



# INSPECT WITH head()
head(df.car_torque)
head(df.car_0_60_times)
head(df.car_engine_size)
head(df.car_horsepower)
head(df.car_top_speed)
head(df.car_power_to_weight)

# identifying duplicates
df.car_torque%>%
  group_by(car_full_nm)%>%
  summarise(count=n()) %>%
  filter(count!=1)

df.car_0_60_times %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_horsepower %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_top_speed %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_power_to_weight %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)

#duplicate records were found as follows
#1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
#2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
#3                   Pontiac Bonneville 6.4L V8 - [1960]     2
# inspect each record
df.car_0_60_times%>% filter(car_full_nm =="Pontiac Bonneville 6.4L V8 - [1960]")
df.car_top_speed %>% filter(car_full_nm == "Pontiac Bonneville 6.4L V8 - [1960]")
df.car_top_speed %>% filter(car_full_nm == "Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]")
df.car_top_speed %>% filter(car_full_nm == "Koenigsegg CCX 4.7 V8 Supercharged - [2006]")
df.car_0_60_times%>% filter(car_full_nm =="Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]")
df.car_0_60_times%>% filter(car_full_nm == "Koenigsegg CCX 4.7 V8 Supercharged - [2006]")

#quick dirty way of removing duplicates
df.car_0_60_times  <-distinct(df.car_0_60_times,car_full_nm, .keep_all = T)
df.car_engine_size <- distinct(df.car_engine_size ,car_full_nm, .keep_all = T)
df.car_horsepower  <- distinct(df.car_horsepower ,car_full_nm, .keep_all = T)
df.car_top_speed   <- distinct(df.car_top_speed ,car_full_nm, .keep_all = T)
df.car_torque      <- distinct(df.car_torque ,car_full_nm, .keep_all = T)
df.car_power_to_weight <- distinct(df.car_power_to_weight, car_full_nm, .keep_all = T)

#reinspecting
# need to join data sets need to find longest one to keep on left for left join
# looking at data variables on right, car horsepower has largest rows, using that for left join
df.car_specs_all<-left_join(df.car_horsepower, df.car_power_to_weight, by="car_full_nm")
df.car_specs_all<-left_join(df.car_specs_all, df.car_engine_size, by="car_full_nm")
df.car_specs_all<-left_join(df.car_specs_all, df.car_torque, by="car_full_nm")
df.car_specs_all<-left_join(df.car_specs_all, df.car_top_speed, by="car_full_nm")
df.car_specs_all<-left_join(df.car_specs_all, df.car_0_60_times, by="car_full_nm")

#inspecting this dataset
head(df.car_specs_all)
glimpse(df.car_specs_all)
df.car_specs_all%>%group_by(car_full_nm)%>%summarize(count=n())%>%filter(count!=1)
# no duplicate but engine size is char so mutating
df.car_specs_all<-mutate(df.car_specs_all, engine_size_cc=as.integer(engine_size_cc))
# the other problem is the year in the car name. this can be made into a separate column
df.car_specs_all$car_full_nm %>% str_sub(-5, -2)
# it extracted the year - so now creating new variable with all years extracted like this
df.car_specs_all<-mutate(df.car_specs_all, year=str_sub(car_full_nm, -5, -2))
#inspecting at every step
glimpse(df.car_specs_all)
#year needs be a factor var, not char
df.car_specs_all$year<-df.car_specs_all$year %>% as.factor()
glimpse(df.car_specs_all)
head(df.car_specs_all)
# wrong command levels(df.car_specs_all) 
levels(df.car_specs_all$year)
#creating decade variable to inspect data by decade improvements
df.car_specs_all<-mutate (df.car_specs_all, decade = as.factor(ifelse(str_sub(df.car_specs_all$year,1,3)=='193', '1930s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='194', '1940s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='195', '1950s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='196', '1960s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='197', '1970s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='198', '1980s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='199', '1990s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='200', '2000s',
                                                               ifelse(str_sub(df.car_specs_all$year,1,3)=='201', '2010s',
                                                               "ERROR")))))))))))
glimpse(df.car_specs_all$decade)
levels(df.car_specs_all$decade)                                                                                        
df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%summarize(count=n()) 
df.car_specs_all %>%  group_by(decade) %>% summarise(count = n())
#directly inspecting the variable data frame
View(df.car_specs_all)
unique(df.car_specs_all$make)
#draw out car brand name as new variable
df.car_specs_all<-mutate(df.car_specs_all, make = str_replace(car_full_nm," .*$", " ") %>% as.factor())
#unique returns a vector, data frame or array like x but with duplicate elements/rows removed.
unique(df.car_specs_all$make)
# add car weight variable
df.car_specs_all<-mutate(df.car_specs_all, car_weight = horsepower_bhp / horsepower_per_ton_bhp)
#add torque per ton variable
df.car_specs_all <- mutate(ddf.car_specs_all, torque_per_ton = torque_lb_ft / car_weight)
#inspecting again via frequency table
df.car_specs_all%>% group_by(make) %>% summarise(count = n()) 
#BAD df.car_specs_all%>% group_by(make) %>% arrange(desc(summarise(count = n())))
df.car_specs_all%>% group_by(make) %>% summarise(make_count = length(make)) %>% arrange(desc(make_count))

#Output was 

R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

Loading required package: sp
> library(tidyr)
> library(forcats)
> library(tidyverse)
Loading tidyverse: ggplot2
Loading tidyverse: tibble
Loading tidyverse: readr
Loading tidyverse: purrr
Loading tidyverse: dplyr
Conflicts with tidy packages ---------------------------------------------------
  filter(): dplyr, stats
lag():    dplyr, stats
> library(stringr)
> 
  > df.car_torque <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_torque_DATA.txt"))
Parsed with column specification:
  cols(
    car_full_nm = col_character(),
    torque_lb_ft = col_integer(),
    rpm_torque_measure_point = col_integer()
  )
> df.car_0_60_times  <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
Parsed with column specification:
  cols(
    car_full_nm = col_character(),
    car_0_60_time_seconds = col_double()
  )
> df.car_engine_size <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_engine-size_DATA.txt"))
Parsed with column specification:
  cols(
    car_full_nm = col_character(),
    engine_size_cc = col_character(),
    engine_size_ci = col_double()
  )
Warning: 1 parsing failure.
row col  expected    actual
712  -- 3 columns 4 columns

> df.car_horsepower  <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_horsepower_DATA.txt"))
Parsed with column specification:
  cols(
    car_full_nm = col_character(),
    horsepower_bhp = col_integer(),
    rpm_horsepower_measure_point = col_integer()
  )
> df.car_top_speed   <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_top-speed_DATA.txt"))
Parsed with column specification:
  cols(
    car_full_nm = col_character(),
    top_speed_mph = col_integer(),
    top_speed_kph = col_integer()
  )
> df.car_power_to_weight <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_power-to-weight_DATA.txt"))
Parsed with column specification:
  cols(
    car_full_nm = col_character(),
    horsepower_per_ton_bhp = col_double()
  )
> 
  > glimpse(df.car_torque)
Observations: 1,580
Variables: 3
$ car_full_nm              <chr> "Bugatti Veyron 16.4 Grand Sport Vitesse -...
$ torque_lb_ft             <int> 1106, 1094, 1042, 981, 972, 922, 885, 885,...
$ rpm_torque_measure_point <int> 3000, 6150, 5000, 1, 4200, 2200, 4100, 410...
> head(df.car_torque)
# A tibble: 6 × 3
car_full_nm torque_lb_ft
<chr>        <int>
1 Bugatti Veyron 16.4 Grand Sport Vitesse - [2012]         1106
2                    SSC Ultimate Aero TT - [2008]         1094
3          SSC Tuatara 6.9L V8 Twin Turbo - [2011]         1042
4                     Fisker Karma Hybrid - [2011]          981
5                      Hennessey Venom GT - [2010]          972
6            Bugatti Veyron 8.0 litre W16 - [2005]          922
# ... with 1 more variables: rpm_torque_measure_point <int>
> View(df.car_0_60_times)
> View(df.car_0_60_times)
> View(df.car_0_60_times)
> View(df.car_0_60_times)
> View(df.car_engine_size)
> View(df.car_horsepower)
> View(df.car_top_speed)
> View(df.car_torque)
> View(df.usa_rivers)
> View(df.usa_rivers)
> View(df.car_horsepower)
> View(df.car_0_60_times)
> df.car_torque%>%
+   group_by(car_full_nm)%>%
+   summarise(count=n()) %>%
+   head()
# A tibble: 6 × 2
car_full_nm count
<chr> <int>
1      AC 428 Coupe 7.0L V8 - [1967]     1
2          AC Cobra 212 S/C - [2001]     1
3              AC Cobra 427 - [1966]     1
4 Alfa-Romeo 145 Cloverleaf - [1994]     1
5         Alfa-Romeo 146 ti - [1994]     1
6        Alfa-Romeo 147 GTA - [2002]     1
> # identifying duplicates
> df.car_torque%>%
+   group_by(car_full_nm)%>%
+   summarise(count=n()) %>%
+   filter(count!=1)
# A tibble: 3 × 2
car_full_nm count
<chr> <int>
1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
3                   Pontiac Bonneville 6.4L V8 - [1960]     2
> View(df.car_torque)
> View(df.car_torque)
> df.car_0_60_times %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 1 × 2
car_full_nm count
<chr> <int>
1 Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
> df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 3 × 2
car_full_nm count
<chr> <int>
1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
3                   Pontiac Bonneville 6.4L V8 - [1960]     2
> df.car_horsepower %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 3 × 2
car_full_nm count
<chr> <int>
1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
3                   Pontiac Bonneville 6.4L V8 - [1960]     2
> df.car_top_speed %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 3 × 2
car_full_nm count
<chr> <int>
1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
3                   Pontiac Bonneville 6.4L V8 - [1960]     2
> df.car_power_to_weight %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 3 × 2
car_full_nm count
<chr> <int>
1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
3                   Pontiac Bonneville 6.4L V8 - [1960]     2
> 1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]     2
Error: unexpected symbol in "1 Chevrolet"
> 2           Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
Error: unexpected symbol in "2           Koenigsegg"
> 3                   Pontiac Bonneville 6.4L V8 - [1960]     2
Error: unexpected symbol in "3                   Pontiac"
> 3                   Pontiac Bonneville 6.4L V8 - [1960]     2
Error: unexpected symbol in "3                   Pontiac"
> 
> df.car_0_60_times%>%group_by(car_full_nm="Pontiac Bonneville 6.4L V8 - [1960]")
Source: local data frame [399 x 2]
Groups: car_full_nm [1]

car_full_nm car_0_60_time_seconds
<chr>                 <dbl>
1  Pontiac Bonneville 6.4L V8 - [1960]                   2.3
2  Pontiac Bonneville 6.4L V8 - [1960]                   2.5
3  Pontiac Bonneville 6.4L V8 - [1960]                   2.5
4  Pontiac Bonneville 6.4L V8 - [1960]                   2.5
5  Pontiac Bonneville 6.4L V8 - [1960]                   2.5
6  Pontiac Bonneville 6.4L V8 - [1960]                   2.5
7  Pontiac Bonneville 6.4L V8 - [1960]                   2.5
8  Pontiac Bonneville 6.4L V8 - [1960]                   2.6
9  Pontiac Bonneville 6.4L V8 - [1960]                   2.7
10 Pontiac Bonneville 6.4L V8 - [1960]                   2.8
# ... with 389 more rows
> View(df.car_0_60_times)
> View(df.car_0_60_times)
> df.car_0_60_times%>%filter(car_full_nm="Pontiac Bonneville 6.4L V8 - [1960]")
Error: filter() takes unnamed arguments. Do you need `==`?
> df.car_0_60_times%>%filter(car_full_nm=="Pontiac Bonneville 6.4L V8 - [1960]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> df.car_top_speed %>% filter(car_full_nm == "Pontiac Bonneville 6.4L V8 - [1960]")
# A tibble: 2 × 3
car_full_nm top_speed_mph top_speed_kph
<chr>         <int>         <int>
1 Pontiac Bonneville 6.4L V8 - [1960]           113           181
2 Pontiac Bonneville 6.4L V8 - [1960]           113           181
> df.car_0_60_times%>% filter(car_full_nm =="Pontiac Bonneville 6.4L V8 - [1960]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> df.car_0_60_times %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 1 × 2
car_full_nm count
<chr> <int>
1 Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
> df.car_0_60_times  <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
Parsed with column specification:
cols(
car_full_nm = col_character(),
car_0_60_time_seconds = col_double()
)
> df.car_0_60_times%>% filter(car_full_nm =="Pontiac Bonneville 6.4L V8 - [1960]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> df.car_0_60_times<-distinct(df.car_0_60_times,car_full_nm, .keep_all = T)
> df.car_engine_size <- distinct(df.car_engine_size ,car_full_nm, .keep_all = T)
> df.car_horsepower  <- distinct(df.car_horsepower ,car_full_nm, .keep_all = T)
> df.car_top_speed   <- distinct(df.car_top_speed ,car_full_nm, .keep_all = T)
> df.car_torque      <- distinct(df.car_torque ,car_full_nm, .keep_all = T)
> df.car_power_to_weight <- distinct(df.car_power_to_weight, car_full_nm, .keep_all = T)
> df.car_0_60_times%>% filter(car_full_nm =="Pontiac Bonneville 6.4L V8 - [1960]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> df.car_top_speed %>% filter(car_full_nm == "Pontiac Bonneville 6.4L V8 - [1960]")
# A tibble: 1 × 3
car_full_nm top_speed_mph top_speed_kph
<chr>         <int>         <int>
1 Pontiac Bonneville 6.4L V8 - [1960]           113           181
> df.car_top_speed %>% filter(car_full_nm == "Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]")
# A tibble: 1 × 3
car_full_nm top_speed_mph top_speed_kph
<chr>         <int>         <int>
1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]           112           180
> df.car_0_60_times%>% filter(car_full_nm =="Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> View(df.car_0_60_times)
> df.car_0_60_times %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, count <int>
> df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, count <int>
> df.car_top_speed %>% filter(car_full_nm == "Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]")
# A tibble: 1 × 3
car_full_nm top_speed_mph top_speed_kph
<chr>         <int>         <int>
1 Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]           112           180
> df.car_0_60_times%>% filter(car_full_nm =="Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> df.car_0_60_times%>% filter(car_full_nm =="Chevrolet Chevy II Nova SS 283 V8 Turbo Fire - [1964]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> df.car_0_60_times  <- read_csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/11/auto-snout_0-60-times_DATA.txt"))
Parsed with column specification:
cols(
car_full_nm = col_character(),
car_0_60_time_seconds = col_double()
)
> df.car_0_60_times%>% filter(car_full_nm =="Pontiac Bonneville 6.4L V8 - [1960]")
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, car_0_60_time_seconds <dbl>
> df.car_0_60_times %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
# A tibble: 1 × 2
car_full_nm count
<chr> <int>
1 Koenigsegg CCX 4.7 V8 Supercharged - [2006]     2
> df.car_0_60_times%>% filter(car_full_nm == "Koenigsegg CCX 4.7 V8 Supercharged - [2006]")
# A tibble: 2 × 2
car_full_nm car_0_60_time_seconds
<chr>                 <dbl>
1 Koenigsegg CCX 4.7 V8 Supercharged - [2006]                   3.2
2 Koenigsegg CCX 4.7 V8 Supercharged - [2006]                   3.2
> df.car_0_60_times  <-distinct(df.car_0_60_times,car_full_nm, .keep_all = T)
> df.car_0_60_times%>% filter(car_full_nm == "Koenigsegg CCX 4.7 V8 Supercharged - [2006]")
# A tibble: 1 × 2
car_full_nm car_0_60_time_seconds
<chr>                 <dbl>
1 Koenigsegg CCX 4.7 V8 Supercharged - [2006]                   3.2
> df.car_specs_all<-left_join(df.car_horsepower, df.car_power_to_weight, by="car_full_nm")
> df.car_specs_all<-left_join(df.car_specs_all, df.car_engine_size, by="car_full_nm")
> df.car_specs_all<-left_join(df.car_specs_all, df.car_torque, by="car_full_nm")
> df.car_specs_all<-left_join(df.car_specs_all, df.car_top_speed, by="car_full_nm")
> df.car_specs_all<-left_join(df.car_specs_all, df.car_0_60_times, by="car_full_nm")
> head(df.car_specs_all)
# A tibble: 6 × 11
car_full_nm horsepower_bhp rpm_horsepower_measure_point
<chr>          <int>                        <int>
1 Bugatti Veyron 8.0 litre W16 Super Sport - [2010]           1184                         6400
2  Bugatti Veyron 16.4 Grand Sport Vitesse - [2012]           1184                         6400
3                     SSC Ultimate Aero TT - [2008]           1183                         6950
4                Koenigsegg Agera R 5.0 V8 - [2012]           1124                         7100
5                         Porsche 9FF GT9R - [2009]           1120                         7850
6                   Koenigsegg Agera 5L V8 - [2011]           1100                         2700
# ... with 8 more variables: horsepower_per_ton_bhp <dbl>, engine_size_cc <chr>, engine_size_ci <dbl>,
#   torque_lb_ft <int>, rpm_torque_measure_point <int>, top_speed_mph <int>, top_speed_kph <int>,
#   car_0_60_time_seconds <dbl>
> glimpse(df.car_specs_all)
Observations: 1,578
Variables: 11
$ car_full_nm                  <chr> "Bugatti Veyron 8.0 litre W16 Super Sport - [2010]", "Bugatti Veyron 16.4...
$ horsepower_bhp               <int> 1184, 1184, 1183, 1124, 1120, 1100, 1030, 1016, 1004, 1001, 987, 908, 898...
$ rpm_horsepower_measure_point <int> 6400, 6400, 6950, 7100, 7850, 2700, 6500, 7100, 7000, 6000, 7850, 6600, 6...
$ horsepower_per_ton_bhp       <dbl> 644.1, 594.9, 946.4, 794.3, 832.0, 766.5, 844.2, 718.0, 784.3, 530.1, 680...
$ engine_size_cc               <chr> "7993", "7993", "6348", "5032", "4000", "5032", "6162", "5032", "4800", "...
$ engine_size_ci               <dbl> 487.7, 487.7, 387.3, 307.0, 244.0, 307.0, 376.0, 307.0, 292.9, 487.7, 244...
$ torque_lb_ft                 <int> NA, 1106, 1094, 885, 774, 885, 972, 811, 796, 922, 711, 771, 811, 650, 67...
$ rpm_torque_measure_point     <int> NA, 3000, 6150, 4100, 5970, 4100, 4200, 4100, 5600, 2200, 5970, 5800, 510...
$ top_speed_mph                <int> 258, 255, 273, 273, 256, 261, 260, 249, 254, 252, 255, 249, 242, 240, 242...
$ top_speed_kph                <int> 415, 410, 439, 439, 411, 420, 418, 400, 408, 405, 410, 400, 389, 386, 389...
$ car_0_60_time_seconds        <dbl> 2.5, 2.5, 2.8, 2.9, 2.9, 2.9, 2.5, 2.8, 2.9, 2.7, 3.0, 2.9, 3.1, 2.8, 3.8...
> df.car_specs_all%>%group_by(car_full_nm)%>%summarize(count=n())%>%filter(count!=1)
# A tibble: 0 × 2
# ... with 2 variables: car_full_nm <chr>, count <int>
> df.car_specs_all$car_full_nm %>% str_sub(-5, -2)
[1] "2010" "2012" "2008" "2012" "2009" "2011" "2010" "2013" "2008" "2005" "2008" "2005" "2010" "2000" "2005"
[16] "2006" "1997" "2013" "2012" "2011" "1996" "2013" "2009" "2013" "2013" "2012" "2013" "2000" "1995" "2013"
[31] "2013" "2011" "2013" "2008" "2011" "2012" "2009" "2008" "2010" "2002" "2011" "2009" "2007" "2009" "2006"
[46] "2005" "2008" "2014" "2014" "2007" "2009" "2006" "2013" "1994" "2014" "2006" "2013" "2012" "2004" "2010"
[61] "2009" "2012" "2013" "2010" "2003" "2009" "2007" "2012" "2012" "2012" "2013" "2003" "1998" "2006" "2003"
[76] "2010" "2005" "2012" "2008" "2004" "2005" "2009" "2008" "2014" "2009" "1998" "2007" "2008" "2007" "2013"
[91] "2013" "2005" "1992" "2011" "2012" "2012" "2001" "2011" "2014" "2013" "2007" "2006" "2004" "2008" "2007"
[106] "2001" "2013" "2012" "2013" "2009" "2011" "2009" "2009" "2011" "2011" "2013" "2013" "2009" "2012" "2003"
[121] "2003" "2005" "2008" "2011" "2012" "2011" "2011" "2012" "2012" "2012" "2013" "2013" "2005" "2014" "1993"
[136] "2003" "2000" "2000" "1997" "2010" "2012" "2013" "2013" "2002" "2009" "2013" "2003" "2000" "2011" "2012"
[151] "2012" "2012" "2011" "2013" "2011" "2013" "2012" "2012" "2004" "2007" "2007" "2012" "1983" "2012" "2010"
[166] "2007" "2013" "1995" "2012" "1994" "2009" "2006" "2012" "2009" "2012" "2008" "2009" "1999" "2006" "2003"
[181] "2005" "2011" "2013" "2013" "2013" "2007" "2012" "2012" "2012" "2011" "2002" "1991" "2006" "2006" "2006"
[196] "2009" "2007" "2009" "2009" "2009" "2012" "2002" "2003" "2009" "2006" "2006" "2006" "2004" "2005" "2006"
[211] "2009" "2008" "2012" "2012" "2007" "2005" "2008" "2013" "2013" "2013" "2003" "2014" "2008" "2007" "2009"
[226] "2011" "2009" "2005" "1993" "2012" "1997" "2004" "2005" "2012" "2006" "2009" "1987" "2006" "2004" "2008"
[241] "2006" "2007" "2004" "2006" "2009" "2012" "2012" "2010" "2003" "1987" "2006" "2001" "2013" "1968" "2000"
[256] "2008" "2003" "2012" "2012" "2008" "2007" "2008" "2003" "2000" "2005" "1996" "1987" "1970" "2006" "1985"
[271] "2002" "2005" "2010" "2010" "1998" "2012" "2011" "2010" "2002" "2012" "2004" "2013" "2013" "2008" "2013"
[286] "2004" "2013" "2013" "2010" "1994" "2007" "1992" "1998" "1998" "2013" "2008" "2009" "2008" "2013" "1995"
[301] "2006" "2013" "2004" "2008" "2003" "2004" "1969" "1966" "1968" "1970" "1971" "1971" "2013" "2013" "2013"
[316] "2014" "2011" "2012" "2012" "1992" "1999" "2008" "2009" "2000" "1997" "2005" "1986" "1998" "2008" "2012"
[331] "2012" "1999" "2014" "2006" "2006" "2006" "2008" "2007" "2005" "2006" "2007" "1995" "1966" "2006" "1995"
[346] "2004" "2013" "2013" "2009" "2011" "2008" "2010" "1991" "2008" "1999" "2000" "1992" "1984" "1999" "2002"
[361] "2003" "2004" "2003" "1964" "1989" "2004" "2002" "2001" "1998" "2005" "1968" "2010" "1968" "1968" "2013"
[376] "2012" "2012" "2011" "2011" "2012" "2011" "2013" "2014" "2004" "1959" "2009" "2003" "2001" "1984" "1970"
[391] "1970" "1970" "1973" "1970" "1970" "1992" "2012" "2012" "1971" "2004" "1966" "1967" "2011" "1971" "1995"
[406] "2005" "2009" "2006" "2003" "2005" "2009" "2008" "1969" "1968" "2013" "1990" "2004" "2000" "2013" "2012"
[421] "2002" "2009" "1997" "1998" "2003" "1969" "1967" "1970" "1982" "1966" "1967" "1993" "2013" "1970" "1970"
[436] "1970" "1970" "1998" "1977" "1974" "1969" "2004" "2006" "1969" "1969" "1962" "1966" "2009" "2013" "2007"
[451] "2008" "2008" "2011" "2003" "2011" "2008" "2006" "2003" "1996" "1967" "1966" "1969" "1965" "2004" "1966"
[466] "1969" "1968" "2012" "1962" "2004" "2013" "2013" "2007" "2001" "2003" "1973" "2006" "1969" "1996" "2003"
[481] "2002" "1967" "1959" "2010" "2009" "2001" "1969" "1970" "2003" "1967" "2008" "1979" "1998" "2006" "2003"
[496] "2003" "2003" "2000" "1967" "2012" "2012" "1991" "1967" "2001" "2011" "1969" "2008" "2007" "2005" "2012"
[511] "2014" "2004" "2001" "1999" "2001" "1998" "1999" "2005" "2003" "2005" "1960" "1969" "1955" "1968" "2004"
[526] "2006" "2006" "2009" "1993" "1992" "2006" "2003" "1971" "1969" "2010" "1976" "1966" "1968" "1969" "2013"
[541] "1986" "2014" "2014" "2014" "2014" "2009" "1988" "2004" "2009" "1995" "1973" "1959" "1969" "1965" "1971"
[556] "2009" "2001" "2012" "2008" "2013" "2014" "2014" "1999" "1995" "2005" "1993" "1993" "1992" "1973" "2008"
[571] "1967" "1968" "2001" "2002" "1964" "1986" "1962" "1967" "1969" "2013" "2007" "2007" "1989" "1990" "1998"
[586] "2001" "1996" "2010" "2005" "2004" "1994" "1993" "1993" "1999" "1986" "1965" "2001" "2012" "1968" "1998"
[601] "1967" "1992" "2004" "2002" "2007" "2011" "1966" "1997" "2009" "2006" "2010" "1969" "2012" "2011" "2011"
[616] "1991" "1989" "1959" "2012" "2013" "2012" "1999" "2001" "1974" "1976" "1968" "2011" "1960" "2011" "2002"
[631] "1967" "1965" "2006" "1984" "1971" "2013" "2013" "2011" "1987" "2003" "1990" "1967" "2008" "1958" "1961"
[646] "1964" "1988" "1964" "2008" "2007" "2006" "2009" "2011" "1959" "2013" "2013" "1997" "2009" "2008" "2003"
[661] "1993" "1999" "1998" "2004" "2004" "2001" "2004" "2003" "1960" "1970" "1986" "1980" "1962" "2004" "1966"
[676] "1966" "2012" "1971" "2013" "1970" "1990" "2002" "1990" "1997" "2003" "1962" "2010" "2012" "1965" "1998"
[691] "1977" "2013" "2013" "2013" "2012" "2012" "2012" "2012" "2008" "2006" "2003" "2005" "2000" "2011" "2008"
[706] "1989" "2013" "1969" "2005" "1995" "2012" "2002" "1969" "1967" "1970" "1997" "1998" "1993" "1985" "2000"
[721] "2010" "1993" "1975" "1972" "1967" "1972" "2002" "2005" "1957" "1963" "1965" "2002" "1992" "1993" "1992"
[736] "2002" "1984" "1999" "2008" "2013" "2006" "1990" "1992" "1964" "2011" "1952" "1991" "1998" "1978" "2009"
[751] "2000" "2000" "2002" "2003" "2003" "1993" "1999" "1998" "2000" "2006" "2007" "1996" "1996" "1997" "1996"
[766] "1997" "1997" "1997" "1998" "1998" "1998" "1999" "1999" "1999" "1995" "1990" "1998" "2002" "2012" "1998"
[781] "1996" "1997" "1998" "1998" "1987" "1963" "1993" "1956" "2001" "1985" "1971" "1993" "2001" "1971" "2009"
[796] "1995" "2011" "2009" "2003" "2012" "1965" "2008" "1990" "2012" "1986" "1972" "1986" "1986" "2008" "2004"
[811] "2008" "2000" "2006" "2003" "2012" "1995" "2009" "1974" "2001" "2012" "1961" "1961" "2010" "1966" "1964"
[826] "1972" "1969" "1989" "2009" "2008" "2011" "1997" "2006" "2003" "2009" "2006" "2008" "2009" "2012" "2005"
[841] "1999" "1969" "2007" "2008" "2012" "2005" "2006" "2007" "2007" "2007" "2003" "1994" "1994" "1995" "2012"
[856] "1994" "2005" "1965" "2010" "1998" "1993" "1994" "1972" "1969" "1986" "2013" "1972" "2001" "2001" "2007"
[871] "1973" "2007" "2001" "2006" "2008" "1975" "2013" "1985" "1988" "1989" "1992" "2000" "2004" "2011" "1954"
[886] "1989" "1967" "1968" "2001" "2012" "1992" "2009" "1973" "2008" "1994" "1994" "1997" "2011" "2005" "1993"
[901] "1999" "2010" "2011" "1991" "1991" "1977" "1992" "1965" "2002" "2009" "2004" "1998" "2002" "1991" "2005"
[916] "1995" "2006" "2011" "2012" "1985" "1981" "2005" "1977" "1999" "2004" "1996" "1958" "2004" "2001" "1996"
[931] "2006" "1992" "1993" "2009" "1993" "2004" "1957" "1959" "1972" "2013" "2007" "2008" "2002" "1992" "1992"
[946] "1996" "1997" "1999" "1982" "1982" "1980" "2010" "2003" "2013" "2009" "1992" "1992" "2010" "2012" "1965"
[961] "2011" "1988" "1996" "2001" "2008" "1993" "2000" "2002" "2001" "1966" "2003" "1984" "1999" "2006" "2001"
[976] "2013" "1992" "2007" "2006" "1986" "2007" "2000" "2013" "1984" "2003" "2002" "2002" "1971" "1982" "1983"
[991] "1956" "1993" "2008" "1987" "1972" "1987" "2005" "2008" "1988" "2002"
[ reached getOption("max.print") -- omitted 578 entries ]
> df.car_specs_all<-mutate(df.car_specs_all, engine_size_cc=as.integer(engine_size_cc))
> df.car_specs_all<-mutate(df.car_specs_all, engine_size_cc=as.integer(engine_size_cc))
> df.car_specs_all<-mutate(df.car_specs_all, year=str_sub(car_full_nm, -5, -2))
> glimpse(df.car_specs_all)
Observations: 1,578
Variables: 12
$ car_full_nm                  <chr> "Bugatti Veyron 8.0 litre W16 Super Sport - [2010]", "Bugatti Veyron 16.4...
$ horsepower_bhp               <int> 1184, 1184, 1183, 1124, 1120, 1100, 1030, 1016, 1004, 1001, 987, 908, 898...
$ rpm_horsepower_measure_point <int> 6400, 6400, 6950, 7100, 7850, 2700, 6500, 7100, 7000, 6000, 7850, 6600, 6...
$ horsepower_per_ton_bhp       <dbl> 644.1, 594.9, 946.4, 794.3, 832.0, 766.5, 844.2, 718.0, 784.3, 530.1, 680...
$ engine_size_cc               <int> 7993, 7993, 6348, 5032, 4000, 5032, 6162, 5032, 4800, 7993, 4000, 6257, 4...
$ engine_size_ci               <dbl> 487.7, 487.7, 387.3, 307.0, 244.0, 307.0, 376.0, 307.0, 292.9, 487.7, 244...
$ torque_lb_ft                 <int> NA, 1106, 1094, 885, 774, 885, 972, 811, 796, 922, 711, 771, 811, 650, 67...
$ rpm_torque_measure_point     <int> NA, 3000, 6150, 4100, 5970, 4100, 4200, 4100, 5600, 2200, 5970, 5800, 510...
$ top_speed_mph                <int> 258, 255, 273, 273, 256, 261, 260, 249, 254, 252, 255, 249, 242, 240, 242...
$ top_speed_kph                <int> 415, 410, 439, 439, 411, 420, 418, 400, 408, 405, 410, 400, 389, 386, 389...
$ car_0_60_time_seconds        <dbl> 2.5, 2.5, 2.8, 2.9, 2.9, 2.9, 2.5, 2.8, 2.9, 2.7, 3.0, 2.9, 3.1, 2.8, 3.8...
$ year                         <chr> "2010", "2012", "2008", "2012", "2009", "2011", "2010", "2013", "2008", "...
> df.car_specs_all<-mutate(df.car_specs_all$year %>% as.factor())
Error in UseMethod("mutate_") : 
no applicable method for 'mutate_' applied to an object of class "factor"
> df.car_specs_all$year<-df.car_specs_all$year %>% as.factor()
> glimpse(df.car_specs_all)
Observations: 1,578
Variables: 12
$ car_full_nm                  <chr> "Bugatti Veyron 8.0 litre W16 Super Sport - [2010]", "Bugatti Veyron 16.4...
$ horsepower_bhp               <int> 1184, 1184, 1183, 1124, 1120, 1100, 1030, 1016, 1004, 1001, 987, 908, 898...
$ rpm_horsepower_measure_point <int> 6400, 6400, 6950, 7100, 7850, 2700, 6500, 7100, 7000, 6000, 7850, 6600, 6...
$ horsepower_per_ton_bhp       <dbl> 644.1, 594.9, 946.4, 794.3, 832.0, 766.5, 844.2, 718.0, 784.3, 530.1, 680...
$ engine_size_cc               <int> 7993, 7993, 6348, 5032, 4000, 5032, 6162, 5032, 4800, 7993, 4000, 6257, 4...
$ engine_size_ci               <dbl> 487.7, 487.7, 387.3, 307.0, 244.0, 307.0, 376.0, 307.0, 292.9, 487.7, 244...
$ torque_lb_ft                 <int> NA, 1106, 1094, 885, 774, 885, 972, 811, 796, 922, 711, 771, 811, 650, 67...
$ rpm_torque_measure_point     <int> NA, 3000, 6150, 4100, 5970, 4100, 4200, 4100, 5600, 2200, 5970, 5800, 510...
$ top_speed_mph                <int> 258, 255, 273, 273, 256, 261, 260, 249, 254, 252, 255, 249, 242, 240, 242...
$ top_speed_kph                <int> 415, 410, 439, 439, 411, 420, 418, 400, 408, 405, 410, 400, 389, 386, 389...
$ car_0_60_time_seconds        <dbl> 2.5, 2.5, 2.8, 2.9, 2.9, 2.9, 2.5, 2.8, 2.9, 2.7, 3.0, 2.9, 3.1, 2.8, 3.8...
$ year                         <fctr> 2010, 2012, 2008, 2012, 2009, 2011, 2010, 2013, 2008, 2005, 2008, 2005, ...
> head(df.car_specs_all)
# A tibble: 6 × 12
car_full_nm horsepower_bhp rpm_horsepower_measure_point
<chr>          <int>                        <int>
  1 Bugatti Veyron 8.0 litre W16 Super Sport - [2010]           1184                         6400
2  Bugatti Veyron 16.4 Grand Sport Vitesse - [2012]           1184                         6400
3                     SSC Ultimate Aero TT - [2008]           1183                         6950
4                Koenigsegg Agera R 5.0 V8 - [2012]           1124                         7100
5                         Porsche 9FF GT9R - [2009]           1120                         7850
6                   Koenigsegg Agera 5L V8 - [2011]           1100                         2700
# ... with 9 more variables: horsepower_per_ton_bhp <dbl>, engine_size_cc <int>, engine_size_ci <dbl>,
#   torque_lb_ft <int>, rpm_torque_measure_point <int>, top_speed_mph <int>, top_speed_kph <int>,
#   car_0_60_time_seconds <dbl>, year <fctr>
> levels(df.car_specs_all)
NULL
> levels(df.car_specs_all$year)
[1] "1939" "1947" "1948" "1949" "1950" "1951" "1952" "1953" "1954" "1955" "1956" "1957" "1958" "1959" "1960"
[16] "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970" "1971" "1972" "1973" "1974" "1975"
[31] "1976" "1977" "1978" "1979" "1980" "1981" "1982" "1983" "1984" "1985" "1986" "1987" "1988" "1989" "1990"
[46] "1991" "1992" "1993" "1994" "1995" "1996" "1997" "1998" "1999" "2000" "2001" "2002" "2003" "2004" "2005"
[61] "2006" "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014"
> df.car_specs_all<-mutate (df.car_specs_all, decade = as.factor(ifelse(str_sub(df.car_specs_all$year,1,3)=='193', '1930s',
                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='194', '1940s',
                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='195', '1950s',
                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='196', '1960s',
                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='197', '1970s',
                                                                                                                                                                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='198', '1980s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='199', '1990s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='200', '2000s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='201', '2010s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        +                                                                "ERROR")))))))))))
> glimpse(df.car_specs_all$decade)
Factor w/ 9 levels "1930s","1940s",..: 9 9 8 9 8 9 9 9 8 8 ...
> df.car_specs_all<-mutate (df.car_specs_all, decade = as.factor(ifelse(str_sub(df.car_specs_all$year,1,3)=='193', '1930s',
                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='194', '1940s',
                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='195', '1950s',
                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='196', '1960s',
                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='197', '1970s',
                                                                                                                                                                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='198', '1980s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='199', '1990s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='200', '2000s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='201', '2010s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        +                                                                "ERROR")))))))))))
> glimpse(df.car_specs_all$decade)
Factor w/ 9 levels "1930s","1940s",..: 9 9 8 9 8 9 9 9 8 8 ...
> levels(df.car_specs_all$decade)
[1] "1930s" "1940s" "1950s" "1960s" "1970s" "1980s" "1990s" "2000s" "2010s"
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%summarize()
Source: local data frame [69 x 2]
Groups: year [?]

year decade
<fctr> <fctr>
  1    1939  1930s
2    1947  1940s
3    1948  1940s
4    1949  1940s
5    1950  1950s
6    1951  1950s
7    1952  1950s
8    1953  1950s
9    1954  1950s
10   1955  1950s
# ... with 59 more rows
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%summarize()%>%print()
Source: local data frame [69 x 2]
Groups: year [?]

year decade
<fctr> <fctr>
  1    1939  1930s
2    1947  1940s
3    1948  1940s
4    1949  1940s
5    1950  1950s
6    1951  1950s
7    1952  1950s
8    1953  1950s
9    1954  1950s
10   1955  1950s
# ... with 59 more rows
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%summarize(year_decade_glimpse)%>%print(year_decade_glimpse)
Error in summarise_impl(.data, dots) : 
  binding not found: 'year_decade_glimpse'
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%year_decade_glimpse=summarize()%>%print(year_decade_glimpse)
Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)) : 
  argument ".data" is missing, with no default
> df.car_specs_all<-mutate (df.car_specs_all, decade = as.factor(ifelse(str_sub(df.car_specs_all$year,1,3)=='193', '1930s',
                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='194', '1940s',
                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='195', '1950s',
                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='196', '1960s',
                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='197', '1970s',
                                                                                                                                                                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='198', '1980s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='199', '1990s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='200', '2000s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                +                                                                ifelse(str_sub(df.car_specs_all$year,1,3)=='201', '2010s',
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        +                                                                "ERROR")))))))))))
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%year_decade_glimpse<-summarize()%>%print(year_decade_glimpse)
Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)) : 
  argument ".data" is missing, with no default
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%summarize(decade=='2000s')
Error in summarise_impl(.data, dots) : expecting a single value
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%summarize(decade)
Error in summarise_impl(.data, dots) : cannot modify grouping variable
> View(df.car_specs_all)
> View(df.car_specs_all)
> df.car_specs_all%>%select(year, decade)%>%group_by(year, decade)%>%summarize(count=n())
Source: local data frame [69 x 3]
Groups: year [?]

year decade count
<fctr> <fctr> <int>
  1    1939  1930s     2
2    1947  1940s     2
3    1948  1940s     3
4    1949  1940s     2
5    1950  1950s     4
6    1951  1950s     3
7    1952  1950s     5
8    1953  1950s     6
9    1954  1950s     8
10   1955  1950s     9
# ... with 59 more rows
> View(df.car_specs_all)
> df.car_specs_all%>%mutate(make = str_replace(car_full_nm," .*$", " ") %>% as.factor())
# A tibble: 1,578 × 14
car_full_nm horsepower_bhp rpm_horsepower_measure_point
<chr>          <int>                        <int>
  1   Bugatti Veyron 8.0 litre W16 Super Sport - [2010]           1184                         6400
2    Bugatti Veyron 16.4 Grand Sport Vitesse - [2012]           1184                         6400
3                       SSC Ultimate Aero TT - [2008]           1183                         6950
4                  Koenigsegg Agera R 5.0 V8 - [2012]           1124                         7100
5                           Porsche 9FF GT9R - [2009]           1120                         7850
6                     Koenigsegg Agera 5L V8 - [2011]           1100                         2700
7                         Hennessey Venom GT - [2010]           1030                         6500
8           Koenigsegg Agera S 5.0 V8 Hundra - [2013]           1016                         7100
9  Koenigsegg CCX R Special Edition 4.8 V8 S - [2008]           1004                         7000
10              Bugatti Veyron 8.0 litre W16 - [2005]           1001                         6000
# ... with 1,568 more rows, and 11 more variables: horsepower_per_ton_bhp <dbl>, engine_size_cc <int>,
#   engine_size_ci <dbl>, torque_lb_ft <int>, rpm_torque_measure_point <int>, top_speed_mph <int>,
#   top_speed_kph <int>, car_0_60_time_seconds <dbl>, year <fctr>, decade <fctr>, make <fctr>
> View(df.car_specs_all)
> df.car_specs_all<-mutate(df.car_specs_all, make = str_replace(car_full_nm," .*$", " ") %>% as.factor())
> View(df.car_specs_all)
> unique(df.car_specs_all$make)
[1] Bugatti         SSC             Koenigsegg      Porsche         Hennessey       TVR            
[7] Ferrari         Gumpert         Aston-Martin    Mercedes        Lamborghini     McLaren        
[13] Ultima          Ford            Pagani          Noble           Chevrolet       Dodge          
[19] Bentley         Ascari          Rolls-Royce     Maserati        Fisker          Maybach        
[25] Vauxhall-Opel   Caparo          Audi            BMW             Lexus           Cadillac       
[31] Saleen          Nissan          Jaguar          Land-Rover      Holden          Wiesmann       
[37] Caterham        Ariel           Lotus           Jeep            Alfa-Romeo      Volkswagen(VW) 
[43] Chrysler        Plymouth        Tesla           AC              Mitsubishi      Pontiac        
[49] Subaru          Iso             Oldsmobile      Monteverdi      MG              Infiniti       
[55] Jensen          Morgan          Buick           Mercury         DeTomaso        AMC            
[61] KIA             Bristol         Toyota          Volvo           Ginetta         Lincoln        
[67] KTM             Honda           BAC             Mazda           Studebaker      Renault        
[73] Seat            Radical         Saab            Westfield       Mastretta       Alpine         
[79] Lancia          Fiat            Skoda           Mini            Peugeot         Lagonda        
[85] Rover           Citroen         Hudson          Hyundai         Talbot          Datsun         
[91] Austin-Healey   Triumph         Marcos          Frazer-Nash     Proton          DeLorean       
[97] Suzuki          Dacia           Daihatsu       
99 Levels: AC  Alfa-Romeo  Alpine  AMC  Ariel  Ascari  Aston-Martin  Audi  Austin-Healey  BAC  Bentley  ... Wiesmann 
> df.car_specs_all<-mutate(df.car_specs_all, car_weight = horsepower_bhp / horsepower_per_ton_bhp)
> df.car_specs_all %>%  group_by(decade) %>% summarize(count = n())
# A tibble: 9 × 2
decade count
<fctr> <int>
  1  1930s     2
2  1940s     7
3  1950s    57
4  1960s   143
5  1970s   125
6  1980s   154
7  1990s   262
8  2000s   526
9  2010s   302
> df.car_specs_all%>% group_by(make) %>% summarise(count = n()) %>% arrange(desc())
Error in arrange_impl(.data, dots) : 
  cannot arrange column of class 'NULL'
> df.car_specs_all%>% group_by(make) %>% arrange(desc(summarise(count = n())))
Error in arrange_impl(.data, dots) : 
  argument ".data" is missing, with no default
> df.car_specs_all%>% group_by(make) %>% summarise(count = n())
# A tibble: 99 × 2
make count
<fctr> <int>
  1             AC      3
2     Alfa-Romeo     41
3         Alpine      4
4            AMC      4
5          Ariel      3
6         Ascari      2
7   Aston-Martin     36
8           Audi     98
9  Austin-Healey      8
10           BAC      1
# ... with 89 more rows
> df.car_specs_all%>% group_by(make) %>% summarise(make_count = count = n())
Error: unexpected '=' in "df.car_specs_all%>% group_by(make) %>% summarise(make_count = count ="
> df.car_specs_all%>% group_by(make) %>% arrange(desc(summarise(count = n())))
Error in arrange_impl(.data, dots) : 
  argument ".data" is missing, with no default
> df.car_specs_all%>% group_by(make) %>% summarise(make_count = length(make)) %>% arrange(desc(make_count))
# A tibble: 99 × 2
# <fctr>      <int>
#   1            Ford         110
# 2            Audi          98
# 3         Porsche          95
# 4             BMW          91
# 5        Mercedes          90
# 6         Ferrari          64
# 7          Subaru          53
# 8          Jaguar          46
# 9  Volkswagen(VW)          43
# 10      Chevrolet          42

#final inspection before plotting 
str(df.car_specs_all)
glimpse(df.car_specs_all)
head(df.car_specs_all)
levels(df.car_specs_all$decade)
levels(df.car_specs_all$make)
table(df.car_specs_all)
table(df.car_specs_all$decade)

#WHAT QUESTIONS DO WE WANT ANSWERED FROM THIS DATASET?
# - What is the relationship between horsepower and speed?
# - How is car speed distributed?  
# - What is a "really really fast car?"
# - What is the relationship between horsepower and 0-60 time?
# - How has car speed changed over time?
# - Who is making the fastest cars today?

# - How are top 3 US companies faring compared with top 3 Japanese (selected automakers)
# - 

# what variables do we have at our disposal?
# - top_speed_mph
# - car_0_60_time_seconds
# - year (for possible trending)
# - decade (for possible trending)
# - make
# - car_full_nm
# - horsepower, torque, weight

#random examination of variables
ggplot(data=df.car_specs_all, aes(x=top_speed_mph))+ geom_histogram(binwidth = 3, na.rm = T)
#looks like the spike after 150 is the top speed regulated for US cars

#individual variable study
ggplot(data=df.car_specs_all, aes(x=car_0_60_time_seconds))+ geom_histogram()
ggplot(data=df.car_specs_all, aes(x=horsepower_bhp))+ geom_histogram()
ggplot(data=df.car_specs_all, aes(x=horsepower_per_ton_bhp))+ geom_histogram()
ggplot(data=df.car_specs_all, aes(x=engine_size_cc))+ geom_histogram()
ggplot(data=df.car_specs_all, aes(x=horsepower_per_ton_bhp))+ geom_histogram()
ggplot(data=df.car_specs_all, aes(x=car_weight))+ geom_histogram()
ggplot(data=df.car_specs_all, aes(x=year))+ geom_bar()

#2 variables study
ggplot(data=df.car_specs_all, aes(x=year, y=horsepower_per_ton_bhp))+ geom_point()
ggplot(data=df.car_specs_all, aes(x=year, y=engine_size_cc))+ geom_point()
ggplot(data=df.car_specs_all, aes(x=year, y=car_weight))+ geom_point()
ggplot(data = df.car_specs_all, aes(x = horsepower_bhp, y = top_speed_mph)) + geom_point(na.rm = T)
str(df.car_specs_all)

#multivariate views
ggplot(data =df.car_specs_all, aes(x=top_speed_mph))+ geom_histogram(na.rm = T) + facet_wrap(~decade)
df.car_specs_all %>% filter(decade =='1990s'| decade == '2000s') %>% 
  ggplot(aes( x= top_speed_mph)) + geom_histogram(na.rm=T) + facet_wrap(~year)
#looking closely at the 150 mph data. improving plotting technique
df.car_specs_all %>% filter(decade =='1990s'| decade == '2000s') %>% 
  filter (top_speed_mph > 149 & top_speed_mph < 159) %>%
  ggplot(aes( x= as.factor(top_speed_mph))) + geom_bar()

# who is making these 155 mph cars?
df.car_155_makers<- df.car_specs_all %>% group_by(top_speed_mph) %>% summarize(make for top_speed_mph=155
                                                                           )
df.car_155_makers<- df.car_specs_all %>% filter(decade =='1990s'| decade == '2000s') %>% 
  filter(top_speed_mph == 155) %>%
  group_by(make) %>% summarize(count_var_car_155_makers=n()) %>% 
  arrange(desc(count_var_car_155_makers))
print(df.car_155_makers, n = Inf)


# - Q: Who makes fast cars?
df.car_specs_all %>% select(make, top_speed_mph) %>%
  group_by(make) %>% summarize(max_car_speed = max(top_speed_mph)) %>% arrange(desc(max_car_speed))

df.car_specs_all %>% select(make, top_speed_mph) %>% filter (min_rank(desc(top_speed_mph)) <=10) %>%
  arrange(desc(top_speed_mph)) %>% ggplot(aes(x=reorder(make,top_speed_mph), y=top_speed_mph)) +
  geom_bar(stat = "identity") +coord_flip()
  
# - Q: How has car speed changed over time
df.car_specs_all %>% group_by(year) %>% summarize(max.speed=max(top_speed_mph, na.rm=TRUE)) %>%
  ggplot(aes(x=year, y=max.speed))+ geom_point()

#Q: What is the relationship between 0-60 and engine power? and the use of jitter
ggplot(data = df.car_specs_all, aes(x = horsepower_bhp, y = car_0_60_time_seconds)) +
  geom_point( na.rm = T)