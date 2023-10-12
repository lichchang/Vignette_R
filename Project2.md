Project2
================
LiChia Chang
2023-10-07

- [Introduction](#introduction)
- [Requirements](#requirements)
- [API Interaction Functions](#api-interaction-functions)
- [Exploratory Data Analysis](#exploratory-data-analysis)

# Introduction

This document is a vignette to show how to retrieve data from an API. To
showcase this, I will create multiple functions for refining API
endpoints and design helper functions to enhance the user-friendliness
of the API. In the final section, I will employ these functions to
extract data that I’m interested in and conduct exploratory data
analysis.

This article dives into the [AirVisual
API](https://www.iqair.com/commercial-air-quality-monitors/api), which
collects data from the largest network of ground-based sensors
worldwide, providing the most accurate and reliable air quality
information. Further details will be discussed in the subsequent
sections.

# Requirements

To interact with the AirVisual API, you will requrie several libraries.
In the R environment console, you can install these libraries by using
the following command: `install.packages(package_name)`. Make sure to
replace package_name with the name of any library from the list below
that you haven’t installed in your environment.

``` r
library(httr)
library(dplyr)
library(jsonlite)
library(ggplot2)
```

# API Interaction Functions

In this section, I will demonstrate how to establish contact with the
AirVisual API and develop functions for specific purposes. I will
provide separate explanations for each function, detailing their usage
and intended purpose.

*Note: For this project, I am utilizing my own API key, which allows for
up to 10,000 calls per month. If you wish to use your own API key,
simply replace “key=” with your key. You can find detailed information
regarding API key usage at this website:
<https://www.iqair.com/dashboard/api>*

- **Get all supported countries**

``` r
get_country <- function(){
  url <- 'http://api.airvisual.com/v2/countries?key=b94cdf68-3b7b-4808-9012-ffb6359d7690'
  response <- GET(url)
  country_list <- content(response, encoding = "UTF-8")$data
  
  ## Convert to data frame
  results <- data.frame(country = sapply(country_list, '[[', 'country'))
  return(results)
}
```

- Verify if the country exists in the supported country list

Verify if the country is in the supported list

``` r
verify_country <- function(country="USA"){
  
  url <- 'http://api.airvisual.com/v2/countries?key=b94cdf68-3b7b-4808-9012-ffb6359d7690'
  
  response <- GET(url)
  country_list <- content(response, encoding = "UTF-8")$data
  
  for (c in country_list) {
     if (country == c) {
       return(TRUE)
     }
  }
  return(FALSE)
}
```

- Get all supported states inside a specified country

``` r
get_state <- function(country="USA"){
  
  custom_country <- gsub(" ", "%20", country)
  
  url <- "http://api.airvisual.com/v2/states?country="
  str_key <- "&key=b94cdf68-3b7b-4808-9012-ffb6359d7690"
  url <- paste0(url, custom_country, str_key)
  
  
  response <- GET(url)
  state_list <- content(response, encoding = "UTF-8")$data
  
  ## Convert to data frame
  results <- data.frame(country = sapply(state_list, '[[', 'state'))
  return(results)
}
```

- Verify supported states in a country

Verify if the state is in the supported list of specific country

``` r
verify_state <- function(country="USA", state="California"){
  
  custom_country <- gsub(" ", "%20", country)
  
  url <- "http://api.airvisual.com/v2/states?country="
  str_key <- "&key=b94cdf68-3b7b-4808-9012-ffb6359d7690"
  url <- paste0(url, custom_country, str_key)
  
  
  response <- GET(url)
  state_list <- content(response, encoding = "UTF-8")$data
  
  for (s in state_list) {
     if (state == s) {
       return(TRUE)
     }
  }
  return(FALSE)
}
```

- Get all supported cities inside a specified country and state

``` r
get_city <- function(country="USA", state="California"){

  custom_state <- gsub(" ", "%20", state)
  custom_country <- gsub(" ", "%20", country)
  
  url <- "http://api.airvisual.com/v2/cities?state="
  str_country <- "&country="
  str_key <- "&key=b94cdf68-3b7b-4808-9012-ffb6359d7690"
  url <- paste0(url, custom_state, str_country, custom_country, str_key)
  
  
  response <- GET(url)
  city_list <- content(response, encoding = "UTF-8")$data
  
  ## Convert to data frame
  results <- data.frame(country = sapply(city_list, '[[', 'city'))
  return(results)
}
```

- Verify supported cities in a state

Verify if the city is in the supported list of specific country and
state

``` r
verify_city <- function(country="USA", state="California", city="Los Angeles"){

  custom_state <- gsub(" ", "%20", state)
  custom_country <- gsub(" ", "%20", country)
  
  url <- "http://api.airvisual.com/v2/cities?state="
  str_country <- "&country="
  str_key <- "&key=b94cdf68-3b7b-4808-9012-ffb6359d7690"
  url <- paste0(url, custom_state, str_country, custom_country, str_key)
  
  
  response <- GET(url)
  city_list <- content(response, encoding = "UTF-8")$data
  
  for (cty in city_list) {
     if (city == cty) {
       return(TRUE)
     }
  }
  return(FALSE)
}
```

- Verify if the connection is sucessfully connected return status of
  connection

``` r
test_connection <- function(country="USA", state="California", city="Los Angeles") {
  ## replace " " to "%20", which is a URL-encoded representation of a space character.
  custom_city <- gsub(" ", "%20", city)
  custom_state <- gsub(" ", "%20", state)
  custom_country <- gsub(" ", "%20", country)
  
  url <- 'http://api.airvisual.com/v2/city?city='
  str_state <- '&state='
  str_country <- '&country='
  str_key <- '&key=b94cdf68-3b7b-4808-9012-ffb6359d7690'
  url <- paste0(url, custom_city, str_state, custom_state, str_country, custom_country, str_key)
  
  
  response <- GET(url)
  status <- content(response)$status
  return(status)
}
```

- Helper function to wrap up all the validation

``` r
wrap_validation <- function(country="USA", state="California", city="Los Angeles") {
  
  status <- test_connection(country, state, city)
  if (status != "success") {
    msg <- "ERROR: Connection failed"
    stop(msg)
  }
  
  if (!verify_country(country)) {
    msg <- "Not a valid country in supported list"
    stop(msg)
  }
  
  if (!verify_state(country, state)) {
    msg <- paste0("Not a valid state in the supported list of country: ", country)
    stop(msg)
  }
  
  if (!verify_city(country, state, city)) {
    msg <- paste0("Not a valid city in the supported list of country: ", country, " and state: ", state)
    stop(msg)
  }
  
  print("Everything works well")
}
```

- Get data object Return the most recent data object by given state,
  city, and country

``` r
get_data <- function(city="Los Angeles", state="California", country="USA"){

  ## replace " " to "%20", which is a URL-encoded representation of a space character.
  custom_city <- gsub(" ", "%20", city)
  custom_state <- gsub(" ", "%20", state)
  custom_country <- gsub(" ", "%20", country)
  
  
  url <- 'http://api.airvisual.com/v2/city?city='
  str_state <- '&state='
  str_country <- '&country='
  str_key <- '&key=b94cdf68-3b7b-4808-9012-ffb6359d7690'
  url <- paste0(url, custom_city, str_state, custom_state, str_country, custom_country, str_key)
  
  
  response <- GET(url)
  results <- content(response, encoding = "UTF-8")$data
  return(results)
}
```

- Get current weather Return the most recent weather string by given
  state, city, and country

``` r
get_weather <- function(city="Los Angeles", state="California", country="USA"){

  ## replace " " to "%20", which is a URL-encoded representation of a space character.
  custom_city <- gsub(" ", "%20", city)
  custom_state <- gsub(" ", "%20", state)
  custom_country <- gsub(" ", "%20", country)
  
  
  url <- 'http://api.airvisual.com/v2/city?city='
  str_state <- '&state='
  str_country <- '&country='
  str_key <- '&key=b94cdf68-3b7b-4808-9012-ffb6359d7690'
  url <- paste0(url, custom_city, str_state, custom_state, str_country, custom_country, str_key)
  
  
  response <- GET(url)
  ic <- content(response, encoding = "UTF-8")$data$current$weather$ic
  
  if (ic == "01d") {
    weather <- "clear sky (day)"
  }
  if (ic == "01n") {
    weather <- "clear sky (night)"
  }
  if (ic == "02d") {
    weather <- "few clouds (day)"
  }
  if (ic == "02n") {
    weather <- "few clouds (night)"
  }
  if (ic == "03d") {
    weather <- "scattered clouds"
  }
  if (ic == "04d") {
    weather <- "broken clouds"
  }
  if (ic == "09d") {
    weather <- "shower rain"
  }
  if (ic == "10d") {
    weather <- "rain (day time)"
  }
  if (ic == "10n") {
    weather <- "rain (night time)"
  }
  if (ic == "11d") {
    weather <- "thunderstorm"
  }
  if (ic == "13d") {
    weather <- "snow"
  }
  if (ic == "50d") {
    weather <- "mist"
  }
  
  return(weather)
}
```

# Exploratory Data Analysis

study the weather and temperature in north carolina state

``` r
city_list <- get_city(country="USA", state="North Carolina")
city_list
```

    ##            country
    ## 1        Albemarle
    ## 2             Apex
    ## 3        Asheville
    ## 4         Beaufort
    ## 5           Bethel
    ## 6     Blowing Rock
    ## 7            Boone
    ## 8          Brevard
    ## 9      Bryson City
    ## 10      Burlington
    ## 11      Burnsville
    ## 12          Camden
    ## 13          Candor
    ## 14        Carrboro
    ## 15            Cary
    ## 16     Chapel Hill
    ## 17       Charlotte
    ## 18        Cherokee
    ## 19       Cornelius
    ## 20       Cullowhee
    ## 21          Durham
    ## 22         Edenton
    ## 23  Elizabeth City
    ## 24    Fayetteville
    ## 25       Flat Rock
    ## 26        Franklin
    ## 27   Fuquay-Varina
    ## 28          Garner
    ## 29          Gaston
    ## 30       Goldsboro
    ## 31          Gorman
    ## 32      Greensboro
    ## 33      Greenville
    ## 34         Grifton
    ## 35       Hampstead
    ## 36      Harrisburg
    ## 37      Hayesville
    ## 38  Hendersonville
    ## 39        Hertford
    ## 40         Hickory
    ## 41       Highlands
    ## 42    Hillsborough
    ## 43   Holly Springs
    ## 44    Huntersville
    ## 45      Knightdale
    ## 46       Lexington
    ## 47      Lillington
    ## 48          Lowell
    ## 49      Lowesville
    ## 50          Mebane
    ## 51         Midland
    ## 52     Morrisville
    ## 53     Mount Holly
    ## 54    Murfreesboro
    ## 55          Murphy
    ## 56           Ogden
    ## 57            Otto
    ## 58       Pineville
    ## 59       Pittsboro
    ## 60   Pleasant Hill
    ## 61       Princeton
    ## 62         Raleigh
    ## 63      Reidsville
    ## 64        Rockwell
    ## 65     Rocky Mount
    ## 66      Rural Hall
    ## 67       Salisbury
    ## 68     Scotts Mill
    ## 69      Siler City
    ## 70      Smithfield
    ## 71 Southern Shores
    ## 72       Southport
    ## 73          Sparta
    ## 74     Spruce Pine
    ## 75     Statesville
    ## 76      Stoneville
    ## 77         Tarboro
    ## 78     Waynesville
    ## 79     Weaverville
    ## 80      Wilmington
    ## 81   Winston-Salem
    ## 82         Zebulon

We want to see the relationship between coordinates and pollution

``` r
show_city <- c("Raleigh", "Cary", "Apex", "Durham", "Chapel Hill", "Garner")

show_city_list_NC <- data.frame(country = show_city)
```

``` r
df <- data.frame()
row_index <- 1

apply(show_city_list_NC, MARGIN = 1, FUN = function(x) {
  Sys.sleep(20)
  
  city_data <- get_data(country="USA", state="North Carolina", city = x)
  latitude <- city_data$location$coordinates[[1]]
  longitude <- city_data$location$coordinates[[2]]
  pollution <- city_data$current$pollution$aqius
  temperature <- city_data$current$weather$tp
  humidity <- city_data$current$weather$hu
  
  tmp <- data.frame(city = x, lat = latitude, lon = longitude, pol = pollution, temp = temperature, hu = humidity)
  
   # Set the row index as the row names of tmp
  rownames(tmp) <- row_index
  
  # Add tmp as a new row to the global df data frame
  df <<- rbind(df, tmp)  # Use '<<-' to assign to the global variable
  
  # Increment the row index for the next row
  row_index <<- row_index + 1
  
})
```

``` r
ggplot(df, aes(x = lat, y = lon, color = pol)) +
  geom_point() +
  geom_text(aes(label = city), vjust = 1, hjust = 1) + 
  labs(x = "latitude", y = "longitude", color = "pollution (AQI)") + 
  scale_color_gradient(low = "green", high = "blue")
```

![](Project2_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggplot(df, aes(x = lat, y = lon, color = temp)) +
  geom_point() +
  geom_text(aes(label = city), vjust = 1, hjust = 1) + 
  labs(x = "latitude", y = "longitude", color = "temperature (Celsius)") + 
  scale_color_gradient(low = "green", high = "blue")
```

![](Project2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggplot(df, aes(x = lat, y = lon, color = hu)) +
  geom_point() +
  geom_text(aes(label = city), vjust = 1, hjust = 1) + 
  labs(x = "latitude", y = "longitude", color = "pollution (AQI)") + 
  scale_color_gradient(low = "green", high = "blue")
```

![](Project2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
