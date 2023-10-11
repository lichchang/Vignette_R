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
simply add your key to “key=”. You can find detailed information
regarding API key usage at this website:
<https://www.iqair.com/dashboard/api>*

- **Get all supported countries** First, to verify whether a country is
  compatible with the AirVisual API, the get_country function retrieves
  a list of countries that are supported by the AirVisual API.

``` r
get_country <- function(){
  
  # Define the API URL with the provided API key for retrieving country data
  url <- 'http://api.airvisual.com/v2/countries?key=b94cdf68-3b7b-4808-9012-ffb6359d7690'
  
  # Get api response and content
  response <- GET(url)
  country_list <- content(response, encoding = "UTF-8")$data
  
  # Convert the list of countries to a data frame
  results <- data.frame(country = sapply(country_list, '[[', 'country'))
  return(results)
}
```

- **Verify whether the country is included in the list of supported
  countries** This function serves as a convenient helper, enabling
  users to effortlessly determine if their provided country name is
  supported by AirVisual. The function returns boolean (True/False) for
  verifing if a given country is supported by AirVisual API.

``` r
verify_country <- function(country="USA"){
  
  # Define the API URL with the provided API key for retrieving country data
  url <- 'http://api.airvisual.com/v2/countries?key=b94cdf68-3b7b-4808-9012-ffb6359d7690'
  
  # Get api response and content
  response <- GET(url)
  country_list <- content(response, encoding = "UTF-8")$data
  
  # iterate through supported country list
  for (c in country_list) {
    
     # Return True if specified country is in the list
     if (country == c) {
       return(TRUE)
     }
  }
  # Return False if not found
  return(FALSE)
}
```

- **Get all supported states inside a specified country**

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
    ## 19           Clyde
    ## 20       Cornelius
    ## 21       Cullowhee
    ## 22          Denver
    ## 23          Durham
    ## 24         Edenton
    ## 25  Elizabeth City
    ## 26       Farmville
    ## 27    Fayetteville
    ## 28       Flat Rock
    ## 29        Franklin
    ## 30   Fuquay-Varina
    ## 31          Garner
    ## 32          Gaston
    ## 33       Goldsboro
    ## 34          Gorman
    ## 35      Greensboro
    ## 36      Greenville
    ## 37         Grifton
    ## 38       Hampstead
    ## 39      Harrisburg
    ## 40      Hayesville
    ## 41  Hendersonville
    ## 42        Hertford
    ## 43         Hickory
    ## 44       Highlands
    ## 45    Hillsborough
    ## 46   Holly Springs
    ## 47      Hope Mills
    ## 48    Huntersville
    ## 49      Knightdale
    ## 50       Lexington
    ## 51      Lillington
    ## 52          Lowell
    ## 53      Lowesville
    ## 54          Marvin
    ## 55          Mebane
    ## 56         Midland
    ## 57     Morrisville
    ## 58     Mount Holly
    ## 59     Mount Olive
    ## 60    Murfreesboro
    ## 61          Murphy
    ## 62         Norwood
    ## 63           Ogden
    ## 64            Otto
    ## 65       Pineville
    ## 66       Pittsboro
    ## 67   Pleasant Hill
    ## 68       Princeton
    ## 69         Raleigh
    ## 70      Reidsville
    ## 71        Rockwell
    ## 72     Rocky Mount
    ## 73      Rural Hall
    ## 74       Salisbury
    ## 75     Scotts Mill
    ## 76      Siler City
    ## 77      Smithfield
    ## 78 Southern Shores
    ## 79       Southport
    ## 80          Sparta
    ## 81     Spruce Pine
    ## 82     Statesville
    ## 83      Stokesdale
    ## 84      Stoneville
    ## 85         Tarboro
    ## 86     Waynesville
    ## 87     Weaverville
    ## 88      Wilmington
    ## 89   Winston-Salem
    ## 90         Zebulon

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

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggplot(df, aes(x = lat, y = lon, color = temp)) +
  geom_point() +
  geom_text(aes(label = city), vjust = 1, hjust = 1) + 
  labs(x = "latitude", y = "longitude", color = "temperature (Celsius)") + 
  scale_color_gradient(low = "green", high = "blue")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggplot(df, aes(x = lat, y = lon, color = hu)) +
  geom_point() +
  geom_text(aes(label = city), vjust = 1, hjust = 1) + 
  labs(x = "latitude", y = "longitude", color = "pollution (AQI)") + 
  scale_color_gradient(low = "green", high = "blue")
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
