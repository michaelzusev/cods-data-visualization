library(data.table)
library(tidyverse)
library(hrbrthemes)
library(ggthemes)
library(jsonlite)

#### Fundamentals ####

# Console

"Prints to Console"

# Variable Assignment

x <- 1

y <- 3

# Environment

x+y

# Data Types

my_character <- "abcd"

my_number <- 1

my_integer <- 2L

my_logical <- TRUE

my_na <- NA

class(my_character)

class(my_number)

class(my_integer)

class(my_logical)

class(my_na)

# Data Structures

my_vector <- c(1,5,9)

my_vector[3]

my_list <- list("first","second","third",1,2,3)

my_list[4]

my_matrix <- matrix(data=c(1,2,3,4,5,6), ncol = 3)

my_matrix[3]

my_matrix[1,2]

my_matrix[2,3]

my_dataframe <- data.frame(first_col = c(1,2,3), 
                           second_col = c("on","sk","ab"), 
                           third_col = c("50",44,TRUE))

# Functions

some_numbers <- c(1,4,2,3,5,6,7)

mean(some_numbers)

median(some_numbers)

some_numbers_missing <- c(1,4,2,3,5,6,7,NA)

mean(some_numbers_missing, na.rm = TRUE)

median(some_numbers_missing, na.rm = TRUE)


#### Importing Data ####

# Quick Aside 

all_cubes <- stream_in(file("https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesList"))





housing_prices_raw <- fread("18100205.csv", encoding = "UTF-8")


str(housing_prices_raw)




unique(housing_prices_raw[,'New housing price indexes'])
unique(housing_prices_raw[,'UOM'])
unique(housing_prices_raw[,'STATUS'])
unique(housing_prices_raw[,'DECIMALS'])


for (colname in colnames(housing_prices_raw)){
  print(unique(housing_prices_raw[,..colname]))
}


housing_prices_clean <- housing_prices_raw %>%
  mutate(date = as.Date(paste0(REF_DATE, "-01"))) %>%
  select(date, GEO, `New housing price indexes`, VALUE) %>%
  rename(geo = GEO,
         type = `New housing price indexes`,
         index = VALUE)
  


str(housing_prices_clean)
unique(housing_prices_clean[,'geo'])

housing_prices_canada <- housing_prices_clean %>%
  filter(geo == "Canada", type == "Total (house and land)")


housing_prices_canada_plot <- ggplot(housing_prices_canada, aes(x = date, y = index)) +
  geom_line() +
  theme_tufte()
  
  # theme_ipsum_tw()

housing_prices_canada_plot



housing_prices_prov <- housing_prices_clean %>%
  filter(geo %in% c("Prince Edward Island",
                  "Newfoundland and Labrador",
                  "Nova Scotia",
                  "New Brunswick",
                  "Quebec",
                  "Ontario",
                  "Manitoba",
                  "Alberta",
                  "Saskatchewan",
                  "British Columbia")) %>%
  filter(type == "Total (house and land)")


housing_prices_prov_lines <- ggplot(housing_prices_prov, aes(x = date, y = index, color = geo)) +
  geom_line() + 
  theme_tufte()
  # theme_ipsum_tw()

housing_prices_prov_lines

housing_prices_prov_facet <- ggplot(housing_prices_prov, aes(x = date, y = index)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_tufte()

housing_prices_prov_facet


housing_prices_prov_compare <- housing_prices_clean %>%
  filter(geo %in% c("Prince Edward Island",
                    "Newfoundland and Labrador",
                    "Nova Scotia",
                    "New Brunswick",
                    "Quebec",
                    "Ontario",
                    "Manitoba",
                    "Alberta",
                    "Saskatchewan",
                    "British Columbia")) %>%
  filter(type != "Total (house and land)")

housing_prices_prov_compare <- ggplot(housing_prices_prov_compare, aes(x = date, y = index, color = type)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_tufte()

housing_prices_prov_compare

housing_prices_cma_compare <- housing_prices_clean %>%
  filter(str_detect(geo,",")) %>%
  filter(type != "Total (house and land)")


housing_prices_cma_compare <- ggplot(housing_prices_cma_compare, aes(x = date, y = index, color = type)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_hc()

housing_prices_cma_compare


