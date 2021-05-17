# Package libraries, organized by importance.
# Essential
library(tidyverse) # Used for majority of this tutorial -- https://www.tidyverse.org/packages/#core-tidyverse
library(data.table) # Used to read csv files -- https://rdatatable.gitlab.io/data.table/

# Essential Utilities
library(skimr) # Quick way to read summary statistics about a dataset -- https://docs.ropensci.org/skimr/
library(lubridate) # Work with dates and times in R -- https://lubridate.tidyverse.org/index.html
library(scales) # Scale helper for ggplot -- https://scales.r-lib.org/

# Theme
library(ggthemes) # Themes for ggplot -- https://jrnold.github.io/ggthemes/

# Required to retrieve All Cube Data from StatsCan
library(jsonlite) # Read JSON -- https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(httr) # Curl Wrapper for R (modern Web API) -- https://httr.r-lib.org/

# Required for Maps
library(mapcan) # Maps for Canada -- https://github.com/mccormackandrew/mapcan
library(transformr) # Necessary for mapcan 

# Required for Animation
library(gganimate) # Animate ggplot -- https://gganimate.com/articles/gganimate.html
library(gifski) # Necessary for making gifs from gganimate

#Other
library(devtools) # Helper


# Installing Packages

# install.package("Package Name")





#### Examples #### 
#' Source this File before running the plots below

#' Disclaimer: No guarantees these plots represent what is actually going on. 
#' Not too much effort on my part was done to make sure these make sense. 


housing_prices_canada_plot # Basic Line Plots

housing_prices_prov_lines_plot # Multiple Line Plots

employment_type_agg_difference_plot # Simple Column Plots

employment_type_year_growth_plot # Multiple Column Plots

housing_prices_cma_compare_plot # Facet Line Plots

housing_prices_cma_growth_plot # Facet Column Plots

housing_prices_map_plot # Maps

animate(housing_prices_map_animate_plot, fps = 2) # Animations


# Check more out here: https://www.r-graph-gallery.com/





#### Fundamentals ####

#' Use the # (hash/pound) to comment out lines of code. Multiple hashes can be used
#' for setting up script headings and a hash apostrophe for multiple lines.  

# Console 
#' The R Console is where R as a language "lives", but typically, we don't use
#' console for writing R code, rather we use scripts to "print to the console", 
#' outputting only elements of the scripts we are interested in. 

"Prints to Console"

# Variable Assignment

#' You can assign values to a variable with the <- (assignment) operator. 
#' Technically, you can do it with the = (equals) operations, but goes against 
#' R convention

x <- 1

y <- 3

# Environment


#' Variables that are assigned a value are automatically mounted onto the Global 
#' R environment. You can see all the variables on the right side of the screen in
#' the environment tab. All variables are uniquely named, so if you reassign a variable
#' a different value, then it will override the previous value. 

x+y

x <- 2

x+y

# Data Types

#' There are only a few primitive data types to keep in mind in R. Each value must
#' be at least on class of data type. 

my_character <- "abcd"

my_number <- 1

my_integer <- 2L

my_logical <- TRUE

my_na <- NA

#' We can check what the class of a variable is by using the class function, which
#' will print the class in the console. 

class(my_character)

class(my_number)

class(my_integer)

class(my_logical)

class(my_na)

# Data Structures

#' Data structures are ways of organizing data in a particular variable.

#' Vectors are multiple values of an identical data type. my_vector is a vector
#' of numeric variables. The c() function creates combinations of variables. 

my_vector <- c(1,5,9)

my_vector[3]

#' You can access a particular variable in a vector with its index. Indexes in R
#' start at 1

my_list <- list("first","second","third",1,2,3)

#' Lists are combinations of variables that can have any variable types. my_list
#' has both characters and numbers

my_list[4]

my_matrix <- matrix(data=c(1,2,3,4,5,6), ncol = 3)

#' Matrices are 2-dimensional data structure that have identical data types. 

my_matrix[3]

#' You can access a particular matrix variable by either its index number.

my_matrix[1,2]

#' Or by referencing the row and column index. 

my_matrix[2,3]

#' A dataframe is a 2 dimensional 

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


# Pipes

my_first_variable <- 3

my_second_variable <- my_first_variable + 6

my_third_variable <- sqrt(my_second_variable)

my_third_variable


pipe_variable <- my_first_variable %>% + 6 %>% sqrt()

pipe_variable


#### Importing Data ####

# How to find Statistics Canada Data 

# Quick Aside 

# all_cubes <- stream_in(file("https://www150.statcan.gc.ca/t1/wds/rest/getAllCubesList"))


get_table <- function(pid,name){
  download.file(paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",pid,"-eng.zip"),
                destfile = paste0(pid,".zip"))
  unzip(paste0(pid,".zip"))
  unlink(paste0(pid,".zip"))
  unlink(paste0(pid,"_MetaData.csv"))
  my_table <- fread(paste0(pid, ".csv"), encoding = "UTF-8")
  assign(name, my_table, envir = .GlobalEnv)
}

get_table("18100205", "housing_prices_raw")
# housing_prices_raw <- fread("18100205.csv", encoding = "UTF-8")


# Return the structure of a dataset

str(housing_prices_raw)




unique(housing_prices_raw[,'New housing price indexes'])
unique(housing_prices_raw[,'UOM'])
unique(housing_prices_raw[,'DECIMALS'])

# A much better way to look inside a dataset, using the skimr package

skim(housing_prices_raw)



housing_prices_clean <- housing_prices_raw %>%
  mutate(date = as.Date(paste0(REF_DATE, "-01"))) %>%
  select(date, GEO, `New housing price indexes`, VALUE) %>%
  rename(geo = GEO,
         type = `New housing price indexes`,
         index = VALUE)

  
skim(housing_prices_clean)


unique(housing_prices_clean[,'geo'])

housing_prices_canada <- housing_prices_clean %>%
  filter(geo == "Canada", type == "Total (house and land)")


housing_prices_canada_plot <- ggplot(housing_prices_canada, aes(x = date, y = index)) +
  geom_line() +
  theme_pander() + 
  labs(title = "New Housing Price Index", 
       subtitle = "Canada",
       x = "Date",
       y = "Index",
       caption = "Statistics Canada PID-18100205")

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


housing_prices_prov_lines_plot <- ggplot(housing_prices_prov, aes(x = date, y = index, color = geo)) +
  geom_line() + 
  theme_pander() + 
  labs(title = "New Housing Price Index", 
       subtitle = "Provinces",
       x = "Date",
       y = "Index",
       caption = "Statistics Canada PID-18100205",
       color = "Province")

housing_prices_prov_lines_plot

housing_prices_prov_last_month <- housing_prices_prov %>%
  filter(date > "2021-02-01")

housing_prices_prov_col_plot <- ggplot(housing_prices_prov_last_month, aes(x = index, y = reorder(geo,index))) +
  geom_col() + 
  theme_pander() + 
  labs(title = "New Housing Price Index", 
       subtitle = "Provinces",
       x = "Index",
       y = "Province",
       caption = "Statistics Canada PID-18100205")

housing_prices_prov_col_plot

housing_prices_prov_last_month_compare <- housing_prices_clean %>%
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
  filter(type != "Total (house and land)",
         date > "2021-02-01")

housing_prices_prov_last_month_compare_plot <- ggplot(housing_prices_prov_last_month_compare, aes(x = index, y = reorder(geo,index), fill = type)) +
  geom_col(position = "dodge") + 
  theme_pander() + 
  labs(title = "New Housing Price Index", 
       subtitle = "Provinces March 2021",
       x = "Index",
       y = "Province",
       caption = "Statistics Canada PID-18100205",
       fill = "Index Type")

housing_prices_prov_last_month_compare_plot

housing_prices_prov_facet_plot <- ggplot(housing_prices_prov, aes(x = date, y = index)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_pander()+
  labs(title = "New Housing Price Index", 
       subtitle = "Provinces",
       x = "Date",
       y = "Index",
       caption = "Statistics Canada PID-18100205")

housing_prices_prov_facet_plot


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

housing_prices_prov_compare_plot <- ggplot(housing_prices_prov_compare, aes(x = date, y = index, color = type)) +
  geom_line()+
  facet_wrap(vars(geo), as.table = FALSE) +
  theme_pander() +
  labs(title = "New Housing Price Index", 
       subtitle = "Province",
       x = "Date",
       y = "Index",
       caption = "Statistics Canada PID-18100205",
       color = "Index Type")

housing_prices_prov_compare_plot

housing_prices_cma_compare <- housing_prices_clean %>%
  filter(str_detect(geo,",")) %>%
  filter(type != "Total (house and land)")


housing_prices_cma_compare_plot <- ggplot(housing_prices_cma_compare, aes(x = date, y = index, color = type)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_pander() +
  labs(title = "New Housing Price Index", 
       subtitle = "Census Metro Areas",
       x = "Date",
       y = "Index",
       caption = "Statistics Canada PID-18100205",
       color = "Index Type")

housing_prices_cma_compare_plot

housing_prices_cma_6mth <- housing_prices_cma_compare %>%
  filter(date > (today()-months(8))) %>%
  mutate(city = word(geo, 1, sep = ","))

housing_prices_cma_6mth_plot <- ggplot(housing_prices_cma_6mth, aes(x = date, y = index, fill = type)) +
  geom_col(position = "stack") + 
  facet_wrap(vars(city), as.table = FALSE) + 
  theme_pander() +
  labs(title = "New Housing Price Index", 
       subtitle = "Census Metro Areas, Last 6 Months",
       x = "Date",
       y = "Index",
       caption = "Statistics Canada PID-18100205",
       color = "Index Type")

housing_prices_cma_6mth_plot

housing_prices_cma_growth <- housing_prices_cma_6mth %>%
  group_by(geo, type) %>%
  mutate(growth = index - lag(index)) %>%
  ungroup %>%
  mutate(city = word(geo, 1, sep = ","))

housing_prices_cma_growth_plot <- ggplot(housing_prices_cma_growth, aes(x = date, y = growth, fill = type)) +
  geom_col(position = "stack") +
  facet_wrap(vars(city), as.table = FALSE) + 
  theme_pander(base_size = 10) + 
  labs (title = "Census Metropolitan Area Index Growth",
        subtitle = "Last 6 months",
        x = "Date",
        y = "Index Growth",
        fill = "Type",
        caption = "Statistics Canada PID-18100205")

housing_prices_cma_growth_plot 

get_table("23100287", "aircraft_raw")

skim(aircraft_raw)

aircraft_clean <- aircraft_raw %>%
  select(REF_DATE, 
         `Domestic and international itinerant aircraft movements`,
         VALUE) %>%
  rename(date = REF_DATE,
         move_type =`Domestic and international itinerant aircraft movements`,
         movements = VALUE) %>%
  mutate(date = as.Date(date))

skim(aircraft_clean)


aircraft_plot <- ggplot(aircraft_clean, aes(x=date, y=movements, color=move_type)) + 
  geom_line() +
  theme_pander() + 
  labs(title = "Aircraft Movements", 
       x = "Date",
       y = "Number of Movements",
       color = "Movement Type",
       caption = "Statistics Canada PID-23100287")

aircraft_plot


get_table("14100221", "weekly_earnings_raw")


skim(weekly_earnings_raw)



weekly_earnings_clean <- weekly_earnings_raw %>%
  select(date=REF_DATE,
         employee_type=`Type of employee`,
         estimate=Estimate,
         industry=`North American Industry Classification System (NAICS)`,
         uom=UOM,
         value=VALUE) %>%
  mutate(date=as.Date(paste0(date,"-01")))


skim(weekly_earnings_clean)


employment_type <- weekly_earnings_clean %>%
  filter(estimate == "Employment",
         str_detect(industry, "aggregate"))





employment_type_plot <- ggplot(employment_type, aes(x=date, y=value, color=employee_type))+
  geom_line() + 
  scale_y_continuous(labels = comma) +
  theme_pander()

employment_type_plot

employment_type_year <- weekly_earnings_clean %>%
  filter(estimate == "Employment",
         str_detect(industry, "aggregate"),
         date > today()-years(2))

employment_type_year_plot <- ggplot(employment_type_year, aes(x=date, y=value, fill=employee_type)) +
  geom_col(position = "dodge")

employment_type_year_plot

employment_type_year_growth <- weekly_earnings_clean %>%
  filter(estimate == "Employment",
         str_detect(industry, "aggregate"),
         date > today()-years(2)) %>% 
  group_by(employee_type) %>%
  mutate(growth = value - lag(value)) %>%
  ungroup


employment_type_year_growth_plot <- ggplot(employment_type_year_growth, aes(x=date,y=growth,fill=employee_type)) +
  geom_col(position="dodge") +
  theme_pander() +
  labs(title = "Employee Growth",
       subtitle = "By Type",
       x = "Date",
       y = "Growth",
       fill = "Employee Type",
       caption = "Statistics Canada PID-14100221") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))

employment_type_year_growth_plot


employee_type_total <- weekly_earnings_clean %>%
  filter(estimate == "Employment",
         str_detect(industry, "aggregate"),
         date > today()-years(2)) %>% 
  group_by(employee_type) %>%
  mutate(growth = value - lag(value)) %>%
  ungroup



weekly_earnings_estimate <- weekly_earnings_clean %>%
  filter(estimate =="Average weekly earnings including overtime",
         str_detect(industry, "aggregate"))

weekly_earnings_estimate_plot <- ggplot(weekly_earnings_estimate, aes(x=date, y=value, color=employee_type))+
  geom_line() + 
  theme_pander()

weekly_earnings_estimate_plot




weekly_earnings_6mth <- weekly_earnings_clean %>%
  filter(estimate =="Average weekly earnings including overtime",
         str_detect(industry, "aggregate"),
         date > today()-months(9))

weekly_earnings_6mth_plot <- ggplot(weekly_earnings_6mth, aes(x=date, y=value, fill=employee_type))+
  geom_col(position = "dodge") + 
  theme_pander()

weekly_earnings_6mth_plot

employment_type_agg <- employment_type %>%
  filter(date > "2020-01-01") %>%
  mutate(period = ifelse(date < "2020-06-01", "After June 2020", "Before June 2020")) %>%
  group_by(employee_type, period) %>%
  summarize(sum = sum(value))

employment_type_agg_plot <- ggplot(employment_type_agg, aes(x=employee_type, y=sum, fill=reorder(period, -sum))) +
  geom_col(position="dodge")

employment_type_agg_plot


employment_type_agg_difference <- employment_type_agg %>%
  group_by(employee_type) %>%
  summarize(difference = diff(sum, lag = 1)) %>%
  ungroup

employment_type_agg_difference_plot <- ggplot(employment_type_agg_difference, aes(x=employee_type, y=difference)) +
  geom_col() + 
  theme_pander() +
  labs(title = "Employee Type Aggregate Difference",
       subtitle = "Before and After June 2020",
       x = "Employee Type",
       y = "Difference in Persons",
       caption = "Statistics Canada PID-14100221") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))

employment_type_agg_difference_plot


unique(weekly_earnings_clean[,estimate])




housing_prices_map <- housing_prices_clean %>%
  filter(geo != "Canada",
         str_detect(geo, ",", negate = TRUE),
         type == "Total (house and land)",
         date > today()-months(3)) %>%
  mutate(pr_alpha = case_when(
    geo=="Newfoundland and Labrador" ~ "NL",
    geo=="Prince Edward Island" ~ "PE",
    geo=="Nova Scotia" ~ "NS",
    geo=="New Brunswick" ~ "NB",
    geo=="Quebec" ~ "QC",
    geo=="Ontario" ~ "ON",
    geo=="Manitoba" ~ "MB",
    geo=="Saskatchewan" ~ "SK",
    geo=="Alberta" ~ "AB",
    geo=="British Columbia" ~ "BC"))

housing_prices_map_join <- mapcan(boundaries = provinces, type = standard) %>%
  left_join(housing_prices_map)


housing_prices_map_plot <- ggplot(housing_prices_map_join, aes(long, lat, group = group, fill = index))+
  geom_polygon() +
  coord_fixed() + 
  theme_mapcan() + 
  scale_fill_gradient(low = "#D6FFFE", high ="#005250", na.value = "#d6d6d6")+
  labs(title = "New Housing Price Index",
       subtitle = "March 2021",
       caption = "Statistics Canada")

housing_prices_map_plot


housing_prices_map_animate <- housing_prices_clean %>%
  filter(geo != "Canada",
         str_detect(geo, ",", negate = TRUE),
         type == "Total (house and land)",
         date >= "1990-01-01") %>%
  mutate(pr_alpha = case_when(
    geo=="Newfoundland and Labrador" ~ "NL",
    geo=="Prince Edward Island" ~ "PE",
    geo=="Nova Scotia" ~ "NS",
    geo=="New Brunswick" ~ "NB",
    geo=="Quebec" ~ "QC",
    geo=="Ontario" ~ "ON",
    geo=="Manitoba" ~ "MB",
    geo=="Saskatchewan" ~ "SK",
    geo=="Alberta" ~ "AB",
    geo=="British Columbia" ~ "BC"))

housing_prices_map_animate_join <- housing_prices_map_animate %>%
  left_join(mapcan(boundaries = provinces, type = standard))

housing_prices_map_animate_plot <- ggplot(housing_prices_map_animate_join, aes(long, lat, group = group, fill = index))+
  geom_polygon() +
  coord_fixed() +
  transition_manual(date) + 
  theme_mapcan() +
  ggtitle('New Housing Price Index',
          subtitle ="{current_frame}")

# + 
#   labs(title = "Index",
#        subtitle = '{date}')


animate(housing_prices_map_animate_plot, fps = 2)

