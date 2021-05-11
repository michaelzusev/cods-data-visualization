library(data.table)
library(tidyverse)
library(ggthemes)
library(jsonlite)
library(lubridate)
library(skimr)
library(httr)
library(scales)
library(maps)
library(devtools)
library(mapcan)
library(gganimate)
library(transformr)

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

# get_table("23100287", "test")

housing_prices_raw <- fread("18100205.csv", encoding = "UTF-8")


str(housing_prices_raw)




unique(housing_prices_raw[,'New housing price indexes'])
unique(housing_prices_raw[,'UOM'])
unique(housing_prices_raw[,'STATUS'])
unique(housing_prices_raw[,'DECIMALS'])


for (colname in colnames(housing_prices_raw)){
  print(unique(housing_prices_raw[,..colname]))
}


skim(housing_prices_raw)



housing_prices_clean <- housing_prices_raw %>%
  mutate(date = as.Date(paste0(REF_DATE, "-01"))) %>%
  select(date, GEO, `New housing price indexes`, VALUE) %>%
  rename(geo = GEO,
         type = `New housing price indexes`,
         index = VALUE)
  
skim(housing_prices_clean)

str(housing_prices_clean)
unique(housing_prices_clean[,'geo'])

housing_prices_canada <- housing_prices_clean %>%
  filter(geo == "Canada", type == "Total (house and land)")


housing_prices_canada_plot <- ggplot(housing_prices_canada, aes(x = date, y = index)) +
  geom_line() +
  theme_tufte()

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
  theme_tufte()

housing_prices_prov_lines_plot

housing_prices_prov_facet_plot <- ggplot(housing_prices_prov, aes(x = date, y = index)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_tufte()

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
  facet_wrap(vars(geo)) +
  theme_tufte()

housing_prices_prov_compare_plot

housing_prices_cma_compare <- housing_prices_clean %>%
  filter(str_detect(geo,",")) %>%
  filter(type != "Total (house and land)")


housing_prices_cma_compare_plot <- ggplot(housing_prices_cma_compare, aes(x = date, y = index, color = type)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_pander()

housing_prices_cma_compare_plot

housing_prices_cma_6mth <- housing_prices_cma_compare %>%
  filter(date > (today()-months(8)))

housing_prices_cma_6mth_plot <- ggplot(housing_prices_cma_6mth, aes(x = date, y = index, fill = type)) +
  geom_col(position = "stack") + 
  facet_wrap(vars(geo))

housing_prices_cma_6mth_plot

housing_prices_cma_growth <- housing_prices_cma_6mth %>%
  group_by(geo, type) %>%
  mutate(growth = index - lag(index)) %>%
  ungroup %>%
  mutate(city = word(geo, 1, sep = ","))

housing_prices_cma_growth_plot <- ggplot(housing_prices_cma_growth, aes(x = date, y = growth, fill = type)) +
  geom_col(position = "stack") +
  facet_wrap(vars(city), as.table = FALSE) + 
  theme_pander(base_size = 10)

housing_prices_cma_growth_plot 



aircraft <- fread("23100287.csv", encoding = "UTF-8")

aircraft_clean <- aircraft %>%
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
  theme_pander()

aircraft_plot


get_table("14100221", "weekly_earnings")


skim(weekly_earnings)



weekly_earnings_clean <- weekly_earnings %>%
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
  theme_pander()

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
  geom_col()

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
  transition_manual(date, len) + 
  theme_mapcan() +
  ggtitle('Index',
          subtitle ="{current_frame}")

# + 
#   labs(title = "Index",
#        subtitle = '{date}')


animate(housing_prices_map_animate_plot, fps = 2)

