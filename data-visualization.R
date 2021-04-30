library(data.table)
library(tidyverse)
library(hrbrthemes)

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
  theme_ipsum_tw()

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
  geom_line()+
  theme_ipsum_tw()

housing_prices_prov_lines

housing_prices_prov_facet <- ggplot(housing_prices_prov, aes(x = date, y = index)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_ipsum_tw()

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
  theme_ipsum_tw()

housing_prices_prov_compare

housing_prices_cma_compare <- housing_prices_clean %>%
  filter(str_detect(geo,",")) %>%
  filter(type != "Total (house and land)")


housing_prices_cma_compare <- ggplot(housing_prices_cma_compare, aes(x = date, y = index, color = type)) +
  geom_line()+
  facet_wrap(vars(geo)) +
  theme_ipsum_tw()

housing_prices_cma_compare


