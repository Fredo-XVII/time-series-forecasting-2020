#
library(tidyverse)
library(fpp3)
library(readxl)
library(sugrrants)
library(feasts)

# Lab #1

#tourism <- readxl::read_excel('http://robjhyndman.com/data/tourism.xlsx')
tourism <- read_excel("data/tourism.xlsx") %>% 
  dplyr::mutate(Quarter = tsibble::yearquarter(Quarter))
  #dplyr::mutate_at(vars(Quarter), .funs = as.Date)

tourism_ts <- tsibble::as_tsibble(tourism, index = Quarter,
                                  key = c(Region,State,Purpose))
str(tourism_ts)

# Lab #2


# Lab #3 - gg_season, gg_subseries frame_calendar prettify
# facet_calendar

## Example
vic_elec %>% 
  filter(year(Date) == 2014) %>% 
  mutate(Hour = hour(Time)) %>% 
  frame_calendar(
    x = Hour, y = Demand, date = Date, nrow = 4) %>% 
  ggplot((aes(x))) + geom_line() %>% 
  prettify()

%>% head()

## Exercise

snowy <- tourism_ts %>% 
  dplyr::filter(Region == "Snowy Mountains")
head(snowy)
feasts::autoplot(snowy)

snowy %>% feasts::gg_season(Trips)
snowy %>% feasts::gg_subseries(Trips)
snowy %>% frame_calendar()

head(pedestrian)

# Lab #4 - Trend, Seasonal, and Cyclical: lag plots autocorrelation update_tsibble

## Examples
new_production %>%  gg_lag(df$Beer, geom = "point")
new_production %>% ACF(Beer, lag_max = 9) %>% 
  autoplot()

update_tsibble(index = trading_date, regular = TRUE)

## Exercises
head(aus_production)
bricks <- aus_production %>% select(Quarter, Bricks) %>% 
  as_tsibble(.,index = Quarter)
head(bricks)
autoplot(bricks)
feasts::gg_lag(bricks)
bricks %>% feasts::ACF() %>% autoplot()

head(pelt)
autoplot(pelt, .vars = Lynx)
feasts::gg_lag(pelt, y = Lynx)
feasts::ACF(pelt, .vars = Lynx) %>% autoplot()

gafa_stock %>% head()
gafa_stock %>% filter(Symbol == "AMAZ")

3 - d
1 - b 
2 - a
4 - c

# Lab #5 White Noise

WN <- tsibble(t = seq(36), y = rnorm(36), index = t)
autoplot(WN)

goog <- gafa_stock %>% 
  filter(Symbol == "GOOG", year(Date) >= 2018) %>% 
  mutate(trading_day = dplyr::row_number()) %>% 
  update_tsibble(index = trading_day, regular = TRUE) %>% 
  mutate(diff = difference(Close))

# Lab #6 - Adjustments for inflation and Population
global_economy %>% 
  autoplot(GDP/Population, alpha = 0.3) +
  geom_line() +
  guides( colour = FALSE)

global_economy %>% #head()
  ggplot(aes(x = Year, y =  GDP/Population), group = Country) +
  geom_line() +
  facet_wrap(Country ~ .)

avg_gdp_pg <- global_economy %>% 
  as_tibble() %>% 
  group_by(Country) %>% 
  summarise(gdp_pc = mean(GDP/Population, na.rm = TRUE),
            last = last(GDP/Population))

top_n(avg_gdp_pg, n = 5)  

# Lab #7 - Transformations: Power and ? Monotonic Transformation
## Box-Cox Transformations: 

features(series, features = guerror)
log1p() # log + 1
box_cox()

## Exercises
# United States           0.282
global_economy %>% filter(Country == "United States") %>% #autoplot(GDP) 
  features(.$GDP, features = guerrero)

# Bulls, bullocks and steers Victoria         -0.0720
aus_livestock %>% #head()
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") %>% #autoplot(Count)
  features(.$Count, features = guerrero)
 
# Demand:   0.887
vic_elec %>% #autoplot()
  #features(Demand, feature = guerrero) # 0.887
  features(log(Demand), feature = guerrero) # 2.00

# Canadian gas production

# Lab #8: Seasonality and Trends - Decompostion









