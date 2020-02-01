#
library(tidyverse)
library(fpp3)
library(readxl)
library(sugrrants)
library(feasts)


##### DAY#1 -------------------------------------------------------------------


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

# Lab #4 - Trend, Seasonal, and Cyclical: 
## Functions:
lag plots autocorrelation update_tsibble

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

## Functions:
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
## mable, dables, fables
## Functions:

model()
STL()
components()
autolayer()

### STL: Seasonal, Trend decomp using Loess - only additive (need transformation othewise)

### Example
dcmp <- elecequip %>% 
  model(STL(value ~ season(window = 7))) %>% 
  components()

autoplot(dcmp) %>% autolayer(dcmp, trend, col = "blue")
gg_season(dcmp, season_year)
electequip %>% autoplot(dcmp) %>% autolayer(dcmp, trend, col = "blue")

### Excercises - canadian gas data
can_gas <- canadian_gas %>% 
  model(STL(log(Volume) ~ season(window = 100) + trend(window = 10))) %>% 
  components() %>% 
  as_tsibble()

can_gas %>% autoplot()
gg_season(can_gas, y = season_year )

can_gas %>% autoplot(.vars = season_adjust)


#Multiple Seasonality, seasonal adjustment
## No Exercises, no lab

# Lab #9: Time Series features - anything computed from a time series.
## Strength of Seasonality
## functions:
features(series, feat_stl)
features(series, feature_set(pkgs = "feasts"))

## Exercises

tourism_ts %>% head()
holiday_feats <- tourism_ts %>% filter(Purpose == "Holiday") %>% 
  features(features = feat_stl) %>% 
  mutate_at(.vars = c("seasonal_peak_year","seasonal_trough_year"), .funs = as.factor)
  
GGally::ggpairs(holiday_feats %>% select_if(is.numeric))


holiday_feats_acf <- tourism_ts %>% filter(Purpose == "Holiday") %>% 
  features(features = feat_acf) 

GGally::ggpairs(holiday_feats_acf %>% select_if(is.numeric))

# Lab #10: Feature Extraction
## functions:
broom::augment()
prcomp()
feature_set()

tourism_features <- tourism_ts %>% 
  features(Trips, feature_set(pkgs = "feasts"))

pcs <- tourism_features %>% 
  select(-State,-Region, -Purpose) %>% 
  prcomp(scale = TRUE) %>% 
  broom::augment(tourism_features)

pcs %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point() + 
  theme(aspect.ratio = 0.5)

pcs %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() + 
  theme(aspect.ratio = 0.5)


## Exercises
PBS %>% head()
pbs_no_zeros <- PBS %>% 
  group_by_key() %>% # tsibble
  filter(!all(Cost == 0)) %>% 
  ungroup()

pbs_features <- pbs_no_zeros %>% 
  features(Cost, feature_set(pkgs = "feasts"))

pbs_pcs <- pbs_features %>% 
  select(-Month, Concession, Type, ATC1, ATC1_desc, ATC2, ATC2_desc) %>% 
  prcomp(scale = TRUE) %>% 
  broom::augment(pbs_features)

##### DAY#2 -------------------------------------------------------------------

# Benchmark

## Simple: naive, mean, seasonal naive, RW with drift
## functions:
fabletools::model()
fabletools::forecast()

# Lab #11: Benchmark methods

hh_budget %>% head()
hh_budget %>% autoplot(.vars = Wealth)
weath_fc <- hh_budget %>% 
  dplyr::filter(!is.na(Wealth)) %>% 
  fabletools::model(
    #snaive = SNAIVE(Wealth),
    naive = NAIVE(Wealth),
    Drift = RW(Wealth ~ drift()),
    Mean = MEAN(Wealth)
  )

### Build the forecast
weath_fc %>% fabletools::forecast(h = "5 years") %>% 
  filter(.model == "naive") %>% 
  autoplot()

aus_retail

# Lab #12: Residuals
## functions: 
augment() - pass the fitted model
gg_tsresiduals()
features()
model()
## Notes: Jacqe-Bera test are too sensitive.

## Exercises
autoplot(aus_production, .vars = Beer)
beer_fit <- aus_production %>% select(Quarter, Beer) %>% 
  model(snaive = SNAIVE(Beer))

forecast(beer_fit)
augment(beer_fit)
gg_tsresiduals(beer_fit)
augment(beer_fit) %>% features(.resid, ljung_box, dof = 4, lag = 20 )


## Lab#13: Forecast Accuracy Mearures
## functions
## NEW MAPE: MASE !!! 

beer_fc <- forecast(beer_fit, h = "3 years")
accuracy(beer_fc, aus_production)

## Exercises
autoplot(hh_budget, .vars = Wealth)
hh_budget %>% nrow()
wealth_test <- hh_budget %>% tail(n = 4)
wealth_train <- hh_budget %>% filter(Year <= max(Year) - 4)
wealth_fit <- wealth_train %>%
  model(naive = NAIVE(Wealth),
        Mean = MEAN(Wealth),
        RW = RW(Wealth ~ drift())
        )

wealth_fit %>% 
  forecast(h = "4 years") %>% 
  autoplot(hh_budget, level = NULL) 
  
hh_budget_forecast %>% 
  accuaryc(hh_budget) %>% 
  group_by(.model) %>% 
  summarise_if(is.numerc, mean)

# Lab#14: ExponenTial Smoothing: ETS
## functions:
report()
ETS()
trend()

## Examples
aus_economy <- gloabl_economy %>% 
  filter(Code == "AUS") %>% 
  mutate(Pop = population/1e6) 
fit <- aus_economy %>% 
  model(AAN = ETS(Pop))
report(fit)

fit %>% 
  forecast(h = 20) %>% 
  autoplot(aus_economy) 

## Exercises
china_gdp <- global_economy %>% filter(Country == "China")

china_fit <- china_gdp %>% 
  model(ets_base = ETS(GDP),
        ets_bx = ETS(box_cox(GDP, lambda = .5)),
        ets_log = ETS(log(GDP))
        )

china_fit %>% augment() %>% autoplot()
china_fit %>% forecast(h = 20) %>% 
  autoplot(china_gdp, level = NULL)

components(fit)
glance(fit)
tidy(fit)
coef(fit)


# Lab #15:  Seasonal Methods - Holt Winters
## functions

## Examples

# Notes: AIC is just like cross-validation for TimeSeries

## Exercises
gas_train <- aus_production %>% 
  filter(Quarter <= max(Quarter) - 8)

gas_fit <- gas_train %>% 
  model(
    ets_base = ETS(Gas),
    ets_log = ETS(log(Gas)),
    ets_damp = ETS(Gas ~ trend("Ad")),
    ets_damp = ETS(log(Gas) ~ trend("Ad"))
    
  )

gas_fit
forecast(gas_fit, h = 16) %>% autoplot(aus_production, level = NULL)
accuracy(gas_fit) # gives MASE
report(gas_fit)
components(gas_fit)
glance(gas_fit)
tidy(gas_fit)
coef(gas_fit)

# Lab #NA: Non-Guassian forecast distributions (not Normal errors)
## functions
generate(h = "3 years" times = 1000, boostrap = TRUE) # build simulations, use sims for forecasting.
forecast(, boostrap = TRUE) # same as generate to sim a future path.

## NOTE: ETS does not work with Cycles!!!


# Lab# 16: ARIMA Models: Must be stationary, requires transformation.
##Note: AICc is only within model class, and same differencing.
## functions:
fable:: # ts cross validation
forecast::tsCV()
difference(12) # creates 12 step lag

us_gdp <- global_economy %>% filter(Code == "USA")

us_fit <- us_gdp %>% 
  model(
    arma = ARIMA(GDP),
    arma_log = ARIMA(log(GDP)),
    arma_log1 = ARIMA(log(GDP) ~ pdq(p=2:3,d=1,q=2))
  )
report(us_fit)
glance(us_fit)
accuracy(us_fit)
us_fit %>% forecast(h = 5) %>% 
  autoplot(us_gdp, level = NULL)

# Lab #17: ARIMA Seasonality
## 
parameters for ARIMA
stepwise = F
approximation = F
order_constraint = p + d + q + >9

tourism %>% head()
tour_hol <- tourism %>% filter(Purpose == "Holiday") %>% #Region == "Snowy Mountains"
  as_tsibble(index = Quarter, key = c(Region, State))
autoplot(tour_hol, .vars = Trips)

tour_hol_fit <- tour_hol %>% 
  model(
    auto_arima = ARIMA(Trips)
  )

tour_hol_fit %>% report()
tour_hol_fit %>% accuracy()
tour_hol_fit %>% glance()

tour_hol_fit %>% filter(Region == "Barkly") %>% 
  forecast(h = 5, level = NULL) %>% 
  autoplot(tour_hol)

# Lab #NA: Ensembles, FFORMA - custom weights for ensembles

# Dynamic Forecast: adding covariates, NOT CAUSAL MODELS!!
# Regression with ARIMA errors: RegARIMA
# Notes: need a df of covariate data for forecast
# functions:
ARIMA(y ~ x1 + x2 +...+xn)
forecast(fit, new_data = us_change_future)

## Example
augment(fit) %>% 
  features(.resid, ljung_box, dof = 6, )

#forecasting with covariates


# Lab #18: Daily Data
## Notes: Piece-wise linear functions.


# Lab #19: Dynamic Harmonic regressions: Daily/Weekly Data
## NOTES: Weekly data, upto K <= 26, 2 coef * 26 weeks = 52 weeks, or seasonal pattern.
##    Much better than Seasonal Arima for weekly/daily data.
## functions
ARIMA(series ~ fourier(K = 1) + PDQ(0,0,0))
fourier(period = 24*365) # aka, hourly seasonality + daily seasonality.
K = 1 sin,
K = 2, sin + cos
K = 13, start with 13 for weekly data, will create 26 coefs.

# Exercises

# Lab #19: Lagged Predictors
## functions:
put lag around predictor 


# Lab #20: Forecast Reconcilation: Hiearachical / Grouped time series
## functions:
aggregate_key(purpose * (State / Region), Trips = sum(Trips))
aggregate_key((Region / Group) * (Class / Dept), Sales = sum(Sales))
reconcile()
min_trace() # minimize the trace of the covariance
filter_index()

## Example
## Exercise 
PBS %>% head()

pbs_agg_base <- PBS %>% 
  aggregate_key(Concession / Type / ATC1,
                Costs = sum(Cost)/1e6)

pbs_agg_fc <- pbs_agg_base %>% 
  filter(Month <= max(Month) - 36) %>% 
  model(
    ml_ets = ETS(Costs),
    ml_arima = ARIMA(Costs),
    ml_snaive = SNAIVE(Costs)
  ) %>% 
  reconcile(adj_ets = min_trace(ml_ets),
            adj_arima = min_trace(ml_arima),
            adj_snaive = min_trace(ml_snaive)
            ) %>% 
  forecast(h = "3 years")

accuracy(fc, pbs_agg_base) %>% 
  group_by(.model) %>% 
  summarise(MASE = mean(MASE)) %>% 
  arrange(MASE)













