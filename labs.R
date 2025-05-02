# All of the above
library(fpp3)

raw_accommodation <- readr::read_csv(
  "https://workshop.nectric.com.au/fable-2025/data/aus_accommodation.csv",
  col_types = cols(
    Date = col_date(format = "%Y-%m-%d"),
    State = col_character(),
    Takings = col_double(),
    Occupancy = col_double(),
    CPI = col_double()
  )
)

library(readr)
spec(raw_accommodation)

readr::read_csv(
  "https://workshop.nectric.com.au/fable-2025/data/aus_accommodation.csv"
) |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(
    index = Quarter,
    key = State
  )

vic_tourism <- tourism |>
  # Use as_tibble() if you no longer want a time series.
  # as_tibble() |>
  filter(State == "Victoria") |>
  summarise(Trips = sum(Trips))


tourism |>
  index_by(Year = year(Quarter)) |>
  group_by(Purpose) |>
  summarise(Trips = sum(Trips))


vic_tourism |>
  autoplot(Trips)

vic_tourism |>
  gg_season(Trips)

vic_tourism |>
  gg_subseries(Trips)

aus_production |>
  filter(year(Quarter) > 1991) |>
  autoplot(Beer)

aus_production |>
  filter(year(Quarter) > 1991) |>
  gg_subseries(Beer)


pbs_scripts <- PBS |>
  summarise(Scripts = sum(Scripts))

pbs_scripts |>
  autoplot()

pbs_scripts |>
  model(
    NAIVE(Scripts),
    SNAIVE(Scripts),
    NAIVE(Scripts ~ drift()),
    SNAIVE(Scripts ~ drift()),
  ) |>
  forecast(h = "5 years") |>
  autoplot(pbs_scripts)




vic_tourism <- tourism |>
  filter(State == "Victoria") |>
  summarise(Trips = sum(Trips))

vic_tourism |>
  autoplot(Trips)

vic_tourism |>
  model(
    SNAIVE(Trips ~ drift()),
    TSLM(Trips ~ trend() + season() + lag(Trips))
  ) |>
  forecast(h = 10) |>
  autoplot(vic_tourism)

lubridate::period()


aus_accommodation_total <- fpp3::aus_accommodation |>
  summarise(Takings = sum(Takings), Occupancy = mean(Occupancy))

aus_accommodation_total |>
  autoplot(Takings)

fit <- aus_accommodation_total |>
  model(
    TSLM(Takings ~ trend() + season() + Occupancy)
  )
report(fit)

fit |>
  forecast(h = "10 years") |>
  autoplot(aus_accommodation_total)



aus_production |>
  filter(year(Quarter) <= 1975) |>
  model(
    TSLM(Beer ~ trend() + season()),
    ETS(Beer ~ error("A") + trend("A") + season("A"))
  ) |>
  forecast(h = "10 years") |>
  autoplot(aus_production)

tot_retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))
tot_retail |>
  autoplot(Turnover)

tot_retail |>
  model(
    ETS(Turnover)
  ) |>
  forecast(h = "5 years") |>
  autoplot(tot_retail)


aus_accommodation_total <- fpp3::aus_accommodation |>
  summarise(Takings = sum(Takings), Occupancy = mean(Occupancy))

aus_accommodation_total |>
  autoplot(Takings)

aus_accommodation_total |>
  model(ETS(Takings)) |>
  forecast(h = "5 years") |>
  autoplot(aus_accommodation_total)


tot_retail |>
  autoplot(Turnover)

tot_retail |>
  autoplot(log(Turnover))

tot_retail |>
  autoplot(sqrt(Turnover))

tot_retail |>
  model(
    ARIMA(log(Turnover)),
    ETS(Turnover)
  ) |>
  forecast(h = "10 years") |>
  autoplot(tot_retail)


library(fpp3)
vic_tourism <- tourism |>
  filter(State == "Victoria") |>
  summarise(Trips = sum(Trips))
fit <- vic_tourism |>
  model(
    ETS(Trips),
    ARIMA(log(Trips))
  )
fit |>
  forecast(h = "5 years") |>
  autoplot(vic_tourism)

augment(fit)
glance(fit)
tidy(fit)

augment(fit)

fit

# Training accuracy
# accuracy on a mable
accuracy(fit)



aus_accommodation_total <- fpp3::aus_accommodation |>
  summarise(Takings = sum(Takings), Occupancy = mean(Occupancy))

aus_accommodation_total |>
  autoplot(log(Takings))

fit <- aus_accommodation_total |>
  model(
    ARIMA(Takings),
    ARIMA(log(Takings)),
    ETS(Takings)
  )
fit
accuracy(fit)


fit |>
  forecast(h = "5 years") |>
  autoplot(aus_accommodation_total)


# Forecast accuracy
# accuracy on a fable
fc <- aus_accommodation_total |>
  filter(Date <= yearquarter("2012 Q2")) |>
  model(
    # ARIMA(Takings),
    ARIMA(log(Takings)),
    ETS(Takings)
  ) |>
  forecast(h = "4 years")

fc |>
  autoplot(aus_accommodation_total)
fc |>
  accuracy(aus_accommodation_total)




# Cross-validated forecast accuracy
# accuracy on a fable
aus_accommodation_total |>
  filter(Date <= yearquarter("2012 Q2")) |>
  stretch_tsibble(.step = 8, .init = 24) |>
  mutate(.id = -.id) |>
  autoplot(Takings) +
  geom_line(aes(y = Takings), colour = "green", data = aus_accommodation_total) +
  geom_line(aes(y = Takings))


fc <- aus_accommodation_total |>
  filter(Date <= yearquarter("2012 Q2")) |>
  # Cross-validation
  stretch_tsibble(.step = 8, .init = 24) |>
  model(
    # ARIMA(Takings),
    ARIMA(log(Takings)),
    ETS(Takings)
  ) |>
  forecast(h = "4 years")

# fc |>
#   autoplot(aus_accommodation_total)
fc |>
  accuracy(aus_accommodation_total)


vic_tourism |>
  autoplot(Trips)

fit <- vic_tourism |>
  model(TSLM(Trips ~ trend()))
augment(fit) |>
  autoplot(.innov)
augment(fit) |>
  gg_season(.innov)


fit |>
  gg_tsresiduals()
fit <- vic_tourism |>
  model(TSLM(log(Trips) ~ trend() + season()))
fit |>
  gg_tsresiduals()

fit <- vic_tourism |>
  model(
    ETS(Trips ~ trend("A"))
  )
fit |>
  gg_tsresiduals()

fit |>
  forecast(h = "5 years") |>
  autoplot(vic_tourism)
