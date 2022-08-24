multi <- tsibble(
  quarter = bed_multi$quarter,
  org_code = bed_multi$`Org Code`,
  value = bed_multi$value,
  key = org_code,
  index = quarter
)


fitted<- multi |>
  filter(org_code == 'R1C' ) |>
  model(arima210 = ARIMA(value ~ pdq(2,1,0)),
        arima013 = ARIMA(value ~ pdq(0,1,3)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise=FALSE))

# Let us find out which model has the lowest AIC value and use that to forcast.
glance(fitted) |> arrange(AICc) |> select(.model:BIC)

# Now let us forecast
forecast(fitted, h=15) |>
  filter(.model=="search") |>
  autoplot(multi |> filter(org_code == 'R1C')) +
  labs(title = "One Single Model Using ARIMA ",
       y="Percentage")

autoplot(multi |> filter(org_code == 'R1C'), value)

https://otexts.com/fpp2/
  https://algotech.netlify.app/blog/purrr-operly-fitting-multiple-time-series-model/
  https://stackoverflow.com/questions/50423078/plotting-timeseries-with-map-from-purrr
#########################################################################################################



###########################################################################################################
sar <- function(x) {
  SRa <- model(
  arima012011 = ARIMA(x ~ pdq(0,1,2) + PDQ(0,1,1)),
  arima210011 = ARIMA(x ~ pdq(2,1,0) + PDQ(0,1,1)),
  auto = ARIMA(x, stepwise = FALSE, approx = FALSE)
)
  gl<- glance(SRa) |> arrange(AICc) |> select(.model:BIC)
  return(gl)
}
#########################################################################################################
multi |>
  group_by(org_code)|>
  nest()|>
  mutate(AIC = map(.x~ model(
    arima012011 = ARIMA(value ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(value ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(value, stepwise = FALSE, approx = FALSE), data =.x
  ))) |>
  tidy()

###########################################################################################################
del_multi |>
  split(del_multi$org_code)|>
  map(~ model(
    arima012011 = ARIMA(value ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(value ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(value, stepwise = FALSE, approx = FALSE), 
  ), data = del_multi)


mtcars
%>%
  split(.$cyl)


%>%
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

###########################################################################################################
SRa <- del_multi |> filter(org_code == 'RA7')|>
  model(
    arima012011 = ARIMA(value ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(value ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(value, stepwise = FALSE, approx = FALSE)
  )

glance(SRa) |> arrange(AICc) |> select(.model:BIC)

forecast(SRa, h=35) |>
  filter(.model=="arima012011") |>
  autoplot(del_multi|> filter(org_code == 'RA7')) +
  labs(title = "TRUSTS",
       y="Percentage OK ")