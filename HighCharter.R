library(highcharter)

data(diamonds, mpg, package = "ggplot2")

highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "A highcharter chart") %>%
  hc_xAxis(categories = 2012:2016) %>%
  hc_add_series(data = c(3900, 4200, 5700, 8500, 11900),
                name = "Downloads")

hchart(diamonds$cut, colorByPoint = TRUE, name = "Cut")


hchart(diamonds$price, color = "#B71C1C", name = "Price") %>%
  hc_title(text = "You can zoom me")


library("forecast")
airforecast <- forecast(auto.arima(AirPassengers), level = 95)
hchart(airforecast)

data(unemployment)

hcmap("countries/us/us-all-all", data = unemployment,
      name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      borderColor = "transparent") %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>%
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%")
