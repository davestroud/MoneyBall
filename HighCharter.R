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


