# Libraries
library(readr)

# Loading dataset
# confirmed cases
confirmed = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed$case = "confirmed"

# death cases
death = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
death$case = "death"

# Recovered cases
recovered = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered$case = "recovered"

data = rbind(confirmed, death, recovered)

# transforming data
data = data %>% dplyr::select(-c("Province/State","Lat","Long"))
data$`Country/Region` = as.factor(data$`Country/Region`)
data$case = as.factor(data$case)

# consolidating data
data.1 = aggregate(. ~ `Country/Region` + case, data = data, FUN = "sum")

aux = data.1 %>% gather(key = "date", value = "number", -`Country/Region`, -case)
aux$date = as.Date(as.POSIXct(strptime(aux$date, "%m/%d/%y")))

datos = aux %>% group_by(`Country/Region`, case) %>%
  filter(number > 0, number <= max(number)) %>%
  mutate(number = number - lag(number, k = 1)) %>%
  summarise(Mean = mean(number, na.rm = TRUE),
            SD = sd(number, na.rm = TRUE),
            n = n()) %>%
  group_by(case) %>%
  filter(n > median(n)) %>%
  # mutate(Mean = round((Mean-min(Mean))/(max(Mean)-min(Mean)), 4),
  #        SD = round((SD-min(SD))/(max(SD)-min(SD)), 4),
  #        n = round((n-min(n))/(max(n)-min(n)), 4)) %>%
  ungroup() %>%
  gather(key = "Measure", value = "Value", -`Country/Region`, -case) %>%
  unite(temp, Measure, case) %>%
  spread(temp, Value) %>%
  na.omit()

health = read.csv("2.12_Health_systems.csv")
health = health %>%
  dplyr::select(c("Country_Region", "Health_exp_pct_GDP_2016",
                  "Health_exp_public_pct_2016", "Health_exp_out_of_pocket_pct_2016",
                  "Nurse_midwife_per_1000_2009.18", "Specialist_surgical_per_1000_2008.18")) %>%
  na.omit() %>%
  aggregate(. ~ Country_Region, data = ., FUN = "sum")
health = health[2:nrow(health), ]

datos1 = inner_join(datos, health, by = c("Country/Region" = "Country_Region"))
# write_csv(datos1, "Data.csv")