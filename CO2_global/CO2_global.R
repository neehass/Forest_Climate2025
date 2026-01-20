# Data loaded from https://gml.noaa.gov/ccgg/trends/gl_data.html

co2_data <- read.csv("C:/Users/maiim/Documents/25-26WS/Forest_Climate_Sys/co2_NOAA_data/co2_trend_gl.csv",
     header = TRUE, sep = ",", skip = 36)
View(co2_data)
co2_data$year <- as.integer(co2_data$year)
co2_data$smoothed <- as.numeric(co2_data$smoothed)

# mean CO2 in 2025
meanCO2_25 <- mean(co2_data$smoothed[co2_data$year == 2025])
print(meanCO2_25)

# approx vorindustirelle CO2
meanCO2_25 - 280 # (IPCC, 2013)

