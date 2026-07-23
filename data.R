library("KnoxStats")
library(tidyverse)
countries = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT6Xmd1syA72bdIb7tiJzJjHdX5NoTff4lOTL3c2r_Il70fCs2nsvsOraQ-2AnrI5MCWX3FheY42li7/pub?output=csv")
set.seed(50)
countrySample = sample(223, 100)
countries[countrySample, ]

write.csv(countries[countrySample, ],"~/Desktop/sample100countries.csv", row.names = FALSE)

d = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQAgzmzZaBF2Zf6LdX_9vUkYAdFZboQRhDPmmIShHPqNoeJMckME9Obb7ww0tf3vU4elvQ78_tFIFSN/pub?output=csv")
fruit = scale(d$FruitConsumptionPerCapita)
veg = scale(d$VegetableConsumptionPerCapita)
wheat = scale(d$WheatConsumptionPerCapita)
sugar = scale(d$SugarPerCapita)

dietScore = rowMeans(
  cbind(
    fruit,
    veg,
    wheat,
    -sugar
  ),
  na.rm = TRUE
)
write.csv(
  data.frame(DietScore = dietScore),
  "~/Desktop/DietScore.csv",
  row.names = FALSE
)