gc()
rm(list=ls())
options(scipen = 999)

library(mgcv)
library(readxl)
library(htmltools)
library(stringi)
library(stringr)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

files.sources = paste0('src/',list.files('src/'))
sapply(files.sources, source)

df <- read.csv('data/housing.csv')

summary(df)

df <- df |> na.omit()

df[,unlist(lapply(df, is.numeric), 
           use.names = FALSE) ] |> 
  lapply(plot_descritiva)

ggplot(df) +
  geom_point(aes(x = longitude, y = latitude, color = median_house_value)) 

ggplot(df) +
  geom_point(aes(x = longitude, y = latitude, color = median_income))

ggplot(df) +
  geom_point(aes(x = median_income, y = median_house_value))

df$ocean_proximity |> table()

df <- df |> 
  separate_rows(ocean_proximity, sep = ",") |> 
  filter(ocean_proximity != "") |> 
  mutate(value = 1) |> distinct() |>
  pivot_wider(
    names_from = ocean_proximity,
    values_from = value,
    values_fill = 0
  )

df$ln_ticket <- df$median_house_value |> log()
df$ln_ticket |> plot_descritiva()


fmla <- ln_ticket ~
  s(latitude, longitude, k = 1000) +
  s(latitude, longitude, housing_median_age, k = 100) +
  s(housing_median_age) +
  s(latitude, longitude, total_rooms, k = 50) +
  s(latitude, longitude, total_bedrooms, k = 50)


model_pred <- full_pipeline(df, fmla, 'v0', test_split = T)

model <- model_pred[[1]]
pred <- model_pred[[2]]

err <- calc_erro_abs(pred$median_house_value, pred$avm, islog = F)

pred$erro <- err[[1]]

err[[2]] |> colnames() <- c('Value')

ggplot(err[[2]]) +
  geom_point(aes(x = seq(0, 1, .01), y = Value))
