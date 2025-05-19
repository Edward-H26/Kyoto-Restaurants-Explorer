library(shiny)
library(tidyverse)
library(DT)
library(markdown)
kyoto = read.csv("data/Kyoto_Restaurant_Info.csv") %>%
  select(-X) %>%
  rename(
    `Japanese Name` = JapaneseName,
    `First Category` = FirstCategory,
    `Second Category` = SecondCategory,
    `Dinner Price` = DinnerPrice,
    `Lunch Price` = LunchPrice,
    `Total Rating` = TotalRating,
    `Dinner Rating` = DinnerRating,
    `Lunch Rating` = LunchRating,
    `Review Number` = ReviewNum,
    `Latitude` = Lat,
    `Longitude` = Long
  )
