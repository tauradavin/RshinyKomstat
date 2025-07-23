# --- Memuat Library yang Dibutuhkan ---
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(EnvStats)
library(e1071)
library(sf)
library(leaflet)
library(car)
library(MASS)
library(knitr)
library(rmarkdown)
library(gridExtra)
library(htmlwidgets)
library(plotly)

# --- Memuat Data Awal ---
tryCatch({
  data_uas_initial <- read_excel("Data Uas.xlsx")
}, error = function(e) {
  data_uas_initial <- data.frame(
    Pesan = "File 'Data Uas.xlsx' tidak ditemukan. Pastikan file berada di direktori yang sama dengan aplikasi."
  )
})

rv_data <- reactiveValues(data = data_uas_initial)
