# Exercido de EDA 
#load
library(tidyverse)
library(DataExplorer)

# Specify URL 
url = "https://vincentarelbundock.github.io/Rdatasets/csv/MASS/anorexia.csv"

#Read csv file
data = read.csv(url)

#view the table
utils::View(data)

# EDA 
data %>% glimpse()
data %>% plot_intro()
data %>% plot_missing()
data %>% plot_density()
data %>% plot_histogram()
data %>% plot_correlation()
data %>% plot_prcomp()
