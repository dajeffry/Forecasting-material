library(tidyverse)
library(readxl)
library(fpp3)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(ggplot2)

# Load and Transform ----
dt <- read_excel("Data Kerja Praktik.XLSX", sheet = "FINAL", skip = 1) %>%
  select("material" = `Short text`, Jan:Des, `2014`:`2018`, `2019` = `2019...30`, 32)
dt <- dt[2:(nrow(dt)-2),]

dt <- dt %>%
  filter(`Rutin/Non Rutin`=="RUTIN")

yearly <- dt %>%
  select(material, `2014`:`2019`) %>%
  pivot_longer(`2014`:`2019`, names_to = "tahun", values_to = "n", ) %>%
  mutate(tahun = as.integer(tahun), n = ceiling(n)) %>%
  arrange(tahun, material) %>%
  distinct(tahun, material, .keep_all = T) %>%
  as_tsibble(index = tahun, key = material)

yearly
yearly %>%
  filter(material %in% c("ACTUATOR ACC;POSITIONER SIPART PS2",
                         "BEAR;DOD;MERRICK490G;020261",
                         "LUBE;S;MOBIL SHC 626", "LUBE;T;SHELL TELLUS T 46",
                         "LUBE;T;SHELL TURBO T 32")) %>%
  ggplot(aes(x = tahun, y = n, color = material)) +
  geom_line(show.legend = FALSE) + 
  facet_wrap(~ material, scales = "free_y")

yearly %>%
  ggplot(aes(x = tahun, y = n, color = material)) +
  geom_line(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ material, ncol = 10, scales = "free_y")

yearly %>%
  ggplot(aes(x = tahun, y = n, color = material)) +
  geom_line(show.legend = FALSE) + 
  scale_y_continuous(labels = scales::comma) 


####
# cara mengelompokkan data berdasarkan jumlah maerial (<1k, 1k - 2k, dst)
# melihat jenis material paling banyak digunakan (top 10 frekuensi material)
# mengelompokan data hasil clustering kedalam data frame
# menghitung hasil prediksi dengan menggunakan model
 

