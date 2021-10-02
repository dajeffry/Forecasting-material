library(tidyverse)
library(readxl)

# Memuat database harga material ----
db <- read_excel("Data Kerja Praktik.XLSX") |> 
  select("material" = `Short Text`, "harga_satuan" = `Net price`) |> 
  slice(-1) |>
  group_by(material) |> 
  slice_min(order_by = harga_satuan, n = 1) |> 
  ungroup()

# Memuat data ramalan ----
performance_report <- read_csv("performance_report.csv")

# Menghitung perkiraan anggaran ----
anggaran <- performance_report |> 
  left_join(db) |> 
  mutate(
    anggaran_by_arima = arima_2020 * harga_satuan,
    anggaran_by_ets = ets_2020 * harga_satuan
  ) |> 
  pivot_longer(
    starts_with("anggaran_by"), 
    names_to = "metode_ramalan", 
    values_to = "hasil_peramalan"
  ) |> 
  group_by(metode_ramalan) |> 
  summarise(total_anggaran = sum(hasil_peramalan, na.rm = T))