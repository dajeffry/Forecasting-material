library(tidyverse)

performance_report <- read_csv("performance_report data (rutin) 2021.csv")

# Anggaran berdasarkan peramalan
anggaran <- performance_report |> 
  mutate(
    anggaran_by_arima = arima_2020 * harga,
    anggaran_by_ets = ets_2020 * harga,
    anggaran_by_roq = ROQ * harga,
    anggaran_by_croston = croston_2020 * harga,
    anggaran_by_sba = sba_2020 * harga
  ) |> 
  pivot_longer(
    starts_with("anggaran_by"), 
    names_to = "metode_ramalan", 
    values_to = "hasil_peramalan"
  ) |> 
  group_by(metode_ramalan) |> 
  summarise(total_anggaran = sum(hasil_peramalan, na.rm = T))

# Anggaran berdasarkan peramalan dan penyesuaian tingkat kritis
anggaran_adjusted <- performance_report |> 
  mutate(
    arima_adjusted = if_else(arima_2021 <= 0 & tingkat_kekritisan == "A", 1, arima_2021),
    ets_adjusted = if_else(ets_2021 <= 0 & tingkat_kekritisan == "A", 1, ets_2021)
  ) |> 
  mutate(
    anggaran_by_arima_adj = arima_adjusted * harga,
    anggaran_by_ets_adj = ets_adjusted * harga
  ) |> 
  pivot_longer(
    starts_with("anggaran_by"), 
    names_to = "metode_ramalan", 
    values_to = "hasil_peramalan"
  ) |> 
  group_by(metode_ramalan) |> 
  summarise(total_anggaran = sum(hasil_peramalan, na.rm = T))
