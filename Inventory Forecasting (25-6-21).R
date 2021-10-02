library(tidyverse)
library(readxl)
library(fpp3)

# Load and Transform ----
dt <- read_excel("Data Kerja Praktik.XLSX", sheet = "FINAL", skip = 1) |>
  select("material" = `Short text`, Jan:Des, `2014`:`2018`, `2019` = `2019...30`, 32)
dt <- dt[2:(nrow(dt)-2),]

monthly19 <- dt |>
  select(material, Jan:Des) |>
  pivot_longer(Jan:Des, names_to = "bulan", values_to = "n") |>
  distinct(bulan, material, .keep_all = T) |>
  group_by(material) |> 
  mutate(
    no = row_number(),
    tanggal = paste0("2019-", no, "-01"),
    tanggal = as_date(tanggal),
    tanggal = yearmonth(tanggal)
  ) |> 
  ungroup() |> 
  arrange(tanggal, material) |> 
  select(tanggal, material, n) |> 
  as_tsibble(index = tanggal, key = material)

# Split training = 10, test = 2 ----
train_ts <- monthly19 |>
  group_by_key(material) |>
  slice(1:11)

test_ts <- monthly19 |>
  group_by_key(material) |>
  slice_tail(n = 1)

# Fitting Model ----
model_fit <- train_ts |>
  model(
    arima = ARIMA(n),
    ets = ETS(n)
  )

# Menghitung Akurasi ----
akurasi <- forecast(model_fit, h = 1) |>
  accuracy(test_ts) |>
  select(.model, material, RMSE:MAPE)

akurasi_summary <- akurasi |>
  na.omit() |>
  group_by(.model) |>
  summarise(RMSE_mean = mean(RMSE))

ggplot(akurasi_summary, aes(fct_reorder(.model, RMSE_mean), RMSE_mean)) +
  geom_col() +
  coord_flip()

# Menghitung nilai ramalan ----
peramalan <- forecast(model_fit, h = 2)

# Tabel report ----
t1 <- peramalan |>
  as_tibble() |>
  filter(tanggal == yearmonth("2020 Jan")) |>
  mutate(prediksi_jan_2020 = paste0(.model, "_2020jan")) |>
  select(material, .mean, prediksi_jan_2020) |>
  pivot_wider(names_from = prediksi_jan_2020, values_from = .mean)

t2 <- akurasi |>
  select(.model, material, RMSE) |>
  pivot_longer(RMSE, names_to = "matriks", values_to = "nilai_akurasi") |>
  mutate(model_akurasi = paste0(.model, "_", matriks)) |>
  select(material, nilai_akurasi, model_akurasi) |>
  pivot_wider(names_from = model_akurasi, values_from = nilai_akurasi)

rutin <- dt |>
  select(material, "jenis" = `Rutin/Non Rutin`) |>
  distinct()

performance_report <- left_join(t1, t2) |>
  left_join(rutin) |>
  select(material, jenis, starts_with("croston"), starts_with("sba"), 
         starts_with("sbj"), starts_with("arima"), starts_with("ets"))

write_csv(performance_report, "performance_report_monthly19.csv")
