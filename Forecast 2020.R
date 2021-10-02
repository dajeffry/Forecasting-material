library(tidyverse)
library(readxl)
library(fpp3)

dt2 <- read_excel("Anlisa ABC SM2 2021 - lampiran.XLSX", sheet = "FINAL", skip = 2) |>
  select("material" = `Material`, "harga" = Harga, "tingkat_kekritisan" = `Tingkat kekritisan`, "rutin_nonrutin" = `Rutin/Non Rutin`, Jan:Des, `2015`:`2020`)

dt2 <- dt2 %>%
  filter(`rutin_nonrutin`=="RUTIN")

is.na.data.frame(dt2)
table(dt2$tingkat_kekritisan)
n_distinct(dt2)
sum(is.na(dt2))
dt2 <- dt2[complete.cases(dt2),]

dt2 |> 
  count(material) |> 
  arrange(desc(n))

yearly <- dt2 |>
  select(material, `2015`:`2020`) |>
  pivot_longer(`2015`:`2020`, names_to = "tahun", values_to = "n") |>
  mutate(tahun = as.integer(tahun)) |>
  arrange(tahun, material) |>
  distinct(tahun, material, .keep_all = T) |>
  as_tsibble(index = tahun, key = material)

# Split training = 5, test = 1 
train_ts <- yearly |>
group_by_key(material) |>
  slice(1:5)

test_ts <- yearly |>
  group_by_key(material) |>
  slice(6)

# Fitting Model ----
model_fit <- train_ts |>
  model(
    croston = CROSTON(n, type = "croston"),
    sba = CROSTON(n, type = "sba"),
    sbj = CROSTON(n, type = "sbj"),
    arima = ARIMA(n),
    ets = ETS(n)
  )

 # Filter material dengan nilai null pada semua model ----
not_null_model <- model_fit |>
  filter(!is_null_model(sba) | !is_null_model(sbj) | !is_null_model(croston) | 
           !is_null_model(arima) | is_null_model(ets))

# Menghitung Akurasi ----
akurasi <- forecast(not_null_model, h = 1) |>
  accuracy(test_ts) |>
  select(.model, material, RMSE:MAPE)

akurasi_summary <- akurasi |>
  na.omit() |>
  group_by(.model) |>
  summarise(RMSE_mean = mean(RMSE),
            MAPE_mean = mean(MAPE))

ggplot(akurasi_summary, aes(fct_reorder(.model, RMSE_mean), RMSE_mean)) +
  geom_col() +
  coord_flip()

# Menghitung nilai ramalan ----
peramalan <- forecast(not_null_model, h = 2)

# Tabel report ----
t1 <- peramalan |>
  as_tibble() |>
  filter(tahun == 2020) |>
  mutate(prediksi_2020 = paste0(.model, "_", as.character(tahun))) |>
  select(material, .mean, prediksi_2020) |>
  pivot_wider(names_from = prediksi_2020, values_from = .mean)

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

write_csv(performance_report, "performance_report.csv")


