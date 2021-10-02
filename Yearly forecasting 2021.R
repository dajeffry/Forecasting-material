library(tidyverse)
library(fpp3)

# Split training = 6, test = 1 ----
train_ts <- yearly %>%
  group_by_key(material) %>%
  slice(1:6)

test_ts <- yearly %>%
  group_by_key(material) %>%
  slice(7)

# Fitting Model ----
model_fit <- train_ts %>%
  model(
    arima = ARIMA(n),
    ets = ETS(n)
  )

# Menghitung Akurasi ----
akurasi <- forecast(model_fit, h = 2) %>%
  accuracy(test_ts) %>%
  select(.model, material, RMSE:MAPE)

akurasi_summary <- akurasi %>%
  na.omit() %>%
  filter(is.finite(MAPE)) %>% 
  group_by(.model) %>%
  summarise(
    RMSE_mean = mean(RMSE),
    MAPE_mean = mean(MAPE)
  ) %>% 
  pivot_longer(ends_with("mean"), names_to = "matriks_akurasi", values_to = "nilai_akurasi")

ggplot(akurasi_summary, aes(.model, nilai_akurasi, fill = matriks_akurasi)) +
  geom_col(position = "dodge")

akurasi_summary <- akurasi %>%
  na.omit() %>%
  group_by(.model) %>%
  summarise(RMSE_mean = mean(RMSE))

ggplot(akurasi_summary, aes(.model, RMSE_mean)) +
  geom_col()

# Menghitung nilai ramalan ----
peramalan <- forecast(model_fit, h = 2)

# Tabel report ----
t1 <- peramalan %>%
  as_tibble() %>%
  filter(tahun == 2021) %>%
  mutate(prediksi_2021 = paste0(.model, "_", as.character(tahun))) %>%
  select(material, .mean, prediksi_2021) %>%
  pivot_wider(names_from = prediksi_2021, values_from = .mean)

t2 <- akurasi %>%
  select(.model, material, RMSE) %>%
  pivot_longer(RMSE, names_to = "matriks", values_to = "nilai_akurasi") %>%
  mutate(model_akurasi = paste0(.model, "_", matriks)) %>%
  select(material, nilai_akurasi, model_akurasi) %>%
  pivot_wider(names_from = model_akurasi, values_from = nilai_akurasi)

performance_report <- left_join(t1, t2) %>%
  left_join(dt_attr) %>%
  select(material, harga, tingkat_kekritisan, rutin_nonrutin, 
         starts_with("arima"), starts_with("ets"))

write_csv(performance_report, "performance_report_2021.csv")
