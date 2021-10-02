library(tidyverse)
library(fpp3)

# Split training = 6, test = 1 ----
train_ts <- yearly %>%
  group_by_key(material) %>%
  slice(1:6)
test_ts <- yearly %>%
  group_by_key(material) %>%
  slice(7)

train_ts
test_ts

# Fitting Model ----
model_fit <- train_ts %>%
  model(
    croston = CROSTON(n, type = "croston"),
    sba = CROSTON(n, type = "sba"),
    sbj = CROSTON(n, type = "sbj"),
    arima = ARIMA(n),
    ets = ETS(n)
  )

model_fit

# Menghitung Akurasi ----
akurasi <- forecast(model_fit, h = 2) %>%
  accuracy(test_ts) %>%
  select(.model, material, RMSE)

akurasi_summary <- akurasi |>
  na.omit() |>
  group_by(.model) |>
  summarise(RMSE_mean = mean(RMSE))

akurasi_summary <- akurasi %>%
  na.omit() %>%
  filter(is.finite(MAPE)) %>% 
  group_by(.model) %>%
  summarise(
    ME_mean = mean(ME),
    MSE_mean = mean(MSE),
    RMSE_mean = mean(RMSE),
    MAPE_mean = mean(MAPE)
  ) %>% 
  pivot_longer(ends_with("mean"), names_to = "matriks_akurasi", values_to = "nilai_akurasi")

####################################################################################################################
####################################################################################################################

dt_prep %>%
  na.omit() %>%
  select(material, ROQ.y) %>%
  pivot_longer(ROQ.y, names_to = ".model", values_to = "RMSE") %>%
  bind_rows(akurasi) %>%
  group_by(.model) %>%
  summarise(mean(RMSE))
is.na(dt_prep$ROQ.y)
write_csv(dt_prep, "dt_prep.csv")
dt_prep2 <- read_excel("dt_prep.xlsx")


akurasi_summary <- akurasi %>%
  na.omit() %>%
  group_by(.model) %>%
  summarise(RMSE_mean = mean(RMSE))

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

# masukin 2 model dan memilih model yang terbaik

akurasi2 <- dt_prep %>%
  select(material, ROQ.y) %>%
  na.omit() %>%
  pivot_longer(ROQ.y, names_to = ".model", values_to = "RMSE") %>%
  bind_rows(akurasi) %>%
  group_by(.model) %>%
  summarise(mean(RMSE))

akurasi3 <- akurasi2 %>%
  group_by(material) %>%
  slice_min(order_by = RMSE, n = 1) 

akurasi2 %>%
  group_by(material) %>%
  slice_min(order_by = RMSE, n = 1) %>%
  ungroup() %>%
  distinct(material, .keep_all = TRUE) %>%
  summarise(mean(RMSE))
  
metode_optimal <- akurasi2 %>%
  group_by(material) %>%
  slice_min(order_by = RMSE, n = 1) %>%
  ungroup() %>%
  distinct(material, .keep_all = TRUE) 
  #summarise(mean(RMSE))
mean(metode_optimal$RMSE)
table(metode_optimal$.model)

nilai_3_peramalan <- dt_prep %>%
  select(material, ROQ.y) %>%
  left_join(t1)

nilai_3_peramalan %>%
  pivot_longer(cols = 2:4, names_to = ".model", values_to = "nilai_prediksi") %>%
  right_join(metode_optimal, by = c("material", ".model")) %>%
  ungroup() %>%
  arrange(.model)
  
result_forecast <- nilai_3_peramalan %>%
  pivot_longer(cols = 2:4, names_to = ".model", values_to = "nilai_prediksi") %>%
  mutate(.model = str_remove(.model, "_2021")) %>%
  right_join(metode_optimal, by = c("material", ".model")) %>%
  ungroup() %>%
  arrange(.model)
#summary()

sum(result_forecast$nilai_prediksi)
table(result_forecast$.model)
mean(result_forecast$RMSE)

result_forecast2 <- result_forecast %>%
  left_join(dt_attr) %>%
  select(material, harga, tingkat_kekritisan, ROQ, rutin_nonrutin, `2020`,
         starts_with(".model"), starts_with("nilai_prediksi"))

result_forecast2 <- result_forecast2 %>%
  mutate(ROQ =
           replace(ROQ,
                   is.na(result_forecast2$ROQ),
                   "0"))
result_forecast2$ROQ <- as.numeric(result_forecast2$ROQ)

sum(result_forecast2$ROQ)
sum(result_forecast2$nilai_prediksi)

mean(result_forecast2$nilai_prediksi)
write_csv(result_forecast2, "Result Forecast4.csv")

summary(result_forecast2)


sum(result_forecast2$nilai_prediksi)
sum(result_forecast2$ROQ)
view(result_forecast2)

write_csv(result_forecast2, "Result Forecast2.csv")


######################################################################################################
#################################################################################################
#Statistik
table(metode_optimal$.model)
tidy(model_fit)
glance(model_fit)


sum(nilai_3_peramalan$ets_2021)

ggplot(akurasi_optimal, aes(.model, RMSE)) +
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
  select(material, harga, tingkat_kekritisan, rutin_nonrutin, ROQ, `2020`,
         starts_with("arima"), starts_with("ets"))

performance_report <- na.omit(performance_report)

write_csv(performance_report, "Hasil 1477 data.csv")

glimpse(performance_report)
summarise(mean$arima_RMSE)
mean(performance_report$arima_RMSE)
mean(performance_report$ets_RMSE)
