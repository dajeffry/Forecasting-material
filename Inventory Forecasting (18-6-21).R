library(tidyverse)
library(readxl)
library(fpp3)
library(tsibble)

# Load and Transform ----
dt <- read_excel("Data Kerja Praktik.XLSX", sheet = "FINAL", skip = 1) %>%
  select("material" = `Short text`, Jan:Des, `2014`:`2018`, `2019` = `2019...30`, 32)
dt <- dt[2:(nrow(dt)-2),]

dt <- dt %>%
  filter(`Rutin/Non Rutin`=="RUTIN")

is.na(dt)

yearly <- dt %>%
  select(material, `2014`:`2019`) %>%
  pivot_longer(`2014`:`2019`, names_to = "tahun", values_to = "n") %>%
  mutate(tahun = as.integer(tahun)) %>%
  arrange(tahun, material) %>%
  distinct(tahun, material, .keep_all = T) %>%
  as_tsibble(index = tahun, key = material)

yearly %>%
  ggplot(aes(x = tahun, y = n, color = material)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ material)
  labs(x = "Year", y = "Material",
       title = "Demand year by year")



# Split training = 5, test = 1 
train_ts <- yearly %>%
  group_by_key(material) %>%
  slice(1:5)

test_ts <- yearly %>%
  group_by_key(material) %>%
  slice(6)

# Fitting Model ----
model_fit <- train_ts %>%
  model(
    arima = ARIMA(n),
    ets = ETS(n)
  )

# Filter material dengan nilai null pada semua model ----
not_null_model <- model_fit %>%
  filter(!is_null_model(arima) | is_null_model(ets))

# Menghitung Akurasi ----
akurasi <- forecast(not_null_model, h = 1) %>%
  accuracy(test_ts) %>%
  select(.model, material, RMSE)

akurasi_summary <- akurasi %>%
  na.omit() %>%
  group_by(.model) %>%
  summarise(RMSE_mean = mean(RMSE))

ggplot(akurasi_summary, aes(fct_reorder(.model, RMSE_mean), RMSE_mean)) +
  geom_col() +
  coord_flip()

# Menghitung nilai ramalan ----
peramalan <- forecast(not_null_model, h = 2)

# Tabel report ----
t1 <- peramalan %>%
  as_tibble() %>%
  filter(tahun == 2020) %>%
  mutate(prediksi_2020 = paste0(.model, "_", as.character(tahun))) %>%
  select(material, .mean, prediksi_2020) %>%
  pivot_wider(names_from = prediksi_2020, values_from = .mean)

t2 <- akurasi %>%
  select(.model, material, RMSE) %>%
  pivot_longer(RMSE, names_to = "matriks", values_to = "nilai_akurasi") %>%
  mutate(model_akurasi = paste0(.model, "_", matriks)) %>%
  select(material, nilai_akurasi, model_akurasi) %>%
  pivot_wider(names_from = model_akurasi, values_from = nilai_akurasi)

rutin <- dt %>%
  select(material, "jenis" = `Rutin/Non Rutin`) %>%
  distinct()

performance_report <- left_join(t1, t2) %>%
  left_join(rutin) %>%
  select(material, jenis, starts_with("croston"), starts_with("sba"), 
         starts_with("sbj"), starts_with("arima"), starts_with("ets"))

write_csv(performance_report, "performance_report.csv")
