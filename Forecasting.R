library(tidyverse)
library(readxl)
library(fpp2)

# Load and Transform ----
dt <- read_excel("Data Kerja Praktik.XLSX", sheet = "FINAL", skip = 1) |>
  select("material" = `Short text`, Jan:Des, `2014`:`2018`, `2019` = `2019...30`)
dt <- dt[2:(nrow(dt)-2),]

yearly <- dt |>
  select(material, `2014`:`2019`) |>
  pivot_longer(`2014`:`2019`, names_to = "tahun", values_to = "n") |>
  group_by(material) |>
  mutate(id = row_number()) |>
  pivot_wider(names_from = material, values_from = n) |>
  na.omit() |>
  select(-id)
yearly
  
monthly19 <- dt |>
  select(1:13) |>
  pivot_longer(Jan:Des, names_to = "bulan", values_to = "n") |>
  group_by(material) |>
  mutate(id = row_number()) |>
  pivot_wider(names_from = material, values_from = n) |>
  na.omit() |>
  select(-id)
monthly19

# Setting parameter forecast yearly ----
ntrain <- 4
npred <- 1
ntest <- nrow(yearly)-ntrain

train_index <- 1:ntrain
test_index <- (1+ntrain):nrow(yearly)
test_start <- 2020-ntest

# Partisi Data
data.ts <- ts(yearly[2], frequency = 1, start = 2014)
train.data <- ts(yearly[train_index, 2], frequency = 1, start = 2014)
test.data <- ts(yearly[test_index, 2], frequency = 1, start = test_start)

# Fitting model ----
fit1 <- auto.arima(train.data)
summary(fit1)

fit2 <- ets(train.data)
summary(fit2)

# Nilai prediksi ----
prediksi_1 <- forecast(fit1, npred+ntest)
prediksi_2 <- forecast(fit2, npred+ntest)

# Nilai akurasi ----
akurasi_1 <- prediksi_1 |>
  accuracy(test.data)
akurasi_2 <- prediksi_2 |>
  accuracy(test.data)

## Mengembalikan nilai prediksi dan akurasi ke vector ----
# Membuat data framee
mt <- c()
p1_2020 <- c()
p1_RMSE <- c()
p2_2020 <- c()
p2_RMSE <- c()

# Akses poin prediksi dan akurasi
mt[length(mt)+1] <- colnames(yearly[2])
p1_2020[length(p1_2020)+1] <- prediksi_1$mean[length(prediksi_1$mean)]
p2_2020[length(p2_2020)+1] <- prediksi_2$mean[length(prediksi_1$mean)]
p1_RMSE[length(p1_RMSE)+1] <- akurasi_1[4]
p2_RMSE[length(p2_RMSE)+1] <- akurasi_2[4]

df_prediksi <- tibble(mt, p1_2020, p1_RMSE, p2_2020, p2_RMSE)

rm(p1_2020)
rm(p1_RMSE)
rm(p2_2020)
rm(p2_RMSE)