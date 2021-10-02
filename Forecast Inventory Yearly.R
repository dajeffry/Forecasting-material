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

# Iterasi forecast ----
# * Setting parameter forecast yearly
ntrain <- 4
npred <- 1
ntest <- nrow(yearly)-ntrain

train_index <- 1:ntrain
test_index <- (1+ntrain):nrow(yearly)
test_start <- 2020-ntest

# * set up vektor
mt <- c()
p1_2020 <- c()
p1_RMSE <- c()
p2_2020 <- c()
p2_RMSE <- c()

# * Iterasi
for (i in 2:ncol(yearly)) {
  ## * Partisi Data
  print(paste("set up data material", as.character(i)))
  
  data.ts <- ts(yearly[i], frequency = 1, start = 2014)
  train.data <- ts(yearly[train_index, i], frequency = 1, start = 2014)
  test.data <- ts(yearly[test_index, i], frequency = 1, start = test_start)
  
  ## * Fitting model ----
  print(paste("membuat model prediksi material", as.character(i)))
  
  fit1 <- auto.arima(train.data)
  fit2 <- ets(train.data)
  
  ## * Nilai prediksi ----
  print(paste("menghitung nilai prediksi material", as.character(i)))
  
  prediksi_1 <- forecast(fit1, npred+ntest)
  prediksi_2 <- forecast(fit2, npred+ntest)
  
  ## * Nilai akurasi ----
  print(paste("mengukur akurasi model prediksi material", as.character(i)))
  
  akurasi_1 <- prediksi_1 |>
    accuracy(test.data)
  akurasi_2 <- prediksi_2 |>
    accuracy(test.data)
  
  ## * Mengembalikan nilai prediksi dan akurasi ke vektor ----
  mt[length(mt)+1] <- colnames(yearly[i])
  p1_2020[length(p1_2020)+1] <- prediksi_1$mean[length(prediksi_1$mean)]
  p2_2020[length(p2_2020)+1] <- prediksi_2$mean[length(prediksi_1$mean)]
  p1_RMSE[length(p1_RMSE)+1] <- akurasi_1[4]
  p2_RMSE[length(p2_RMSE)+1] <- akurasi_2[4]
}

# Output ----
# * Bind hasil ----
df_prediksi <- tibble(mt, p1_2020, p1_RMSE, p2_2020, p2_RMSE)

rm(p1_2020)
rm(p1_RMSE)
rm(p2_2020)
rm(p2_RMSE)

# * Cetak output ----
write_csv(df_prediksi, "prediksi inventory.csv")
