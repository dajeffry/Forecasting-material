library(tidyverse)
library(readxl)

# Load and Cleansing ----
# Memuat dan memilih kolom yang relevan
dt <- read_excel("Data Kerja Praktik.XLSX", sheet = "FINAL", skip = 1) |>
  select("material" = `Short text`, "harga" = Harga, "tingkat_kekritisan" = `Tingkat kekritisan`, "rutin_nonrutin" = `Rutin/Non Rutin`, Jan:Des, `2014`:`2018`, `2019` = `2019...30`)

dt2 <- read_excel("Anlisa ABC SM2 2021 - lampiran.XLSX", sheet = "FINAL", skip = 2) |>
  select("material" = `Short text`, "harga" = Harga, "tingkat_kekritisan" = `Tingkat kekritisan`, "rutin_nonrutin" = `Rutin/Non Rutin`, Jan:Des, `2015`:`2020`)

list_clean <- lapply(list(dt, dt2), \(x){x |> filter(!is.na(material))})

# Cleansing
dt_prep <- list_clean[[1]] |> 
  inner_join(list_clean[[2]], by = c("material", "2015", "2016", "2017", "2018", "2019")) |> 
  select(material, ends_with(".y"), Jan.x:Des.x, `2014`:`2019`, `2020`) |> 
  group_by(material) |> 
  slice_min(order_by = harga.y, n = 1) |> 
  distinct(material, .keep_all = T)

dt_prep |> 
  count(material) |> 
  arrange(desc(n))

# Material yang tak memiliki pasangan
dt_dump <- list_clean[[1]] |> 
  anti_join(list_clean[[2]], by = c("material", "2015", "2016", "2017", "2018", "2019")) |> 
  select(material)
write_csv(dt_dump, "material_dump1.csv")

dt2_dump <- list_clean[[2]] |> 
  anti_join(list_clean[[1]], by = c("material", "2015", "2016", "2017", "2018", "2019")) |> 
  select(material)
write_csv(dt2_dump, "material_dump2.csv")

# Transformasi data tahunan untuk forecasting
yearly <- dt_prep |> 
  select(material, `2014`:`2020`) |>
  pivot_longer(`2014`:`2020`, names_to = "tahun", values_to = "n") |>
  mutate(tahun = as.integer(tahun)) |>
  arrange(tahun, material) |>
  as_tsibble(index = tahun, key = material)

# Transformasi data bulanan untuk forecasting
monthly <- dt_prep |> 
  select(material, Jan.x:Des.x, Jan.y:Des.y) |> 
  pivot_longer(Jan.x:Des.y, names_to = "bulan", values_to = "n") |>
  group_by(material) |> 
  mutate(
    no = row_number(),
    tanggal = if_else(no <= 12, paste0("2019-", no, "-01"), paste0("2020-", no-12, "-01")),
    tanggal = as_date(tanggal),
    tanggal = yearmonth(tanggal)
  ) |> 
  ungroup() |> 
  arrange(tanggal, material) |> 
  select(tanggal, material, n) |> 
  as_tsibble(index = tanggal, key = material)

# Data harga, tingkat kekritisan, dan rutin/non rutin material
dt_attr <- dt_prep |> 
  select(1, "harga" = 2, "tingkat_kekritisan" = 3, "rutin_nonrutin" = 4)
