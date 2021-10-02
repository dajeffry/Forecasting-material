library(ggplot2)
library(readxl)
# Load and Cleansing ----
# Memuat dan memilih kolom yang relevan
dt <- read_excel("Data Kerja Praktik.XLSX", sheet = "FINAL", skip = 1) %>%
  select("material" = `Short text`, "harga" = Harga, "tingkat_kekritisan" = `Tingkat kekritisan`, "rutin_nonrutin" = `Rutin/Non Rutin`, Jan:Des, `2014`:`2018`, `2019` = `2019...30`, "ROQ" = ROQ)

dt2 <- read_excel("Anlisa ABC SM2 2021 - lampiran.XLSX", sheet = "FINAL", skip = 2) %>%
  select("material" = `Short text`, "harga" = Harga, "tingkat_kekritisan" = `Tingkat kekritisan`, "rutin_nonrutin" = `Rutin/Non Rutin`, Jan:Des, `2015`:`2020`, "ROQ" = ROQ)

dt$tingkat_kekritisan <- as.factor(dt$tingkat_kekritisan)
dt$rutin_nonrutin <- as.factor(dt$rutin_nonrutin)
dt2$tingkat_kekritisan <- as.factor(dt2$tingkat_kekritisan)
dt2$rutin_nonrutin <- as.factor(dt2$rutin_nonrutin)


dt$tingkat_kekritisan[dt$tingkat_kekritisan == "c"]
dt <- dt %>%
  mutate(tingkat_kekritisan =
           replace(tingkat_kekritisan,
                   tingkat_kekritisan == "c",
                   "C"))

dt2$tingkat_kekritisan[dt2$tingkat_kekritisan == "c"]
dt2 <- dt2 %>%
  mutate(tingkat_kekritisan =
           replace(tingkat_kekritisan,
                   tingkat_kekritisan == "c",
                   "C"))

# EDA
p1a <- ggplot(data=dt, aes(x = tingkat_kekritisan)) +
  geom_bar(fill = rainbow(4),
           color = "blue")+
  stat_count(geom = "text", color = "black", size = 5,
             aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  labs(title = "Data 2019")
p1b <- ggplot(data=dt2, aes(x = tingkat_kekritisan)) +
  geom_bar(fill = rainbow(4),
           color = "blue")+
  stat_count(geom = "text", color = "black", size = 5,
             aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  labs(title = "Data 2020")

library(flexclust)
library(gridExtra)
plot_klas <- grid.arrange(p1a, p1b, ncol = 2, 
                          top=textGrob("Klasifikasi Material",gp=gpar(fontsize=30,font=3)))

p2a <- ggplot(data=dt, aes(x = rutin_nonrutin)) +
  geom_bar(fill = rainbow(3),
           color = "blue")+
  stat_count(geom = "text", color = "black", size = 5,
             aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  labs(title = "Data 2019")

p2b <- ggplot(data=dt2, aes(x = rutin_nonrutin)) +
  geom_bar(fill = rainbow(3),
           color = "blue")+
  stat_count(geom = "text", color = "black", size = 5,
             aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  labs(title = "Data 2020")

install.packages("gridExtra")
library(gridExtra)
library(flexclust)
plot_sifat <- grid.arrange(p2a, p2b, ncol = 2, 
                         top=textGrob("Sifat Material",gp=gpar(fontsize=30,font=3)))

p3a <- ggplot(data=dt_attr, aes(x = tingkat_kekritisan)) +
  geom_bar(fill = rainbow(4),
           color = "blue")+
  stat_count(geom = "text", color = "black", size = 5,
             aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  labs(title = "Klasifikasi Material")

p3b <- ggplot(data=dt_attr, aes(x = rutin_nonrutin)) +
  geom_bar(fill = rainbow(2),
           color = "blue")+
  stat_count(geom = "text", color = "black", size = 5,
             aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  labs(title = "Sifat Material")

plot_terbaru <- grid.arrange(p1a, p1b, p2a, p2b, p3a, p3b, ncol = 2, 
                             top=textGrob("Plot Data",gp=gpar(fontsize=30,font=3)))
