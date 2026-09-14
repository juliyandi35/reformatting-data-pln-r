library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

data <- read_excel('tl baru.xlsx',sheet = "Sheet1")
head(data)
data <- data.frame(data)

# Convert TGL.MOHON to Datetime
data$TGL.MOHON <- ymd_hms(data$TGL.MOHON)

# Extract year and month
data <- data %>%
  mutate(Year = year(TGL.MOHON),
         Month = month(TGL.MOHON))

# Handle NA values (replace with your preferred NA handling)
data$TARIF.LAMA[is.na(data$TARIF.LAMA)] <- 0
data$DAYA.LAMA[is.na(data$DAYA.LAMA)] <- 0
data

# Categorize transactions
data <- data %>%
  mutate(Category = case_when(
    substr(TARIF, 1, 1) == "R" & substr(TARIF, nchar(TARIF) - 1, nchar(TARIF)) == "MT" ~ "Rumah Tangga Token Non Subsidi",
    substr(TARIF, 1, 1) == "R" & substr(TARIF, nchar(TARIF), nchar(TARIF)) == "T" ~ "Rumah Tangga Token Subsidi",
    substr(TARIF, 1, 1) == "R" & substr(TARIF, nchar(TARIF), nchar(TARIF)) == "M" ~ "Rumah Tangga Listrik Mandiri",
    substr(TARIF, 1, 1) == "R" & grepl("\\d$", TARIF) ~ "Rumah Tangga Listrik Subsidi",
    substr(TARIF, 1, 1) == "B" & substr(TARIF, nchar(TARIF) - 1, nchar(TARIF)) == "MT" ~ "Bisnis Token Non Subsidi",
    substr(TARIF, 1, 1) == "B" & substr(TARIF, nchar(TARIF), nchar(TARIF)) == "T" ~ "Bisnis Token Subsidi",
    substr(TARIF, 1, 1) == "B" & substr(TARIF, nchar(TARIF), nchar(TARIF)) == "M" ~ "Bisnis Listrik Mandiri",
    substr(TARIF, 1, 1) == "B" & grepl("\\d$", TARIF) ~ "Bisnis Listrik Subsidi",
    substr(TARIF, 1, 1) == "S" & substr(TARIF, nchar(TARIF) - 1, nchar(TARIF)) == "MT" ~ "Sosial Token Non Subsidi",
    substr(TARIF, 1, 1) == "S" & substr(TARIF, nchar(TARIF), nchar(TARIF)) == "T" ~ "Sosial Token Subsidi",
    substr(TARIF, 1, 1) == "S" & substr(TARIF, nchar(TARIF), nchar(TARIF)) == "M" ~ "Sosial Listrik Mandiri",
    substr(TARIF, 1, 1) == "S" & grepl("\\d$", TARIF) ~ "Sosial Listrik Subsidi",
    TRUE ~ "Other"
  ))

#Filter data based on daya and jenis transaksi
data <- data %>% filter(between(as.numeric(DAYA), 450, 11000), JENIS.TRANSAKSI %in% c("PASANG BARU", "PERUBAHAN DAYA", "PENERANGAN SEMENTARA"))

# Count transactions by category, year, and month
transaction_counts <- data %>%
  group_by(Category, Year, Month) %>%
  summarize(Count = n())
transaction_counts_year <- data %>%
  group_by(Category, Year) %>%
  summarize(Count = n())
transaction_counts_month <- data %>%
  group_by(Category, Month) %>%
  summarize(Count = n())

print("Transaction Counts by Category:")
print(transaction_counts)
print("Transaction Counts by Category per Year:")
print(transaction_counts_year)
print("Transaction Counts by Category per Month:")
print(transaction_counts_month)
writexl::write_xlsx(transaction_counts,"Transaction Counts.xlsx")
writexl::write_xlsx(transaction_counts_year,"Transaction Counts per Year.xlsx")
writexl::write_xlsx(transaction_counts_month,"Transaction Counts per Month.xlsx")

# Count Pasang Baru transactions
pasang_baru <- data %>%
  filter(JENIS.TRANSAKSI == "PASANG BARU") %>%
  group_by(Year, Month) %>%
  summarize(Count = n())

# Count Penerangan Sementara transactions
penerangan_sementara <- data %>%
  filter(JENIS.TRANSAKSI == "PENERANGAN SEMENTARA") %>%
  group_by(Year, Month) %>%
  summarize(Count = n())

print("\nPasang Baru Counts:")
print(pasang_baru)
print("\nPenerangan Sementara Counts:")
print(penerangan_sementara)
writexl::write_xlsx(pasang_baru,"Pasang Baru.xlsx")
writexl::write_xlsx(penerangan_sementara,"Penerangan Sementara.xlsx")

# "DAYA LAMA-DAYA" Calculations
data <- data %>%
  mutate(
    SELISIH.DAYA = as.numeric(DAYA.LAMA) - as.numeric(DAYA)
  )

print("\nData with Differences:")
print(data)
data <- na.omit(data)
writexl::write_xlsx(data,"Data Final.xlsx")

# Example:  Assuming "ESDM" and "BPBL" are substrings in NAMA
esdm_counts <- data %>%
  mutate(HasESDM = grepl("ESDM", NAMA)) %>%
  group_by(Year) %>%
  summarize(ESDMCount = sum(HasESDM, na.rm = TRUE))

bpbl_counts <- data %>%
  mutate(HasBPBL = grepl("BPBL", ASAL.MOHON)) %>%
  group_by(Year) %>%
  summarize(BPBLCount = sum(HasBPBL, na.rm = TRUE))

print("\nESDM Counts:")
print(esdm_counts)
print("\nBPBL Counts:")
print(bpbl_counts)
writexl::write_xlsx(esdm_counts,"ESDM.xlsx")
writexl::write_xlsx(bpbl_counts,"BPBL.xlsx")

# Plotting (requires ggplot2)
# plot for transaction counts by category
transaction_counts <- na.omit(transaction_counts)
ggplot(transaction_counts, aes(x = as.factor(Month), y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  facet_wrap(~Year) +
  labs(title = "Transaction Counts by Category", x = "Month", y = "Count")

# Bar plot showing average DAYA LAMA-DAYA per month and year
ggplot(data, aes(x = as.factor(Month), y = SELISIH.DAYA, fill = as.factor(Year))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", na.rm = TRUE) +  # Show average
  labs(title = "Rata-rata Selisih Daya per Bulan dan Tahun", x = "Bulan", y = "Rata-rata Selisih", fill = "Tahun")
