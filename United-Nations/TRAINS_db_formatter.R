rm(list=ls())
library(tidyverse)
library(ggplot2)
setwd("set your wd here")

## NTM Measure HS Code Formatter

## This code automates the process of separating HS_code classifications for NTM (Non-Tariff Measure) data that includes both numeric and text representations. It serves three main purposes:

## 1. **Data Formatting**: It transforms the database to ensure that each row contains only one HS_code, simplifying data structure.

## 2. **HS Nomenclature Harmonization**: The code aligns the HS_code nomenclature with the HS6 classification. For instance, if an NTM measure is imposed on an HS2 classification (e.g., "01"), the code populates the database with all HS_codes starting with "01," providing a comprehensive view of affected products in 6-digit granularity.

## 3. **Enhanced Data Utility**: This formatting becomes invaluable when you need to merge, join, or search for specific items within the database, particularly when working with unique identifiers like "tickers."

# By using this code, you can efficiently manage and analyze NTM data, ensuring consistent and precise results while saving valuable research time.

# ---
  
#  *Note: Replace 'NTM Measure' with the relevant dataset or use case, if applicable.*
  
  # Read the Excel files: go on TRAINS select whatever you want and download it. It works both for IMPORT and EXPORT
  ## NTM data
  ntm_data <- readxl::read_excel("NTM_Downloads here in xlsx", skip = 8)
## Import ticker data for green goods: the example here shows that you can gather tickers for specific goods that are classified as "green"
green_goods_tickers <- readxl::read_excel("Environmental_Goods_Harmonized_System_Codes.xlsx", skip = 1)
green_goods_tickers <- green_goods_tickers[, 1]  # Select the first column
colnames(green_goods_tickers) <- "tickers"  # Rename the column to "tickers"
## Import HS6 codes all: you can download it here https://wits.worldbank.org/trade/country-byhs6product.aspx?lang=en
hs_code <- readxl::read_xls("HSProducts.xls", sheet = "HS Nomenclature")
hs_code <- hs_code[grepl("^[0-9]{6}$", hs_code$ProductCode), ]

## 1. Extract each code from NTM data that relies on more than one category
# Rename the 'HS code' column to 'HS_Code'
ntm_data <- ntm_data %>% rename(HS_Code = `HS code`)
# Separate rows with multiple HS_Codes
ntm_data_long <- ntm_data %>%
  separate_rows(HS_Code, sep = ',\\s*') %>%
  mutate(HS_Code = str_trim(HS_Code))
# Extract and clean the 'code' column
ntm_data_long$code <- str_extract(ntm_data_long$HS_Code, "\\d+")
# Filter out rows with empty or non-numeric 'code'
ntm_data_long <- ntm_data_long %>%
  filter(!is.na(code) & code != "")

## Trim the last 2 digits of 'code'
ntm_data_long$code <- sapply(ntm_data_long$code, function(x) {
  if (nchar(x) <= 6) {
    return(x)
  } else {
    return(substr(x, 1, 6))
  }
})

############### Create a function to check matching codes #############

check_matching_codes <- function(code) {
  if (nchar(code) < 6) {
    matching_tickers <- hs_code$ProductCode[grepl(paste0("^", code), hs_code$ProductCode)]
    if (length(matching_tickers) > 0) {
      return(paste(matching_tickers, collapse = ", "))
    } else {
      return(NA)
    }
  } else {
    return(code)
  }
}
#######################################################################
# Use the function to create the 'matching_code' column
ntm_data_long <- ntm_data_long %>%
  mutate(matching_code = sapply(code, check_matching_codes))
ntm_data_all <- ntm_data_long %>% separate_rows(matching_code, sep = ",\\s*")
ntm_data_filtered <- ntm_data_all[!is.na(ntm_data_all$code), ]

### Now we have the database with all HS6 digits uniform, so we can look for green
## goods according to the list of tickers. We will use a 0 1 variable

ntm_data_filtered$green <- lapply(ntm_data_filtered$matching_code, function(x) if (x %in% green_goods_tickers$tickers) 1 else 0)
ntm_data_filtered <- ntm_data_filtered %>% select("Implementation date", "Country imposing NTM(s)", "NTM code", "matching_code", "green")

# Rename columns
colnames(ntm_data_filtered) <- c("date", "country_imposer", "NTM_code", "HS_code", "green")

# Convert date to a suitable format
ntm_data_filtered$date <- as.Date(ntm_data_filtered$date)
ntm_data_filtered$date <- format(ntm_data_filtered$date, "%Y")

# Ensure that the "green" column is numeric (convert if necessary)
ntm_data_filtered$green <- as.numeric(ntm_data_filtered$green)

# Filter the data for the desired date range
ntm_data_filtered <- ntm_data_filtered %>% filter(ntm_data_filtered$date >= 2013 & ntm_data_filtered$date < 2023)