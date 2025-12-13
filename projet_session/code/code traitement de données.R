
setwd("C:/Users/tom12/OneDrive - Université Laval/Université/Automne 2025/POL-6078/travail de session/Données")

library(tidyverse)
library(readr)

imports <- read_csv(
  "import-export-values_1950-2025.csv",
  skip = 8,
  show_col_types = FALSE
)

names(imports)

imports_2024 <- imports %>%
  select(
    Country = Recipient,   
    imports_2024 = `2024`  
  ) %>%
  mutate(
    imports_2024 = as.numeric(imports_2024)
  )

military <- read_delim(
  "SIPRI-Milex-data-1949-2025.csv",
  delim = ";",
  skip = 5,                        
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

names(military)

military_2024 <- military %>%
  select(
    Country,
    milex_2024 = `2024`
  ) %>%
  mutate(
    milex_2024 = parse_number(
      milex_2024,
      locale = locale(decimal_mark = ",")
    )
  )

data_2024 <- imports_2024 %>%
  inner_join(military_2024, by = "Country") %>%
  filter(!is.na(imports_2024), !is.na(milex_2024))

# Graphique
glimpse(data_2024)


ggplot(data_2024, aes(x = milex_2024, y = imports_2024)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Dépenses militaires et importations d'armes (2024)",
    x = "Dépenses militaires (millions US$, 2024)",
    y = "Importations d’armes (TIV, 2024)"
  )

model_2024 <- lm(imports_2024 ~ milex_2024, data = data_2024)
summary(model_2024)



