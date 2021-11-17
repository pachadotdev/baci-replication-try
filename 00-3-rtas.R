source("99-1-pkgs.R")
source("99-2-clean-funs.R")

treaties_url <- "https://www.designoftradeagreements.org/media/filer_public/8d/56/8d56dfb3-5bcc-4e8d-8d2a-39bc8333155e/desta_list_of_treaties_02_00_dyads_data.csv"
treaties_csv <- gsub(".*/", "", treaties_url)
treaties_rds <- paste0("rtas/", gsub("csv", "rds", treaties_csv))

if (!file.exists(treaties_rds)) {
  download.file(treaties_url, treaties_csv)
  treaties <- readr::read_csv(treaties_csv)
  saveRDS(treaties, treaties_rds)
  file.remove(treaties_csv)
} else {
  treaties <- readRDS(treaties_rds)
}

withdrawals_url <- "https://www.designoftradeagreements.org/media/filer_public/26/5f/265f3079-f7c6-47fb-bf47-930aedee5ae1/desta_dyadic_withdrawal_02_00.csv"
withdrawals_csv <- gsub(".*/", "", withdrawals_url)
withdrawals_rds <- paste0("rtas/", gsub("csv", "rds", withdrawals_csv))

if (!file.exists(withdrawals_rds)) {
  download.file(withdrawals_url, withdrawals_csv)
  withdrawals <- readr::read_csv(withdrawals_csv)
  saveRDS(withdrawals, withdrawals_rds)
  file.remove(withdrawals_csv)
} else {
  withdrawals <- readRDS(withdrawals_rds)
}

unique(treaties$entry_type)

load("~/UN ESCAP/comtrade-codes/01-2-tidy-country-data/country-codes.RData")

treaties <- treaties %>%
  filter(entry_type == "base_treaty") %>%
  rowwise() %>%
  mutate(c1 = min(country1, country2), c2 = max(country1, country2)) %>%
  ungroup() %>%
  select(year, c1, c2, base_treaty) %>%
  distinct() %>%
  group_by(c1, c2, base_treaty) %>%
  summarise(year = min(year)) %>%
  mutate(rta = 1)

treaties <- treaties %>%
  mutate(
    c1 = str_trim(str_to_lower(iconv(c1, to = "ASCII//TRANSLIT", sub = ""))),
    c2 = str_trim(str_to_lower(iconv(c2, to = "ASCII//TRANSLIT", sub = "")))
  ) %>%
  left_join(
    geo_cepii %>%
      mutate_if(is.character, str_to_lower) %>%
      select(c1 = country, country1 = iso3) %>%
      distinct()
  ) %>%
  left_join(
    geo_cepii %>%
      mutate_if(is.character, str_to_lower) %>%
      select(c2 = country, country2 = iso3) %>%
      distinct()
  ) %>%
  ungroup() %>%
  mutate(
    country1 = case_when(
      c1 == "antigua & barbuda" ~ "atg",
      c1 == "belgium" ~ "bel",
      c1 == "bosnia & herzegovina" ~ "bih",
      c1 == "british indian ocean territory" ~ "iot",
      c1 == "brunei" ~ "brn",
      c1 == "congo - brazzaville" ~ "cog",
      c1 == "congo - kinshasa" ~ "cod",
      c1 == "czechia" ~ "cze",
      c1 == "cte divoire" ~ "civ",
      c1 == "french southern territories" ~ "atf",
      c1 == "hong kong sar china" ~ "hkg",
      c1 == "kazakhstan" ~ "kaz",
      c1 == "laos" ~ "lao",
      c1 == "libya" ~ "lby",
      c1 == "macau sar china" ~ "mac",
      c1 == "macedonia" ~ "mkd",
      c1 == "mayotte" ~ "myt",
      c1 == "moldova" ~ "mda",
      c1 == "montenegro" ~ "mne",
      c1 == "myanmar (burma)" ~ "mmr",
      c1 == "netherlands antilles" ~ "ant",
      c1 == "pitcairn islands" ~ "pcn",
      c1 == "north korea" ~ "pkr",
      c1 == "russia" ~ "rus",
      c1 == "serbia" ~ "srb",
      c1 == "south georgia & south sandwich islands" ~ "sgs",
      c1 == "south korea" ~ "kor",
      c1 == "st. helena" ~ "shn",
      c1 == "st. kitts & nevis" ~ "kna",
      c1 == "st. pierre & miquelon" ~ "spm",
      c1 == "st. lucia" ~ "lca",
      c1 == "st. vincent & grenadines" ~ "vct",
      c1 == "syria" ~ "syr",
      c1 == "so tom & prncipe" ~ "stp",
      c1 == "tanzania" ~ "tza",
      c1 == "trinidad & tobago" ~ "tto",
      c1 == "turks & caicos islands" ~ "tca",
      c1 == "united states" ~ "usa",
      c1 == "vietnam" ~ "vnm",
      c1 == "wallis & futuna" ~ "wlf",
      TRUE ~ country1
    ),
    country2 = case_when(
      c2 == "antigua & barbuda" ~ "atg",
      c2 == "belgium" ~ "bel",
      c2 == "bosnia & herzegovina" ~ "bih",
      c2 == "british indian ocean territory" ~ "iot",
      c2 == "brunei" ~ "brn",
      c2 == "congo - brazzaville" ~ "cog",
      c2 == "congo - kinshasa" ~ "cod",
      c2 == "czechia" ~ "cze",
      c2 == "cte divoire" ~ "civ",
      c2 == "french southern territories" ~ "atf",
      c2 == "hong kong sar china" ~ "hkg",
      c2 == "kazakhstan" ~ "kaz",
      c2 == "laos" ~ "lao",
      c2 == "libya" ~ "lby",
      c2 == "macau sar china" ~ "mac",
      c2 == "macedonia" ~ "mkd",
      c2 == "mayotte" ~ "myt",
      c2 == "moldova" ~ "mda",
      c2 == "montenegro" ~ "mne",
      c2 == "myanmar (burma)" ~ "mmr",
      c2 == "netherlands antilles" ~ "ant",
      c2 == "pitcairn islands" ~ "pcn",
      c2 == "north korea" ~ "pkr",
      c2 == "russia" ~ "rus",
      c2 == "serbia" ~ "srb",
      c2 == "south georgia & south sandwich islands" ~ "sgs",
      c2 == "south korea" ~ "kor",
      c2 == "st. helena" ~ "shn",
      c2 == "st. kitts & nevis" ~ "kna",
      c2 == "st. pierre & miquelon" ~ "spm",
      c2 == "st. lucia" ~ "lca",
      c2 == "st. vincent & grenadines" ~ "vct",
      c2 == "syria" ~ "syr",
      c2 == "so tom & prncipe" ~ "stp",
      c2 == "tanzania" ~ "tza",
      c2 == "trinidad & tobago" ~ "tto",
      c2 == "turks & caicos islands" ~ "tca",
      c2 == "united states" ~ "usa",
      c2 == "vietnam" ~ "vnm",
      c2 == "wallis & futuna" ~ "wlf",
      TRUE ~ country2
    )
  )

treaties %>%
  select(c1, country1) %>%
  filter(is.na(country1)) %>%
  distinct()

treaties %>%
  select(c2, country2) %>%
  filter(is.na(country2)) %>%
  distinct()

treaties <- treaties %>%
  select(year, country1, country2, base_treaty, rta) %>%
  drop_na()

withdrawals <- withdrawals %>%
  filter(entry_type == "withdrawal") %>%
  rowwise() %>%
  mutate(c1 = min(country1, country2), c2 = max(country1, country2)) %>%
  ungroup() %>%
  select(year, c1, c2, base_treaty) %>%
  distinct() %>%
  group_by(c1, c2, base_treaty) %>%
  summarise(year = min(year)) %>%
  mutate(rta = -1)

withdrawals <- withdrawals %>%
  mutate(
    c1 = str_trim(str_to_lower(iconv(c1, to = "ASCII//TRANSLIT", sub = ""))),
    c2 = str_trim(str_to_lower(iconv(c2, to = "ASCII//TRANSLIT", sub = "")))
  ) %>%
  left_join(
    geo_cepii %>%
      mutate_if(is.character, str_to_lower) %>%
      select(c1 = country, country1 = iso3) %>%
      distinct()
  ) %>%
  left_join(
    geo_cepii %>%
      mutate_if(is.character, str_to_lower) %>%
      select(c2 = country, country2 = iso3) %>%
      distinct()
  ) %>%
  ungroup() %>%
  mutate(
    country1 = case_when(
      c1 == "antigua & barbuda" ~ "atg",
      c1 == "belgium" ~ "bel",
      c1 == "bosnia & herzegovina" ~ "bih",
      c1 == "british indian ocean territory" ~ "iot",
      c1 == "brunei" ~ "brn",
      c1 == "congo - brazzaville" ~ "cog",
      c1 == "congo - kinshasa" ~ "cod",
      c1 == "czechia" ~ "cze",
      c1 == "cte divoire" ~ "civ",
      c1 == "french southern territories" ~ "atf",
      c1 == "hong kong sar china" ~ "hkg",
      c1 == "kazakhstan" ~ "kaz",
      c1 == "laos" ~ "lao",
      c1 == "libya" ~ "lby",
      c1 == "macau sar china" ~ "mac",
      c1 == "macedonia" ~ "mkd",
      c1 == "mayotte" ~ "myt",
      c1 == "moldova" ~ "mda",
      c1 == "montenegro" ~ "mne",
      c1 == "myanmar (burma)" ~ "mmr",
      c1 == "netherlands antilles" ~ "ant",
      c1 == "pitcairn islands" ~ "pcn",
      c1 == "north korea" ~ "pkr",
      c1 == "russia" ~ "rus",
      c1 == "serbia" ~ "srb",
      c1 == "south georgia & south sandwich islands" ~ "sgs",
      c1 == "south korea" ~ "kor",
      c1 == "st. helena" ~ "shn",
      c1 == "st. kitts & nevis" ~ "kna",
      c1 == "st. pierre & miquelon" ~ "spm",
      c1 == "st. lucia" ~ "lca",
      c1 == "st. vincent & grenadines" ~ "vct",
      c1 == "syria" ~ "syr",
      c1 == "so tom & prncipe" ~ "stp",
      c1 == "tanzania" ~ "tza",
      c1 == "trinidad & tobago" ~ "tto",
      c1 == "turks & caicos islands" ~ "tca",
      c1 == "united states" ~ "usa",
      c1 == "vietnam" ~ "vnm",
      c1 == "wallis & futuna" ~ "wlf",
      TRUE ~ country1
    ),
    country2 = case_when(
      c2 == "antigua & barbuda" ~ "atg",
      c2 == "belgium" ~ "bel",
      c2 == "bosnia & herzegovina" ~ "bih",
      c2 == "british indian ocean territory" ~ "iot",
      c2 == "brunei" ~ "brn",
      c2 == "congo - brazzaville" ~ "cog",
      c2 == "congo - kinshasa" ~ "cod",
      c2 == "czechia" ~ "cze",
      c2 == "cte divoire" ~ "civ",
      c2 == "french southern territories" ~ "atf",
      c2 == "hong kong sar china" ~ "hkg",
      c2 == "kazakhstan" ~ "kaz",
      c2 == "laos" ~ "lao",
      c2 == "libya" ~ "lby",
      c2 == "macau sar china" ~ "mac",
      c2 == "macedonia" ~ "mkd",
      c2 == "mayotte" ~ "myt",
      c2 == "moldova" ~ "mda",
      c2 == "montenegro" ~ "mne",
      c2 == "myanmar (burma)" ~ "mmr",
      c2 == "netherlands antilles" ~ "ant",
      c2 == "pitcairn islands" ~ "pcn",
      c2 == "north korea" ~ "pkr",
      c2 == "russia" ~ "rus",
      c2 == "serbia" ~ "srb",
      c2 == "south georgia & south sandwich islands" ~ "sgs",
      c2 == "south korea" ~ "kor",
      c2 == "st. helena" ~ "shn",
      c2 == "st. kitts & nevis" ~ "kna",
      c2 == "st. pierre & miquelon" ~ "spm",
      c2 == "st. lucia" ~ "lca",
      c2 == "st. vincent & grenadines" ~ "vct",
      c2 == "syria" ~ "syr",
      c2 == "so tom & prncipe" ~ "stp",
      c2 == "tanzania" ~ "tza",
      c2 == "trinidad & tobago" ~ "tto",
      c2 == "turks & caicos islands" ~ "tca",
      c2 == "united states" ~ "usa",
      c2 == "vietnam" ~ "vnm",
      c2 == "wallis & futuna" ~ "wlf",
      TRUE ~ country2
    )
  )

withdrawals %>%
  select(c1, country1) %>%
  filter(is.na(country1)) %>%
  distinct()

withdrawals %>%
  select(c2, country2) %>%
  filter(is.na(country2)) %>%
  distinct()

withdrawals <- withdrawals %>%
  select(year, country1, country2, base_treaty, rta) %>%
  drop_na()

d_rta <- crossing(
  year = min(treaties$year, withdrawals$year):max(treaties$year, withdrawals$year),
  treaties %>%
    select(country1, country2, base_treaty) %>%
    bind_rows(
      withdrawals %>% select(country1, country2, base_treaty)
    ) %>%
    distinct()
) %>%
  left_join(
    treaties %>%
      bind_rows(withdrawals)
  ) %>%
  arrange(year, country1, country2, base_treaty) %>%
  group_by(country1, country2, base_treaty) %>%
  fill(rta, .direction = "down") %>%
  ungroup()

d_rta <- d_rta %>%
  group_by(country1, country2, base_treaty) %>%
  # filter(country1 == "chl", country2 == "chn") %>%
  mutate(
    rta = ifelse(is.na(rta), 0, rta),
    rta = ifelse(is.na(lag(rta)), rta, cumsum(rta)),
    rta = case_when(
      is.na(lag(rta)) ~ rta,
      rta > lag(rta) & !is.na(lag(rta)) ~ 1,
      rta <= lag(rta) & !is.na(lag(rta)) ~ 0
    )
  )

d_rta <- d_rta %>% ungroup() %>% mutate_if(is.numeric, as.integer)

saveRDS(d_rta, file = "rtas/rtas_at_least_one_in_force_per_year.rds")
rm(treaties, withdrawals, country_codes)
gc()
