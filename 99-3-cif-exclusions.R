library(RSelenium)
library(rvest)
library(janitor)
library(dplyr)
library(readxl)
library(stringr)
library(readr)

rmDr <- rsDriver(port = 4444L, browser = "firefox")

client <- rmDr$client

Y <- 1989:2004

url <- glue::glue("https://comtrade.un.org/db/mr/daExpNoteDetail.aspx?")
client$navigate(url)

html <- client$getPageSource()[[1]]

countries <- read_html(html) %>%
  html_element(css = "select#cR_ddlR.InputText") %>%
  html_nodes("option") %>%
  html_text()

ids <- read_html(html) %>%
  html_element(css = "select#cR_ddlR.InputText") %>%
  html_nodes("option") %>%
  html_attr("value")

countries <- tibble(
  country = countries,
  country_id = ids
)

R <- str_split(countries$country_id, ",")
R <- as.integer(unique(unlist(R)))
R <- R[!is.na(R)]

try(dir.create("temp"))

for (y in Y) {
  message(y)
  fout <- glue::glue("temp/{y}.csv")

  if (!file.exists(fout)) {
    final_table <- data.frame()

    for (r in R) {
      # explore https://comtrade.un.org/db/mr/daExpNoteDetail.aspx in Firefox
      # javascript:__doPostBack('dgXPNotes$ctl24$ctl01','')

      url <- glue::glue("https://comtrade.un.org/db/mr/daExpNoteDetail.aspx?y={y}&r={r}")
      client$navigate(url)

      # i <- stringr::str_pad(i, 2, "left", "0")
      # client$executeScript(glue::glue("__doPostBack('dgXPNotes$ctl24$ctl{i}','')"))

      html <- client$getPageSource()[[1]]

      table <- read_html(html) %>%
        html_element(css = "table#dgXPNotes") %>%
        html_table(header = T) %>%
        clean_names() %>%
        filter(reporter != "1")

      table$r <- r
      table$y <- y

      final_table <- rbind(final_table, table)
    }

    readr::write_csv(final_table, fout)
    rm(final_table); gc()
  }
}

exclusions <- purrr::map_df(
  list.files("temp", full.names = T),
  function(x) {
    readr::read_csv(x)
  }
)

exclusions <- exclusions %>%
  mutate(reporter = gsub("\\(.*", "", reporter)) %>%
  rename(
    uncomtrade_id = r,
    year = y
  ) %>%
  select(year, reporter, everything())

readr::write_csv(exclusions, "trade_reporting_systems_per_country.csv")

# This is what BACI mentions, but there are more cases not reporting imports as CIF
# not_cif <- c(
#   "dza", "geo",
#   # sacu
#   "zaf", "bwa", "lso", "nam", "swz"
# )

load("~/UN ESCAP/comtrade-codes/01-2-tidy-country-data/country-codes.RData")

exclusions <- exclusions %>%
  filter(trade_flow == "Import", valuation != "CIF") %>%
  left_join(country_codes %>%
              select(reporter = country_name_english, iso3_digit_alpha),
            by = "reporter")

exclusions <- exclusions %>%
  mutate(
    iso3_digit_alpha = tolower(iso3_digit_alpha),
    iso3_digit_alpha = fix_iso_codes(iso3_digit_alpha)
  )
