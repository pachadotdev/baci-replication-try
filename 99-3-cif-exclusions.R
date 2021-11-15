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

final_table <- purrr::map_df(
  list.files("temp", full.names = T),
  function(x) {
    readr::read_csv(x)
  }
)

final_table <- final_table %>%
  mutate(reporter = gsub("\\(.*", "", reporter)) %>%
  rename(
    uncomtrade_id = r,
    year = y
  ) %>%
  select(year, reporter, everything())

readr::write_csv(final_table, "trade_reporting_systems_per_country.csv")

# This is what BACI mentions, but there are more cases not reporting imports as CIF
# not_cif <- c(
#   "dza", "geo",
#   # sacu
#   "zaf", "bwa", "lso", "nam", "swz"
# )

final_table %>%
  filter(trade_flow == "Import", valuation != "CIF")
