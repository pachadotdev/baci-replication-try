# Clear and join data ----

# 1) Select the trade flows for which there is a quantity available, expressed
# in kg by the importer and the exporter, and for which value is above 10$ and
# quantity above 2 tonnes.

# 2) Compute unit values as reported by the exporter (i.e. value reported by the
# exporter/quantity reported by the exporter) and as reported by the importer
# (i.e. value reported by the importer/quantity reported by the importer). This
# yields a ratio of unit values (unit value as reported by the importer/unit
# value as reported by the exporter) that reflects the CIF/FOB ratio ("CIF
# rate").

# This is done by using helper functions

raw_data <- "raw_dataset_1989_2004"
prop_data <- "remaining_no_of_rows_after_filtering.rds"

if (!file.exists(raw_data)) {
  try(dir.create(raw_data))

  d <- map(
    # 1989,
    1989:2004,
    function(y) {
      message(y)
      conciliate_flows(y)
    }
  )

  d2 <- map_df(
    seq_along(d),
    function(y) {
      tibble(
        year = d[[y]][[1]],
        prop_rows_after_filtering = d[[y]][[2]]
      )
    }
  )

  d <- map_df(seq_along(d), function(y) d[[y]][[3]])

  d %>%
    group_by(year) %>%
    write_dataset(raw_data)

  saveRDS(d2, prop_data)
} else {
  d <- arrow::open_dataset(raw_data, partitioning = "year") %>%
    collect() %>%
    mutate(year = as.factor(gsub(".*=", "", year))) %>%
    select(year, everything())

  d2 <- readRDS(prop_data)
}
