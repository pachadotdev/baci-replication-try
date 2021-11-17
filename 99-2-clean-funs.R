filter_flow <- function(d, y, f) {
  d %>%
    filter(
      year == y,
      trade_flow == f,
      aggregate_level == 6
    ) %>%
    filter(
      !reporter_iso %in% c("wld", "0-unspecified"),
      !partner_iso %in% c("wld", "0-unspecified")
    ) %>%
    select(year, reporter_iso, partner_iso, commodity_code, trade_value_usd,
           qty_unit, qty) %>%
    collect()
}

compute_ratios <- function(dexp) {
  dexp %>%

    mutate(
      trade_value_usd_reexp = ifelse(is.na(trade_value_usd_reexp), 0, trade_value_usd_reexp),
      trade_value_usd_reimp = ifelse(is.na(trade_value_usd_reimp), 0, trade_value_usd_reimp),
      qty_reexp = ifelse(is.na(qty_reexp), 0, qty_reexp),
      qty_reimp = ifelse(is.na(qty_reimp), 0, qty_reimp),
    ) %>%

    rowwise() %>%
    mutate(
      trade_value_usd_netexp = max(trade_value_usd_exp - trade_value_usd_reexp, 0, na.rm = T),
      trade_value_usd_netimp = max(trade_value_usd_imp - trade_value_usd_reimp, 0, na.rm = T),
      qty_netexp = max(qty_exp - qty_reexp, 0, na.rm = T),
      qty_netimp = max(qty_imp - qty_reimp, 0, na.rm = T)
    ) %>%
    ungroup() %>%

    # value reported by the exporter / quantity reported by the exporter +
    # value reported by the importer/quantity reported by the importer)
    # This yields a ratio of unit values
    # unit value as reported by the importer/unit value as reported by the
    # exporter) that reflects the CIF/FOB ratio ("CIF rate").
    mutate(
      unit_value_exp = (trade_value_usd_exp / qty_exp),
      unit_value_imp = (trade_value_usd_imp / qty_imp),

      unit_value_netexp = (trade_value_usd_netexp / qty_netexp),
      unit_value_netimp = (trade_value_usd_netimp / qty_netimp)
    ) %>%

    # Create UV explanatory variable
    # SEE PAGE 14 IN THE ARTICLE
    group_by(commodity_code) %>%
    mutate(
      uv_exp = median(unit_value_exp, na.rm = T),
      uv_netexp = median(unit_value_netexp, na.rm = T)
    ) %>%
    ungroup() %>%

    group_by(commodity_code) %>%
    mutate(
      uv_imp = median(unit_value_imp, na.rm = T),
      uv_netimp = median(unit_value_netimp, na.rm = T)
    ) %>%
    ungroup() %>%

    mutate(
      cif_fob_unit_ratio = unit_value_imp / unit_value_exp,
      cif_fob_ratio = trade_value_usd_imp / trade_value_usd_exp,

      cif_fob_unit_ratio_net = unit_value_netimp / unit_value_netexp,
      cif_fob_ratio_net = trade_value_usd_netimp / trade_value_usd_netexp
    ) %>%

    # The unit or net CIF/FOB ratios can be weighted, or not, by the inverse of
    # the gap between reported mirror quantities Min(Qxij,Qmji) / Max(Qxij,Qmji)
    # SEE PAGE 15 IN THE ARTICLE
    rowwise() %>%
    mutate(
      cif_fob_weights = 1 /
        (max(qty_imp, qty_exp, na.rm = T) / min(qty_imp, qty_exp, na.rm = T)),

      cif_fob_weights_net = 1 /
        (max(qty_netimp, qty_netexp, na.rm = T) / min(qty_netimp, qty_netexp, na.rm = T))
    ) %>%
    ungroup()
}

# clear_hive <- function(x) {
#   gsub(".*=", "", x)
# }

data_partitioned <- function() {
  open_dataset("../uncomtrade-datasets-arrow/hs-rev1992/parquet",
               partitioning = c("aggregate_level", "trade_flow",
                                "year", "reporter_iso")) %>%
    # need to pass gsub one by one bc arrow translation won't accept the
    # clear_hive function above
    mutate(
      aggregate_level = as.integer(gsub(".*=", "", aggregate_level)),
      trade_flow = gsub(".*=", "", trade_flow),
      year = as.integer(gsub(".*=", "", year)),
      reporter_iso = gsub(".*=", "", reporter_iso)
    )
}

join_flows <- function(dexp, dimp, dreexp, dreimp) {
  dexp %>%
    full_join(dimp, by = c("reporter_iso" = "partner_iso",
                           "partner_iso" = "reporter_iso",
                           "commodity_code")) %>%
    left_join(dreexp, by = c("reporter_iso",
                           "partner_iso",
                           "commodity_code")) %>%
    left_join(dreimp, by = c("reporter_iso" = "partner_iso",
                           "partner_iso" = "reporter_iso",
                           "commodity_code")) %>%
    rename(
      trade_value_usd_exp = trade_value_usd.x,
      trade_value_usd_imp = trade_value_usd.y,
      trade_value_usd_reexp = trade_value_usd.x.x,
      trade_value_usd_reimp = trade_value_usd.y.y,
      qty_unit_exp = qty_unit.x,
      qty_unit_imp = qty_unit.y,
      qty_unit_reexp = qty_unit.x.x,
      qty_unit_reimp = qty_unit.y.y,
      qty_exp = qty.x,
      qty_imp = qty.y,
      qty_reexp = qty.x.x,
      qty_reimp = qty.y.y
    ) %>%
    mutate(
      reported_by = case_when(
        !is.na(trade_value_usd_exp) &
          !is.na(trade_value_usd_imp) ~ "both parties",
        !is.na(trade_value_usd_exp) &
          is.na(trade_value_usd_imp) ~ "exporter only",
        is.na(trade_value_usd_exp) &
          !is.na(trade_value_usd_imp) ~ "importer only"
      ),
      reported_by = as_factor(reported_by)
    )
}

# ton <- 1016.0469 # kg, UK
# ton <- 907.18474 kg # kg, US
# ton <- 1000 # kg, what BACI uses

filter_kg <- function(dexp, ton = 1000) {
  dexp %>%
    filter(
      qty_unit_exp == "weight in kilograms" &
        qty_unit_imp == "weight in kilograms",
      trade_value_usd_exp > 10 & trade_value_usd_imp > 10,
      qty_exp > 2 * ton & qty_imp > 2 * ton
    )
}

conciliate_flows <- function(y) {
  # read ----
  dexp <- data_partitioned() %>%
    filter_flow(y = y, f = "export")

  dimp <- data_partitioned() %>%
    filter_flow(y = y, f = "import") %>%
    select(-year)

  dreexp <- data_partitioned() %>%
    filter_flow(y = y, f = "re-export") %>%
    select(-year)

  dreimp <- data_partitioned() %>%
    filter_flow(y = y, f = "re-import") %>%
    select(-year)

  # join mirrored flows ----

  # we do a full join, so all trade flows reported either by the importer or the
  # exporter are present in our intermediary datasets
  dexp <- join_flows(dexp, dimp, dreexp, dreimp)
  rm(dimp, dreexp, dreimp); gc()

  # work on obs. with kilograms ----

  # NO NEED TO RUN THIS COMMENTED SECTION, IT WAS RUN ONCE TO EXTRACT THE UNIQUE
  # CASES
  # units <- purrr::map_df(
  #   1988:2019,
  #   function(t) {
  #     open_dataset("../uncomtrade-datasets-arrow/hs-rev1992/parquet",
  #       partitioning = c("year", "trade_flow", "reporter_iso")) %>%
  #       filter(
  #         year == t,
  #         trade_flow == "import",
  #         aggregate_level == 4) %>%
  #       select(qty_unit) %>%
  #       collect() %>%
  #       distinct()
  #   }
  # )
  #
  # units <- units %>% distinct() %>% pull()
  #
  # [1]  "no quantity"
  # "number of items"
  # [3]  "number of pairs"
  # "area in square metres"
  # [5]  "weight in kilograms"
  # "length in metres"
  # [7]  "volume in litres"
  # "electrical energy in thousands of kilowatt-hours"
  # [9]  "weight in carats"
  # "volume in cubic meters"
  # [11] "thousands of items"
  # "dozen of items"
  # [13] "number of packages"

  # 1) Select the trade flows for which there is a quantity available,
  # expressed in kg by the importer and the exporter, and for which value is
  # above 10$ and quantity above 2 tonnes.

  n <- nrow(dexp)

  dexp <- filter_kg(dexp)

  # obtain unit values ----

  # 2) Compute unit values as reported by the exporter
  # See the comments in the helper function

  dexp <- dexp %>%
    compute_ratios() %>%
    select(year, reporter_iso, partner_iso, commodity_code,
           starts_with("uv_"), starts_with("cif_fob_"), reported_by) %>%
    mutate(year = as_factor(year))

  p <- nrow(dexp) / n
  message(paste("Proportion of filtered rows / total rows", p))

  return(list(
    y,
    p,
    dexp %>%
      add_gravity_cols() %>%
      mutate(
        contig = as.integer(contig),
        landlocked_reporter = as.integer(landlocked_reporter),
        landlocked_partner = as.integer(landlocked_partner)
      ) %>%
      rename(
        exporter_iso = reporter_iso,
        importer_iso = partner_iso
      )
  ))
}

fix_iso_codes <- function(val) {
  case_when(
    val == "rom" ~ "rou", # Romania
    val == "yug" ~ "scg", # Just for joins purposes, Yugoslavia splitted for the
    # analyzed period
    val == "tmp" ~ "tls", # East Timor
    val == "zar" ~ "cod", # Congo (Democratic Republic of the)
    TRUE ~ val
  )
}

add_gravity_cols <- function(d) {
  # Geographic variables come from the previous version of Gravity (legacy
  # version) and from Geodist. In the next version of BACI the more recent
  # Gravity dataset will be used.

  d %>%
    inner_join(
      dist_cepii %>%
        select(reporter_iso = iso_o, partner_iso = iso_d, dist, contig,
               colony, comlang_off) %>%
        mutate_if(is.character, tolower) %>%
        mutate(
          reporter_iso = fix_iso_codes(reporter_iso),
          partner_iso = fix_iso_codes(partner_iso)
        )
    ) %>%
    inner_join(
      geo_cepii %>%
        mutate_if(is.character, tolower) %>%
        select(reporter_iso = iso3, landlocked_reporter = landlocked) %>%
        mutate(reporter_iso = fix_iso_codes(reporter_iso)) %>%
        distinct()
    ) %>%
    inner_join(
      geo_cepii %>%
        mutate_if(is.character, tolower) %>%
        select(partner_iso = iso3, landlocked_partner = landlocked) %>%
        mutate(partner_iso = fix_iso_codes(partner_iso)) %>%
        distinct()
    ) %>%
    mutate_if(is.character, as_factor)
}

add_gravity_cols_left <- function(d) {
  # Geographic variables come from the previous version of Gravity (legacy
  # version) and from Geodist. In the next version of BACI the more recent
  # Gravity dataset will be used.

  d %>%
    left_join(
      dist_cepii %>%
        select(reporter_iso = iso_o, partner_iso = iso_d, dist, contig,
               colony, comlang_off) %>%
        mutate_if(is.character, tolower) %>%
        mutate(
          reporter_iso = fix_iso_codes(reporter_iso),
          partner_iso = fix_iso_codes(partner_iso)
        )
    ) %>%
    left_join(
      geo_cepii %>%
        mutate_if(is.character, tolower) %>%
        select(reporter_iso = iso3, landlocked_reporter = landlocked) %>%
        mutate(reporter_iso = fix_iso_codes(reporter_iso)) %>%
        distinct()
    ) %>%
    left_join(
      geo_cepii %>%
        mutate_if(is.character, tolower) %>%
        select(partner_iso = iso3, landlocked_partner = landlocked) %>%
        mutate(partner_iso = fix_iso_codes(partner_iso)) %>%
        distinct()
    ) %>%
    mutate_if(is.character, as_factor)
}
