# flag
# 0 = no estimation
# 2 = quantity estimation only
# 4 = net weight estimation only
# 6 = both quantity and net weight are estimated.

num_flag <- function(val) {
  case_when(
    val == 0 ~ "no estimation",
    val == 2 ~ "quantity estimation only",
    val == 4 ~ "net weight estimation only",
    val == 6 ~ "both quantity and net weight are estimated",
    TRUE ~ NA_character_
  )
}

fct_flag <- function(d) {
  d %>%
    mutate(
      flag_exp = as_factor(num_flag(flag_exp)),
      flag_imp = as_factor(num_flag(flag_imp)),
    )
}

impute_kg <- function(d) {
  d %>%
    mutate(
      qty_exp = case_when(
        qty_unit_exp == "weight in kilograms" ~ qty_exp,
        qty_unit_exp != "weight in kilograms" ~ netweight_kg_exp
      ),
      qty_imp = case_when(
        qty_unit_imp == "weight in kilograms" ~ qty_imp,
        qty_unit_imp != "weight in kilograms" ~ netweight_kg_imp
      )
    )
}

fobization_val <- function(d) {
  d %>%
    mutate(
      fobization = case_when(
        .fitted < 0 ~ "no fobization",
        is.na(.fitted) ~ "no fobization",
        .fitted >= 0 ~ "fobization"
      )
    )
}

fobization_units_val <- function(d) {
  d %>%
    mutate(
      fobization = case_when(
        is.na(qty_exp) & is.na(qty_imp) ~ "no fobization",
        is.na(qty_unit_exp) & is.na(qty_unit_imp) ~ "no fobization",
        qty_unit_exp == "electrical energy in thousands of kilowatt-hours" |
          qty_unit_imp == "electrical energy in thousands of kilowatt-hours" ~
          "no fobization",
        TRUE ~ fobization
      )
    )
}

mismatch_val <- function(d) {
  d %>%
    mutate(trade_value_usd_imp_fob = trade_value_usd_imp / exp(.fitted)) %>%

    rowwise() %>%
    mutate(
      direct_mismatch = abs(trade_value_usd_imp - trade_value_usd_exp),
      fobization_mismatch = abs(trade_value_usd_imp_fob - trade_value_usd_exp),
    ) %>%
    ungroup() %>%

    mutate(
      difference_result = case_when(
        fobization_mismatch >= direct_mismatch & fobization == "fobization" ~
          "use exports",
        fobization_mismatch < direct_mismatch & fobization == "fobization" ~
          "use imports",
        TRUE ~ NA_character_
      )
    )
}

export_val <- function(d) {
  d %>%
    mutate(
      reported_value = case_when(
        reported_by == "exporter only" ~ "exports, no change",
        reported_by == "importer only" & fobization == "no fobization" ~
          "imports, no change",
        reported_by == "importer only" & fobization == "fobization" ~
          "imports, divided by cif/fob ratio",
        reported_by == "both parties" & fobization == "no fobization" ~
          "exports, no change",
        reported_by == "both parties" & fobization == "fobization" &
          difference_result == "use exports" ~ "exports, no change",
        reported_by == "both parties" & fobization == "fobization" &
          difference_result == "use imports" ~
          "imports, divided by cif/fob ratio"
      ),

      trade_value_usd_exp = case_when(
        reported_value == "exports, no change" ~ trade_value_usd_exp,
        reported_value == "imports, no change" ~ trade_value_usd_imp,
        reported_value == "imports, divided by cif/fob ratio" ~
          trade_value_usd_imp_fob
      )
    )
}

impute_exp <- function(y) {
  message(y)

  dexp <- data_partitioned() %>%
    filter_flow_kg(y, "export")

  dimp <- data_partitioned() %>%
    filter_flow_kg(y, "import") %>%
    select(-year)

  dexp <- dexp %>%
    join_flows_kg(dimp) %>%
    fct_flag()

  rm(dimp)

  dexp <- dexp %>%
    impute_kg() %>%
    compute_ratios() %>%
    add_gravity_cols_left() %>%
    mutate(year = as_factor(year))

  dexp <- augment(fit, newdata = dexp) %>%
    fobization_val() %>%
    fobization_units_val() %>%

    mismatch_val() %>%
    select(reporter_iso, partner_iso, commodity_code, trade_value_usd_exp,
           trade_value_usd_imp, trade_value_usd_imp_fob, reported_by,
           fobization, difference_result) %>%

    export_val() %>%
    select(reporter_iso, partner_iso, commodity_code, trade_value_usd_exp,
           reported_by, reported_value) %>%

    mutate_if(is.character, as_factor) %>%
    mutate(year = as.integer(y)) %>%
    select(year, everything())

  gc()
  return(dexp)
}
