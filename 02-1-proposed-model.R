source("99-1-pkgs.R")
source("99-2-clean-funs.R")
source("99-3-model-funs.R")

extended_ds <- "extended_dataset_1989_2004.rds"

if (!file.exists(extended_ds)) {
  dfit <- arrow::open_dataset("raw_dataset_1989_2004", partitioning = "year") %>%
    collect() %>%
    mutate(year = as.factor(gsub(".*=", "", year))) %>%
    select(year, everything())

  dfit <- dfit %>%
    select(year, exporter_iso, importer_iso, cif_fob_unit_ratio, cif_fob_weights,
           dist, uv_exp, contig, landlocked_reporter, landlocked_partner,
           colony, comlang_off)

  dfit <- dfit %>%
    mutate(
      year = as.integer(as.character(year)),
      exporter_iso = as.character(exporter_iso),
      importer_iso = as.character(importer_iso)
    )

  d_rtas <- readRDS("rtas/rtas_at_least_one_in_force_per_year.rds") %>%
    filter(year %in% 1989:2004)

  d_rtas <- d_rtas %>%
    group_by(year, country1, country2) %>%
    summarise(rta = sum(rta)) %>%
    rowwise() %>%
    mutate(rta = min(1, rta)) %>%
    ungroup()

  dc <- dfit %>%
    select(exporter_iso, importer_iso) %>%
    rowwise() %>%
    mutate(
      country1 = min(exporter_iso, importer_iso),
      country2 = max(exporter_iso, importer_iso)
    ) %>%
    ungroup() %>%
    select(country1, country2)

  dfit <- dfit %>%
    bind_cols(dc)

  rm(dc); gc()

  dfit <- dfit %>%
    left_join(d_rtas)

  dfit <- dfit %>%
    mutate(rta = ifelse(is.na(rta), 0, rta))

  # dfit %>%
  #   select(year, exporter_iso, importer_iso, rta) %>%
  #   filter(exporter_iso == "chl", importer_iso == "chn") %>%
  #   distinct()

  dval <- readRDS("trade_valuation_system_per_country.rds")

  dval_exp <- dval %>%
    filter(trade_flow == "Export", valuation == "FOB") %>%
    select(year, exporter_iso = iso3_digit_alpha)

  dval_imp <- dval %>%
    filter(trade_flow == "Import", valuation == "CIF") %>%
    select(year, importer_iso = iso3_digit_alpha)

  rm(dval)

  length(unique(dval_exp$exporter_iso))
  length(unique(dval_imp$importer_iso))

  # remove not FOB export / CIF imports
  dfit <- dfit %>%
    inner_join(dval_exp) %>%
    inner_join(dval_imp)

  rm(dval_exp, dval_imp)

  dfit <- dfit %>%
    mutate(
      year = as.factor(year),

      # exporter_iso = ifelse(exporter_iso == "can", "0-can", exporter_iso),
      # importer_iso = ifelse(importer_iso == "can", "0-can", importer_iso),

      exporter_iso = as.factor(exporter_iso),
      importer_iso = as.factor(importer_iso)
    )

  dfit <- dfit %>%
    mutate(
      colony = as.integer(colony),
      comlang_off = as.integer(comlang_off),
      rta = as.integer(rta)
    )

  dfit <- dfit %>%
    select(-c(country1, country2))

  rm(d_rtas); gc()
  saveRDS(dfit, extended_ds, compress = "xz")
} else {
  dfit <- readRDS(extended_ds)
}

# create fixed effects
# dfit <- dfit %>%
#   mutate(
#     pi = as.factor(paste0(exporter_iso, year)),
#     chi = as.factor(paste0(importer_iso, year))
#   )

# creater clustering variable
# dfit <- dfit %>%
#   mutate(pair_id = paste(exporter_iso, importer_iso, sep = "_"))

form <- cif_fob_unit_ratio ~ log(dist) + uv_exp +
  landlocked_reporter + landlocked_partner +
  contig + comlang_off + colony + rta + year

fit <- function() {
  glm(
    form,
    data = dfit,
    weights = cif_fob_weights,
    family = quasipoisson
  )
}

fit2 <- fit(); gc()

summary(fit2)

n <- nrow(fit$model)
p <- length(fit$coefficients)
# rm(dfit)

dfit <- dfit %>%
  mutate(
    .cooksd = cooks.distance(fit),
    .std.resid = MASS::studres(fit),
    .hat = hatvalues(fit)
  ) %>%
  filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

rm(dfit, fit); gc()

fit2 <- fit(); gc()

dfit <- remove_outliers(fit2); gc()

fit2 <- fit(); gc()

dfit <- remove_outliers(fit2); gc()

fit2 <- fit()

rm(dfit); gc()

summary(fit2)

n <- nrow(fit$model) # 8552796
outliers <- number_outliers(fit) # 876039

model <- list(fit = fit, outliers = outliers)
rm(fit, n, outliers); gc()

try(dir.create("models"))

fout <- "models/proposed_model_2.rds"
saveRDS(model, fout, compress = "xz")
