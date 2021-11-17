source("99-1-pkgs.R")
source("99-3-model-funs.R")

fout <- "raw_dataset_1989_2004.rds"

if (!file.exists(fout)) {
  source("99-2-clean-funs.R")
  source("00-1-data.R")

  d_rtas <- readRDS("rtas/rtas_at_least_one_in_force_per_year.rds") %>%
    filter(year %in% 1989:2004)

  d <- d %>%
    mutate(
      year = as.integer(as.character(year)),
      exporter_iso = as.character(exporter_iso),
      importer_iso = as.character(importer_iso)
    )

  dc <- d %>%
    select(exporter_iso, importer_iso) %>%
    rowwise() %>%
    mutate(
      country1 = min(exporter_iso, importer_iso),
      country2 = max(exporter_iso, importer_iso)
    ) %>%
    ungroup() %>%
    select(country1, country2)

  d <- d %>%
    bind_cols(dc)

  rm(dc); gc()

  d <- d %>%
    left_join(d_rtas)

  d <- d %>%
    # filter(is.na(rta))
    mutate(rta = ifelse(is.na(rta), 0, rta))

  # create RDS dataset for prof Reid
  saveRDS(d, "raw_dataset_1989_2004.rds")
} else {
  d <- readRDS(fout)
}

dval <- readr::read_csv("trade_valuation_system_per_country.csv")

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
d <- d %>%
  inner_join(dval_exp) %>%
  inner_join(dval_imp)

rm(dval_exp, dval_imp)

d <- d %>%
  mutate(
    year = as.factor(year),

    # exporter_iso = ifelse(exporter_iso == "can", "0-can", exporter_iso),
    # importer_iso = ifelse(importer_iso == "can", "0-can", importer_iso),

    exporter_iso = as.factor(exporter_iso),
    importer_iso = as.factor(importer_iso)
  )

# create fixed effects
# d <- d %>%
#   mutate(
#     pi = as.factor(paste0(exporter_iso, year)),
#     chi = as.factor(paste0(importer_iso, year))
#   )

# creater clustering variable
# d <- d %>%
#   mutate(pair_id = paste(exporter_iso, importer_iso, sep = "_"))

d <- d %>%
  select(year, cif_fob_unit_ratio, cif_fob_weights,
         dist, uv_exp, contig, landlocked_reporter, landlocked_partner,
         colony, comlang_off, rta)

gc()

fit <- glm(
  cif_fob_unit_ratio ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + colony + comlang_off + rta + year,
  data = d,
  weights = cif_fob_weights,
  family = quasipoisson()
)

# summary(fit)
# saveRDS(fit, "models/proposed_model.rds")

gc()

n <- nrow(fit$model)
p <- length(fit$coefficients)
# rm(d)

dfit <- d %>%
  mutate(
    .cooksd = cooks.distance(fit),
    .std.resid = MASS::studres(fit),
    .hat = hatvalues(fit)
  ) %>%
  filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

rm(d, fit); gc()

fit <- glm(
  cif_fob_unit_ratio ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + colony + comlang_off + rta + year,
  data = dfit,
  weights = cif_fob_weights,
  family = quasipoisson()
)

gc()

dfit <- remove_outliers(fit)

gc()

fit <- glm(
  cif_fob_unit_ratio ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + colony + comlang_off + rta + year,
  data = dfit,
  weights = cif_fob_weights,
  family = quasipoisson()
)

gc()

dfit <- remove_outliers(fit)

gc()

fit <- glm(
  cif_fob_unit_ratio ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + colony + comlang_off + rta + year,
  data = dfit,
  weights = cif_fob_weights,
  family = quasipoisson()
)

rm(dfit)
gc()

n <- nrow(fit$model) # 8552796
outliers <- number_outliers(fit) # 876039

model <- list(fit = fit, outliers = outliers)
rm(fit, n, outliers); gc()

try(dir.create("models"))

fout <- "models/proposed_model_2.rds"
saveRDS(model, fout, compress = "xz")
