source("99-1-pkgs.R")
source("99-2-clean-funs.R")
source("99-3-predict-funs.R")

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
prop_data <- "remaining_no_of_rows_after_filtering.csv"

if (!file.exists(raw_data)) {
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

  readr::write_csv(d2, prop_data)
} else {
  d <- arrow::open_dataset(raw_data, partitioning = "year")
  d2 <- readr::read_csv(prop_data)
}

# create RDS dataset
d <- d %>%
  collect()

saveRDS(d, "raw_dataset_1989_2004.rds", compress = "xz")

# Model ----

# 3) Estimate the determinants of this ratio of unit values, by regressing the
# observed CIF rates on log distance, square of log distance, contiguity,
# landlocked, median unit value of the product and time fixed effects. Each
# observation is weighted by the inverse of a "discrepancy ratio" for quantity,
# i.e. 1/(larger quantity/smaller quantity), the idea being that flows for which
# both reporters disagree a lot on quantity have less reliable CIF rates.

# Geographic variables come from the previous version of Gravity (legacy
# version) and from Geodist. In the next version of BACI the more recent Gravity
# dataset will be used.

# After removing influential rows twice, I get a result very similar to model II
# in the article

fit <- lm(
  log_cif_fob_unit_ratio_unweighted ~ log_dist + log_dist_sq + contig +
    landlocked_reporter + landlocked_partner + log_uv_exp + year,
  data = d,
  weights = cif_fob_weights
)

# 4) Based on the results of this estimation, we eliminate the outliers and
# influential observations. These are the observations for which the studentized
# residual is above 2; or the Cook's distance is above 4/(n-p-1), where n is the
# number of observations used in the estimation, and p the number of estimated
# parameters; or where the leverage is above 2p/n.

sfit <- summary(fit)
n <- sfit$df[2]
p <- sfit$df[1]
# rm(d)

# dfit <- augment(fit) %>%
#   filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

dfit <- d %>%
  mutate(
    .cooksd = cooks.distance(fit),
    .std.resid = MASS::studres(fit),
    .hat = hatvalues(fit)
  ) %>%
  filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

# 5) We reestimate the equation without these outliers. The results of the
# regression without outliers are used to predict a FOB value for all CIF values
# reporter by importers.

rm(fit); gc()

fit <- lm(
  log_cif_fob_unit_ratio_unweighted ~ log_dist + log_dist_sq + contig +
    landlocked_reporter + landlocked_partner + log_uv_exp + year,
  data = dfit,
  weights = cif_fob_weights
)

# rm(dfit)
gc()

sfit <- summary(fit)
n <- sfit$df[2]
p <- sfit$df[1]

# dfit <- augment(fit) %>%
#   filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

dfit <- dfit %>%
  mutate(
    .cooksd = cooks.distance(fit),
    .std.resid = MASS::studres(fit),
    .hat = hatvalues(fit)
  ) %>%
  filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

fit <- lm(
  log_cif_fob_unit_ratio_unweighted ~ log_dist + log_dist_sq + contig +
    landlocked_reporter + landlocked_partner + log_uv_exp + year,
  data = dfit,
  weights = cif_fob_weights
)

sfit <- summary(fit)
n <- sfit$df[2]
p <- sfit$df[1]

# dfit <- augment(fit) %>%
#   filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

dfit <- dfit %>%
  mutate(
    .cooksd = cooks.distance(fit),
    .std.resid = MASS::studres(fit),
    .hat = hatvalues(fit)
  ) %>%
  filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)

fit <- lm(
  log_cif_fob_unit_ratio_unweighted ~ log_dist + log_dist_sq + contig +
    landlocked_reporter + landlocked_partner + log_uv_exp + year,
  data = dfit,
  weights = cif_fob_weights
)

rm(dfit)

sfit <- summary(fit)
n <- sfit$df[2]
p <- sfit$df[1]

# report outliers
outliers <- augment(fit) %>%
  filter(!(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2*p / n)) %>%
  nrow()

# Predict ----

# The fobization is not applied if the predicted CIF rate is below 1, if
# applying it would increase the bilateral asymmetry, or if the importer does
# not report CIF.

# Here "not in kilograms" refers to the cases when not both parties report
# weights in kilograms

# Kilowatt/hour units are labelled as "no fobization" and are not converted
# nor corrected

# rm(sfit)
#
# dexp <- map_df(1994:2004, impute_exp)
#
# dexp %>%
#   group_by(reported_by) %>%
#   count()
#
# dexp %>%
#   group_by(reported_value) %>%
#   count()
