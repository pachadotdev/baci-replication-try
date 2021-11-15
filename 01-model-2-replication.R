source("99-1-pkgs.R")
source("99-2-clean-funs.R")
source("99-3-model-funs.R")
source("00-data.R")

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

# log(dist^2) is collinear
fit <- lm(
  log(cif_fob_unit_ratio) ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + year,
  data = d,
  weights = cif_fob_weights
)

# 4) Based on the results of this estimation, we eliminate the outliers and
# influential observations. These are the observations for which the studentized
# residual is above 2; or the Cook's distance is above 4/(n-p-1), where n is the
# number of observations used in the estimation, and p the number of estimated
# parameters; or where the leverage is above 2p/n.

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

# 5) We reestimate the equation without these outliers. The results of the
# regression without outliers are used to predict a FOB value for all CIF values
# reporter by importers.

rm(d,fit); gc()

fit <- lm(
  log(cif_fob_unit_ratio) ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + year,
  data = dfit,
  weights = cif_fob_weights
)

# rm(dfit)
gc()

dfit <- remove_outliers(fit)

fit <- lm(
  log(cif_fob_unit_ratio) ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + year,
  data = dfit,
  weights = cif_fob_weights
)

n <- nrow(fit$model) # 9576253

dfit <- remove_outliers(fit)

fit <- lm(
  log(cif_fob_unit_ratio) ~ log(dist) + log(uv_exp) + contig +
    landlocked_reporter + landlocked_partner + year,
  data = dfit,
  weights = cif_fob_weights
)

rm(dfit)

n <- nrow(fit$model) # 8921447
outliers <- number_outliers(fit) # 457939

model2 <- list(fit = fit, outliers = outliers)
rm(fit, n, p, outliers); gc()

try(dir.create("models"))

fout <- "models/model2.rds"
saveRDS(model2, fout, compress = "xz")
