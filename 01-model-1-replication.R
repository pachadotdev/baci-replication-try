source("99-1-pkgs.R")
source("99-2-clean-funs.R")
source("99-3-model-funs.R")

fout <- "models/model.rds"

if (!file.exists(fout)) {
  source("00-1-data.R")

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

  dfit <- dfit %>%
    mutate(
      log_cif_fob_unit_ratio = log(cif_fob_unit_ratio),
      log_dist = log(dist),
      log_dist_sq = (log(dist))^2,
      log_uv_exp = log(uv_exp)
    )

  # dfit %>%
  #   filter(row_number() <= 10) %>%
  #   select(matches("dist"))

  form <- log_cif_fob_unit_ratio ~ log_dist + log_dist_sq + log_uv_exp + contig +
    landlocked_reporter + landlocked_partner + year

  fit <- function() {
    lm(
      form,
      data = dfit
    )
  }

  # log(dist^2) is collinear
  fit2 <- fit(); gc()

  # 4) Based on the results of this estimation, we eliminate the outliers and
  # influential observations. These are the observations for which the studentized
  # residual is above 2; or the Cook's distance is above 4/(n-p-1), where n is the
  # number of observations used in the estimation, and p the number of estimated
  # parameters; or where the leverage is above 2p/n.

  dfit <- remove_outliers(fit2)

  # 5) We reestimate the equation without these outliers. The results of the
  # regression without outliers are used to predict a FOB value for all CIF values
  # reporter by importers.

  fit2 <- fit(); gc()

  dfit <- remove_outliers(fit2)

  fit2 <- fit(); gc()

  dfit <- remove_outliers(fit2)

  fit2 <- fit(); gc()

  rm(dfit)

  model <- list(fit = fit2, outliers = number_outliers(fit2))
  rm(fit2); gc()

  try(dir.create("models"))

  saveRDS(model, fout, compress = "xz")
}
