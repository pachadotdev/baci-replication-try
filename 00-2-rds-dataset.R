fout <- "raw_dataset_1989_2004.rds"

if (!file.exists(fout)) {
  source("99-1-pkgs.R")
  source("99-2-clean-funs.R")
  source("99-3-model-funs.R")
  source("00-1-data.R")

  d <- d %>%
    mutate(
      year = as.integer(as.character(year)),
      exporter_iso = as.character(exporter_iso),
      importer_iso = as.character(importer_iso)
    )

  # d_rtas <- readRDS("rtas/rtas_at_least_one_in_force_per_year.rds") %>%
  #   filter(year %in% 1989:2004)
  #
  # dc <- d %>%
  #   select(exporter_iso, importer_iso) %>%
  #   rowwise() %>%
  #   mutate(
  #     country1 = min(exporter_iso, importer_iso),
  #     country2 = max(exporter_iso, importer_iso)
  #   ) %>%
  #   ungroup() %>%
  #   select(country1, country2)
  #
  # d <- d %>%
  #   bind_cols(dc)
  #
  # rm(dc); gc()
  #
  # d <- d %>%
  #   left_join(d_rtas)
  #
  # d <- d %>%
  #   # filter(is.na(rta))
  #   mutate(rta = ifelse(is.na(rta), 0, rta))

  d <- d %>%
    select(year, exporter_iso, importer_iso, commodity_code, uv_exp,
           cif_fob_unit_ratio, cif_fob_ratio, cif_fob_weights, reported_by,
           dist, contig)

  saveRDS(d, "raw_dataset_1989_2004.rds")
}
