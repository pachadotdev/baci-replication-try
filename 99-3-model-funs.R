remove_outliers <- function(fit) {
  n <- nrow(fit$model)
  p <- length(fit$coefficients)

  dfit %>%
    mutate(
      .cooksd = cooks.distance(fit),
      .std.resid = MASS::studres(fit),
      .hat = hatvalues(fit)
    ) %>%
    filter(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2 * p / n)
}

number_outliers <- function(fit) {
  augment(fit) %>%
    filter(!(.std.resid < 2 & .cooksd < 4 / (n - p - 1) & .hat < 2*p / n)) %>%
    nrow()
}
