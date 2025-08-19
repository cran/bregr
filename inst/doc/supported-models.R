## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bregr)

## -----------------------------------------------------------------------------
br_avail_methods()

## -----------------------------------------------------------------------------
rv <- br_pipeline(
  data = mtcars,
  y = "mpg", x = c("cyl", "disp", "hp"), x2 = "am",
  method = "lm"
)

## -----------------------------------------------------------------------------
br_get_results(rv, tidy = TRUE)

## -----------------------------------------------------------------------------
br_get_results(rv, tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)

## -----------------------------------------------------------------------------
br_pipeline(
  data = mtcars,
  y = "mpg", x = c("cyl", "disp", "hp"), x2 = "am",
  method = "gaussian"
) |>
  br_get_results(tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)

## -----------------------------------------------------------------------------
br_pipeline(
  data = mtcars,
  y = "vs", x = c("cyl", "disp", "hp"), x2 = "am",
  method = "binomial"
) |>
  suppressWarnings() |>
  br_get_results(tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)

## -----------------------------------------------------------------------------
br_pipeline(
  data = survival::lung,
  y = c("time", "status"),
  x = c("ph.ecog", "ph.karno", "pat.karno", "meal.cal"), x2 = c("age", "sex"),
  method = "coxph"
) |>
  suppressWarnings() |>
  br_get_results(tidy = TRUE) |>
  dplyr::filter(Focal_variable == term)

## -----------------------------------------------------------------------------
knitr::kable(
  broom.helpers::supported_models
)

## -----------------------------------------------------------------------------
br_avail_method_config("coxph")

## -----------------------------------------------------------------------------
br_avail_method_config("binomial")

## -----------------------------------------------------------------------------
if (requireNamespace("lme4")) {
  md_config <- list(
    f_call = "lme4::lmer",
    f_cnst_y = NULL,
    args_method = NULL,
    args_data = "data = data"
  )
}

## -----------------------------------------------------------------------------
if (requireNamespace("lme4") && requireNamespace("merDeriv") && requireNamespace("broom.mixed")) {
  br_pipeline(
    data = lme4::sleepstudy,
    y = "Reaction",
    x = c("Days", "Subject"), x2 = "(Days | Subject)",
    method = md_config
  ) |>
    # br_get_results(tidy = TRUE)
    br_show_table(export = TRUE, args_table_export = list(format = "html"))
}

