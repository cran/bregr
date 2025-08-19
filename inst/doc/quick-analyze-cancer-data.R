## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(bregr)
library(dplyr)

if (!requireNamespace("UCSCXenaShiny")) {
  install.packages("UCSCXenaShiny")
}
library(UCSCXenaShiny)

## -----------------------------------------------------------------------------
data <- inner_join(
  tcga_clinical_fine,
  tcga_surv |> select(sample, OS, OS.time),
  by = c("Sample" = "sample")
) |> filter(!is.na(Stage_ajcc), !is.na(Gender))
head(data)

## -----------------------------------------------------------------------------
m <- br_pipeline(
  data = data,
  y = c("OS.time", "OS"),
  x = "Stage_ajcc", x2 = "Age",
  group_by = "Gender",
  method = "coxph"
)
m

## -----------------------------------------------------------------------------
br_get_results(m, tidy = TRUE) |>
  knitr::kable()

## ----fig.dpi=150, fig.width=6, fig.height=6-----------------------------------
m <- br_rename_models(m, c("Female", "Male", "All"))

br_show_forest_ggstats(m)

