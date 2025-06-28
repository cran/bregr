## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(bregr)

data <- survival::lung
data <- data |>
  dplyr::mutate(
    ph.ecog = factor(ph.ecog),
    sex = ifelse(sex == 1, "Male", "Female")
  )

## ----groupby-pipeline---------------------------------------------------------
mds <- br_pipeline(
  data,
  y = c("time", "status"),
  x = "ph.ecog",
  group_by = "sex",
  method = "coxph"
)

## ----get-models---------------------------------------------------------------
br_get_models(mds)

## ----show-forest, fig.width=9, fig.height=5, dpi=150--------------------------
br_show_forest(mds)

## ----show-forest2, fig.width=8, fig.height=4, dpi=150-------------------------
br_show_forest(
  mds,
  drop = 2,
  subset = !(Group_variable == "2" & variable == "ph.ecog" & label == 3)
)

## ----show-forest-rm-all, fig.width=8, fig.height=4, dpi=150-------------------
br_show_forest(
  mds,
  drop = 2,
  subset = !((Group_variable == "Female" & variable == "ph.ecog" & label == 3) |
    (Group_variable == "All"))
)

