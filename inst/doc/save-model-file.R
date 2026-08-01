## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
qs2_available <- requireNamespace("qs2", quietly = TRUE) &&
  requireNamespace("ids", quietly = TRUE)

## ----setup--------------------------------------------------------------------
library(bregr)

## ----eval = qs2_available-----------------------------------------------------
options(bregr.save_model = TRUE)
# Set model save path if necessary
# options(bregr.path = "/model/to/path")

m <- breg(mtcars) |>
  br_set_y("mpg") |>
  br_set_x(colnames(mtcars)[2:4]) |>
  br_set_x2("vs") |>
  br_set_model("gaussian") |>
  br_run()

options(bregr.save_model = FALSE)

## ----eval = qs2_available-----------------------------------------------------
m@models

## ----eval = qs2_available-----------------------------------------------------
br_get_models(m, 1)
br_get_models(m, c(1, 3))
br_get_models(m, "cyl")
br_get_models(m, c("cyl", "hp"))
br_get_models(m)

## ----eval = qs2_available-----------------------------------------------------
m2 <- breg(mtcars) |>
  br_set_y("mpg") |>
  br_set_x(colnames(mtcars)[2:4]) |>
  br_set_x2("vs") |>
  br_set_model("gaussian") |>
  br_run()
m2@models

## ----eval = qs2_available-----------------------------------------------------
all.equal(m, m2)

