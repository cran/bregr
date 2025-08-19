## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(bregr)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
m@models

## -----------------------------------------------------------------------------
br_get_models(m, 1)
br_get_models(m, c(1, 3))
br_get_models(m, "cyl")
br_get_models(m, c("cyl", "hp"))
br_get_models(m)

## -----------------------------------------------------------------------------
m2 <- breg(mtcars) |>
  br_set_y("mpg") |>
  br_set_x(colnames(mtcars)[2:4]) |>
  br_set_x2("vs") |>
  br_set_model("gaussian") |>
  br_run()
m2@models

## -----------------------------------------------------------------------------
all.equal(m, m2)

