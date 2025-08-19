## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(bregr)

## -----------------------------------------------------------------------------
rv <- br_pipeline(
  data = mtcars,
  y = "mpg", x = c("cyl", "disp*hp"), x2 = "am",
  method = "lm"
)

## -----------------------------------------------------------------------------
br_get_results(rv, tidy = TRUE)

## ----fig.width=8, fig.height=4, dpi=150---------------------------------------
br_show_forest(rv)

## ----fig.width=7, fig.height=3------------------------------------------------
br_show_forest_ggstats(rv, idx = 2)

## ----fig.width=7, fig.height=4------------------------------------------------
br_show_forest_ggstats(rv)

## -----------------------------------------------------------------------------
br_avail_methods()

## -----------------------------------------------------------------------------
data <- data.frame(
  x = rnorm(100)
)
data$y <- rpois(100, exp(1 + data$x))
head(data)

## -----------------------------------------------------------------------------
rv <- br_pipeline(
  data = data,
  y = "y", x = "x",
  method = 'quasi(variance = "mu", link = "log")'
)
br_get_results(rv, tidy = TRUE)

## -----------------------------------------------------------------------------
glm(y ~ x, data = data, family = quasi(variance = "mu", link = "log")) |>
  summary()

