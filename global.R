library(shiny)

clean_data2_subset <- readRDS("clean_data2_subset.rds")

selected_vars <- c(
  "sex",
  "age",
  "Nicotine",
  "Cholesterol",
  "DM",
  "BPQ050A",
  "ACES_with_complexity_combined"
)

multivariable_formula <- as.formula(
  paste("CVD ~", paste(selected_vars, collapse = " + "))
)

dd <- datadist(clean_data2_subset)
options(datadist = "dd")

nom_model <- lrm(
  formula = multivariable_formula,
  data = clean_data2_subset,
  x = TRUE,
  y = TRUE
)

nom <- nomogram(
  nom_model,
  fun = plogis,
  fun.at = c(0.05, .1, .3, .5, .7, .9, .95),
  lp = FALSE,
  funlabel = "Probability of CVD",
  maxscale = 40
)
