####function for calculating the OR and CI####
calc_OR_CI <- function(formula, data) {
  m <- glm(formula, data = data, family = "binomial")
  exp(cbind(OR = coef(m), confint(m)))
}
## call fcuntion
formula <-
  as.formula(paste(CP, "~", paste(c(CM, covariates), collapse = "+")))
calc_OR_CI(formula, df)

####PERM####
#OR_basic： OR for the CM variable of the basic model
#OR_reduced： OR for the CM variable of the model with additional covariates

PERM <- function(OR) {
  (OR_basic - OR_reduced) / (OR_basic - 1) * 100 %>%
    return()
}


####mediation####
# Specify the model with the name(s) of the M and covariates
model <- paste0(
  '
    ',
  M_variables,
  ' ~ a*CM
    group ~ b*',
  M_variables,
  '
    group ~ c*CM
    group ~ ',
  paste(covariates , collapse = "+"),
  '
    ',
  M_variables,
  '~',
  paste(covariates , collapse = "+"),
  '
    ab := a*b
    total := c + (a*b)'
)

# Run the mediation analysis
mediation_result <-
  summary(sem(model, data = df, estimator = "MLR"))

