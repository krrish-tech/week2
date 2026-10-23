library(tidyverse)
library(tidymodels)
library(readxl)
library(ranger)
library(glmnet)

# 1. Data Ingestion & Pivoting from Long to Wide Format
raw_data <- read_excel("data.xlsx", sheet = "Data")

df_pivoted <- raw_data %>%
  filter(dimension == "Sex") %>%
  select(Country, whoreg6, wbincome2025, date, subgroup, indicator_abbr, estimate) %>%
  filter(indicator_abbr %in% c("aids_mortality", "hiv_incidence", 
                               "hiv_prevalence_15", "hiv_prevalence_1549")) %>%
  pivot_wider(names_from = indicator_abbr, values_from = estimate) %>%
  mutate(across(c(whoreg6, wbincome2025, subgroup), as.factor)) %>%
  drop_na()

# 2. Stratified Data Partitioning & Cross-Validation Setup
set.seed(42)
data_split <- initial_split(df_pivoted, prop = 0.80, strata = aids_mortality)
train_data <- training(data_split)
test_data  <- testing(data_split)

cv_folds <- vfold_cv(train_data, v = 5, strata = aids_mortality)

# 3. Feature Engineering Recipe Definition
hiv_recipe <- recipe(aids_mortality ~ ., data = train_data) %>%
  step_rm(Country) %>%  # Exclude raw Country identifier from predictors
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors(), -all_outcomes())

# 4. Model Specifications
# Elastic Net Specification
glmnet_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Random Forest Specification
rf_spec <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

# 5. Workflow Configuration & Hyperparameter Grid Tuning
glmnet_wf <- workflow() %>% add_recipe(hiv_recipe) %>% add_model(glmnet_spec)
rf_wf     <- workflow() %>% add_recipe(hiv_recipe) %>% add_model(rf_spec)

# Tuning Elastic Net
glmnet_grid <- grid_regular(penalty(), mixture(), levels = 5)
glmnet_res  <- tune_grid(glmnet_wf, resamples = cv_folds, grid = glmnet_grid)

# Tuning Random Forest
rf_grid <- grid_regular(
  finalize(mtry(), train_data %>% select(-aids_mortality, -Country)),
  min_n(),
  levels = 3
)
rf_res <- tune_grid(rf_wf, resamples = cv_folds, grid = rf_grid)

# 6. Final Model Selection & Out-of-Sample Evaluation
best_rf <- select_best(rf_res, metric = "rmse")
final_rf_wf <- finalize_workflow(rf_wf, best_rf)

# Fit on full training set and evaluate on test set
final_fit <- last_fit(final_rf_wf, data_split)
test_performance <- collect_metrics(final_fit)
test_predictions <- collect_predictions(final_fit)

print(test_performance)


# 1. View MAE (Mean Absolute Error) alongside RMSE and R^2
collect_metrics(final_fit)

# 2. Extract predictions and calculate residuals
predictions_df <- collect_predictions(final_fit) %>%
  mutate(residual = aids_mortality - .pred)

# 3. Quick Residual Diagnostic Plot
ggplot(predictions_df, aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.4, color = "#1f77b4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Random Forest Residuals vs Fitted",
    x = "Predicted AIDS Mortality (per 1,000)",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal()
