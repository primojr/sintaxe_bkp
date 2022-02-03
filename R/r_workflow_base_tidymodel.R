
## 
# Teste com outros algoritimos

library(tidymodels)
library(tidyverse)
library(ranger)


# Ler Base

# 01.Splits

splitis <- initial_split(DataFrame, strata = Variavel_de_separacao )

df_train <- training(splitis)
df_test <- testing(df_test)

# 02. Pré processamento
reg_recipe <- recipe(y ~ ., data = df_train) %>%
  step_select() %>% 
  step_mutate() %>%
  step_impute_knn(all_numeric()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(altitute,agents)
#step_corr(all_numeric_predictors(), threshold = .8, method = "pearson")
#juice(prep(reg_recipe)) 

# 03. Engine

# Ex: RLinaer
algoritimo  <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# Ex: Randon Forest
reg_mod  <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_args('' = 3, importance = TRUE) %>%
  set_mode("classification")
  

# 04. Workflow
reg_workflow <- workflow() %>% 
  add_model(reg_mod) %>% 
  add_recipe(reg_recipe)

# 05.Cross validation
val_set <- vfold_cv(df_train, v = 4, strata = variavel_de_separacao)

# 07.trainning
reg_trained <- reg_workflow %>% 
  tune_grid(
    val_set,
    grid = 5,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse) # Metrica de seleção conforme o modelo
  )

reg_trained %>% show_best()

# autoplot
ggplot2::autoplot(reg_trained)

# selecaop
reg_best_tune <- select_best(reg_trained, "rmse")
final_reg_model <- reg_mod %>%
  finalize_model(reg_best_tune)


final_reg_model$eng_args

workflow() %>%
  add_recipe(reg_recipe) %>%
  add_model(final_reg_model) %>%
  collect_predictions() %>%
  select(.row, price, .pred) %>%
  ggplot() +
  aes(x= price, y = .pred) +
  geom_point()

# # save the results
# reg_fitted <- workflow() %>% 
#   add_recipe(reg_recipe) %>% 
#   add_model(final_reg_model) %>% 
#   fit(NOME)   
