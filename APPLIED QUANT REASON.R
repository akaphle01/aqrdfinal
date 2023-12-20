#LOADING NECESSARY LIBRARIES

library(readxl)
library(gt)
library(modelsummary)
library(fixest)
library(lme4)
library(broom)
library(dplyr)
library(tidyverse)
install.packages("fixest")
library(fixest)
library(cowplot)
library(ggthemes)
library(usethis)
use_git_config(user.name = "akaphle01", user.email = "aparajita.kaphle@yale.edu")



xlsx::write.xlsx(fqhc_census_tg, file = "data/fqhc_census_tg2.xlsx", sheetName = "First")

non_ACA <- c("ID","UT","SD", "NE", "OK", "MO", "NC","VA", "ME", "WY", "KS", "TX", "WI","TN", "MS", "AL", "GA", "SC", "FL")


fqhc_census <- read_excel("data/fqhc_census_tg.xlsx")




table2 <- fqhc_census |>
  filter(!is.na(region)) |> 
  mutate(ACA_exp = ifelse(ACA_exp == 0, "ACA Not Expanded", "ACA Expanded"),
         ACA_exp = fct_relevel(ACA_exp, "ACA Not Expanded")) |>
  dplyr::group_by(year, region) |>
  summarize(median_white = median(perc_white, na.rm = TRUE),
            median_black = median(perc_black, na.rm = TRUE),
            below_pov_median = median(below_pov, na.rm = TRUE),
            est_median_inc_median = median(est_median_fam_inc, na.rm = TRUE),
            cash_assis_median = median(cash_assis, na.rm = TRUE),
            unemployed_median = median(unemployed, na.rm = TRUE),
            avg_pop = mean(avg_pop, na.rm = TRUE),
            number_obs = n()
  ) |>
  gt(rowname_col = "region",
     groupname_col = "year"
  ) |>
  summary_rows(
    groups = everything(),
    columns = c(median_white, median_black, below_pov_median,
                est_median_inc_median, cash_assis_median,
                unemployed_median, avg_pop),
    fns = list(
      average = "mean",
      SD = "sd"
    ), fmt = list(~ fmt_number(., decimals = 2))) |>
  cols_label(median_white = "Median White Population Percentage (%)",
             median_black = "Median Black Population Percentage (%)",
             below_pov_median = "Median Percentage Below Poverty (%)",
             est_median_inc_median = "Median Estimated Family Income (USD)",
             cash_assis_median = "Median Percentage On Cash Assistance (%)",
             unemployed_median = "Median Unemployment Rate (%)",
             avg_pop = "Average Population",
             number_obs = "Number of Observations") |> 
  tab_header(title = "FQHC and Census Data by Region") |> 
  fmt_number(columns = 2:7,
             decimals = 2)


gtsave(table2, "finaltable.png", vwidth = 1500, vheight = 1000)

table3 <- fqhc_census |> 
  filter(!is.na(region)) |>
  mutate(ACA_exp = ifelse(ACA_exp == 0, "ACA Not Expanded", "ACA Expanded"),
         ACA_exp = fct_relevel(ACA_exp, "ACA Not Expanded")) |>
  dplyr::group_by(state_status, region) |>
  summarize(median_white = median(perc_white, na.rm = TRUE),
            median_black = median(perc_black, na.rm = TRUE),
            below_pov_median = median(below_pov, na.rm = TRUE),
            est_median_inc_median = median(est_median_fam_inc, na.rm = TRUE),
            cash_assis_median = median(cash_assis, na.rm = TRUE),
            unemployed_median = median(unemployed, na.rm = TRUE),
            avg_pop = mean(avg_pop, na.rm = TRUE),
            number_obs = n()
  ) |>
  gt(rowname_col = "region",
     groupname_col = "state_status"
  ) |>
  summary_rows(
    groups = everything(),
    columns = c(median_white, median_black, below_pov_median,
                est_median_inc_median, cash_assis_median,
                unemployed_median, avg_pop),
    fns = list(
      average = "mean",
      SD = "sd"
    ), fmt = list(~ fmt_number(., decimals = 2))) |>
  cols_label(median_white = "Median White Population Percentage (%)",
             median_black = "Median Black Population Percentage (%)",
             below_pov_median = "Median Percentage Below Poverty (%)",
             est_median_inc_median = "Median Estimated Family Income (USD)",
             cash_assis_median = "Median Percentage On Cash Assistance (%)",
             unemployed_median = "Median Unemployment Rate (%)",
             avg_pop = "Average Population",
             number_obs = "Number of Observations") |> 
  tab_header(title = "FQHC and Census Data by Region and Implementation of ACA") |> 
  fmt_number(columns = 2:7,
             decimals = 2)




fqhc_census2 <- fqhc_census |>
  group_by(year, state_status) |>
  summarize(median_white = median(perc_white, na.rm = TRUE),
            median_black = median(perc_black, na.rm = TRUE),
            below_pov_median = median(below_pov, na.rm = TRUE),
            est_median_inc_median = median(est_median_fam_inc, na.rm = TRUE),
            cash_assis_median = median(cash_assis, na.rm = TRUE),
            unemployed_median = median(unemployed, na.rm = TRUE),
            rural_code = mean(RUCA_code, na.rm = TRUE))



library(patchwork)
g1 <- ggplot(fqhc_census2, aes(x = as.factor(year), y = log(est_median_inc_median), col = state_status, group = state_status)) +
  geom_point(size = 3) +
  labs(x = "Year",
       y = "Median Family Income (USD)",
       title = "Change in Income from 2011 and \n2019 for ACA & Non-ACA states") +
  geom_line(show.legend = F, 
            linewidth = 2) +
  guides(col =guide_legend(title="ACA Status")) +
  theme_gray()

g2 <- ggplot(fqhc_census2, aes(x = as.factor(year), y = unemployed_median, col = state_status, group = state_status)) +
  geom_point(size = 3) +
  labs(x = "Year",
       y = "Unemployment Rate (%)",
       title = "Change in Unemployment Rate from 2011 and \n2019 for ACA & Non-ACA states") +
  geom_line(show.legend = F,
            linewidth = 2) +
  guides(col =guide_legend(title="ACA Status"))

g3 <- ggplot(fqhc_census2, aes(x = as.factor(year), y = below_pov_median, col = state_status, group = state_status)) +
  geom_point(size = 3) +
  labs(x = "Year",
       y = "Percentage Points Below Poverty",
       title = "Change in Percentage Below Poverty from 2011 and \n2019 for ACA & Non-ACA states") +
  geom_line(show.legend = F,
            linewidth = 2) +
  guides(col =guide_legend(title="ACA Status"))

g4 <- ggplot(fqhc_census2, aes(x = as.factor(year), y = rural_code, col = state_status, group = state_status)) +
  geom_point(size = 3) +
  labs(x = "Year",
       y = "Averaged RUCA Code",
       title = "Change in Rural/Urban designation from 2011 and \n2019 for ACA & Non-ACA states",
       caption = "A high RUCA Code (max. 10) indicates a rural area \nA low RUCA code (min. 0) indicating an urban area") + 
  geom_line(show.legend = F,
            linewidth = 2) +
  guides(col =guide_legend(title="ACA Status"))

plot_grid(
  g1, g2, g3, g4,
  labels = c('A', 'B', 'C', 'D'))

(g1 | g2) / (g3 |g4)


g1 + g2 + g3 + g4


lm1 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status), data = fqhc_census |> filter(!is.na(state_status)))
lm2 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status) + unemployed, data = fqhc_census |> filter(!is.na(state_status)))
lm3 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status) + RUCA_code, data = fqhc_census |> filter(!is.na(state_status)))
lm4 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status) + RUCA_code + unemployed, data = fqhc_census |> filter(!is.na(state_status)))


fqhc_censusalph <- fqhc_census |>
  group_by(year, state_status) |>
  summarize(mean = mean(log(est_median_fam_inc), na.rm = TRUE)) |>
  pivot_wider(names_from = year, values_from = mean) |>
  gt() |>
  fmt_number(decimals = 3) |> 
  cols_label(state_status = "State Status") |> 
  tab_spanner(label = "Year", columns = 2:3) |>
  tab_header("Change in Log Median Family Income")


summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)


ols_model_list <- list(
  "OLS Short Regression Model" = lm1,
  "OLS Regression Model with Unemployment Controls" = lm2,
  "OLS Regression Model with Rural/Urban Controls" = lm3,
  "OLS Long Regression Model" = lm4
)

coef_newnames <- c(
  "as.factor(state_status)Implemented ACA" = "States where ACA was Adopted",
  "as.factor(ACA_status)Post-ACA" = "Post-ACA Federally Passed",
  "as.factor(state_status)Implemented ACA:as.factor(ACA_status)Post-ACA" = "Effect of Post-ACA Federal Passage on States where ACA was Implemented",
  "unemployed" = "Unemployment Rate (%)",
  "RUCA_code" = "RUCA Designation",
  "ACA_exp" = "ACA Expansion"
)
library(modelsummary)
OLS_models <- modelsummary(ols_model_list,
                           stars = TRUE,
                           coef_rename = coef_newnames,
                           gof_map = c("r.squared", "rmse"),
                           output = "gt")
OLS_models <- OLS_models |>
  tab_header(title = "Model Results for Log Median Family Income") |> 
  fmt_number(columns = everything(),
             decimals = 2)

gtsave(OLS_models, "OLS_models.png", vwidth = 1500, vheight = 1000)

library(rsample)
library(glmnet)
library(glmnetUtils)

fqhc_census_split1 <- initial_split(fqhc_census |> filter(!is.na(region) & year == 2011), prop = 0.8)
fqhc_census_split2 <- initial_split(fqhc_census |> filter(!is.na(region) & year == 2019), prop = 0.8)



fqhc_census_train <- rbind(training(fqhc_census_split1),training(fqhc_census_split2))
fqhc_census_test  <- rbind(testing(fqhc_census_split1),testing(fqhc_census_split2))


fit_lm1 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status), data = fqhc_census_train)
fit_lm2 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status) + unemployed, data = fqhc_census_train)
fit_lm3 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status) + RUCA_code, data = fqhc_census_train)
fit_lm4 <- lm(log(est_median_fam_inc) ~  as.factor(state_status)*as.factor(ACA_status) + RUCA_code + unemployed, 
                data = fqhc_census_train)


fqhc_train <- fqhc_census_train |> 
  mutate(
    set = "train",
    pred_lm1 = predict(fit_lm1, fqhc_census_train),
    pred_lm2 = predict(fit_lm2, fqhc_census_train),
    pred_lm3 = predict(fit_lm3, fqhc_census_train),
    pred_lm4 = predict(fit_lm4, fqhc_census_train))


## Test
fqhc_test <- fqhc_census_test |> 
  mutate(
    set = "test",
    pred_lm1 = predict(fit_lm1, fqhc_census_test),
    pred_lm2 = predict(fit_lm2, fqhc_census_test),
    pred_lm3 = predict(fit_lm3, fqhc_census_test),
    pred_lm4 = predict(fit_lm4, fqhc_census_test))


## RMSE
tbl_rmse <- bind_rows(fqhc_train, fqhc_test) |> 
  pivot_longer(
    cols = starts_with("pred_"), 
    names_to = "model", names_prefix = "pred_", values_to = "pred") |> 
  summarize(RMSE = sqrt(mean((log(est_median_fam_inc) - pred)^2)),
            .by = c(set, model)) # shorthand for group_by(set, model) |> 


## formatting
rmse_wide <- tbl_rmse |> 
  pivot_wider(id_cols = model, names_from = set, values_from = RMSE, names_prefix = "RMSE_") |> 
  mutate(model = recode_factor(model, 
                               lm1 = "Short Reg",
                               lm2 = "Short Reg with Unemployment Controls",
                               lm3 = "Short Reg with Rural/Urban Controls",
                               lm4 = "Long Reg")) |> 
  arrange(model) |> 
  relocate(model, RMSE_train, RMSE_test)

rmse_table <- rmse_wide |> 
  gt() |> 
  fmt_number(decimals = 3) |> 
  cols_label(model = "OLS Models", RMSE_train = "Training", RMSE_test = "Testing") |> 
  tab_spanner(label = "RMSE", columns = 2:3) |>
  tab_header("Testing and Training of OLS Models")
gtsave(rmse_table, "rmse_table.png", vwidth = 1500, vheight = 1000 )
