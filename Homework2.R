# STAT 184 - Homework 2

install.packages(c("dplyr", "tidyr", "tibble", "readr", "stringr", "forcats", "purrr"))

library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)


# ===================== PROBLEM 1 =====================

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)


# 1A: str_squish removes leading/trailing spaces and extra internal spaces.
# parse_number extracts numbers from currency and percent strings.
# A percent is divided by 100 to become a proportion.
sales_clean <- sales_raw |>
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )
print(sales_clean |> select(sale_id, item_clean, amount, discount_rate, net_amount, size))
# Output: net_amount by sale is 10, 18, NA, 15, 24, 22.5, NA, 20.
# Sizes: small, small, unrecorded, small, large, large, unrecorded, large.


# 1B: ^ and $ require the entire string to match the pattern.
sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )
invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
print(invalid_codes)
# Invalid sale IDs: 5 (bad) and 7 (ON-107X).
valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-", names = c("channel", "number")) |>
  select(sale_id, code_raw, channel, number)
print(valid_code_parts)
sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
print(sale_tags)
# separate_wider_delim splits a string into new columns, whereas
# separate_longer_delim splits a string into multiple rows.


# 1C: sum of a logical condition counts TRUE values; na.rm ignores NA.
# If there are no known amounts, return NA_real_ instead of dividing by zero.
customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(!is.na(net_amount) & net_amount >= 20),
    large_share = if (sum(!is.na(net_amount)) == 0) NA_real_
    else sum(!is.na(net_amount) & net_amount >= 20) /
      sum(!is.na(net_amount)),
    .by = customer_id
  ) |>
  arrange(customer_id)
print(customer_summary)
# C1's numerator is 0 large known sales; its denominator is 1 known sale.
# C1's other sale has missing amount, so large_share = 0 / 1 = 0.
# Customer summary: C1 (2,1,0,0), C2 (3,3,2,2/3),
# C3 (1,1,0,0), C4 (1,1,1,1), C5 (1,0,0,NA).
sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )
print(levels(sales_factors$item_factor))
print(table(sales_factors$item_factor))
print(levels(sales_factors$item_group))
print(table(sales_factors$item_group))
# Counts: pen 4, folder 2, notebook 1, eraser 1.
# item_factor changes the order of categories based on frequency
# without removing or combining any categories.
# item_group keeps pen and folder as separate categories
# and combines notebook and eraser into "Other".


# ===================== PROBLEM 2 =====================
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)


# 2A: Fill customer IDs only within their own form.
reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()
reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )
print(reports_clean |>
        select(form, line, customer_id, round, primary_amount, backup_amount, amount))
# Amounts: 10, 15, 20, NA, 0, 30.
# coalesce keeps the primary value whenever it is present, including 0.
# A backup is used only when the cleaned primary is NA.
reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))
print(reports_complete)
original_n <- nrow(reports_clean)
added_n <- sum(!reports_complete$recorded_row)
missing_before <- sum(is.na(reports_clean$amount))
missing_after <- sum(is.na(reports_complete$amount))
print(tibble(original_rows = original_n, added_rows = added_n,
             missing_before = missing_before, missing_after = missing_after))
# 6 original rows, 6 added rows, 1 missing amount before, 7 after.
# Explicit missing value: C3, round 1 (record exists but amount is NA).
# Implicit missing value: C1, round 2 (no original row exists).
# complete() added the missing customer-round combination.

# 2B: Check metadata keys and customer-round duplicates.
customer_key_check <- tibble(
  duplicate_ids = sum(duplicated(customers$customer_id)),
  missing_ids = sum(is.na(customers$customer_id))
)
print(customer_key_check)
report_duplicate_keys <- reports_clean |>
  count(customer_id, round, name = "records") |>
  filter(records > 1)
print(report_duplicate_keys)
# No missing or duplicate customer metadata IDs; no repeated customer-round keys.
customers_marked <- customers |>
  mutate(customer_found = TRUE)
reports_joined <- reports_clean |>
  left_join(customers_marked, by = join_by(customer_id),
            relationship = "many-to-one", na_matches = "never") |>
  mutate(customer_found = coalesce(customer_found, FALSE))
print(reports_joined |> select(customer_id, round, amount, region, customer_found))
only_in_reports <- reports_clean |>
  distinct(customer_id) |>
  anti_join(customers |> distinct(customer_id),
            by = join_by(customer_id), na_matches = "never")
only_in_customers <- customers |>
  distinct(customer_id) |>
  anti_join(reports_clean |> distinct(customer_id),
            by = join_by(customer_id), na_matches = "never")
print(only_in_reports)
print(only_in_customers)
# Only in reports: C4. Only in customers: C5.
# C3 has customer_found = TRUE even though its region is NA: a matching
# customer exists, but its region is missing.
# C4 has customer_found = FALSE and region NA: no matching customer exists.

# 2C
# The homework PDF references customers_bad but does not
# provide its definition. This example adds a second C2
# record with a conflicting region to demonstrate
# how a many-to-many join can duplicate report amounts.
customers_bad <- bind_rows(
  customers,
  tibble(customer_id = "C2", region = "East")
)
reports_duplicated <- reports_clean |>
  left_join(customers_bad, by = join_by(customer_id),
            relationship = "many-to-many")
join_comparison <- tibble(
  stage = c("Before join", "After join"),
  rows = c(nrow(reports_clean), nrow(reports_duplicated)),
  total_amount = c(sum(reports_clean$amount, na.rm = TRUE),
                   sum(reports_duplicated$amount, na.rm = TRUE))
)
print(join_comparison)
print(reports_duplicated |> count(customer_id, round, name = "records") |>
        filter(records > 1))
# For this illustrative customers_bad: rows rise from 6 to 7 and total
# rises from 75 to 95. 
# C2 round 1 appears twice after the join.
# Its amount of 20 is counted twice in the joined total,
# instead of once, increasing the total by 20.
# With relationship = "many-to-one", this join would error because C2
# matches two customer rows. Allowing many-to-many avoids the error but
# does not resolve the conflicting regions (South vs East).
reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, values_from = amount,
              names_prefix = "round_")
print(reports_wide)
reports_long_nonmissing <- reports_wide |>
  pivot_longer(cols = starts_with("round_"),
               names_to = "round", values_to = "amount",
               values_drop_na = TRUE)
print(nrow(reports_long_nonmissing))
# 5 rows remain after dropping missing values (one for each known amount).
# Dropping NA rows cannot recover amounts that were never recorded.


# ===================== PROBLEM 3 =====================
# 3A: Manually calculate pooled or Welch two-sample t-test statistics.
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  if (equal_var) {
    pooled_var <- ((nx - 1) * var_x + (ny - 1) * var_y) / (nx + ny - 2)
    se <- sqrt(pooled_var * (1 / nx + 1 / ny))
    df <- nx + ny - 2
    method <- "Pooled"
  } else {
    se <- sqrt(var_x / nx + var_y / ny)
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) + (var_y / ny)^2 / (ny - 1))
    method <- "Welch"
  }
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  list(method = method, t_stat = t_stat, df = df,
       p_value = p_value, reject = p_value < alpha)
}
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)
welch_own <- two_sample_t(x_check, y_check)
pooled_own <- two_sample_t(x_check, y_check, equal_var = TRUE)
print(welch_own)
print(pooled_own)
welch_ref <- t.test(x_check, y_check, var.equal = FALSE)
pooled_ref <- t.test(x_check, y_check, var.equal = TRUE)
verification <- tibble(
  method = c("Welch", "Pooled"),
  t_diff = c(abs(welch_own$t_stat - unname(welch_ref$statistic)),
             abs(pooled_own$t_stat - unname(pooled_ref$statistic))),
  df_diff = c(abs(welch_own$df - unname(welch_ref$parameter)),
              abs(pooled_own$df - unname(pooled_ref$parameter))),
  p_diff = c(abs(welch_own$p_value - welch_ref$p.value),
             abs(pooled_own$p_value - pooled_ref$p.value))
) |>
  mutate(all_below_1e8 = t_diff < 1e-8 & df_diff < 1e-8 & p_diff < 1e-8)
print(verification)

# Welch: t = 3.403072, df = 7.678614, p = 0.009907123.
# Pooled: t = 3.526436, df = 11, p = 0.004744470.
# All differences compared with t.test() are below 1e-8.
# Both methods match t.test() within 1e-8 for t, df, and p-value.


# 3B: Same independently drawn x and y samples are used for both tests.
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05
scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null", "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)
set.seed(2026)
# Allocate the full storage before starting the nested loops.
simulation_rows <- vector("list", nrow(scenarios) * B * 2)
k <- 1L
for (i in seq_len(nrow(scenarios))) {
  for (rep in seq_len(B)) {
    x <- rnorm(n_x, mean = scenarios$mean_x[i], sd = scenarios$sd_x[i])
    y <- rnorm(n_y, mean = scenarios$mean_y[i], sd = scenarios$sd_y[i])
    for (use_equal_var in c(FALSE, TRUE)) {
      test_result <- two_sample_t(x, y, equal_var = use_equal_var, alpha = alpha)
      simulation_rows[[k]] <- data.frame(
        scenario = scenarios$scenario[i],
        replicate = rep,
        method = test_result$method,
        t_stat = test_result$t_stat,
        df = test_result$df,
        p_value = test_result$p_value,
        reject = test_result$reject
      )
      k <- k + 1L
    }
  }
}
simulation_results <- bind_rows(simulation_rows)
print(head(simulation_results, 6))
print(nrow(simulation_results))
# 4 scenarios x 1000 repetitions x 2 methods = 8000 rows.
simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  arrange(scenario, method)
print(simulation_summary |> mutate(across(c(mean_t, sd_t, rejection_rate),
                                          ~ round(.x, 3))))

# Simulation results (Pooled, Welch rejection rates):
# equal_null: 0.038, 0.044
# unequal_null: 0.139, 0.061
# equal_shift: 0.957, 0.956
# unequal_shift: 0.497, 0.282
# Optional plot of simulated rejection rates.
plot_data <- simulation_summary
barplot(plot_data$rejection_rate, names.arg = paste(plot_data$scenario,
                                                    plot_data$method, sep = "\n"), las = 2, cex.names = 0.65,
        ylab = "Rejection rate", main = "Two-sample t-test simulation")
abline(h = 0.05, lty = 2)

# Short Report:
# Under equal means and variances, rejection rates were 0.038
# for Pooled and 0.044 for Welch, both close to 0.05.
# With unequal variances but equal means, Pooled rejected
# 0.139 of the time versus 0.061 for Welch, showing an
# inflated Type I error rate for Pooled.
# When the means differed, power increased, reaching
# 0.957 and 0.956 under equal variances and 0.497
# and 0.282 under unequal variances for Pooled and Welch.
# Higher power alone does not make a test better because
# it must also control false positives, and small
# differences can result from random simulation variation.