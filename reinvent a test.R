
set.seed(12112025)
required_pkgs <- c("tidyverse", "broom")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")
library(tidyverse)
library(broom)

n_datasets <- 8              
n_per_group <- 10           
base_mean <- 10              
base_sd <- 2                
out_dir <- "fake_data_output"
        


dataset_specs <- tibble(
  dataset_id = paste0("D", seq_len(n_datasets)),
  effect_size = c(0, 0.8, 0, 1.5, 0.4, 0, 2.0, 0.6),
  sd_A = c(base_sd, base_sd, base_sd*1.8, base_sd, base_sd, base_sd*2.5, base_sd, base_sd),
  sd_B = c(base_sd, base_sd, base_sd*1.8, base_sd*1.2, base_sd, base_sd*2.5, base_sd*1.5, base_sd)
) %>%
  mutate(
    true_mean_A = base_mean,
    true_mean_B = base_mean + effect_size,
    demonstrator_note = case_when(
      effect_size == 0 & sd_A > base_sd ~ "No effect, high variance (tricky)",
      effect_size == 0 ~ "No effect, typical variance",
      effect_size > 1.5 ~ "Large effect",
      effect_size > 0.7 ~ "Medium effect",
      effect_size > 0 ~ "Small effect",
      TRUE ~ "Other"
    )
  )


make_dataset <- function(spec_row, n = n_per_group) {
  id <- spec_row$dataset_id
  A <- rnorm(n, mean = spec_row$true_mean_A, sd = spec_row$sd_A)
  B <- rnorm(n, mean = spec_row$true_mean_B, sd = spec_row$sd_B)
  tibble(
    dataset_id = id,
    group = rep(c("A","B"), each = n),
    value = c(A, B)
  )
}

all_data <- dataset_specs %>%
  group_split(dataset_id) %>%
  map_dfr(~ make_dataset(.x, n = n_per_group))


student_handout <- all_data %>%
  arrange(dataset_id, group) %>%  
  mutate(row = row_number()) %>%
  select(-row)

dir.create(out_dir, showWarnings = FALSE)
write_csv(student_handout, file.path(out_dir, "student_handout.csv"))


answers <- all_data %>%
  group_by(dataset_id, group) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = group, values_from = c(mean, sd, n), names_sep = "_") %>%
  left_join(dataset_specs %>% select(dataset_id, effect_size, demonstrator_note, true_mean_A, true_mean_B), by = "dataset_id")

t_test_results <- all_data %>%
  group_by(dataset_id) %>%
  nest() %>%
  mutate(tt = map(data, ~ t.test(value ~ group, data = .x, var.equal = FALSE) ),
         tt_tidy = map(tt, broom::tidy),
         conf_int = map(tt, broom::glance)) %>%
  unnest(tt_tidy) %>%
  select(dataset_id, estimate, statistic, p.value, conf.low, conf.high, method)

demonstrator_answers <- answers %>%
  left_join(t_test_results, by = "dataset_id") %>%
  rename(
    mean_A = mean_A, mean_B = mean_B,
    sd_A = sd_A, sd_B = sd_B
  ) %>%
  select(dataset_id, demonstrator_note, effect_size, true_mean_A, true_mean_B,
         mean_A, sd_A, n_A = n_A, mean_B, sd_B, n_B = n_B,
         estimate, statistic, p.value, conf.low, conf.high, method)

write_csv(demonstrator_answers, file.path(out_dir, "demonstrator_answers.csv"))

