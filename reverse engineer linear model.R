set.seed(12112025)

required_pkgs <- c("tidyverse", "broom", "glue", "zip")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(broom)
library(glue)

n_total       <- 160        
n_subsets     <- 8          
n_per_subset  <- n_total / n_subsets  
x_min         <- 0
x_max         <- 10
true_beta0    <- 2.0        
true_beta1    <- 0.5         
true_sigma    <- 1.0        
out_dir       <- "linear_challenge_one_model"


student_plots_dir <- file.path(out_dir, "student_plots")
demo_plots_dir    <- file.path(out_dir, "demo_plots")
dir.create(student_plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(demo_plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

x_base <- seq(x_min, x_max, length.out = n_total)
x <- x_base + rnorm(n_total, mean = 0, sd = (x_max - x_min) * 0.02)
y <- true_beta0 + true_beta1 * x + rnorm(n_total, mean = 0, sd = true_sigma)

full_data <- tibble(point_id = seq_len(n_total), x = x, y = y)


full_fit <- lm(y ~ x, data = full_data)
full_fit_tidy <- broom::tidy(full_fit)
full_fit_glance <- broom::glance(full_fit)


shuffled <- full_data %>% sample_frac(1)
shuffled <- shuffled %>% mutate(subset_id = rep(paste0("D", seq_len(n_subsets)), each = n_per_subset))

subset_list <- shuffled %>%
  group_by(subset_id) %>%
  group_split(.keep = TRUE) %>%
  set_names(paste0("D", seq_len(n_subsets)))

student_handout <- shuffled %>% arrange(subset_id, point_id) %>% select(subset_id, x, y)
write_csv(student_handout, file.path(out_dir, "student_handout.csv"))


plot_student_points <- function(df, out_file) {
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(size = 2, alpha = 0.9) +
    labs(title = glue("Dataset {unique(df$subset_id)}"),
         subtitle = "",
         x = "X",
         y = "Y")+
    theme_minimal(base_size = 14)
  ggsave(outfile <- out_file, plot = p, width = 5, height = 4, dpi = 300)
  invisible(outfile)
}

plot_demo_full_model <- function(df, full_fit, out_file) {
  coefs <- coef(full_fit)
  intercept <- round(coefs[1], 3)
  slope <- round(coefs[2], 3)
  rsq <- round(summary(full_fit)$r.squared, 3)
  eq_text <- glue("Full-data fit: y = {intercept} + {slope} x   (R² = {rsq})")
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(size = 2, alpha = 0.9) +
    # draw full model line across x range
    geom_abline(intercept = coefs[1], slope = coefs[2], linetype = "solid", size = 0.9) +
    labs(title = glue("Dataset {unique(df$subset_id)}"),
         subtitle = "Demonstrator version: points + full-data fitted line",
         x = "X",
         y = "Y",
         caption = eq_text) +
    theme_minimal(base_size = 14)
  ggsave(out_file, plot = p, width = 5, height = 4, dpi = 300)
  invisible(out_file)
}


for (name in names(subset_list)) {
  df <- subset_list[[name]]
  student_png <- file.path(student_plots_dir, paste0(name, ".png"))
  demo_png    <- file.path(demo_plots_dir, paste0(name, ".png"))
  plot_student_points(df, student_png)
  df_demo <- df %>% mutate(dataset_id = subset_id)
  plot_demo_full_model(df_demo, full_fit, demo_png)
}


full_model_params <- tibble(
  model = "full_data_fit",
  true_beta0 = true_beta0,
  true_beta1 = true_beta1,
  true_sigma = true_sigma,
  fitted_intercept = full_fit_tidy$estimate[full_fit_tidy$term == "(Intercept)"],
  intercept_se = full_fit_tidy$std.error[full_fit_tidy$term == "(Intercept)"],
  fitted_slope = full_fit_tidy$estimate[full_fit_tidy$term == "x"],
  slope_se = full_fit_tidy$std.error[full_fit_tidy$term == "x"],
  t_slope = full_fit_tidy$statistic[full_fit_tidy$term == "x"],
  p_slope = full_fit_tidy$p.value[full_fit_tidy$term == "x"],
  r_squared = full_fit_glance$r.squared,
  adj_r_squared = full_fit_glance$adj.r.squared
)

subset_summaries <- map_dfr(names(subset_list), function(name) {
  df <- subset_list[[name]]
  fit_sub <- lm(y ~ x, data = df)  
  tidy_sub <- broom::tidy(fit_sub)
  glance_sub <- broom::glance(fit_sub)
  tibble(
    subset_id = name,
    n = nrow(df),
    x_mean = mean(df$x),
    x_sd = sd(df$x),
    y_mean = mean(df$y),
    y_sd = sd(df$y),
    subset_fitted_intercept = tidy_sub$estimate[tidy_sub$term == "(Intercept)"],
    subset_intercept_se = tidy_sub$std.error[tidy_sub$term == "(Intercept)"],
    subset_fitted_slope = tidy_sub$estimate[tidy_sub$term == "x"],
    subset_slope_se = tidy_sub$std.error[tidy_sub$term == "x"],
    subset_t_slope = tidy_sub$statistic[tidy_sub$term == "x"],
    subset_p_slope = tidy_sub$p.value[tidy_sub$term == "x"],
    subset_r_squared = glance_sub$r.squared,
    subset_rmse = sqrt(mean(residuals(fit_sub)^2))
  )
})

demonstrator_answers <- full_model_params %>%
  mutate(dummy = 1) %>%
  left_join(tibble(dummy = 1, note = "Full-model fit (80 points) used for demonstrator reveal"), by = "dummy") %>%
  select(-dummy) %>%
  bind_rows(tibble()) %>%  # ensures it's a tibble before binding
  bind_rows(subset_summaries %>% mutate(note = NA))  # append subset info

write_csv(demonstrator_answers, file.path(out_dir, "demonstrator_answers.csv"))


model_object <- list(
  created = Sys.time(),
  seed = 12112025,
  generation_params = list(n_total = n_total, n_subsets = n_subsets,
                           n_per_subset = n_per_subset, x_min = x_min, x_max = x_max,
                           true_beta0 = true_beta0, true_beta1 = true_beta1, true_sigma = true_sigma),
  full_data = full_data,
  shuffled_and_split = shuffled,
  subset_list = subset_list,
  full_fit = full_fit,
  demonstrator_answers = demonstrator_answers
)

saveRDS(model_object, file.path(out_dir, "model_object.rds"))