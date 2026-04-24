## data visualization figure drafts ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(purrr)
library(rcompanion)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

## old veg data ##

VMMI_FOA_NETN <- read.csv("data/processed_data/vegetation_data/FOA_NETN_VMMI_2011_2024.csv") %>%
  as_tibble()

species_list <- read.csv("data/processed_data/vegetation_data/FOA_NETN_spplist_2011_2024.csv")

## new veg data ##

vmmi <- read.csv("data/processed_data/vegetation_data/vis_FOA_NETN_VMMI_2011_2025_20260324.csv")

## water level stats data ##

wl_stats <- read.csv("data/processed_data/hydrology_data/gm_gl_wl_stats_2025_20260304.csv") %>% 
  select(year, stat, `Gilmore Meadow 1` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(wetland = if_else(grepl("Great Meadow", site), "Great Meadow", "Gilmore Meadow"))


#----------------------------#
####    Plot Generation   #### 
#----------------------------#

# generic plot of VMMI values by site over time
ggplot(VMMI_FOA_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "VMMI Trends Over Time by Site",
       x = "Year",
       y = "VMMI",
       color = "Site Name") +
  theme_minimal()

# facet by wetland
ggplot(VMMI_FOA_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ wetland) +  # Facet by wetland
  labs(title = "VMMI Trends Over Time by Site and Wetland",
       x = "Year",
       y = "VMMI",
       color = "Site") +
  theme_minimal()

# facet by wetland with threshold VMMI rankings
ggplot(VMMI_FOA_NETN, aes(x = year, y = vmmi, color = site.name, group = site.name)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 65.22746, linetype = "dashed", color = "darkgreen") +  # Good
  geom_hline(yintercept = 52.785, linetype = "dashed", color = "red") +          # Poor
  facet_wrap(~ wetland) +
  labs(title = "VMMI Trends Over Time by Site and Wetland",
       x = "Year",
       y = "VMMI",
       color = "Site") +
  theme_minimal()


# plot for average VMMI trends per wetland
# first calculate average VMMI per year per wetland
wetland_summary <- VMMI_FOA_NETN %>%
  group_by(wetland, year) %>%
  summarise(mean_vmmi = mean(vmmi, na.rm = TRUE))

# plot
ggplot(wetland_summary, aes(x = year, y = mean_vmmi, color = wetland, group = wetland)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average VMMI Trends by Wetland",
       x = "Year",
       y = "Mean VMMI",
       color = "Wetland") +
  theme_minimal()


# plot box plots for summarizing VMMI per year across all sites 
ggplot(VMMI_FOA_NETN, aes(x = factor(year), y = vmmi)) +
  geom_boxplot() +
  facet_wrap(~ wetland) + 
  labs(title = "Distribution of VMMI by Year",
       x = "Year", 
       y = "VMMI") +
  theme_light()


# significance testing ---------------------------------------------------------

site_models <- VMMI_FOA_NETN %>%
  group_by(site.name) %>%
  do(tidy(lm(vmmi ~ year, data = .)))

site_models %>% filter(term == "year")


# residuals
models_nested <- VMMI_FOA_NETN %>%
  group_by(site.name) %>%
  nest() %>%
  mutate(model = map(data, ~lm(vmmi ~ year, data = .)))

# Use `glance()` on each model to get model-level stats (e.g., R², p-value)
model_summaries <- models_nested %>%
  mutate(glance_out = map(model, glance)) %>%
  unnest(glance_out)

# stats testing Gilmore vs Great Meadow

wet1 <- VMMI_FOA_NETN$vmmi[VMMI_FOA_NETN$wetland == "Great Meadow"]
wet2 <- VMMI_FOA_NETN$vmmi[VMMI_FOA_NETN$wetland == "Gilmore Meadow"]

# normality
shapiro.test(wet1)
shapiro.test(wet2)

# equal variances
var.test(wet1, wet2)  # F-test
  # sample sizes are not equal; variances are NOT equal

# non-parametric test (doesn’t assume normality or equal variances)
# Mann–Whitney U test for independent samples
wilcox.test(wet1, wet2, exact = FALSE)

# calculate rank-biserial correlation (effect size)
# Combine data
values <- c(wet1, wet2)
group <- c(rep("Wetland1", length(wet1)), rep("Wetland2", length(wet2)))
# Compute rank-biserial correlation
wilcoxonR(x = values, g = group)


# Combine data into one dataframe
df_plot <- data.frame(
  VMMI = c(wet1, wet2),
  Wetland = c(rep("Wetland 1", length(wet1)), rep("Wetland 2", length(wet2)))
)

# Run Wilcoxon test
test <- wilcox.test(wet1, wet2, exact = FALSE)

# Calculate effect size (r)
p <- test$p.value
N <- length(wet1) + length(wet2)
r <- abs(qnorm(p / 2)) / sqrt(N)

# Create boxplot
p_plot <- ggplot(df_plot, aes(x = Wetland, y = VMMI, fill = Wetland)) +
  geom_boxplot(alpha = 0.7, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "VMMI Comparison Between Wetlands",
       subtitle = paste0("Wilcoxon p = ", signif(test$p.value, 3),
                         " | Effect size r = ", round(r, 3),
                         " (", ifelse(r < 0.3, "Small",
                                      ifelse(r < 0.5, "Medium", "Large")), ")"),
       y = "VMMI Score",
       x = "")

# Display plot
print(p_plot)



# graph invasive species by year -----------------------------------------------

# number of unique species by year and wetland
summary_table <- species_list %>%
  filter(invasive == "TRUE") %>%
  group_by(year, wetland) %>%
  summarise(
    count = n_distinct(latin.name),
    invasive_species = paste(unique(latin.name), collapse = ", ")
  )

# total number of invasive species by year and wetland
invasives <- species_list %>%
  filter(invasive == TRUE) %>%
  group_by(year, wetland) %>%
  summarise(
    count = n(),
    invasive_species = paste(unique(latin.name), collapse = ", "),
    .groups = "drop"
  )

# ggplot(summary_table, aes(x = factor(year), y = count)) +
#   geom_bar(stat = "identity", fill = "tomato") +
#   labs(
#     title = "Number of Invasive Species by Year",
#     x = "Year",
#     y = "Number of Invasive Species"
#   ) +
#   theme_minimal()



####----------------------------------------------------------------------------

# Hydrograph and growing season summary stats - comparison between Great Meadow and Gilmore Meadow

# group sites by wetland
wl_grouped_stats <- wl_stats %>%
  mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))

# Calculate mean of stats by wetland
wl_stats_summary <- wl_grouped_stats %>%
  group_by(site_group) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  ungroup()

# calculate mean of stats by individual site
wl_stats_sites_summary <- wl_stats %>%
  group_by(site) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) %>%
  ungroup()


# run significance tests for comparing stats between great meadow and gilmore meadow

# Get all numeric stat columns
stat_cols <- wl_grouped_stats %>% select(where(is.numeric)) %>% names()

# Run t-tests for each variable and store p-values
t_test_results <- lapply(stat_cols, function(var) {
  formula <- as.formula(paste(var, "~ site_group"))
  test <- t.test(formula, data = wl_grouped_stats)
  data.frame(
    variable = var,
    p_value = test$p.value,
    mean_Great_Meadow = mean(wl_grouped_stats[[var]][wl_grouped_stats$site_group == "Great Meadow"], na.rm = TRUE),
    mean_Gilmore_Meadow = mean(wl_grouped_stats[[var]][wl_grouped_stats$site_group == "Gilmore Meadow"], na.rm = TRUE)
  )
})

# Combine results into a single data frame
t_test_df <- bind_rows(t_test_results)

# View
print(t_test_df)


# visualize through box-plots
ggplot(wl_grouped_stats, aes(x = site_group, y = WL_mean)) +
  geom_boxplot() +
  labs(y = "Mean Water Level (cm)", x = "Wetland Group") +
  theme_minimal()

ggplot(wl_grouped_stats, aes(x = site_group, y = WL_sd)) +
  geom_boxplot() +
  labs(y = "Mean Standard Deviation (cm)", x = "Wetland Group") +
  theme_minimal()


####----------------------------------------------------------------------------

# box and whisker plots 

# summarize VMMI by wetland ----------------------------------------------------
summary_vmmi_wetland <- VMMI_FOA_NETN %>%
  group_by(wetland) %>%
  summarise(
    mean.vmmi = mean(vmmi, na.rm = TRUE),
    mean.c = mean(mean.coc, na.rm = TRUE),
    mean.inv.cov = mean(inv.cov, na.rm = TRUE),
    mean.bryo.cov = mean(bryo.cov, na.rm = TRUE),
    mean.strol.cov = mean(strtol.cov, na.rm = TRUE),
    good.sites = n_distinct(site.name[vmmi.rating == "Good"]),
    fair.sites = n_distinct(site.name[vmmi.rating == "Fair"]),
    poor.sites = n_distinct(site.name[vmmi.rating == "Poor"]),
    .groups = "drop"
  )

# significance tests 
# Welch two-sample t-test comparing VMMI between the two wetlands
t_test_vmmi <- t.test(vmmi ~ wetland,
                      data = VMMI_FOA_NETN %>%
                        filter(wetland %in% c("Great Meadow", "Gilmore Meadow")))

t_test_vmmi <- t.test(strtol.cov ~ wetland,
                      data = VMMI_FOA_NETN %>%
                        filter(wetland %in% c("Great Meadow", "Gilmore Meadow")))

t_test_vmmi


# box and whisker plots --------------------------------------------------------
# pick variables of interest
vars_to_plot <- c("vmmi", "mean.coc", "inv.cov", "bryo.cov", "strtol.cov")

# reshape into long format
vmmi_long <- VMMI_FOA_NETN %>%
  select(wetland, all_of(vars_to_plot)) %>%
  pivot_longer(
    cols = all_of(vars_to_plot),
    names_to = "variable",
    values_to = "value"
  )

# make boxplots
# plot with faceting
ggplot(vmmi_long, aes(x = wetland, y = value, fill = wetland)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.6) +
  facet_wrap(~variable, nrow = 1, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  labs(y = "Value", x = "Wetland", title = "Boxplots of Metrics by Wetland")




#### vegetation dashboard test visualizations ####------------------------------

# plot for average VMMI trends per wetland
# first calculate average VMMI per year per wetland
wetland_vmmi <- vmmi %>%
  group_by(wetland, year) %>%
  summarise(mean_vmmi = mean(vmmi, na.rm = TRUE),
            n_sites = n_distinct(site.name))

# summary for all stats
wetland_summary <- vmmi %>%
  group_by(wetland, year) %>%
  summarise(
    across(
      c(vmmi, mean.coc, inv.cov, bryo.cov, strtol.cov),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    n_sites = n_distinct(site.name),
    .groups = "drop"
  )

# plot vmmi
ggplot(wetland_vmmi, aes(x = year, y = mean_vmmi, color = wetland, group = wetland)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  # threshold lines (no legend)
  geom_hline(yintercept = 60.94853, linetype = "dashed", color = "green4") +
  geom_hline(yintercept = 41.48136, linetype = "dashed", color = "red") +
  
  # direct labels
  annotate("text", x = max(wetland_vmmi$year), y = 60.94853,
           label = "Good", color = "green4", hjust = 1, vjust = 1.5, size = 4) +
  annotate("text", x = max(wetland_vmmi$year), y = 41.48136,
           label = "Poor", color = "red", hjust = 1, vjust = 1.5, size = 4) +
  
  scale_color_manual(values = c(
    "Great Meadow" = "black",
    "Gilmore Meadow" = "grey"
  )) +
  
  labs(
    title = "Average VMMI Trends by Wetland",
    x = "Year",
    y = "Mean VMMI",
    color = "Wetland"
  ) +
  
  theme_minimal()

# plot mean coc
ggplot(wetland_summary, aes(x = year, y = mean_mean.coc, color = wetland, group = wetland)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Site Mean COC Trends by Wetland",
       x = "Year",
       y = "Mean COC",
       color = "Wetland") +
  theme_minimal()

# plot inv cov
ggplot(wetland_summary, aes(x = year, y = mean_inv.cov, color = wetland, group = wetland)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Invasive Cover Trends by Wetland",
       x = "Year",
       y = "Mean Inv Cov",
       color = "Wetland") +
  theme_minimal()

# plot bryo cov
ggplot(wetland_summary, aes(x = year, y = mean_bryo.cov, color = wetland, group = wetland)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Bryophyte Cover Trends by Wetland",
       x = "Year",
       y = "Mean Bryo Cov",
       color = "Wetland") +
  theme_minimal()

# plot strtol cov
ggplot(wetland_summary, aes(x = year, y = mean_strtol.cov, color = wetland, group = wetland)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Stress Tolerant Species Cover Trends by Wetland",
       x = "Year",
       y = "Mean Strtol Cov",
       color = "Wetland") +
  theme_minimal()


# plot box plots for summarizing VMMI per year across all sites 
ggplot(vmmi, aes(x = factor(year), y = vmmi)) +
  geom_boxplot() +
  facet_wrap(~ wetland) + 
  labs(title = "Distribution of VMMI by Year",
       x = "Year", 
       y = "VMMI") +
  theme_light()

# plot box plots for vmmi with thresholds
ggplot(vmmi, aes(x = factor(year), y = vmmi)) +
  geom_boxplot() +
  facet_wrap(~ wetland) + 
  
  geom_hline(yintercept = 60.94853, linetype = "dashed", color = "green4") +
  geom_hline(yintercept = 41.48136, linetype = "dashed", color = "red") +
  
  annotate("text",
           x = 1, y = 60.94853,
           label = "Good",
           color = "green4",
           hjust = -0.1, vjust = -0.5, size = 3.5) +
  
  annotate("text",
           x = 1, y = 41.48136,
           label = "Poor",
           color = "red",
           hjust = -0.1, vjust = -0.5, size = 3.5) +
  
  labs(title = "Distribution of VMMI by Year",
       x = "Year", 
       y = "VMMI") +
  theme_light()

# vmmi mean with SD and n_sites -----------------------------------



#### Hydrology dashboard test visualizations ####------------------------------

## water level stats ##

# summary for all stats
wl_summary <- wl_stats %>%
  group_by(wetland, year) %>%
  summarise(
    across(
      c(WL_mean, WL_sd, WL_min, WL_max, max_inc, max_dec, prop_GS_comp, 
        GS_change, prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm),
      ~ mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    ),
    n_sites = n_distinct(site),
    .groups = "drop"
  )

# plot mean coc
ggplot(wl_summary, aes(x = year, y = mean_WL_mean, color = wetland, group = wetland)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Mean WL Trends by Wetland",
       x = "Year",
       y = "Mean WL",
       color = "Wetland") +
  theme_minimal()


# test
wl_test <- wl_stats %>%
  group_by(wetland, year) %>%
  summarise(
    across(
      c(WL_mean, WL_min, WL_max, max_inc, max_dec,
        prop_GS_comp, GS_change,
        prop_over_0cm, prop_bet_0_neg30cm, prop_under_neg30cm),
      list(avg = ~mean(.x, na.rm = TRUE),
           sd  = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    
    # separate because different concept (within-site variability)
    WL_sd_avg = mean(WL_sd, na.rm = TRUE),
    
    n_sites = n_distinct(site),
    .groups = "drop"
  )

# compute grand means
grand_means <- wl_test %>%
  group_by(wetland) %>%
  summarise(
    across(
      ends_with("_avg"),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_grand"
    ),
    .groups = "drop"
  )

# plot for Wl_mean
ggplot(wl_test, aes(x = year, y = WL_mean_avg, color = wetland, group = wetland)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # variability across sites (within each year)
  geom_errorbar(aes(ymin = WL_mean_avg - WL_mean_sd,
                    ymax = WL_mean_avg + WL_mean_sd),
                width = 0.2, alpha = 0.6) +
  
  # grand mean per wetland
  geom_hline(data = grand_means,
             aes(yintercept = grand_mean, color = wetland),
             linetype = "dashed", linewidth = 1) +
  
  labs(title = "Wetland Mean Water Level Over Time",
       x = "Year",
       y = "Mean Water Level",
       color = "Wetland") +
  
  theme_minimal()

# plot for WL_sd
ggplot(wl_test, aes(x = year, y = WL_sd_avg, color = wetland, group = wetland)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  geom_hline(data = grand_means,
             aes(yintercept = WL_sd_avg_grand, color = wetland),
             linetype = "dashed") +
  
  labs(title = "Within-Site Water Level Variability Over Time",
       x = "Year",
       y = "Mean Site-Level SD",
       color = "Wetland") +
  
  theme_minimal()

# plotting function for all stats
plot_wl_metric <- function(data, grand_data, metric, y_label, title) {
  
  avg_col   <- paste0(metric, "_avg")
  sd_col    <- paste0(metric, "_sd")
  grand_col <- paste0(metric, "_avg_grand")
  
  ggplot(data, aes(x = year, y = .data[[avg_col]], color = wetland, group = wetland)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    
    # error bars (only if SD exists)
    geom_errorbar(
      aes(ymin = .data[[avg_col]] - .data[[sd_col]],
          ymax = .data[[avg_col]] + .data[[sd_col]]),
      width = 0.2, alpha = 0.6
    ) +
    
    # grand mean line
    geom_hline(
      data = grand_data,
      aes(yintercept = .data[[grand_col]], color = wetland),
      linetype = "dashed", linewidth = 1
    ) +
    
    labs(
      title = title,
      x = "Year",
      y = y_label,
      color = "Wetland"
    ) +
    
    theme_minimal()
}

metrics <- c("WL_mean", "WL_min", "WL_max",
             "max_inc", "max_dec",
             "prop_GS_comp", "GS_change",
             "prop_over_0cm", "prop_bet_0_neg30cm", "prop_under_neg30cm")

plots <- lapply(metrics, function(m) {
  plot_wl_metric(
    wl_test,
    grand_means,
    metric = m,
    y_label = m,
    title = paste(m, "Over Time")
  )
})

