# ==============================================================================
# 02_analysis.R
# Purpose: Descriptive stats, plots, and OLS regressions with fixed effects
#          using the merged aid + V-Dem panel dataset.
# Run AFTER 01_merge_aid_vdem.R
# ==============================================================================

# --- 0. Install packages if needed --------------------------------------------
# Run once if any of these are missing.
# install.packages(c("dplyr", "readr", "ggplot2", "fixest", "modelsummary", "gt"))

library(readr)        # Fast CSV reading
library(dplyr)        # Data wrangling
library(ggplot2)      # Plotting
library(fixest)       # Fast OLS with fixed effects (feols)
library(modelsummary) # Side-by-side regression tables


# ==============================================================================
# STEP 1: Load the merged dataset
# ==============================================================================

data_path <- "C:/Users/Patrick Shea/Dropbox/AI Upskilling/Walkthru/april26session/CC_example/aid_vdem_merged.csv"

df <- read_csv(data_path, show_col_types = FALSE)

cat("Loaded", nrow(df), "country-year observations\n")


# ==============================================================================
# STEP 2: Prepare variables
# ==============================================================================

df <- df %>%
  mutate(
    # Log-transform GDP per capita and population for regressions.
    # We add a small check: log() of zero or negative is undefined, so any
    # non-positive values become NA rather than crashing.
    log_gdppc = ifelse(e_gdppc > 0,  log(e_gdppc), NA),
    log_pop   = ifelse(e_pop   > 0,  log(e_pop),   NA),

    # Classify countries as high vs. low democracy based on the
    # sample-wide median of v2x_polyarchy (computed ignoring NAs).
    demo_group = ifelse(
      v2x_polyarchy >= median(v2x_polyarchy, na.rm = TRUE),
      "High democracy",
      "Low democracy"
    )
  )


# ==============================================================================
# STEP 3: Descriptive statistics
# ==============================================================================

# Define a helper that computes mean, SD, min, max, and N for one variable.
desc_one <- function(x, label) {
  tibble(
    Variable = label,
    Mean     = round(mean(x, na.rm = TRUE), 3),
    SD       = round(sd(x,   na.rm = TRUE), 3),
    Min      = round(min(x,  na.rm = TRUE), 3),
    Max      = round(max(x,  na.rm = TRUE), 3),
    N        = sum(!is.na(x))
  )
}

desc_table <- bind_rows(
  desc_one(df$oda_pct_gni,    "Aid (% of GNI)"),
  desc_one(df$v2x_polyarchy,  "Electoral democracy (V-Dem)"),
  desc_one(df$v2x_corr,       "Political corruption (V-Dem)"),
  desc_one(df$e_gdppc,        "GDP per capita (2011 USD PPP)")
)

cat("\n========== DESCRIPTIVE STATISTICS ==========\n")
print(desc_table)


# ==============================================================================
# STEP 4: Plots
# ==============================================================================

# ---- Plot 1: Scatter of aid vs. democracy with a loess smoother -------------

# Drop rows with missing values on either axis so ggplot doesn't warn.
scatter_data <- df %>% filter(!is.na(oda_pct_gni), !is.na(v2x_polyarchy))

p1 <- ggplot(scatter_data, aes(x = v2x_polyarchy, y = oda_pct_gni)) +
  # Semi-transparent points to handle overplotting
  geom_point(alpha = 0.15, size = 0.8, color = "steelblue") +
  # Loess smoother with 95% confidence ribbon
  geom_smooth(method = "loess", color = "firebrick", se = TRUE) +
  # Keep y-axis sensible: most aid values are small but a few are extreme
  coord_cartesian(ylim = c(0, quantile(scatter_data$oda_pct_gni, 0.99, na.rm = TRUE))) +
  labs(
    title = "Aid dependency and electoral democracy",
    x     = "Electoral democracy (V-Dem v2x_polyarchy, 0–1)",
    y     = "Net ODA received (% of GNI)",
    caption = "Loess smoother with 95% CI. Y-axis trimmed at 99th percentile."
  ) +
  theme_bw()

ggsave(
  filename = "C:/Users/Patrick Shea/Dropbox/AI Upskilling/Walkthru/april26session/CC_example/plot1_aid_democracy_scatter.png",
  plot   = p1,
  width  = 7, height = 5, dpi = 150
)
cat("Saved plot1_aid_democracy_scatter.png\n")


# ---- Plot 2: Time trend of average aid by democracy group -------------------

# Compute the group-year mean, dropping NAs on both aid and the group label.
trend_data <- df %>%
  filter(!is.na(oda_pct_gni), !is.na(demo_group)) %>%
  group_by(year, demo_group) %>%
  summarise(mean_aid = mean(oda_pct_gni, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(trend_data, aes(x = year, y = mean_aid, color = demo_group, group = demo_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("High democracy" = "steelblue", "Low democracy" = "firebrick")) +
  labs(
    title   = "Average aid over time by democracy level",
    x       = "Year",
    y       = "Mean net ODA received (% of GNI)",
    color   = NULL,
    caption = "Split at sample-wide median of v2x_polyarchy."
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  filename = "C:/Users/Patrick Shea/Dropbox/AI Upskilling/Walkthru/april26session/CC_example/plot2_aid_trend_by_democracy.png",
  plot   = p2,
  width  = 7, height = 5, dpi = 150
)
cat("Saved plot2_aid_trend_by_democracy.png\n")


# ==============================================================================
# STEP 5: OLS regressions with country + year fixed effects
# ==============================================================================

# feols() from the fixest package is the standard tool for high-dimensional
# fixed effects. The formula syntax is:
#   outcome ~ controls | fixed_effect_1 + fixed_effect_2
#
# Standard errors are clustered by country by default when a panel ID is
# present; here we cluster explicitly with cluster = ~iso3c.

# Model 1: Aid ~ Democracy + log(GDP/capita) + log(population) + FE
m1 <- feols(
  oda_pct_gni ~ v2x_polyarchy + log_gdppc + log_pop | iso3c + year,
  data    = df,
  cluster = ~iso3c   # Cluster SEs by country to account for serial correlation
)

# Model 2: Aid ~ Corruption + log(GDP/capita) + log(population) + FE
m2 <- feols(
  oda_pct_gni ~ v2x_corr + log_gdppc + log_pop | iso3c + year,
  data    = df,
  cluster = ~iso3c
)

# Quick console summary of each model
summary(m1)
summary(m2)


# ==============================================================================
# STEP 6: Regression table (side by side)
# ==============================================================================

# modelsummary() produces clean tables from a named list of model objects.
# We customize: rename coefficients, add goodness-of-fit rows, save as HTML.

# Map internal variable names to readable labels
coef_labels <- c(
  "v2x_polyarchy" = "Electoral democracy",
  "v2x_corr"      = "Political corruption",
  "log_gdppc"     = "Log GDP per capita",
  "log_pop"       = "Log population"
)

# Which goodness-of-fit stats to show in the table
gof_rows <- c("nobs", "r.squared", "adj.r.squared")

modelsummary(
  models      = list("Model 1: Democracy" = m1,
                     "Model 2: Corruption" = m2),
  coef_map    = coef_labels,        # Only show these rows, in this order
  gof_map     = gof_rows,
  stars       = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes       = "Country and year fixed effects included. SEs clustered by country.",
  output      = "C:/Users/Patrick Shea/Dropbox/AI Upskilling/Walkthru/april26session/CC_example/regression_table.html"
)

cat("Saved regression_table.html\n")


# ==============================================================================
# STEP 7: Substantive interpretation
# ==============================================================================

cat("\n========== INTERPRETATION ==========\n")
cat("
The regression results indicate whether democracy or corruption systematically
predict aid receipts within countries over time, controlling for economic size
and population. A negative coefficient on democracy would suggest that donors
reward autocrats with more aid, or reduce aid as countries democratize; a
positive coefficient on corruption would suggest aid flows to more corrupt
states — patterns consistent with donor strategic interests overriding
governance conditionality. The most important endogeneity concern is reverse
causality: aid itself may undermine democracy or entrench corruption rather
than simply responding to them, so these OLS estimates should be read as
associations rather than causal effects; instrumental variables or a
difference-in-differences design exploiting donor-side shocks would be needed
to establish causality.
\n")
