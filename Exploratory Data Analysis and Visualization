library(tidyverse)
library(janitor)
library(scales) 

# 1. Load and Clean Data
raw_data <- read_csv("export.csv", show_col_types = FALSE)

cleaned_data <- raw_data %>%
  clean_names() %>%
  mutate(across(
    total_disability_compensation_recipients:female, 
    ~ suppressWarnings(as.numeric(str_remove_all(as.character(.), ",")))
  )) %>%
  drop_na(total_disability_compensation_recipients) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

glimpse(cleaned_data)
cleaned_data %>% select(where(is.numeric)) %>% plot()

# 2. Visualization 1: Histogram of Total Recipients (Log Scale)
cleaned_data %>%
  filter(total_disability_compensation_recipients > 0) %>%
  ggplot(aes(x = total_disability_compensation_recipients)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 40) +
  scale_x_log10(labels = comma) + 
  labs(
    title = "Distribution of Total Disability Recipients per County",
    subtitle = "Note: X-axis is on a logarithmic scale",
    x = "Total Recipients (Log Scale)",
    y = "Frequency (Number of Counties)"
  ) +
  theme_minimal()

# 3. Visualization 2: Male vs. Female Recipients (Log-Log Scale)
cleaned_data %>%
  filter(male > 0, female > 0) %>% 
  ggplot(aes(x = male, y = female)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_x_log10(labels = comma) + 
  scale_y_log10(labels = comma) +
  labs(
    title = "Relationship Between Male and Female Recipients by County",
    x = "Number of Male Recipients (Log Scale)",
    y = "Number of Female Recipients (Log Scale)"
  ) +
  theme_minimal()

# 4. Visualization 3: Correlation Heatmap
cor_matrix <- cleaned_data %>%
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") 

cor_data <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable_1") %>%
  pivot_longer(
    cols = -variable_1, 
    names_to = "variable_2", 
    values_to = "correlation"
  )

ggplot(cor_data, aes(x = variable_1, y = variable_2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1, 1)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Correlation Heat Map of Dataset Variables",
    x = NULL, y = NULL, fill = "Correlation"
  )
