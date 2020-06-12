library(tidyverse)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(grid)
library(gridExtra)
ds_theme_set()

# Importing the datasets from csv files
SPL_2020_05_15 <- read_csv("data/SPL-2020-05-15.csv")
X2018PopAll <- read_csv("data/2018PopAll.csv")
lasregionew2019 <- read_csv("data/lasregionew2019.csv")

# manipulating the data

# creating a vector containing age groups to classify together so SPL and Population data can be brought into one data frame
age_cat <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

population_2021 <-
  X2018PopAll %>%
  mutate('AGE GROUP' = ifelse(`AGE GROUP` %in% age_cat,"<70",ifelse(`AGE GROUP` == "All ages", "All", "70+"))) %>%
  group_by(AREA, `AGE GROUP`) %>%
  summarise(population = sum(`2021`)) %>%
  ungroup()

shielding_data <-
  SPL_2020_05_15 %>%
  filter(`Breakdown Field` != "Gender") %>%
  mutate(`Breakdown Value` = ifelse(`Breakdown Value` == "ALL", "All", ifelse(`Breakdown Value` == "70+", "70+", "<70"))) %>%
  group_by(`LA Name`, `Breakdown Value`) %>%
  summarise(shielding = sum(`Patient Count`)) %>%
  ungroup()

# bringing the data sets together
combined_data <-
  left_join(population_2021, shielding_data, by = c("AREA" = "LA Name", "AGE GROUP" = "Breakdown Value")) %>%
  left_join(lasregionew2019, by = c("AREA" = "LA name")) %>%
  drop_na() %>%
  mutate(shielding_rate = shielding / population * 1000) %>%
  rename("area" = "AREA", "age" = "AGE GROUP", "region" = "Region name") %>%
  select(area, region, age, population, shielding, shielding_rate)

# Define a group so we can highlight Warrinton and others  
plot_labels <- c("Warrington",
                 "Liverpool",
                 "Wigan",
                 "St. Helens",
                 "Cheshire West and Chester",
                 "Cheshire East",
                 "Wirral",
                 "Knowsley",
                 "Sefton",
                 "Halton",
                 "Stockport",
                 "Salford",
                 "Manchester")

# Plots

# Overall population vs shielding
highlight_df <-
  combined_data %>%
  filter(area %in% plot_labels, age == "All")

scatter_plot_psall <-
  combined_data %>%
  filter(age == "All") %>%
  ggplot(aes(population, shielding)) +
  geom_smooth(method="lm") +
  geom_point(size = 2) +
  geom_point(data = highlight_df, color = "red", alpha = 1) +
  geom_text_repel(aes(label=ifelse(area %in% plot_labels,as.character(area),"")), nudge_x = 0.07) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population (log scale)") +
  ylab("Shielding patients (log scale)") +
  ggtitle("Coronavirus Shielding Patients (All ages)") +
  labs(caption = "Population is the ONS 2021 estimate (2018 based) \n Shielding patients numbers are as at 15 May 2020 from NHS Digital")

scatter_plot_psall
ggsave("figs/scatter_plot.png")

# Comparing age groups, with density also shown
highlight_df <-
  combined_data %>%
  filter(area %in% plot_labels, age != "All")

scatter_plot_psage <-
  combined_data %>%
  filter(age != "All") %>%
  ggplot(aes(population, shielding, alpha = 0.2, color = age)) +
  geom_smooth(method="lm") +
  geom_point(size = 2) +
  geom_point(data = highlight_df, color = "red", alpha = 1) +
  geom_text_repel(aes(label=ifelse(area %in% plot_labels,as.character(area),"")), nudge_x = 0.07) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population (log scale)") +
  ylab("Shielding patients (log scale)") +
  ggtitle("Coronavirus Shielding Patients (Age groups compared)") +
  guides(alpha = "none", color = "none") +
  labs(caption = "Population is the ONS 2021 estimate (2018 based) \n Shielding patients numbers are as at 15 May 2020 from NHS Digital") +
  facet_grid(.~age) +
  scale_color_colorblind()

density_plot_srage <- 
  combined_data %>%
  filter(age != "All") %>%
  ggplot(aes(shielding_rate, y = ..count.., fill = age, alpha = 0.2)) +
  geom_density() +
  xlab("Shielding patients per 1,000 population") +
  ylab("Population Density") +
  guides(alpha = "none") +
  theme(legend.position = "top") +
  facet_grid(.~age) +
  scale_color_colorblind() +
  scale_fill_colorblind(name = "Key")

grid.arrange(scatter_plot_psage, density_plot_srage, ncol = 1, nrow = 2, heights = c(2/3, 1/3))
ggsave("figs/age_plots.png")

# Looking at regional variation & geographical trends
box_plot_regall <- 
  combined_data %>%
  filter(age == 'All') %>%
  mutate(region = reorder(region, shielding_rate, FUN = median)) %>%
  ggplot(aes(region, shielding_rate, fill = region, alpha = 0.2)) +
  geom_boxplot() +
  geom_point() +
  xlab("Region") +
  ylab("Shielding Rate") +
  ggtitle("Coronavirus Shielding Patients (Regions)") +
  guides(alpha = "none") +
  theme(legend.position = "none")

box_plot_regall

box_plot_regage <- 
  combined_data %>%
  filter(age != 'All') %>%
  mutate(region = reorder(region, shielding_rate, FUN = median)) %>%
  ggplot(aes(region, shielding_rate, fill = region, alpha = 0.2)) +
  geom_boxplot() +
  geom_point() +
  xlab("Region") +
  ylab("Shielding Rate") +
  ggtitle("Coronavirus Shielding Patients (Regions - age groups compared)") +
  guides(alpha = "none") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(.~age)

box_plot_regage

# Data tables for display
table_for_display_1 <- 
  combined_data %>% 
  filter(age == "70+", region == "North West") %>%
  arrange(desc(shielding_rate))

table_for_display_2 <-
  combined_data %>%
  summarise(lowest = min(shielding_rate),
            percentile25 = quantile(shielding_rate, 0.25),
            median = median(shielding_rate),
            percentile75 = quantile(shielding_rate,0.75),
            highest = max(shielding_rate),
            mean = mean(shielding_rate),
            standard_deviation = sd(shielding_rate))
            

table_for_display_1
table_for_display_2

# Correlation calculations
cor_data <- combined_data %>% filter(age == "All")
cor(cor_data$population, cor_data$shielding, method = "spearman")
cor(cor_data$population, cor_data$shielding_rate, method = "spearman")


