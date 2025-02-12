## Boxplots Colon Cancer Project Kakimi Kazuhiro - Zlatko  Tue, 11 Feb 2025

df <- read_csv("Eosinophil Counts.csv")
# Loading required libraries
library(ggplot2)
library(ggsignif)
library(tidyr)


# Combine the margin and center columns


data_sub <- select(df, -Margin１, -Margin2,-Center2,-Center1, -`Adjacent normal`) 
pStage_counts <- table(data_sub$pStage)

df_long <- data_sub %>%
  gather(key = "variable", value = "value", `Margin average`:`Center average`) 

df_long %>% mutate(type = ifelse(grepl("Margin", variable), "Tumor Margin", "Tumor Center"),
         stage = factor(pStage, levels = c("II", "III")))

t_test_combined <- t.test(value ~ pStage, data = df_long)
sample_sizes <- df_long %>%
  group_by(pStage) %>%
  summarise(n = n())
sample_sizes <- sample_sizes %>%
  mutate(x_label = paste(pStage, "\nn =", n))



# View the sample_sizes table to check the calculation
print(sample_sizes)






t_test_combined <- t.test(value ~ pStage, data = df_long)

summary_stats <- df_long %>%
  group_by(pStage) %>%
  summarise(

    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    Q2 = median(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),

  )

# Check the summary statistics
print(summary_stats)

# Define a label with custom statistics and sample size
summary_label <- paste(
  "\n",
  "Max: ", round(summary_stats$Max[1], 2), "\n",
  "Q3: ", round(summary_stats$Q3[1], 2), "\n",
  "Median: ", round(summary_stats$Q2[1], 2), "\n",
  "Q1: ", round(summary_stats$Q1[1], 2), "\n",
  "Min: ", round(summary_stats$Min[1], 2), "\n"

)

summary_label_III <- paste("\n",
                           "Max: ", round(summary_stats$Max[2], 2), "\n",
  
 
  "Q3: ", round(summary_stats$Q3[1], 2), "\n",
  "Median: ", round(summary_stats$Q2[2], 2), "\n",
  "Q1: ", round(summary_stats$Q1[2], 2), "\n",
  "Min: ", round(summary_stats$Min[1], 2), "\n"
  
)





plot <- ggplot(df_long, aes(x = pStage, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(y = "Tumor Maring + Center", x = "Tumor Stage") +
  theme(legend.position = "none") +
  #scale_fill_manual(values = c("lightblue", "lightcoral")) + 
  # Update x-axis labels with sample sizes
  scale_x_discrete(labels = sample_sizes$x_label) +
  
  # Add custom statistics and sample size to the top-right corner
  geom_text(
    aes(x = 1.5, y = max(df_long$value, na.rm = TRUE) + 2, label = summary_label),
    inherit.aes = FALSE,
    size = 4,
    hjust = 0,
    color = "black", family = "Arial"
  ) +
  geom_text(
    aes(x = 2.5, y = max(df_long$value, na.rm = TRUE) + 2, label = summary_label_III),
    inherit.aes = FALSE,
    size = 4,
    hjust = 0,
    color = "black", family = "Arial"
  )  + theme(
    panel.background = element_rect(fill = "white", color = "white"),  # White background for the plot area
    plot.background = element_rect(fill = "white", color = "white"),  # White background for the entire plot
    axis.title.x = element_text(size = 14, face = "bold"),  # Increase x-axis label font size and make it bold
    axis.title.y = element_text(size = 14, face = "bold"),  # Increase y-axis label font size and make it bold
    axis.text.x = element_text(size = 12),  # Increase font size for x-axis ticks
    axis.text.y = element_text(size = 12)   # Increase font size for y-axis ticks
  )

ggsave("boxplot_tumor_stage.png", plot = plot, width = 20, height = 16, dpi = 300)####################



