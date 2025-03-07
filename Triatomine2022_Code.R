library(gtsummary)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(esquisse)
library(readxl)
library(ggpubr)
library(forcats)
library(patchwork)

df <- read_excel("Supplemental_Dataset.xlsx", 
                 sheet = "df")

df1 <- read_excel("Supplemental_Dataset.xlsx", 
                  sheet = "df1")

df1.1 <- df1 %>%
  filter(!Community %in% c("EA", "NA")) %>%
  mutate(Positive = ifelse(Presence == "Yes", 1, 0),
         Colonization = ifelse(Ninf_Presence == "Yes", 1, 0))%>%
  replace(is.na(.),0)

RTable1 <- df1.1 %>%
  mutate(
    Dom_Col = ifelse (rowSums(select(., starts_with("Dom_N"))) > 0,1,0),
    Per_Col = ifelse(rowSums(select(., starts_with("Per_N"))) > 0, 1, 0),
    Dom_Inf = ifelse(rowSums(select(., starts_with("Dom_")))>0,1,0),
    Per_Inf = ifelse(rowSums(select(., starts_with("Per_")))>0,1,0)
    ) %>%
  group_by(Community) %>%
  summarise(
    Total_Households = n(),
    Infested_Households = sum(Positive),
    Percentage_Infested = (Infested_Households / Total_Households) * 100,
    Dom_Inf = sum(Dom_Inf),
    Per_Inf = sum(Per_Inf),
    Dom_PercentageInf = (Dom_Inf / Total_Households) * 100,
    Per_PercentageInf = (Per_Inf / Total_Households) * 100,
    Dom_Col = sum(Dom_Col),
    Per_Col = sum(Per_Col),
    Colonization= sum(Colonization),
    Dom_PercentageCol = (Dom_Col / Total_Households) * 100,
    Per_PercentageCol = (Per_Col / Total_Households) * 100,
    Col_Percentage = (Colonization/ Total_Households) * 100
  )

print(RTable1)


df %>%
  select(Community, Life_Stage, Per_Dom, qPCR_Result, Strain) %>%
  tbl_summary(by = Community,
              missing = "no") %>%
  add_overall() %>%
  bold_labels()


# Infestation data
infestation_table <- matrix(
  c(14, 62,  # Infested: Domicile and Peridomicile
    (7), (76 - 7)), # Not Infested: Total Houses - Infested
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    Location = c("Domicile", "Peridomicile"),
    Status = c("Infested", "Not Infested")
  )
)

# View the infestation table
print("Infestation Table:")
print(infestation_table)

# Perform chi-squared test for infestation
infestation_chi2 <- chisq.test(infestation_table)

# Perform Fisher's exact test for infestation (if needed)
infestation_fisher <- fisher.test(infestation_table)

# Colonization data
colonization_table <- matrix(
  c(8, 68,  # Colonized: Domicile and Peridomicile
    (6), (76 - 6)), # Not Colonized: Total Houses - Colonized
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    Location = c("Domicile", "Peridomicile"),
    Status = c("Colonized", "Not Colonized")
  )
)

# View the colonization table
print("Colonization Table:")
print(colonization_table)

# Perform chi-squared test for colonization
colonization_chi2 <- chisq.test(colonization_table)

# Perform Fisher's exact test for colonization (if needed)
colonization_fisher <- fisher.test(colonization_table)

# Print the results
print("Infestation Chi-squared Test:")
print(infestation_chi2)

print("Colonization Chi-squared Test:")
print(colonization_chi2)


Tmean <- df1.1 %>%
  group_by(Community) %>%
  summarise(
    Mean_Triatomines = mean(Number_Triatomines, na.rm = TRUE),
    SD_Triatomines = sd(Number_Triatomines, na.rm =TRUE)
  )

print(Tmean)

shapiro.test(df1.1$Number_Triatomines)

RTable2 <- df1.1 %>%
  mutate(
    Dom_Col = rowSums(select(., starts_with("Dom_N"))),
    Per_Col = rowSums(select(., starts_with("Per_N"))),
    Dom_Inf = rowSums(select(., starts_with("Dom_"))),
    Per_Inf = rowSums(select(., starts_with("Per_")))
  ) 

df2stack <- RTable2 %>%
  pivot_longer(
    cols = c(Dom_Inf, Per_Inf),
    names_to = "Location",
    values_to = "Infestation"
  ) %>%
  mutate(Location = ifelse(Location == "Dom_Inf", "Domicile", "Peridomicile")) %>%
  select(Community, Location, Infestation)


# Function to perform statistical test for each community
compare_colonization <- function(data, community) {
  # Filter data for the specific community
  community_data <- data %>% filter(Community == community)
  
  # Check normality for Colonization
  shapiro_test <- shapiro.test(community_data$Infestation)
  
  # Choose the appropriate test based on normality
  if (shapiro_test$p.value > 0.05) {
    # Perform independent t-test (if data is normal)
    test_result <- t.test(Infestation ~ Location, data = community_data)
  } else {
    # Perform Wilcoxon rank-sum test (non-parametric alternative)
    test_result <- wilcox.test(Infestation ~ Location, data = community_data)
  }
  
  # Return results
  return(list(Community = community, Test = test_result))
}

# Get the list of unique communities
communities <- unique(df2stack$Community)

# Apply the comparison function to each community
results <- lapply(communities, function(community) compare_colonization(df2stack, community))

# Print results for each community
for (result in results) {
  cat("\nComparison for", result$Community, ":\n")
  print(result$Test)
}


df <- df %>%
  mutate(qPCR_Result = ifelse(qPCR_Result == "Positive", 1, 0))

summary_table <- df %>%
  filter(qPCR_Result == 1) %>%
  tbl_summary(include = c(Community, Life_Stage, Household, Per_Dom),
              by= Per_Dom) %>%
  add_p()

print(summary_table)

###Blood meal####

df$Life_Stage <- as.factor(df$Life_Stage)

bloodmeal_sources <- c("Rat", "Chicken", "Dog", "Human", "Ducks", "Cat", "Boar", "Roach", "Mouse")

df1.2 <- df %>%
  select(Life_Stage, Per_Dom, all_of(bloodmeal_sources)) %>%
  mutate(across(all_of(bloodmeal_sources), as.character)) 

species_labels <- c(
  "Chicken" = expression(italic("Gallus gallus")),
  "Rat" = expression(italic("Rattus rattus")),
  "Dog" = expression(italic("Canis lupus familiaris")),
  "Human" = expression(italic("Homo sapiens")),  # No need for italics
  "Ducks" = expression("Anatidae"),
  "Cat" = expression(italic("Felis catus")),
  "Boar" = expression(italic("Sus scrofa")),
  "Roach" = expression(italic("Archimandrita sp.")),
  "Mouse" = expression(italic("Mus musculus"))
)

# Reshape data into long format
df_long <- df1.2 %>%
  pivot_longer(cols = all_of(bloodmeal_sources), names_to = "Bloodmeal_Source", values_to = "Presence") %>%
  filter(Presence == 1) %>%  # Keep only detected bloodmeal sources
  group_by(Life_Stage, Bloodmeal_Source) %>%
  summarize(Total = n(), .groups = "drop") %>%
  mutate(Percentage = Total / sum(Total) * 100,
         Bloodmeal_Source = fct_reorder(Bloodmeal_Source, Percentage, .fun= sum, .desc = TRUE))

Fig1 <- ggplot(df_long, aes(x = Bloodmeal_Source, y = Percentage, fill = Life_Stage)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "",
       y = "Proportion of triatomine life stage by \nbloodmeal sources (%)",
       fill = "Life Stage") +
  theme_linedraw()+
  theme(axis.text.x = element_text(size= 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +  # Rotate x-axis labels
  scale_x_discrete(labels = species_labels) + 
  scale_fill_brewer(palette = "Spectral") 

plot(Fig1)
ggsave(Fig1, device = "png", filename = "Fig1.png", width = 7, height = 5, dpi = 300)

###Domestic and Peridomestic####
df1.3 <- df %>%
  select(Life_Stage, Per_Dom, Community)%>%
  mutate(Per_Dom = recode(Per_Dom, "Dom" ="Domicile", "Per" = "Peridomicile"))

# Reshape data into long format
df_long2 <- df1.3 %>%
  group_by(Community, Per_Dom, Life_Stage) %>%
  summarize(Total = n(), .groups = "drop") %>%
  group_by(Community) %>%
  mutate(Percentage = Total / sum(Total) * 100)%>%
  ungroup()

Fig2 <- ggplot(df_long2, aes(x = Community, y = Percentage, fill = Life_Stage)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "",
       y = "Proportion of triatomine life stage by \nCommunity (%)",
       fill = "Life Stage") +
  theme_linedraw()+
  theme(axis.text.x = element_text(size= 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +  # Rotate x-axis labels
  scale_x_discrete(labels = species_labels) + 
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~Per_Dom, ncol = 1)

plot(Fig2)
ggsave(Fig2, device = "png", filename = "Fig2.png", width = 5, height = 5, dpi = 300)

Figure <- Fig1 + Fig2
Figure
