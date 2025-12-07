library(dplyr)
library(ggplot2)

# Load data
unemployment <- read.csv("/Users/kristinzamfirov/Library/CloudStorage/OneDrive-ImperialCollegeLondon/global_unemployment_data.csv")                  
continents <-read.csv("/Users/kristinzamfirov/Downloads/data sets/continents-according-to-our-world-in-data.csv")

# Select only the years we need (2020 and 2024)
unemp <- unemployment %>%
  select(country_name, X2020, X2024) %>%
  # Remove countries with both years missing
  filter(!(is.na(X2020) & is.na(X2024)))

# Merge with continent info
merged <- unemp %>%
  left_join(continents, by = c("country_name" = "Entity")) %>%
  filter(!is.na(Continent))

# Compute mean unemployment by continent for 2020 and 2024
continent_unemp <- merged %>%
  group_by(Continent) %>%
  summarize(
    mean_2020 = mean(X2020, na.rm = TRUE),
    mean_2024 = mean(X2024, na.rm = TRUE)
  )

# Calculate directional change (positive = increase, negative = decrease)
continent_change <- continent_unemp %>%
  mutate(change = mean_2024 - mean_2020)

# Plot horizontal bar chart with positive/negative axis
ggplot(continent_change, aes(x = change, y = reorder(Continent, change), fill = change > 0)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green")) +
  labs(
    title = "Change in Unemployment Rates (2020–2024)",
    x = "Change in Unemployment Rate (%)",
    y = "Continent",
    fill = "Increased"
  ) +
  theme_minimal()
