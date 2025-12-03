# ----- Least developed countries (LDC) list from the UN -----
# This is the list of LDCs we focus on for the 7% growth target.
# Names should match (as closely as possible) the country names in gdp_full$country.
ldc <- c(
  # Africa (32)
  "Angola","Benin","Burkina Faso","Burundi","Central African Republic","Chad","Comoros",
  "Democratic Republic of Congo","Djibouti","Eritrea","Ethiopia","Gambia","Guinea",
  "Guinea-Bissau","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania",
  "Mozambique","Niger","Rwanda","Senegal","Sierra Leone","Somalia","South Sudan",
  "Sudan","Togo","Uganda","Tanzania","Zambia",
  # Asia (8)
  "Afghanistan","Bangladesh","Cambodia","Laos","Myanmar",
  "Nepal","East Timor","Yemen",
  # Caribbean (1)
  "Haiti",
  # Pacific (3)
  "Kiribati","Solomon Islands","Tuvalu"
)

# ----- Filter GDP growth data for LDCs -----
# We take the growth data that you already computed (gdp_growth)
# and keep only those observations that belong to LDCs.
ldc_growth <- gdp_growth %>%
  filter(country %in% ldc)

# ----- Create an indicator for meeting the 7% growth target -----
# SDG target: at least 7% GDP growth per year in LDCs.
# We approximate this with per capita GDP growth (g_yoy >= 0.07).
ldc_growth <- ldc_growth %>%
  mutate(
    meet_7 = g_yoy >= 0.07    # TRUE if the country-year meets or exceeds 7% growth
  )

# ----- Helper function: longest consecutive run of TRUE values -----
# This is used to measure how persistent the 7% growth is for each LDC.
calc_max_run_indicator <- function(x) {
  x <- ifelse(is.na(x), FALSE, x)  # treat NA as not meeting the target
  r <- rle(x)
  if (length(r$lengths) == 0) return(0L)
  max(r$lengths[r$values == TRUE], na.rm = TRUE)
}

# ----- Country-level summary for LDCs -----
# For each LDC, we compute:
# - avg_yoy: average per capita GDP growth
# - share_years_meet7: share of years with growth >= 7%
# - max_run_meet7: longest streak (in years) with growth >= 7%
# - n_years: number of years with available data
ldc_country_summary <- ldc_growth %>%
  group_by(country, Continent) %>%
  summarise(
    avg_yoy           = mean(g_yoy, na.rm = TRUE),
    share_years_meet7 = mean(meet_7, na.rm = TRUE),
    max_run_meet7     = calc_max_run_indicator(meet_7),
    n_years           = sum(!is.na(g_yoy)),
    .groups = "drop"
  )

print(ldc_country_summary)

# ----- Continent-year summary for LDCs -----
# Here we move from individual countries to continent-level aggregates for LDCs.
# For each Continent × Year, we compute:
# - mean_g_yoy: average per capita GDP growth among LDCs in that continent
# - share_countries_meet7: share of LDCs that meet the 7% target in that year
# - n_ldc: number of LDCs with data in that continent-year
ldc_continent_year <- ldc_growth %>%
  group_by(Continent, Year) %>%
  summarise(
    mean_g_yoy            = mean(g_yoy, na.rm = TRUE),
    share_countries_meet7 = mean(meet_7, na.rm = TRUE),
    n_ldc                 = n_distinct(country),
    .groups = "drop"
  )

print(ldc_continent_year)

# ----- Plot 1: Average growth of LDCs by continent -----
# Line plot of average per capita GDP growth for LDCs in each continent.
# The dashed horizontal line marks the 7% SDG target.
p_ldc_growth <- ggplot(ldc_continent_year,
                       aes(x = Year, y = mean_g_yoy, color = Continent)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0.07, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Average per capita GDP growth of LDCs by continent",
       x = "Year",
       y = "Average year-on-year growth (LDCs)") +
  theme_minimal()

print(p_ldc_growth)

# ----- Plot 2: Share of LDCs meeting the 7% target -----
# For each continent and year, we plot the proportion of LDCs with growth >= 7%.
p_ldc_share7 <- ggplot(ldc_continent_year,
                       aes(x = Year, y = share_countries_meet7, color = Continent)) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(title = "Share of LDCs meeting ≥7% GDP growth (by continent)",
       x = "Year",
       y = "Share of LDCs meeting target") +
  theme_minimal()

print(p_ldc_share7)

# ----- Plot 3: Distribution of “years meeting the target” across LDCs -----
# Boxplot of the share of years with growth >= 7% for each LDC,
# grouped by continent. This shows which continents have LDCs
# that more often reach the SDG target.
p_ldc_country_box <- ggplot(ldc_country_summary,
                            aes(x = Continent, y = share_years_meet7, fill = Continent)) +
  geom_boxplot(outlier.alpha = 0.4) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Share of years with ≥7% growth for each LDC",
       x = "Continent",
       y = "Share of years meeting target") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_ldc_country_box)

# ----- Ranking table: which LDCs are closest to the SDG target? -----
# We rank LDCs by the share of years in which they meet the 7% growth target.
ldc_rank <- ldc_country_summary %>%
  arrange(desc(share_years_meet7))

print(ldc_rank)

# ----- Export LDC-related outputs (optional, for further analysis/reporting) -----
write_csv(ldc_growth,           "out_ldc_growth_yoy.csv")
write_csv(ldc_country_summary,  "out_ldc_country_summary.csv")
write_csv(ldc_continent_year,   "out_ldc_continent_year.csv")
write_csv(ldc_rank,             "out_ldc_rank_share_years_meet7.csv")
