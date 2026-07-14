#### Challenge 1

- Pick a county in the U.S. (not honolulu)

- Read in data for median age with `get_acs()`

- `median age = "B01002_001"`

- Make a histogram or area plot with `ggplot()`

- Note the min, max, mean, median and quartiles with `summary()`

- Make a map of median age by census tract for your chosen county with ggplot

- Read in data for median income for the same county you chose

```         
median_income = "B19013_001"
```

- Make a histogram or area plot with `ggplot()`

- Note the min, max, mean, median and quartiles with `summary()`

- Make a map of median income by census tract for your chosen county with `mapview()`

- Make a scatterplot with `ggplot` showing the relationship between median age and median income between census tracts for your chosen county

install.packages(c("tidyverse", "tidycensus", "ggiraph", "mapview", "leaflet", "sf", "viridisLite"))
# ==========================================
# PART 1: MEDIAN AGE DATA & VISUALIZATION
# ==========================================

# 1. Fetch Median Age Data for San Diego
sd_age <- get_acs(
  state = "CA",
  county = "San Diego",
  geography = "tract",
  variables = "B01002_001",
  geometry = TRUE,
  year = 2023,
  key = "4fce1ed5285c5f8f816e6d1d71b270dc011a6487"
)

# 2. Plot the histogram of Median Age
ggplot(data = sd_age, aes(x = estimate)) +
  geom_histogram(color = "black", fill = "aquamarine4", bins = 50) +
  theme_minimal() + 
  labs(title = "Distribution of Median Age",
       subtitle = "San Diego County Census Tracts",
       x = "Median Age",
       y = "Count")

# 3. See the summary stats (min, max, mean, median, quartiles)
summary(sd_age$estimate)

# 4. Map of Median Age by Census Tract
ggplot(data = sd_age, aes(fill = estimate)) + 
  geom_sf(color = "white", linewidth = 0.05) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Median Age by Census Tract",
    subtitle = "San Diego County, CA",
    fill = "Age"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )


# ==========================================
# PART 2: MEDIAN INCOME DATA & VISUALIZATION
# ==========================================

# 5. Fetch Median Income Data for San Diego (Passing the key explicitly)
sd_inc <- get_acs(
  state = "CA",
  county = "San Diego",
  geography = "tract",
  variables = "B19013_001",
  geometry = TRUE,
  year = 2023,
  key = "4fce1ed5285c5f8f816e6d1d71b270dc011a6487" # Add this line!
)

# 6. Plot the histogram of Median Income
ggplot(data = sd_inc, aes(x = estimate)) +
  geom_histogram(color = "black", fill = "steelblue", bins = 50) +
  theme_minimal() + 
  labs(title = "Distribution of Median Household Income",
       subtitle = "San Diego County Census Tracts",
       x = "Median Income ($USD)",
       y = "Count")

# 7. See the summary stats for Median Income
summary(sd_inc$estimate)

# 8. Interactive map of Median Income via mapview

mapview(
  sd_inc, 
  zcol = "estimate", 
  col.regions = viridisLite::viridis(100),
  layer.name = "Median Income ($)",
  alpha.regions = 0.75
)


# ==========================================
# PART 3: AGE VS. INCOME RELATIONSHIP
# ==========================================

# 9. Strip spatial components to cleanly join datasets side-by-side
sd_joined <- left_join(
  as_tibble(sd_age) |> select(GEOID, age = estimate),
  as_tibble(sd_inc) |> select(GEOID, income = estimate),
  by = "GEOID"
)

# 10. Generate Scatterplot showing the interaction between Age and Wealth
ggplot(sd_joined, aes(x = age, y = income)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "darkgreen", se = TRUE) +
  scale_y_continuous(labels = scales::label_dollar()) +
  theme_minimal() +
  labs(
    title = "Relationship Between Median Age & Median Income",
    subtitle = "San Diego County Census Tracts",
    x = "Median Age",
    y = "Median Household Income"
  )
library(biscale)
library(ggplot2)
library(cowplot)

#### Challenge 2
[Follow this guide](https://walker-data.com/mapgl/articles/map-design.html#bivariate-styling:~:text=80-,Bivariate%20styling,-Bivariate%20maps%20visualize) to make a bivariate map of median age and income for your chosen county
                    
# ==========================================
# CHALLENGE 2: MAPBOX BIVARIATE AGE & INCOME
# ==========================================

# 1. Classify the age and income data into 3x3 buckets (bi-classes)
sd_map_data <- bi_class(
  sd_spatial_joined, 
  x = income, 
  y = age, 
  style = "quantile", 
  dim = 3
)

# 2. Create the primary map layer (Using "DkViolet")
bi_map <- ggplot() +
  geom_sf(data = sd_map_data, mapping = aes(fill = bi_class), color = "white", linewidth = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +  # Changed here
  theme_void() +
  labs(
    title = "Bivariate Map of Income and Age",
    subtitle = "San Diego County Census Tracts"
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# 3. Create the custom 3x3 square matrix legend (Using "DkViolet")
bi_legend <- bi_legend(
  pal = "DkViolet",                            # Changed here
  dim = 3,
  xlab = "Higher Income ",
  ylab = "Higher Age ",
  size = 7
)

# 4. Combine the map and the legend together using cowplot
final_bivariate_output <- ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(bi_legend, 0.05, 0.05, 0.25, 0.25)

# 5. Render the final composite visualization
print(final_bivariate_output)
