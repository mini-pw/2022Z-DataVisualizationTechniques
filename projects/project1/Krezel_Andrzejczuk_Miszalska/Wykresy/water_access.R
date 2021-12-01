library(dplyr)
library(ggplot2)
library(forcats)
library(Cairo)
library(tidyr)
library(scales)

### Prepare data
water.data.raw <- read.csv("Data/water-and-sanitation.csv", fileEncoding = "UTF-8-BOM")
water.data <- water.data.raw %>%  select(Entity, Year, Access.to.improved.drinking.water) 
colnames(water.data) <- c("Entity", "Year", "Improved.water.access")

most.recent.data <- water.data %>%
  filter(!is.na(Improved.water.access)) %>% 
  group_by(Entity) %>% 
  summarise(Year = tail(Year, 1), .groups = "drop")
most.recent.data <- inner_join(most.recent.data, water.data, 
                               by = c("Entity" = "Entity", "Year" = "Year"))

least.recent.data <- water.data %>%
  filter(!is.na(Improved.water.access)) %>% 
  group_by(Entity) %>% 
  summarise(Year = head(Year, 1), .groups = "drop")
least.recent.data <- inner_join(least.recent.data, water.data,
                                by = c("Entity" = "Entity", "Year" = "Year"))

water.data <- full_join(most.recent.data, least.recent.data, 
                 by = c("Entity" = "Entity"),
                 suffix = c(".most.recent", ".least.recent"))

water.data <- water.data %>% 
  mutate(Change = Improved.water.access.most.recent - Improved.water.access.least.recent)

## clean and fix water.data

# remove grouping entities
entities.to.remove = c("Low income", "High income", "Lower-middle income", 
                       "Upper-middle income", "North America and Europe", "World",
                       "Australia and New Zealand", "Central and Southern Asia",
                       "Eastern and South-Eastern Asia","Fragile or Extremely Fragile",
                       "Landlocked Developing Countries","Latin America and the Caribbean",
                       "Least Developed Countries","Oceania","Small Island Developing States",
                       "Sub-Saharan Africa","Western Asia and Northern Africa")

water.data <- water.data %>% 
  filter(!(Entity %in% entities.to.remove))

# standardise entity names
water.data <- water.data %>% 
  mutate(Entity = case_when(
    Entity == "United States" ~ "USA",
    Entity == "United Kingdom" ~ "UK",
    Entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    Entity == "Congo" ~ "Republic of Congo",
    Entity == "Czechia" ~ "Czech Republic",
    Entity == "Saint Martin (French part)" ~ "Saint Martin",
    Entity == "Sint Maarten (Dutch part)" ~ "Sint Maarten",
    Entity == "Cote d'Ivoire" ~ "Ivory Coast",
    Entity == "Timor" ~ "Timor-Leste",
    Entity == "Faeroe Islands" ~ "Faroe Islands",
    Entity == "United States Virgin Islands" ~ "Virgin Islands",
    Entity == "Wallis and Futuna Islands" ~ "Wallis and Futuna",
    TRUE ~ Entity
  ))

# split joined entities
split_entities <- function(entity_name, names)
{
  r <- water.data %>% filter(Entity == entity_name)
  r <- r %>% mutate(count = length(names)) %>% uncount(count)
  r[1] <- names
  water.data <- water.data %>% filter(Entity != entity_name)
  rbind(water.data, r)
}

water.data <- split_entities("Antigua and Barbuda", c("Antigua", "Barbuda"))
water.data <- split_entities("Saint Vincent and the Grenadines", c("Saint Vincent", "Grenadines"))
water.data <- split_entities("Saint Kitts and Nevis", c("Saint Kitts", "Nevis"))
water.data <- split_entities("Trinidad and Tobago", c("Trinidad", "Tobago"))

## Join data frames
map.data = map_data("world")
map.data <- map.data %>% select(-subregion)
map.data <- map.data %>% filter(region != "Antarctica")
data <- full_join(map.data, water.data, by = c("region" = "Entity"), keep = TRUE)

# Remove solomon islands as it is an outlier that is not even visible on the map (and skews the scale)
data <- data %>% filter(region != "Solomon Islands")

# missing.from.water.data <- data %>% filter(is.na(Improved.water.access.most.recent)) %>% 
#   select(region) %>% unique()
# missing.from.map.data <- data %>% filter(is.na(region)) %>% select(Entity) %>% unique()

### Plot data
low_color = "#ffffff"
high_color = "#40a624"
background.color = "transparent"
grid.color = "transparent"
border.color = "#193a5f"
legend.frame.color = "#4083ad"

steps = c("#ff1c60", "#193a5f", "#00ffaa")
min.val = min(data$Change, na.rm = TRUE)
max.val = max(data$Change, na.rm = TRUE)

p <- data %>%
  ggplot(aes(long, lat, fill = Change, group = group)) +
  geom_polygon(color = border.color, size = 0.3) +
  labs(
    title = "",
    subtitle = "",
    fill = "CHANGE"
  ) +
  scale_fill_gradientn(
    colours = steps, 
    values = rescale(c(min.val, 0, max.val)),
    breaks = c(seq(0, 50, 10), -5)) +
  theme(
    legend.key.height = unit(8, "mm"),
  ) +
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = background.color, color = background.color),
    panel.background = element_rect(fill = background.color),
    legend.background = element_rect(fill = background.color),
    legend.title = element_text(size = 8),
  ) +
  guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 1))

p

ggsave(file="Render/access_change.png", type="cairo-png", width = 8, height = 5)


