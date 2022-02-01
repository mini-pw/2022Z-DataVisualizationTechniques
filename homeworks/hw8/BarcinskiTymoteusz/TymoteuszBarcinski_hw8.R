library(plotly)
library(ggplot2)
library(dplyr)
library(dataReporter)
library(DataExplorer)
library(tidyverse)
library(data.table)
library(forcats)

df <- read.csv("mushrooms.csv")

# wstępne analizy
head(df)
dim(df)
colnames(df)
summary(df)
lapply(df, unique)

plot_intro(df)
plot_missing(df)

##################

plot_bar2 <- function(data, group, with = NULL, maxcat = 50, order_bar = TRUE, binary_as_factor = TRUE, title = NULL, ggtheme = theme_gray(), theme_config = list(), nrow = 3L, ncol = 3L, parallel = FALSE) {
  frequency <- measure <- variable <- value <- NULL
  if (!is.data.table(data)) data <- data.table(data)
  split_data <- split_columns(data, binary_as_factor = binary_as_factor)
  if (split_data$num_discrete == 0) stop("No discrete features found!")
  discrete <- split_data$discrete
  ind <- DataExplorer:::.ignoreCat(discrete, maxcat = maxcat)
  if (length(ind)) {
    message(length(ind), " columns ignored with more than ", maxcat, " categories.\n", paste0(names(ind), ": ", ind, " categories\n"))
    drop_columns(discrete, names(ind))
    if (length(discrete) == 0) stop("Note: All discrete features ignored! Nothing to plot!")
  }
  feature_names <- names(discrete)
  if (is.null(with)) {
    dt <- discrete[, list(frequency = .N), by = feature_names]
  } else {
    if (is.factor(data[[with]])) {
      measure_var <- suppressWarnings(as.numeric(levels(data[[with]]))[data[[with]]])
    } else if (is.character(data[[with]])) {
      measure_var <- as.numeric(data[[with]])
    } else {
      measure_var <- data[[with]]
    }
    if (all(is.na(measure_var))) stop("Failed to convert `", with, "` to continuous!")
    if (with %in% names(discrete)) drop_columns(discrete, with)
    tmp_dt <- data.table(discrete, "measure" = measure_var)
    dt <- tmp_dt[, list(frequency = sum(measure, na.rm = TRUE)), by = feature_names]
  }
  dt2 <- suppressWarnings(melt.data.table(dt, id.vars = c(group, "frequency"), measure.vars = setdiff(feature_names, group))) # This line is updated
  layout <- DataExplorer:::.getPageLayout(nrow, ncol, ncol(discrete))
  plot_list <- DataExplorer:::.lapply(
    parallel = parallel,
    X = layout,
    FUN = function(x) {
      if (order_bar) {
        base_plot <- ggplot(dt2[variable %in% feature_names[x]], aes(x = reorder(value, frequency), y = frequency))
      } else {
        base_plot <- ggplot(dt2[variable %in% feature_names[x]], aes(x = value, y = frequency))
      }
      base_plot +
        geom_bar(stat = "identity", aes_string(fill = (group))) + # This line is updated
        coord_flip() +
        xlab("") + ylab(ifelse(is.null(with), "Frequency", toTitleCase(with)))
    }
  )
  class(plot_list) <- c("multiple", class(plot_list))
  plotDataExplorer(
    plot_obj = plot_list,
    page_layout = layout,
    title = title,
    ggtheme = ggtheme,
    theme_config = theme_config,
    facet_wrap_args = list(
      "facet" = ~ variable,
      "nrow" = nrow,
      "ncol" = ncol,
      "scales" = "free"
    )
  )
}

plots <- plot_bar(df)
l <- mget(plots)
invisible(mapply(ggsave, file=paste0("plot-", names(l), ".pdf"), plot=l))
?mget
ggsave("basic_bar.png", width = 30, height = 20, units = "cm")

plot_bar2(df, group = "class")
ggsave("advanced_bar.png", width = 30, height = 20, units = "cm")


df_2 <- df %>% 
  select("class", "ring.type", "gill.color", "odor", "spore.print.color") %>% 
  rename(class = "class", ring_type = "ring.type", odor = "odor",
         spore_print_color = "spore.print.color", gill_color = "gill.color") %>% 
  mutate()
  

### Ring type ######

new <- df_2 %>%
  group_by(class, ring_type) %>%
  summarise(n = n())

plot_ring_type <- ggplot(new) +
  geom_col(aes(x = n, y = reorder(ring_type, n), fill = class)) +
  labs(x = "Number of observations", y = "Ring type", title = "Ring types and poisonousenss of mushrooms") +
  scale_y_discrete(labels = c("p" = "pendant", "e" = "evanescent", "l" = "large",
                              "f" = "flaring", "n" = "none")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  theme(legend.position = c(0.85, 0.2)) +
  scale_fill_discrete(breaks = c("e", "p"), labels = c("edible", "poisonous"))
plot_ring_type

ggsave("ring_type.png", width = 17, height = 13, units = "cm")

#####  Gill color #########

new <- df_2 %>%
  group_by(class, gill_color) %>%
  summarise(n = n())

plot_gill_color <- ggplot(new) +
  geom_col(aes(x = n, y = reorder(gill_color, n), fill = class)) +
  labs(x = "Number of observations", y = "Gill color", title = "Gill colors and poisonousenss of mushrooms") +
  scale_y_discrete(labels = c("k" = "black", "n" = "brown", "b" = "buff",
                              "h" = "chocolate", "g" = "gray", "r" = "green", 
                              "o" = "orange", "p" = "pink", "u" = "purple", "e" = "red",
                              "w" = "white", "y" = "yellow")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  theme(legend.position = c(0.85, 0.2)) +
  scale_fill_discrete(breaks = c("e", "p"), labels = c("edible", "poisonous"))
plot_gill_color
ggsave("gill_color.png", width = 17, height = 13, units = "cm")

######### spore_print_color ###########

new <- df_2 %>%
  group_by(class, spore_print_color) %>%
  summarise(n = n())

plot_spore_print_color <- ggplot(new) +
  geom_col(aes(x = n, y = reorder(spore_print_color, n), fill = class)) +
  labs(x = "Number of observations", y = "spore print color", title = "Spore print color and poisonousenss of mushrooms") +
  scale_y_discrete(labels = c("k" = "black", "n" = "brown", "b" = "buff",
                              "h" = "chocolate", "g" = "gray", "r" = "green", 
                              "o" = "orange", "u" = "purple",
                              "w" = "white", "y" = "yellow")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  theme(legend.position = c(0.85, 0.2)) +
  scale_fill_discrete(breaks = c("e", "p"), labels = c("edible", "poisonous"))
plot_spore_print_color
ggsave("spore_print_color.png", width = 17, height = 13, units = "cm")

######### Odor ###########

new <- df_2 %>%
  group_by(class, odor) %>%
  summarise(n = n())

plot_odor <- ggplot(new) +
  geom_col(aes(x = n, y = reorder(odor, n), fill = class)) +
  labs(x = "Number of observations", y = "Odor", title = "Odor and poisonousenss of mushrooms") +
  scale_y_discrete(labels = c("a" = "almond", "l" = "anise", "c" = "creosote",
                              "y" = "fishy", "f" = "foul", "m" = "musty", 
                              "n" = "none", "p" = "pungent",
                              "s" = "spicy")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.04))) +
  theme(legend.position = c(0.85, 0.2)) +
  scale_fill_discrete(breaks = c("e", "p"), labels = c("edible", "poisonous"))
plot_odor
ggsave("plot_odor.png", width = 17, height = 13, units = "cm")


