
setwd("C:/Users/adria/Documents/JOBS/Data-Driven EnviroLab/Adriana_JimenezWillis_DDL_test/Data")

library(ClimActor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(multcompView)
library(purrr)
library(forcats)
library(multcompView)


Actors <- read.csv("ClimActor_2_actor-1.csv") 
Emissions_Agg <- read.csv("ClimActor_3_emissions_agg-1.csv") 
Emissions_Sectors <- read.csv("ClimActor_4_emissions_sectors-1.csv") 
Targets <- read.csv("ClimActor_5_targets-1.csv") 
Plans <- read.csv("ClimActor_6_plans-1.csv") 
Initiatives <- read.csv("ClimActor_7_initiatives.csv")
load_key_dict()


########VIZ 1

emissions_2020_comparable <- Emissions_Agg %>%
  filter(
    inv_year == 2020,
    inv_unit == "tCO2e",
    inv_scopes == "1+2+3",
    inv_boundary == "Same"
  ) %>%
  mutate(source_priority = if_else(
    grepl("CDP", datasource_id, ignore.case = TRUE), 1, 2
  )) %>%
  arrange(source_priority) %>%
  distinct(climactor_id, .keep_all = TRUE)

map_data <- emissions_2020_comparable %>%
  left_join(
    key_dict %>%
      select(climactor_id, long_lat) %>%
      distinct(climactor_id, .keep_all = TRUE),
    by = "climactor_id"
  ) %>%
  separate(long_lat, into = c("longitude", "latitude"),
           sep = " - ", convert = TRUE)

ggplot(map_data, aes(x = longitude, y = latitude)) +
  annotation_borders(colour = "gray80", fill = "gray95") +
  geom_point(
    aes(colour = cut(
      total_emissions,
      breaks = c(-Inf, 1e5, 1e6, 1e7, Inf),
      labels = c("Under 100K", "100K–1M", "1M–10M", "Over 10M")
    )),
    size = 2.5,
    alpha = 0.85
  ) +
  scale_colour_brewer(palette = "YlOrRd", name = "Emissions (tCO2e)") +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "Reported Urban Greenhouse Gas Emissions, 2020",
    subtitle = "Scope 1+2+3 | Same reporting boundary",
    x = "Longitude",
    y = "Latitude"
  )



#############VIZ 2
population_data <- key_dict %>%
  select(climactor_id, population) %>%
  distinct(climactor_id, .keep_all = TRUE)

per_capita_data <- emissions_2020_comparable %>%
  left_join(population_data, by = "climactor_id") %>%
  filter(
    !is.na(population),
    population > 0,
    total_emissions > 0
  ) %>%
  mutate(
    emissions_per_person = total_emissions / population
  )

model <- lm(log10(emissions_per_person) ~ log10(population),
            data = per_capita_data)

p_value <- summary(model)$coefficients[2, 4]

ggplot(per_capita_data, aes(x = population, y = emissions_per_person)) +
  geom_point(colour = "steelblue", alpha = 0.65, size = 2.5) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "darkred",
              se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Population vs. Per-Capita Greenhouse Gas Emissions",
    subtitle = paste("2020 inventories | p =", format.pval(p_value, digits = 3)),
    x = "Population (log scale)",
    y = "Emissions per person (tCO₂e, log scale)"
  )



######## VIZ 3

viz3_base <- Actors %>%
  distinct(climactor_id) %>%
  left_join(
    Emissions_Agg %>% distinct(climactor_id) %>%
      mutate(has_inventory = "Yes"),
    by = "climactor_id"
  ) %>%
  mutate(has_inventory = replace_na(has_inventory, "No")) %>%
  left_join(
    Plans %>% distinct(climactor_id) %>% mutate(has_plan = TRUE),
    by = "climactor_id"
  ) %>%
  left_join(
    Targets %>% distinct(climactor_id) %>% mutate(has_target = TRUE),
    by = "climactor_id"
  ) %>%
  mutate(
    has_plan = replace_na(has_plan, FALSE),
    has_target = replace_na(has_target, FALSE)
  )

viz3_data <- viz3_base %>%
  pivot_longer(
    c(has_plan, has_target),
    names_to = "measure",
    values_to = "present"
  ) %>%
  group_by(has_inventory, measure) %>%
  summarise(
    n = n(),
    successes = sum(present),
    percent = mean(present) * 100,
    lower = binom.test(successes, n)$conf.int[1] * 100,
    upper = binom.test(successes, n)$conf.int[2] * 100,
    .groups = "drop"
  ) %>%
  mutate(
    measure = recode(measure,
                     has_plan = "Climate plan",
                     has_target = "Climate target"
    ),
    bar_id = paste0(
      if_else(has_inventory == "Yes", "Inv", "NoInv"),
      if_else(measure == "Climate plan", "_Plan", "_Target")
    )
  )


pairs <- combn(seq_len(nrow(viz3_data)), 2)

p_values <- apply(pairs, 2, function(pair) {
  i <- pair[1]
  j <- pair[2]
  
  prop.test(
    x = viz3_data$successes[c(i, j)],
    n = viz3_data$n[c(i, j)],
    correct = FALSE
  )$p.value
})

names(p_values) <- apply(pairs, 2, function(pair) {
  paste(viz3_data$bar_id[pair], collapse = "-")
})

p_adjusted <- p.adjust(p_values, method = "holm")
letters <- multcompView::multcompLetters(p_adjusted)$Letters

viz3_data$letter <- unname(letters[viz3_data$bar_id])

ggplot(viz3_data, aes(x = has_inventory, y = percent, fill = measure)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(0.75),
    width = 0.15
  ) +
  geom_text(
    aes(y = upper + 3, label = letter, group = measure),
    position = position_dodge(0.75),
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Climate plan" = "#287D8E",
    "Climate target" = "#E09F3E"
  )) +
  scale_y_continuous(
    limits = c(0, min(120, max(viz3_data$upper) + 12)),
    breaks = seq(0, 100, 20),
    labels = \(x) paste0(x, "%")
  ) +
  theme_minimal() +
  labs(
    title = "Climate Planning and Target-Setting",
    subtitle = "Bars sharing a letter are not significantly different (Holm-adjusted p < 0.05)",
    x = "Has reported emissions inventory?",
    y = "Jurisdictions with recorded plan or target",
    fill = NULL
  ) +
  theme(panel.grid.major.x = element_blank())



######## VIZ 4

initiative_counts <- Initiatives %>%
  distinct(climactor_id, initiative_id) %>%
  count(climactor_id, name = "initiative_count")

quantified_targets <- Targets %>%
  filter(!is.na(target_value), !is.na(target_unit), target_unit != "") %>%
  distinct(climactor_id) %>%
  mutate(has_quantified_target = TRUE)

viz4_base <- Actors %>%
  distinct(climactor_id) %>%
  left_join(initiative_counts, by = "climactor_id") %>%
  left_join(quantified_targets, by = "climactor_id") %>%
  mutate(
    initiative_count = replace_na(initiative_count, 0L),
    has_quantified_target = replace_na(has_quantified_target, FALSE),
    initiative_group = if_else(
      initiative_count >= 4, "4+", as.character(initiative_count)
    ),
    initiative_group = factor(
      initiative_group, levels = c("0", "1", "2", "3", "4+")
    )
  )

viz4_data <- viz4_base %>%
  group_by(initiative_group) %>%
  summarise(
    jurisdictions = n(),
    successes = sum(has_quantified_target),
    percent_with_target = mean(has_quantified_target) * 100,
    .groups = "drop"
  ) %>%
  filter(!is.na(initiative_group)) %>%
  mutate(group_id = as.character(initiative_group))

pairs <- combn(seq_len(nrow(viz4_data)), 2)

p_values <- apply(pairs, 2, function(pair) {
  i <- pair[1]
  j <- pair[2]
  
  prop.test(
    x = viz4_data$successes[c(i, j)],
    n = viz4_data$jurisdictions[c(i, j)],
    correct = FALSE
  )$p.value
})

names(p_values) <- apply(pairs, 2, function(pair) {
  paste(viz4_data$group_id[pair], collapse = "-")
})

p_adjusted <- p.adjust(p_values, method = "holm")
letters <- multcompView::multcompLetters(p_adjusted)$Letters

viz4_data$letter <- unname(letters[viz4_data$group_id])

ggplot(viz4_data, aes(x = initiative_group, y = percent_with_target)) +
  geom_col(fill = "#287D8E", width = 0.7) +
  geom_errorbar(
    aes(
      ymin = purrr::map_dbl(
        seq_len(nrow(viz4_data)),
        \(i) binom.test(
          viz4_data$successes[i],
          viz4_data$jurisdictions[i]
        )$conf.int[1] * 100
      ),
      ymax = purrr::map_dbl(
        seq_len(nrow(viz4_data)),
        \(i) binom.test(
          viz4_data$successes[i],
          viz4_data$jurisdictions[i]
        )$conf.int[2] * 100
      )
    ),
    width = 0.15
  ) +
  geom_text(
    aes(
      y = pmin(
        purrr::map_dbl(
          seq_len(nrow(viz4_data)),
          \(i) binom.test(
            viz4_data$successes[i],
            viz4_data$jurisdictions[i]
          )$conf.int[2] * 100
        ) + 7,
        112
      ),
      label = paste0(
        round(percent_with_target), "% (n=", jurisdictions, ")\n", letter
      )
    ),
    size = 3.5,
    fontface = "bold",
    lineheight = 1.2
  ) +
  scale_y_continuous(
    limits = c(0, 125),
    breaks = seq(0, 100, 20),
    labels = \(x) paste0(x, "%")
  ) +
  theme_minimal() +
  labs(
    title = "Climate Initiatives and Quantified Targets",
    subtitle = "Error bars show 95% confidence intervals; groups sharing a letter are not significantly different (Holm-adjusted p < 0.05)",
    x = "Number of distinct climate initiatives",
    y = "Jurisdictions with quantified targets"
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )




######## VIZ 5

sector_totals <- Emissions_Sectors %>%
  mutate(
    sector = if_else(
      is.na(sector) | sector == "Unspecified",
      "Unspecified / Unknown",
      sector
    )
  ) %>%
  group_by(sector) %>%
  summarise(
    total_emissions = sum(emissions_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sector = fct_reorder(sector, total_emissions)
  )

ggplot(sector_totals, aes(x = sector, y = total_emissions)) +
  geom_col(fill = "#287D8E", width = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  theme_minimal() +
  labs(
    title = "Reported Greenhouse Gas Emissions by Sector",
    subtitle = "Total reported emissions across available inventories",
    x = NULL,
    y = "Total emissions"
  ) +
  theme(
    panel.grid.major.y = element_blank()
  )


######## VIZ 6

earliest_plans <- Plans %>%
  filter(!is.na(plan_adopted_year)) %>%
  group_by(climactor_id) %>%
  summarise(first_plan_year = min(plan_adopted_year), .groups = "drop")

map_plans <- earliest_plans %>%
  left_join(
    key_dict %>%
      select(climactor_id, long_lat) %>%
      distinct(climactor_id, .keep_all = TRUE),
    by = "climactor_id"
  ) %>%
  filter(!is.na(long_lat)) %>%
  separate(
    long_lat,
    into = c("longitude", "latitude"),
    sep = " - ",
    convert = TRUE
  ) %>%
  arrange(desc(first_plan_year))

ggplot(map_plans, aes(x = longitude, y = latitude)) +
  annotation_borders(colour = "gray75", fill = "gray97") +
  geom_point(
    aes(colour = first_plan_year),
    size = 1.5,
    alpha = 0.8
  ) +
  scale_colour_viridis_c(
    option = "plasma",
    direction = -1,
    name = "Earliest recorded\nplan year"
  ) +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "The Geography of Recorded Climate Planning",
    subtitle = "Earliest recorded plan adoption year per jurisdiction",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    panel.grid = element_line(colour = "gray90"),
    legend.position = "right"
  )

