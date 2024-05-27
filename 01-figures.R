#---------------------------------------Figures--------------------------------#
#-Author: Francisca Castro ----------------------------- Created: May 07, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: May 17, 2024-#

# 0) Load Packages

pacman::p_load(ggplot2, dplyr, gridExtra, panelView, tidyr)

# Figure 1: Representative countries regime change

argentina_plot <- ggplot(transitional_countries %>% filter(country_name == "Argentina"), 
       aes(x = year, y = e_p_polity)) +
  geom_ribbon(aes(ymin = e_p_polity - std_dev_polity, ymax = e_p_polity + std_dev_polity), fill = "gray50", alpha = 0.1) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(-10, 10)) +
  labs(title = "Argentina", x = "Year",
       y = "Polity Score") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


argentina_plot

bangladesh_plot <- ggplot(transitional_countries %>% filter(country_name == "Bangladesh"), 
                         aes(x = year, y = e_p_polity)) +
  geom_ribbon(aes(ymin = e_p_polity - std_dev_polity, ymax = e_p_polity + std_dev_polity), fill = "gray50", alpha = 0.1) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(-10, 10)) +
  labs(title = "Bangladesh", x = "Year",
       y = "Polity Score") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


bangladesh_plot

ghana_plot <- ggplot(transitional_countries %>% filter(country_name == "Ghana"), 
                          aes(x = year, y = e_p_polity)) +
  geom_ribbon(aes(ymin = e_p_polity - std_dev_polity, ymax = e_p_polity + std_dev_polity), fill = "gray50", alpha = 0.1) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(-10, 10)) +
  labs(title = "Ghana", x = "Year",
       y = "Polity Score") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))


ghana_plot

south_korea_plot <- ggplot(transitional_countries %>% filter(country_name == "South Korea"), 
                     aes(x = year, y = e_p_polity)) +
  geom_ribbon(aes(ymin = e_p_polity - std_dev_polity, ymax = e_p_polity + std_dev_polity), fill = "gray50", alpha = 0.1) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(-10, 10)) +
  labs(title = "South Korea", x = "Year",
       y = "Polity Score") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

south_korea_plot

polity_score_countries <- grid.arrange(argentina_plot, bangladesh_plot, ghana_plot, south_korea_plot, ncol = 2, nrow = 2)

ggsave("01-outputs/polity_score_countries.png", plot = polity_score_countries, dpi = 600, width = 9, height = 5)

# Figure 2: Panel view

names(tj_countries_full)

tj_countries_plot_data <- tj_countries_full %>%
  # Filter out rows where e_p_polity < 0
  filter(e_p_polity >= 0) %>%
  mutate(
    # Transitional justice measures
    TJ = if_else(trial == 1 | tc == 1 | amnesty == 1 | reparation == 1 | lustration == 1, 1, 0),
    # Post-transition without TJ measures
    post_transition = if_else(TJ == 0, 1, 0),
    # Post-transition with TJ measures
    TJ_implementations = if_else(TJ == 1, 1, 0)
  ) %>%
  mutate(
    country_name = case_when(
      country_name == "Democratic Republic of the Congo" ~ "DRC",
      country_name == "The Gambia" ~ "Gambia",
      TRUE ~ country_name
    ))

#- Keep countries for which there's public opinion data 
public_opinion_countries <- unique(c(
  afrobarometer_all$country_cod,
  arab_barometer_all$country_cod,
  asiabarometer_all$country_cod,
  lapop$country_cod,
  wvs_full$country_cod
))

tj_countries_plot_data %<>%
  filter(country_cod %in% public_opinion_countries)

panel_figure <- panelview(post_transition ~ TJ_implementations, data = tj_countries_plot_data,
                          index = c("country_name", "year"), main = "",
                          axis.lab.gap = c(2,0), xlab="", ylab="",
                          legend.labs = c("Partial Democracy/Democracy", "TJ Procedures", "Non-Democracy")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = c("bottom"),
        plot.title = element_text(hjust = 0.5)) +
  geom_tile(color = "lightgray", linewidth = 0.2)  # Adding borders to the tiles

# Print the figure
panel_figure

ggsave("01-outputs/panel_figure.png", plot = panel_figure, dpi = 600, 
       unit = "mm", width = 170, height = 210)


