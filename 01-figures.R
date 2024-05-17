#---------------------------------------Figures--------------------------------#
#-Author: Francisca Castro ----------------------------- Created: May 07, 2024-#
#-R Version: 4.4.0 ------------------------------------- Revised: May 17, 2024-#

# 0) Load Packages

pacman::p_load(ggplot2, dplyr, gridExtra)

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
