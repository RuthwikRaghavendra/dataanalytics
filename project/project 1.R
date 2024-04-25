install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)

unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_join <- full_join(unicef_indicator_2,unicef_metadata, by = join_by(country , time_period == year))
unicef_join <- full_join(unicef_indicator_2,unicef_metadata , by = c("country","time_period"= "year"))

unicef_join <- unicef_indicator_2 %>%
  full_join(unicef_metadata,by = c("country","time_period"= "year")) 

#Bar chart
filtered_data <- unicef_join %>%
  filter(country == "Brazil", time_period >= 2010, time_period <= 2020)

ggplot(filtered_data, aes(x = time_period, y = obs_value, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Use pretty_breaks to generate whole number breaks
  labs(x = "Time Period", y = "% of Adolscents", title = "Bar Chart of % Adolscents for Brazil") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly()

# Scatterplot
filtered_data <- unicef_join %>%
  filter( time_period >= 2010, time_period <= 2020)

top_TotalPopulation <- filtered_data %>%
  group_by(`Total Population`, sex,time_period ) %>%
  summarise(max_value = max('Total Population', na.rm = TRUE)) %>%
  top_n(45000000, max_value) %>%
  ungroup() %>%
  arrange(desc(max_value))

ggplot(top_TotalPopulation, aes(x = time_period, y = `Total Population`)) +
  geom_point(aes(size = max_value, color = sex), alpha = 0.6) +  # Assuming max_value is what you want to size your points by
  geom_smooth(method = "lm", color = "PURPLE") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Population Over Time Periods",
       x = "Time Period",
       y = "Total Population") +
  theme_minimal()
ggplotly()















top_TotalPopulation <- filtered_data %>%
  group_by(`Total Population`) %>%
  summarise(max_value = max(obs_value, na.rm = TRUE)) %>%
  top_n(10, max_value) %>%
  ungroup() %>%
  arrange(desc(max_value))

ggplot(top_TotalPopulation, aes(x = time_period, y = `Total Population`)) +
  geom_point(aes(color = sex, size = obs_value), alpha = 0.6) +
  geom_smooth(method = "lm", color = "PURPLE") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Use pretty_breaks to generate whole number breaks
  labs(title = "Relationship between Time Period and Total Population",
       x = "Time Period",
       y = "Total Population") +
  theme_minimal()
ggplotly() 

#Time series
ggplot(unicef_join, aes(x = time_period, y = obs_value)) +
  geom_line() +
  labs(x = "Year", y = "% Adolscents", title = "Time Series of % Adolscents")
ggplotly()

