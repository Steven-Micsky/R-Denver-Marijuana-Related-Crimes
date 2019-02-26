library(dplyr)
library(ggplot2)

# https://www.denvergov.org/opendata/dataset/city-and-county-of-denver-crime-marijuana

raw_data <- read.csv('crime_marijuana.csv')

mj_source <- raw_data %>%
  mutate(Incident.Date = as.Date(FIRST_OCCURENCE_DATE, format = "%d-%b-%y")) %>%
  mutate(Incident.Month = format(Incident.Date,'%Y-%m')) %>%
  mutate(Incident.Year = format(Incident.Date,'%Y'))
  

### total incidents by year
incidents_year <- mj_source %>%
  group_by(Incident.Year) %>%
  summarise(Total.Count = n())

# quick plot to understand total incidents by year
ggplot(incidents_year,aes(Incident.Year,Total.Count)) +
  geom_bar(stat='identity',fill='#00AED6') +
  labs(title = 'Marijuana Related Crimes by Year',
       subtitle = 'City of Denver',
       x = 'Incident Date',
       y = 'Incident Count') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# incidents grouped by neighborhood
incidents_year_neighborhood <- mj_source %>%
  group_by(Incident.Year,NEIGHBORHOOD_ID) %>%
  summarise(Total.Count = n())

# plot using facets for neighborhood
ggplot(incidents_year_neighborhood,aes(Incident.Year,Total.Count)) +
  geom_bar(stat='identity',fill='#00AED6') +
  facet_wrap(~NEIGHBORHOOD_ID) +
  labs(title = 'Marijuana Related Crimes by Neighborhood',
       subtitle = 'City of Denver',
       x = 'Incident Date',
       y = 'Incident Count') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
