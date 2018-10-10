library(tidyverse)

# source des données : https://donnees.banquemondiale.org/indicateur/NY.gdp.pcap.cd

data <- read_csv("API_NY.GDP.PCAP.CD_DS2_fr_csv_v2_10140258.csv")

codes <- c("EAP", "LCN", "MEA", "SAS", "SSA", "USA")

pib_usa <- data$PIB[data$code == "USA"]

pib <- data %>%
  filter(code %in% codes) %>%
  gather(année, PIB, -country, -code) %>%
  select(-code) %>%
  spread(country, PIB) %>%
  mutate_at(.vars = vars(-année, -`États-Unis`), .funs = funs(. / `États-Unis`)) %>%
  select(-`États-Unis`) %>%
  gather(pays, PIB, -année)

pib %>%
  ggplot(mapping = aes(x = année, y = PIB, group = pays, color = pays)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Évolution du PIB/habitant de diverses régions du Tiers Monde en pourcentage du PIB/habitant des États-Unis") +
  xlab("Année") +
  ylab("PIB/habitant en proportion du PIB/habitant des États-Unis") +
  labs(color = "Région") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5))
  