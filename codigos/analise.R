#Analise

##Basicamente um documento para adicionar analises acerca do conteúdo e lidar com os dados

library(tidyverse)
library(tidytuesdayR)
library(skimr)

demo_by_first_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_first_language.csv')
demo_by_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_nationality.csv')
demo_by_reasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/demo_by_reasons.csv')
performance_by_first_language <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_first_language.csv')
performance_by_nationality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-08-18/performance_by_nationality.csv')

## Parte inicial de gerar o csv dos dados, para posteriormente serem movidos para a pasta dados (facilita processos futuros):

# write.csv(demo_by_first_language, "demo_by_first_language.csv", row.names = FALSE)
# write.csv(demo_by_nationality, "demo_by_nationality.csv", row.names = FALSE)
# write.csv(demo_by_reasons, "demo_by_reasons.csv", row.names = FALSE)
# write.csv(performance_by_first_language, "performance_by_first_language.csv", row.names = FALSE)
# write.csv(performance_by_nationality, "performance_by_nationality.csv", row.names = FALSE)


## Analise exploratória inicial:

glimpse(demo_by_first_language)       #Só é necessário verificar cada banco para então saber quais os nomes das variáveis
