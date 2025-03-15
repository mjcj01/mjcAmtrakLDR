library(tidyverse)
library(sf)

ipcd %>%
  filter(FAC_TYPE == 1) %>%
  reframe("C_BUS" = sum(MODE_BUS),
          "C_RAIL" = sum(MODE_RAIL),
          "C_I_SERV" = sum(I_SERVICE),
          "C_T_SERV" = sum(T_SERVICE)) %>%
  pivot_longer(cols = colnames(.)) %>%
  mutate("PCT" = value / 666)

