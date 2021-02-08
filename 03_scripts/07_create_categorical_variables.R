if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
sp1<-read_rds("02_wdata/sponges_pa_Nov162020.rds")

sp2<-sp1 %>%
  mutate(pH=as.numeric(pH),
        ph.cat = case_when(
        pH < 6 ~"acidic",
        pH > 6 & pH < 7 ~ "slightly.acidic",
        pH > 7 & pH < 8 ~"slightly alkaline",
        pH >= 8 ~"alkaline"))

x<-sp2[is.na(sp2$ph.cat),]




