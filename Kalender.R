# LIB ----

library(readr)
library(janitor)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ragg)

# IMPORT DATA ----

indonesia <- read_csv("KawalCovid19.csv") %>% clean_names()

indonesia$tanggal <- as.Date(indonesia$tanggal, format = "%d/%m/%y") # MENGUBAH KARAKTER MENJADI TANGGAL

indonesia <- indonesia %>%
  complete(tanggal = seq(ymd("2020-01-01"),
                         ymd("2020-12-31"),
                         "day")) %>%
  mutate(
    weekday = wday(tanggal, label = T, week_start = 1),
    month = month(tanggal, label = T, abbr = F),
    week = isoweek(tanggal),
    day = day(tanggal)
  )

indonesia[is.na(indonesia)] <- 0 # Mengubah NULL menjadi 0

# TEMA KALENDER ----
theme_calendar <- function(){
  
  theme(aspect.ratio = 1/2,
        
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        #axis.text = element_text(family = "Montserrat"),
        
        panel.grid = element_blank(),
        panel.background = element_blank(),
        
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        
        legend.position = "top",
        legend.text = element_text(hjust = .5),
        legend.title = element_text(size = 9, hjust = 1),
        
        plot.caption =  element_text(hjust = 1, size = 8, colour = "#333333", vjust = -5),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = .5, size = 26, 
                                  face = "bold", 
                                  margin = margin(0,0,0.5,0, unit = "cm")),
        plot.subtitle = element_text(hjust = .5, size = 16)
        
  )
}
# GRAFIK JUMLAH ORANG YANG DITES HARIAN ----

ggplot(indonesia,
       aes(weekday,-week, fill = orang_yang_dites_harian)) +
  geom_tile(colour = "white", size = .4)  +
  geom_text(aes(label = day, colour = "black"), size = 2.5) +
  guides(fill = guide_colorsteps(
    barwidth = 25,
    barheight = .4,
    title.position = "top",
    show.limits = TRUE
  )) +
  
  scale_fill_gradient2(
    low = "#57BB8A",
    mid = '#FFFF00',
    midpoint = 22739,
    high = '#FF0000',
    limits = c(0, 45479),
    breaks = c(seq(
      from = 5000, to = 45479, by = 5000
    ))
  ) +
  
  scale_colour_manual(values = c("#333333", "white"), guide = FALSE) +
  facet_wrap( ~ month,
              nrow = 4,
              ncol = 3,
              scales = "free") +
  labs(
    title = "COVID-19 Indonesia 2020",
    subtitle = "Jumlah Orang Yang Dites Harian",
    caption = "Grafik: M. Zakiyuddin | Data: KawalCovid19",
    fill = ""
  ) +
  theme_calendar() +
  ggsave(
    "Jumlah Orang Yang Dites.png",
    width = 8,
    height = 10,
    device = agg_png()
  )

# GRAFIK KASUS AKTIF ----

ggplot(indonesia,
       aes(weekday,-week, fill = kasus_aktif)) +
  geom_tile(colour = "white", size = .4)  +
  geom_text(aes(label = day, colour = "black"), size = 2.5) +
  guides(fill = guide_colorsteps(
    barwidth = 25,
    barheight = .4,
    title.position = "top",
    show.limits = TRUE
  )) +
  
  scale_fill_gradient2(
    low = "#57BB8A",
    mid = '#FFFF00',
    midpoint = 54981.5,
    high = '#FF0000',
    limits = c(0, 109963),
    breaks = c(seq(
      from = 15000, to = 109963, by = 15000
    ))
  ) +
  
  scale_colour_manual(values = c("#333333", "white"), guide = FALSE) +
  facet_wrap( ~ month,
              nrow = 4,
              ncol = 3,
              scales = "free") +
  labs(
    title = "COVID-19 Indonesia 2020",
    subtitle = "Kasus Aktif",
    caption = "Grafik: M. Zakiyuddin | Data: KawalCovid19",
    fill = ""
  ) +
  theme_calendar() +
  ggsave(
    "Kasus Aktif Indonesia.png",
    width = 8,
    height = 10,
    device = agg_png()
  )

# GRAFIK PENAMBAHAN KASUS HARIAN ----

ggplot(indonesia,
       aes(weekday,-week, fill = positif_harian)) +
  geom_tile(colour = "white", size = .4)  +
  geom_text(aes(label = day, colour = "black"), size = 2.5) +
  guides(fill = guide_colorsteps(
    barwidth = 25,
    barheight = .4,
    title.position = "top",
    show.limits = TRUE
  )) +
  
  scale_fill_gradient2(
    low = "#57BB8A",
    mid = '#FFFF00',
    midpoint = 4184.5,
    high = '#FF0000',
    limits = c(0, 8369),
    breaks = c(seq(
      from = 1000, to = 8369, by = 1000
    ))
  ) +
  
  scale_colour_manual(values = c("#333333", "white"), guide = FALSE) +
  facet_wrap( ~ month,
              nrow = 4,
              ncol = 3,
              scales = "free") +
  labs(
    title = "COVID-19 Indonesia 2020",
    subtitle = "Penambahan Kasus Harian",
    caption = "Grafik: M. Zakiyuddin | Data: KawalCovid19",
    fill = ""
  ) +
  theme_calendar() +
  ggsave(
    "Penambahan Kasus Harian Indonesia.png",
    width = 8,
    height = 10,
    device = agg_png()
  )

# GRAFIK KEMATIAN HARIAN ----

ggplot(indonesia,
       aes(weekday,-week, fill = meninggal_harian)) +
  geom_tile(colour = "white", size = .4)  +
  geom_text(aes(label = day, colour = "black"), size = 2.5) +
  guides(fill = guide_colorsteps(
    barwidth = 25,
    barheight = .4,
    title.position = "top",
    show.limits = TRUE
  )) +
  scale_fill_gradient2(
    low = "#57BB8A",
    mid = '#FFFF00',
    midpoint = 129,
    high = '#FF0000',
    limits = c(0, 258),
    breaks = c(seq(
      from = 20, to = 258, by = 20
    ))
  ) +
  scale_colour_manual(values = c("#333333", "white"), guide = FALSE) +
  facet_wrap( ~ month,
              nrow = 4,
              ncol = 3,
              scales = "free") +
  labs(
    title = "COVID-19 Indonesia 2020",
    subtitle = "Kasus Kematian Harian",
    caption = "Grafik: M. Zakiyuddin | Data: KawalCovid19",
    fill = ""
  ) +
  theme_calendar() +
  ggsave(
    "Kasus Kematian Harian Indonesia.png",
    width = 8,
    height = 10,
    device = agg_png()
  )
