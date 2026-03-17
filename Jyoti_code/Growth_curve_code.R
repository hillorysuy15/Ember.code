### Loading library and files ----

library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)


A <- read_excel("~/Documents/BCB5200/Isolate A.xlsx")
B <- read_excel("~/Documents/BCB5200/Isolate B .xlsx")
G <- read_excel("~/Documents/BCB5200/Isolate G .xlsx")
H <- read_excel("~/Documents/BCB5200/Isolate H .xlsx")

## Bind rows to one dataset ----
A$Isolate <- "A"
B$Isolate <- "B"
G$Isolate <- "G"
H$Isolate <- "H"
dat <- bind_rows(A, B, G, H) 

# Without log scale graph ----

p1 <- ggplot(dat, aes(x = Time_min, y = Mean_WT_Norm, color = Isolate)) +
  geom_point() +
  geom_line() +
  labs(title = "Growth Curve", x = "Time (min)", y = "Normalize OD 600 nm",
       subtitle = "based on different isolates ") +
  theme_bw() +
  theme(text = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1,
                                   size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 10, 
                                     hjust = 0.5,
                                     face = "bold"))


## Log scale Graph ----


p2 <- ggplot(dat, aes(x = Time_min, y = Mean_WT_Norm, color = Strain)) +
  geom_point() +
  geom_line() +
  labs(title = "Growth Curve", x = "Time (min)", y = "Normalize OD 600 nm") +
  scale_y_log10() +
  theme_bw() +
  labs(title = "Growth Curve", x = "Time (min)", y = "Normalize OD 600 nm",
       subtitle = "based on different isolates ") +
  theme_bw() +
  theme(text = element_text(size = 15), 
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1,
                                   size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 10, 
                                     hjust = 0.5,
                                     face = "bold"))

p1 + p2

## Save it to your laptop ----

ggsave(p1, file = "jyoti_code/plots/growth_curve.png", width = 12, height = 9, unit = "in", dpi = 400)
ggsave(p2, file = "jyoti_code/plots/growth_curve_log.png", width = 12, height = 9, unit = "in", dpi = 400)
