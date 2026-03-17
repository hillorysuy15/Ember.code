
### Loading library and files

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


### Wrangling Part ----

A1 <- A %>% 
  mutate(
    ave_blank = rowMeans(across(c(A12, B12, C12)), na.rm = TRUE),
    A_norm = pmax(A1 - ave_blank, 0),
    B_norm = pmax(B1 - ave_blank, 0),
    C_norm = pmax(C1 - ave_blank, 0),
    ave_norm_WT = rowMeans(across(c(A_norm, B_norm, C_norm)), na.rm = TRUE),
    minutes = (row_number() - 1) * 10,  # 0, 10, 20, 30…
    hour = minutes / 60 
  )

A2 <- A1 %>%
  mutate(
    #Time_parsed = ymd_hms(Time),
                     # convert minutes to hours
  )
# Work on Lubridate ----



                            
### Visualizations ----
p1 <- ggplot(A1, aes(x = Time_min, y = Mean_WT_Norm)) +
  geom_point(color = "lightblue") +
  geom_line() 

p2 <- ggplot(B, aes(x = Time_min, y = Mean_WT_Norm )) +
  geom_point(color = "pink") +
  geom_line()

p1 + p2

ggplot() +
  geom_point(data = A1, aes(x = Time_min, y = Mean_WT_Norm), color = "lightblue") +
  geom_line(data = A1, aes(x = Time_min, y = Mean_WT_Norm), color = "lightblue") +
  geom_point(data = B, aes(x = Time_min, y = Mean_WT_Norm), color = "pink") +
  geom_line(data = B, aes(x = Time_min, y = Mean_WT_Norm), color = "red") +
  geom_point(data = G, aes(x = Time_min, y = Mean_WT_Norm), color = "Blue") +
  geom_line(data = G, aes(x = Time_min, y = Mean_WT_Norm), color = "Blue") +
  geom_point(data = H, aes(x = Time_min, y = Mean_WT_Norm), color = "darkGreen") +
  geom_line(data = H, aes(x = Time_min, y = Mean_WT_Norm), color = "darkGreen") +
  scale_y_log10()

#### Graphs for Quarto docs 

library(dplyr)

A$Isolate <- "A"
B$Isolate <- "B"
G$Isolate <- "G"
H$Isolate <- "H"
dat <- bind_rows(A, B, G, H)

View(dat)

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

######################################################################################

## Working on these 
dat <- bind_rows(A1, B)
ggplot(combined, aes(x = Time_min, y = Mean_WT_Norm)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Strain)

ggplot(combined, aes(x = Time_min, y = Mean_WT_Norm, group = Strain)) +
  geom_line() +
  geom_point()

datasets <- list(A1 = A1, B = B)

ggplot() +
  geom_line(data = bind_rows(datasets, .id = "Strain"),
            aes(Time_min, Mean_WT_Norm, color = Strain))



## Publication Style graph ----
library(ggplot2)
library(dplyr)
A$Strain <- "A1"
B$Strain <- "B"
G$Strain <- "G"
H$Strain <- "H"
combined <- bind_rows(A, B, G, H)
combined <- combined %>% arrange(Strain, Time_min)
ggplot(combined, aes(x = Time_min, y = Mean_WT_Norm,
                     color = Strain, group = Strain)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Normalized WT Signal Over Time",
    x = "Time (minutes)",
    y = "Normalized WT Signal",
    color = "Strain"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black"),
    legend.position = "right"
  )


ggplot(combined, aes(Time_min, Mean_WT_Norm,
                     color = Strain, group = Strain)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_log10() +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Time (minutes)",
    y = "Normalized WT Signal (log scale)",
    color = "Strain"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    legend.position = "top"
  )
