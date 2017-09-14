library(readr)
library(ggplot2)
library(scales)
library(ggthemes)
library(ggExtra)

N1N2_subset_nozero <- read_delim("N1N2_stops.txt", 
                                 "\t", escape_double = FALSE, col_names = FALSE, 
                                 trim_ws = TRUE)

colnames(N1N2_subset_nozero) <- c("Samp1","Samp2","RT1","RT2")

p <- ggplot(N1N2_subset_nozero) +
  geom_point(aes(RT1,RT2),size=0.25,alpha=0.3) +
  scale_x_log10(labels=comma, breaks=c(10,100,1000,10000)) +
  scale_y_log10(labels=comma, breaks=c(10,100,1000,10000)) +
  theme_bw() +
  labs(x="RT Stops, NAI-N3 Replicate 1",y="RT Stops, NAI-N3 Replicate 2", title="HepG2 Replicate RT Stops")

ggMarginal(p, type = "density")