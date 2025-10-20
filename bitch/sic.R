setwd("E:/summer/nzdxw/rstudio")
getwd()
library(ggplot2)
library(egg)
library(grid)
apr <- read.csv("sic_apr.csv", header = TRUE, stringsAsFactors = FALSE)
jul <- read.csv("sic_jul.csv", header = TRUE, stringsAsFactors = FALSE)
dec <- read.csv("sic_dec.csv", header = TRUE, stringsAsFactors = FALSE)
level = c("NZD-A","NZD-B","NZD-C","XW-A","XW-B","XW-C")
color = c("#A4BDDB","#3491BF","#054D7B","#FDD49C","#F06546","#FF0000")
## April
apr$site <- factor(apr$site, levels = level)
p1 <- ggplot(apr, aes(x = depth, y = sic, color = site)) +
  geom_line(linewidth = 1) +
  labs(x = "Depth (m)",y = "Apr. SIc",color = NULL) +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  scale_color_manual(values = color)+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p1fixed <- set_panel_size(p1, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('sic-apr.svg',plot = p1fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## July
jul$site <- factor(jul$site, levels = level)
p2 <- ggplot(jul, aes(x = depth, y = sic, color = site)) +
  geom_line(linewidth = 1) +
  labs(x = NULL,y = "Jul. SIc",color = NULL) +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  scale_color_manual(values = color)+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p2fixed <- set_panel_size(p2, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('sic-jul.svg',plot = p2fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## December
dec$site <- factor(dec$site, levels = level)
p3 <- ggplot(dec, aes(x = depth, y = sic, color = site)) +
  geom_line(linewidth = 1) +
  labs(x = NULL,y = "Dec. SIc",color = NULL) +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  scale_color_manual(values = color)+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.text = element_text(size = 14,family = 'serif'),
  )
p3fixed <- set_panel_size(p3, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('sic-dec.svg',plot = p3fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
