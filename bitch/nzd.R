setwd("E:/summer/nzdxw/rstudio")
library(ggplot2)
library(egg)
library(grid)
wt <- read.csv("nzd_wt.csv",header = TRUE, stringsAsFactors = FALSE)
ph <- read.csv("nzd_ph.csv",header = TRUE, stringsAsFactors = FALSE)
do <- read.csv("nzd_do2.csv",header = TRUE, stringsAsFactors = FALSE)
pco2 <- read.csv("nzd_pco2.csv",header = TRUE, stringsAsFactors = FALSE)
cond <- read.csv("nzd_cond.csv",header = TRUE, stringsAsFactors = FALSE)
level1 = c("NZD-A","NZD-B","NZD-C")
level2 = c("Apr","Jul","Dec")

## water temperature
wt$month <- factor(wt$month, levels = level2)
wt$site <- factor(wt$site, levels = level1)
p1 <- ggplot(wt, aes(x = depth, y = T, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = "Depth (m)",y = "T (°C)",color = NULL, linetype = NULL) +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.key.width = unit(0.9, "cm"),
    legend.position = "inside",
    legend.position.inside  = c(0.7, 0.35),
    legend.text = element_text(size = 16,family = 'serif')
  )
p1fixed <- set_panel_size(p1, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd-wt.svg',plot = p1fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
dev.off()

## conductivity
cond$month <- factor(cond$month, levels = level2)
cond$site <- factor(cond$site, levels = level1)
p2 <- ggplot(cond, aes(x = depth, y = EC, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = "EC (μS/cm)") +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22, family = "serif"),
    axis.text = element_text(size = 18, family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p2fixed <- set_panel_size(p2, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd-ec.svg',plot = p2fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## pH
ph$month <- factor(ph$month, levels = level2)
ph$site <- factor(ph$site, levels = level1)
p3 <- ggplot(ph, aes(x = depth, y = pH, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = "pH") +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p3fixed <- set_panel_size(p3, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd-ph.svg',plot = p3fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## dissolved oxygen
do$month <- factor(do$month, levels = level2)
do$site <- factor(do$site, levels = level1)
p4 <- ggplot(do, aes(x = Depth, y = DO, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = "DO (mg/L)") +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p4fixed <- set_panel_size(p4, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd-do2.svg',plot = p4fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## partial pressure of CO2
pco2$month <- factor(pco2$month, levels = level2)
pco2$site <- factor(pco2$site, levels = level1)

ylable = expression(pCO[2]~(mu*atm))

p5 <- ggplot(pco2, aes(x = depth, y = pCO2, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = expression(pCO[2]~'('*μatm*')')) +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(200,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,color = 'black',family = 'serif'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p5fixed <- set_panel_size(p5, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd-co2.svg',plot = p5fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
