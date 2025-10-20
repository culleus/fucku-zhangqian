setwd("E:/summer/nzdxw/rstudio")
library(ggplot2)
library(gridExtra)
library(egg)
xw <- read.csv("xw_data.csv",header = TRUE, stringsAsFactors = FALSE)
xw_do <- read.csv("xw_do.csv",header = TRUE, stringsAsFactors = FALSE)
level3 = c("XW-A","XW-B","XW-C")
level4 = c("Apr","Jul","Dec")

## water temperature
xw$site <- factor(xw$site, levels = level3)
xw$month <- factor(xw$month, levels = level4)
p6 <- ggplot(xw, aes(x = depth, y = T, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = "Depth (m)",y = "T (°C)",color = NULL,linetype = NULL) +
  coord_flip()+
  scale_y_continuous(limits = c(13,30),expand = c(0,0),position = "right")+
  scale_x_reverse(limits = c(160,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.key.width = unit(0.9, "cm"),
    legend.position = "inside",
    legend.position.inside  = c(0.75, 0.35),
    legend.text = element_text(size = 14,family = 'serif')
  )
p6fixed <- set_panel_size(p6, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw-wt2.svg',plot = p6fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## conductivity
p7 <- ggplot(xw, aes(x = depth, y = cond, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = "EC (μS/cm)") +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(160,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p7fixed <- set_panel_size(p7, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw-ec.svg',plot = p7fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## pH
p8 <- ggplot(xw, aes(x = depth, y = ph, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = "pH") +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(160,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p8fixed <- set_panel_size(p8, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw-ph.svg',plot = p8fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## dissolved oxygen
p9 <- ggplot(xw_do, aes(x = depth, y = DO, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = "DO (mg/L)") +
  coord_flip()+
  scale_y_continuous(expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(160,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p9fixed <- set_panel_size(p9, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw-do2.svg',plot = p9fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

## partial pressure of CO2
p10 <- ggplot(xw, aes(x = depth, y = pco2, color = site, linetype = month)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = NULL,y = expression(pCO[2]~'('*μatm*')')) +
  coord_flip()+
  scale_y_continuous(breaks = seq(0,12000,3000),expand = c(0.03,0),position = "right")+
  scale_x_reverse(limits = c(160,0),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 22,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p10fixed <- set_panel_size(p10, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw-co.svg',plot = p10fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
