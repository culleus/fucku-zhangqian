setwd("E:/summer/nzdxw/rstudio")
library(ggplot2)
library(egg)
library(grid)
df <- read.csv("氮的数据.csv",header = TRUE, stringsAsFactors = FALSE)
xw <- df[c(1:27),]
nzd <- df[c(28:51),]
# XW-TN
p1 <- ggplot(xw, aes(x = depth, y = TN, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_shape_manual(values = c(15,16,17,22,21,24))+
  labs(x = "Depth (m)",y = "TN (mg/L)") +
  coord_flip()+
  scale_color_manual(values = c("#00bfc4", "#619cff", "#f564e3"))+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  scale_y_continuous(limits = c(0.05,0.75),breaks = seq(0,0.75,0.2),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.key.width = unit(0.9, "cm"),
    legend.position = "inside",
    legend.position.inside  = c(0.25, 0.3),
    legend.text = element_text(size = 14,family = 'serif'),
    legend.title = element_blank()
  )

p1fixed <- set_panel_size(p1, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw_tn.svg',plot = p1fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
# XW-NO3-N
p2 <- ggplot(xw, aes(x = depth, y = NO, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_shape_manual(values = c(15,16,17,22,21,24))+
  scale_y_continuous(limits = c(0.02,0.55),breaks = seq(0,0.55,0.1),expand = c(0,0))+
  labs(x = NULL, y = expression(paste(NO[3]^"-","-N ","(mg/L)",sep = "")))+
  coord_flip()+
  scale_color_manual(values = c("#00bfc4", "#619cff", "#f564e3"))+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )

p2fixed <- set_panel_size(p2, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw_no3n.svg',plot = p2fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
# xw-δ15N
p3 <- ggplot(xw, aes(x = depth, y = N15, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_color_manual(values = c("#00bfc4", "#619cff", "#f564e3"))+
  scale_y_continuous(limits = c(1.8,8),breaks = seq(0,8,2),expand = c(0,0))+
  scale_shape_manual(values = c(15,16,17,22,21,24))+
  labs(x = NULL, y = expression(paste(delta^15,"N ","(‰)",sep = ""))) +
  coord_flip()+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p3fixed <- set_panel_size(p3, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('xw_d15N.svg',plot = p3fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
# δ18O
p4 <- ggplot(df, aes(x = depth, y = O18, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_shape_manual(values = c(15,16,17,22,21,24))+
  labs(x = NULL, y = expression(paste(delta^18,"O ","(‰)",sep = ""))) +
  coord_flip()+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.key.width = unit(0.9, "cm"),
    legend.position = "inside",
    legend.position.inside  = c(0.7, 0.3),
    legend.text = element_text(size = 14,family = 'serif'),
    legend.title = element_blank()
  )

p4fixed <- set_panel_size(p4, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('d18O.svg',plot = p4fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
# AT O18/O16
p5 <- ggplot(df, aes(x = depth, y = ATO, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_shape_manual(values = c(15,16,17,22,21,24))+
  labs(x = NULL, y = "AT%") +
  coord_flip()+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )

p5fixed <- set_panel_size(p5, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('d18Oat.svg',plot = p5fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
# NZD-TN
p6 <- ggplot(nzd, aes(x = depth, y = TN, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_color_manual(values = c("#f8766d", "#b79f00", "#00ba38"))+
  scale_shape_manual(values = c(15,16,17,22,21))+
  labs(x = "Depth (m)",y = "TN (mg/L)") +
  coord_flip()+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  scale_y_continuous(limits = c(0.05,0.75),breaks = seq(0,0.75,0.2),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.key.width = unit(0.9, "cm"),
    legend.position = "inside",
    legend.position.inside  = c(0.25, 0.3),
    legend.text = element_text(size = 14,family = 'serif'),
    legend.title = element_blank()
  )
p6fixed <- set_panel_size(p6, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd_tn.svg',plot = p6fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
# NZD-NO3-N
p7 <- ggplot(nzd, aes(x = depth, y = NO, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_shape_manual(values = c(15,16,17,22,21))+
  scale_y_continuous(limits = c(0.02,0.55),breaks = seq(0,0.55,0.1),expand = c(0,0))+
  labs(x = NULL, y = expression(paste(NO[3]^"-","-N ","(mg/L)",sep = "")))+
  coord_flip()+
  scale_color_manual(values = c("#f8766d", "#b79f00", "#00ba38"))+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p7fixed <- set_panel_size(p7, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd_no3n.svg',plot = p7fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')
# NZD-δ15N
p8 <- ggplot(nzd, aes(x = depth, y = N15, color = site, shape = site, fill = site)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5)+
  scale_shape_manual(values = c(15,16,17,22,21))+
  scale_y_continuous(limits = c(1.8,8),breaks = seq(0,8,2),expand = c(0,0))+
  scale_color_manual(values = c("#f8766d", "#b79f00", "#00ba38"))+
  labs(x = NULL, y = expression(paste(delta^15,"N ","(‰)",sep = ""))) +
  coord_flip()+
  scale_x_reverse(limits = c(200,-10),expand = c(0,0))+
  theme(
    axis.title = element_text(size = 20,family = 'serif'),
    axis.text = element_text(size = 18,family = 'serif',color = 'black'),
    axis.text.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill = NA,linewidth  = 1),
    legend.position = "none"
  )
p8fixed <- set_panel_size(p8, width = unit(2.6, "in"), height = unit(3.3, "in"))
ggsave('nzd_d15N.svg',plot = p8fixed,
       path = 'E:/summer/nzdxw/graphs',
       width = 4, height = 5, units = 'in')

