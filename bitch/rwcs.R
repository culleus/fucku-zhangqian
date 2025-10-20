setwd("E:/summer/nzdxw/rstudio")
df <- read.csv("rwcs_value.csv", header = TRUE, stringsAsFactors = FALSE)
# 箱线图
level = c("April","July","December")
df$month <- factor(df$month, levels = level, ordered = TRUE)
library(ggplot2)
color <- c("#9BBBE1", "#F09BA0")
p <- ggplot(df, aes(x = month, y = rwcs, fill = reservoir)) +
  geom_boxplot(width=0.6,alpha=0.8) +
  scale_fill_manual(values = color) +
  labs(x = NULL, y = "RWCS",fill = NULL)+
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text = element_text(size = 18, color = "black",family = "serif"),
    axis.title = element_text(size = 20, color = "black",family = 'serif'),
    legend.text = element_text(size = 16,family = 'serif'),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.8)
  )
ggsave('rwcs_boxplot.tiff', plot = p,
       path = 'E:/summer/nzdxw/tiff',
       width = 6, height = 5, units = 'in')
