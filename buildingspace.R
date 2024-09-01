library(ggplot2)
library(ggbreak)
library(plyr)
#library(gridExtra)
library(ggpubr)
library(ggpmisc)
#library(sciplot)
#library(Hmisc)
library(dplyr)
library(maps)
library(maptools)
library(mapproj)
library(ggforce)
#library(ggh4x)
library(grid)
library(gridExtra)
library(ggrepel)
#library(R0)
#library(tidyr)
library(sf)
library(ineq)
theme_set(theme_bw())
setwd ("D:/英文")
#============================Module 1: Data reading=============================
#基础数据读取与处理 
mydata1 <- read.csv("D:/全球建筑高度.csv", 
                    header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空
mydata2 <- read.csv("D:/bvbs_city.csv", 
                    header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空
reg_gini <- read.csv("D:/全球分区gini梯度2.csv", 
                     header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空
country_gini <- read.csv("D:/全球国家gini梯度2.csv", 
                         header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空
reg_mean <- read.csv("D:/梯度region2.csv", 
                     header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空
city <- read.csv("D:/city.csv", 
                 header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空
beta <- read.csv("D:/beta.csv", 
                 header=TRUE, sep=",",na.strings = 0) #读取数据时0值设空

#============================Module 2: Building volume per capita=========
# 按照首字母排序给 region 编号
region_order <- c(
  "East Asia and the Pacific" = "#1 East Asia and the Pacific",
  "Europe and Japan" = "#2 Europe and Japan",
  "Land-Rich Developed Countries" = "#3 Land-Rich Developed Countries",
  "Latin America and The Caribbean" = "#4 Latin America and The Caribbean",
  "Southeast Asia" = "#5 Southeast Asia",
  "South and Central Asia" = "#6 South and Central Asia",
  "Sub-Saharan Africa" = "#7 Sub-Saharan Africa",
  "West Asia and North Africa" = "#8 West Asia and North Africa"
)
mydata2 <- mydata2 %>%
  mutate(region2_new = region_order[region2])
mydata2$region_number <- sub(" .*", "", mydata2$region2_new)  # 只提取 #编号


region_colors <- c(
  "#3" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6" = "#D1D759"            # Slightly Lighter Deep Lemon
)

# 计算每个 region 的平均值
region_means <- mydata2 %>%
  group_by(region_number) %>%
  summarize(mean_bv_pop = mean(bv_pop)) %>%
  arrange(desc(mean_bv_pop))

# 按照平均值对 region 进行降序排列
mydata2$region_number <- factor(mydata2$region_number, levels = region_means$region_number)

unique(mydata2$region_number)

mydata2[which(mydata2$region_number==""),]$region

p8 <- ggplot(mydata2, aes(region_number, bv_pop, fill=region_number)) +
  geom_boxplot(outlier.size = 0.2, size=0.2) +
  stat_boxplot(geom="errorbar", width=0.2, size=0.2) +
  scale_fill_manual(values = region_colors) +  # 按 region 指定颜色
  
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red") +
  stat_summary(fun=mean, geom="text", aes(label=round(..y.., 1)), vjust=-0.5, size=2.2) +
  labs(x="Region", y="Volume per capita (m³)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(fill = FALSE)  # 关闭图例显


region_colors <- c(
  "#3 Land-Rich Developed Countries" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2 Europe and Japan" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1 East Asia and the Pacific" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4 Latin America and The Caribbean" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7 Sub-Saharan Africa" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8 West Asia and North Africa" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5 Southeast Asia" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6 South and Central Asia" = "#D1D759"            # Slightly Lighter Deep Lemon
)

bv3 <- ggplot(mydata2, 
              aes(log10(bvpop_2000),log10(bv_pop),
                  colour=region2_new,
                  size=pop_city2/1000000)) +
  geom_point(shape=21,stroke = 0.4) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2) +
  scale_y_continuous(limits = c(2, 3.5)) +
  scale_x_continuous(limits = c(2, 3.5)) +
  labs( x = expression(paste(Log[10],"[Volume per capita (", m^3,", 2000)]")),  
        y = expression(paste(Log[10],"[Volume per capita (", m^3,", 2020)]"))) +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        
        legend.text = element_text(size = 6))+
  guides(size="none")  # 关闭SUM的图例显


# 使用dplyr的mutate函数添加排名
mydata2 <- mydata2 %>%
  mutate(rank_bs_city = rank(bs_city, ties.method = "average"), # 对bs_2000进行排名
         rank_bv_city = rank(bv_city, ties.method = "average")) # 对bv_2000进行排名

p2 <- ggplot(mydata2, aes(x = rank_bs_city, y = rank_bv_city,
                          colour=region2_new,
                          size=pop_city2/1000000)) +
  geom_point(shape = 21, stroke = 0.4) +
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  labs(x = "Rank of building surface (2020)",
       y = "Rank of building volume (2020)") +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(size = "none")


# 设置各个图的主题，减小边缘的间距



tiff(file="图2 人均建筑体量.tiff", res = 600, width = 4000, height = 1500, compression = "lzw")
ggarrange(p8,
          bv3,
          p2,
          nrow = 1,
          labels = c("b", "c", "d"),
          align = "h", 
          font.label = list(size = 11),
          common.legend = TRUE,
          legend = "bottom",
          label.x = c(0, -0.02, -0.04)  # 只调整 c 和 d 的位置
)
dev.off()
getwd()

#============================Module 3: scaling law of Building=============================
region_order <- c(
  "East Asia and the Pacific" = "#1 East Asia and the Pacific",
  "Europe and Japan" = "#2 Europe and Japan",
  "Land-Rich Developed Countries" = "#3 Land-Rich Developed Countries",
  "Latin America and The Caribbean" = "#4 Latin America and The Caribbean",
  "Southeast Asia" = "#5 Southeast Asia",
  "South and Central Asia" = "#6 South and Central Asia",
  "Sub-Saharan Africa" = "#7 Sub-Saharan Africa",
  "West Asia and North Africa" = "#8 West Asia and North Africa"
)
city <- city %>%
  mutate(region_new = region_order[region])

region_colors <- c(
  "#3 Land-Rich Developed Countries" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2 Europe and Japan" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1 East Asia and the Pacific" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4 Latin America and The Caribbean" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7 Sub-Saharan Africa" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8 West Asia and North Africa" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5 Southeast Asia" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6 South and Central Asia" = "#D1D759"            # Slightly Lighter Deep Lemon
)

city1 <- ggplot(city, 
                aes(log10(pop2_city),
                    log10(bv2_city),
                    colour=factor(region_new))) +
  geom_point(size=0.8, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE,linewidth = 1) +  # 修改拟合线的颜色为蓝色
  #scale_color_manual(values = c('#F7903D', '#4D85BD', '#59A95A', '#E6AF0C')) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.045 # 修正字体设置
  ) +
  scale_y_continuous(limits = c(8, 11)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10],"[Population (person,2020)]")),  # 修改X轴名称
    y = expression(paste(Log[10],"[Sum of volume (m"^3~",2020)]"))  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors) +  # 按 region 指定颜色
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 6))


city2 <- ggplot(city, 
                aes(log10(pop2_city),
                    log10(bs2_city),
                    colour=factor(region_new))) +
  geom_point(size=0.8, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE,linewidth = 1) +  # 修改拟合线的颜色为蓝色
  #scale_color_manual(values = c('#F7903D', '#4D85BD', '#59A95A', '#E6AF0C')) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.045 # 修正字体设置
  ) +
  scale_y_continuous(limits = c(7, 9.8)) +
  scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10],"[Population (person,2020)]")),  # 修改X轴名称
    y = expression(paste(Log[10],"[Sum of surface (m"^3~",2020)]"))  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors) +  # 按 region 指定颜色
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 6))

region_order <- c(
  "East Asia and the Pacific" = "#1 East Asia and the Pacific",
  "Europe and Japan" = "#2 Europe and Japan",
  "Land-Rich Developed Countries" = "#3 Land-Rich Developed Countries",
  "Latin America and The Caribbean" = "#4 Latin America and The Caribbean",
  "Southeast Asia" = "#5 Southeast Asia",
  "South and Central Asia" = "#6 South and Central Asia",
  "Sub-Saharan Africa" = "#7 Sub-Saharan Africa",
  "West Asia and North Africa" = "#8 West Asia and North Africa"
)
mydata2 <- mydata2 %>%
  mutate(region2_new = region_order[region2])

city3 <- ggplot(mydata2, 
                aes(log10(pop_city2),
                    gini_bv,
                    colour=factor(region2_new))) +
  geom_point(size=0.8, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE,linewidth = 1) +  # 修改拟合线的颜色为蓝色
  #scale_color_manual(values = c('#F7903D', '#4D85BD', '#59A95A', '#E6AF0C')) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.045 # 修正字体设置
  ) +
  scale_y_continuous(limits = c(0.15, 0.75)) +
  #scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10],"[Population (person,2020)]")),  # 修改X轴名称
    y = "Gini Coefficient of\n volume per capita (2020)"  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors) +  # 按 region 指定颜色
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 6))


city4 <- ggplot(mydata2, 
                aes(log10(pop_city2),
                    gini_bs,
                    colour=factor(region2_new))) +
  geom_point(size=0.8, shape=21, stroke = 0.3) +
  geom_smooth(method = "lm", se = FALSE,linewidth = 1) +  # 修改拟合线的颜色为蓝色
  #scale_color_manual(values = c('#F7903D', '#4D85BD', '#59A95A', '#E6AF0C')) +
  stat_poly_eq(
    aes(label = paste(..eq.label.., ..adj.rr.label.., sep = '~~~')),
    formula = y ~ x,
    parse = TRUE,
    size = 2,
    vstep = 0.045 # 修正字体设置
  ) +
  scale_y_continuous(limits = c(0.15, 0.75)) +
  #scale_x_continuous(limits = c(NA, NA)) +
  labs(
    x = expression(paste(Log[10],"[Population (person,2020)]")),  # 修改X轴名称
    y = "Gini Coefficient of\n surface per capita (2020)"  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors) +  # 按 region 指定颜色
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 6))


tiff(file="图10 city线性.tiff", res = 600, width = 3600, height =3100,  compression = "lzw")
ggarrange(city1,
          city2,
          city3,
          city4,
          nrow = 2,
          ncol = 2,
          labels=c("a","b","c","d"),
          align = "v",
          common.legend = TRUE,legend="bottom")
dev.off()
getwd()

#============================Module 4: Gini Coefficient of BVPC=============================

region_colors <- c(
  "#3" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6" = "#D1D759"            # Slightly Lighter Deep Lemon
)
# 计算每个 region 的平均值
region_means <- mydata2 %>%
  group_by(region_number) %>%
  summarize(mean_gini_bv = mean(gini_bv)) %>%
  arrange(desc(mean_gini_bv))

# 按照平均值对 region 进行降序排列
mydata2$region_number <- factor(mydata2$region_number, levels = region_means$region_number)

unique(mydata2$region_number)

mydata2[which(mydata2$region_number==""),]$region



p7 <- ggplot(mydata2, aes(region_number, gini_bv, fill=region_number)) +
  geom_boxplot(outlier.size = 0.2, size=0.2) +
  stat_boxplot(geom="errorbar", width=0.2, size=0.2) +
  scale_fill_manual(values = region_colors) +  # 按 region 指定颜色
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red") +
  stat_summary(fun=mean, geom="text", aes(label=sprintf("%.2f", ..y..)), vjust=-0.5, size=2.2) +
  labs(x="Region", y="Gini Coefficient of\n volume per capita (2020)") +
  #scale_x_discrete(label =c("#1","#2","#3","#4","#5","#6","#7","#8"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(fill = FALSE)  

region_colors <- c(
  "#3 Land-Rich Developed Countries" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2 Europe and Japan" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1 East Asia and the Pacific" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4 Latin America and The Caribbean" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7 Sub-Saharan Africa" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8 West Asia and North Africa" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5 Southeast Asia" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6 South and Central Asia" = "#D1D759"            # Slightly Lighter Deep Lemon
)
bv_gini <- ggplot(mydata2, aes(ginibv_2000,gini_bv,
                               colour=region2_new,
                               size=pop_city2/1000000)) +
  geom_point(shape=21,stroke = 0.4) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2)  +
  scale_y_continuous(limits = c(0.1, 0.7)) +
  scale_x_continuous(limits = c(0.1, 0.7)) +
  labs(
    x = expression(paste("Gini Coefficient of volume per capita(2000)")),  # 修改X轴名称
    y ="Gini Coefficient of\n volume per capita (2020)"  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title.y =element_text(size=7.5),
        axis.title.x =element_text(size=6.7))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(size="none")  # 关闭SUM的图例显



gini_2020 <- ggplot(mydata2, aes(gini_bs,gini_bv,
                                 colour=region2_new,
                                 size=pop_city2/1000000)) +
  geom_point(shape=21,stroke = 0.4) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2)  +
  scale_y_continuous(limits = c(0.05, 0.7)) +
  scale_x_continuous(limits = c(0.05, 0.7)) +
  labs(
    y ="Gini Coefficient of\n volume per capita (2020)",  # 修改X轴名称
    x = expression(paste("Gini Coefficient of surface per capita(2020)"))  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title.y =element_text(size=7.5),
        axis.title.x =element_text(size=6.7))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(size="none")  # 关闭SUM的图例显

tiff(file="图4 gini人均建筑体量.tiff", res = 600, width = 4000, height = 1500, compression = "lzw")
ggarrange(p7,
          bv_gini,
          gini_2020,
          nrow = 1,
          labels = c("b", "c", "d"),
          align = "h", 
          font.label = list(size = 11),
          common.legend = TRUE,legend="bottom")
dev.off()
getwd()

#============================Module 5: Gini Coefficient of BVPC(Urban-Rural)=============================
value_colors <- c("UC" = "#D54846", "DUC" = "#DA8F87", "SUC" = "#F3DBD6", 
                  "SU" = "#D9E0E7", "RC" = "#98CADD", "LDR" = "#61AACF")

country_gini$VALUE <- factor(country_gini$VALUE, levels = c("UC", "DUC", "SUC", "SU", "RC", "LDR", "VLDR"))

country_bv <- ggplot(country_gini, 
                     aes(x = VALUE, 
                         y = gini_bv, 
                         group = VALUE, 
                         fill = VALUE)) +
  guides(fill=FALSE) +
  geom_boxplot(size = 0.2, outlier.size = 0.2) +
  stat_boxplot(geom = "errorbar",size=0.2, width=0.3)+  
  facet_wrap(~region,nrow=2)+
  scale_fill_manual(values = value_colors) +
  scale_x_discrete(labels = c("UC", "DU", "SU", "SC", "RC", "LC", "VLDR")) +  # Modify x-axis labels
  labs(x="Urban-rural typologies", 
       y="Gini Coefficient  of \nvolume per capita in 2020")+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(    axis.text = element_text(size = 7),
            axis.title = element_text(size = 8),
            strip.background = element_blank(),  
            strip.text = element_text(color = "black",size=6) )



country_bv2 <- ggplot(country_gini,
                      aes(x = VALUE, 
                          y = gini_bvc, 
                          group = VALUE, 
                          fill = VALUE)) +
  guides(fill=FALSE) +
  geom_boxplot(size =0.2, outlier.size = 0.2) +
  stat_boxplot(geom = "errorbar", size = 0.2, width=0.3)+ 
  facet_wrap(~region,nrow=2)+
  scale_fill_manual(values = value_colors) +
  scale_x_discrete(labels = c("UC", "DU", "SU", "SC", "RC", "LC")) +  # Modify x-axis labels
  scale_y_continuous(limits = c(-0.25, 0.25))+
  geom_hline(yintercept = 0, color="blue", size=0.5,linetype = "dashed") +  # 添加红色虚线
  labs(x="Urban-rural typologies", #Date
       y="Changes of Gini Coefficient  \nof volume per capita (2000-2020)")+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    strip.background = element_blank(),  
    strip.text = element_text(color = "black",size=6) )

tiff(file="图5 gini城乡梯度.tiff", res = 600, width = 3600, height =3200,  compression = "lzw")
ggarrange(country_bv,country_bv2,
          nrow = 2,
          ncol=1,
          labels=c("a","b"),
          align = "v",label.y = 1.02, label.x = 0.02,
          font.label = list(size = 12))
dev.off()
getwd()

#============================Module 6: average Building Height=============================
region_order <- c(
  "East Asia and the Pacific" = "#1 East Asia and the Pacific",
  "Europe and Japan" = "#2 Europe and Japan",
  "Land-Rich Developed Countries" = "#3 Land-Rich Developed Countries",
  "Latin America and The Caribbean" = "#4 Latin America and The Caribbean",
  "Southeast Asia" = "#5 Southeast Asia",
  "South and Central Asia" = "#6 South and Central Asia",
  "Sub-Saharan Africa" = "#7 Sub-Saharan Africa",
  "West Asia and North Africa" = "#8 West Asia and North Africa"
)
mydata1 <- mydata1 %>%
  mutate(Region_new = region_order[Region])

region_colors <- c(
  "#3 Land-Rich Developed Countries" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2 Europe and Japan" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1 East Asia and the Pacific" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4 Latin America and The Caribbean" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7 Sub-Saharan Africa" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8 West Asia and North Africa" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5 Southeast Asia" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6 South and Central Asia" = "#D1D759"            # Slightly Lighter Deep Lemon
)

# 计算每个 region_new 的平均值
region_new_means <- mydata1 %>%
  group_by(Region_new) %>%
  summarize(mean_height = mean(平均建筑高度)) %>%
  arrange(desc(mean_height))

# 按照平均值对 region_new 进行降序排列
mydata1$Region_new <- factor(mydata1$Region_new, levels = region_new_means$Region_new)
unique(mydata1$Region_new)
mydata1[which(mydata1$Region_new==""),]$region_new

p6 <- ggplot(mydata1,aes(x=Region_new,y=平均建筑高度,fill=Region_new))+geom_boxplot(linewidth=0.3,outlier.size =0.3 )+
  # guides(fill=FALSE) + # fill指的是图例中的fill，和aes中对应
  stat_boxplot(geom="errorbar", width=0.2, size=0.2) +
  scale_fill_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  # 添加用于计算和显示均值的代码
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="red") +
  stat_summary(fun=mean, geom="text", aes(label=round(..y.., 1)), vjust=-0.5, size=2.2) +
  #scale_y_continuous(limits = c(0, 3000))+
  # theme(#legend.title=element_blank(),
  # legend.position = c(0.9, 0.9))+
  labs(x="Region", #Date
       y="Average Height (m)")+  
  scale_x_discrete(label =c("#7","#4","#8","#6","#1","#2","#5","#3"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))#+
#guides(fill = FALSE)

mydata1$pop2 <- cut(mydata1$pop, 
                    breaks = c(-Inf, 1000000, 2000000, 4000000, 8000000, Inf), 
                    labels = c(1, 2, 3, 4, 5), 
                    right = FALSE)

p8 <- ggplot(mydata1, aes(pop2, 平均建筑高度)) +
  geom_boxplot(linewidth = 0.3, outlier.size = 0.3, fill = "white") +
  geom_boxplot(outlier.size = 0.2, size = 0.2, fill = "white") + 
  stat_boxplot(geom = "errorbar", width = 0.2, size = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color = "red") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), vjust = -0.5, size = 2.2) +
  labs(x = expression(paste("Population (", 10^4, " person)")), y = "Average Height (m)") +
  scale_x_discrete(label =c("50-100","100-200","200-400","400-800","＞800"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "black")) +
  theme(axis.text = element_text(size = 7), 
        axis.title = element_text(size = 8)) +
  theme(legend.title = element_blank(),
        legend.key.size = unit(4, 'mm'),
        legend.text = element_text(size = 6)) +
  guides(fill = FALSE)

tiff(file="图7 globweight_ave height.tiff", res = 600, width = 3600, height =1500,  compression = "lzw")
ggarrange(p6,
          p8,
          nrow = 1,
          labels=c("b","c"),
          align = "v",
          common.legend = TRUE,legend="bottom")
dev.off()
getwd()  
#============================Module 7: Change of scaling exponents=============================
region_order <- c(
  "East Asia and the Pacific" = "#1 East Asia and the Pacific",
  "Europe and Japan" = "#2 Europe and Japan",
  "Land-Rich Developed Countries" = "#3 Land-Rich Developed Countries",
  "Latin America and The Caribbean" = "#4 Latin America and The Caribbean",
  "Southeast Asia" = "#5 Southeast Asia",
  "South and Central Asia" = "#6 South and Central Asia",
  "Sub-Saharan Africa" = "#7 Sub-Saharan Africa",
  "West Asia and North Africa" = "#8 West Asia and North Africa"
)
beta <- beta %>%
  mutate(region_new = region_order[region])

region_colors <- c(
  "#3 Land-Rich Developed Countries" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2 Europe and Japan" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1 East Asia and the Pacific" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4 Latin America and The Caribbean" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7 Sub-Saharan Africa" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8 West Asia and North Africa" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5 Southeast Asia" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6 South and Central Asia" = "#D1D759"            # Slightly Lighter Deep Lemon
)
expo.var <- ggplot(beta, aes(bvbeta2000, bvbeta2020)) +
  geom_point(aes(col = region_new), shape = 16, stroke = 0.4, size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  scale_y_continuous(limits = c(0.7, 1)) +
  scale_x_continuous(limits = c(0.7, 1)) +
  labs(x = "Scaling exponents of volume (β,2000)",
       y = "Scaling exponents of volume (β,2020)") + 
  scale_color_manual(values = region_colors) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 6))+
  guides() 

expo.var2 <- ggplot(beta, aes(bsbeta2000, bsbeta2020)) +
  geom_point(aes(col = region_new), shape = 16, stroke = 0.4, size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  scale_y_continuous(limits = c(0.6, 0.9)) +
  scale_x_continuous(limits = c(0.6, 0.9)) +
  labs(x = "Scaling exponents of surface (β,2000)",
       y = "Scaling exponents of surface (β,2020)") + 
  scale_color_manual(values = region_colors) +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text = element_text(size = 6))+
  guides() 

tiff(file="图11 city线性变化.tiff", res = 600, width = 3600, height =1700,  compression = "lzw")
ggarrange(expo.var,
          expo.var2,
          nrow = 1,
          ncol = 2,
          labels=c("a","b"),
          align = "v",
          common.legend = TRUE,legend="bottom"
          ,
          label.x = c(-0.018, -0.027),  # 只调整 c 和 d 的位置
          label.y = c(1.04, 1.028)
)
dev.off()
getwd()

#============================Module 8: Building surface per capita=========
region_order <- c(
  "East Asia and the Pacific" = "#1 East Asia and the Pacific",
  "Europe and Japan" = "#2 Europe and Japan",
  "Land-Rich Developed Countries" = "#3 Land-Rich Developed Countries",
  "Latin America and The Caribbean" = "#4 Latin America and The Caribbean",
  "Southeast Asia" = "#5 Southeast Asia",
  "South and Central Asia" = "#6 South and Central Asia",
  "Sub-Saharan Africa" = "#7 Sub-Saharan Africa",
  "West Asia and North Africa" = "#8 West Asia and North Africa"
)
mydata2 <- mydata2 %>%
  mutate(region2_new = region_order[region2])
mydata2$region_number <- sub(" .*", "", mydata2$region2_new)  # 只提取 #编号


region_colors <- c(
  "#3" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6" = "#D1D759"            # Slightly Lighter Deep Lemon
)


# 计算每个 region 的平均值
region_means <- mydata2 %>%
  group_by(region_number) %>%
  summarize(mean_bs_pop = mean(bs_pop)) %>%
  arrange(desc(mean_bs_pop))

# 按照平均值对 region 进行降序排列
mydata2$region_number <- factor(mydata2$region_number, levels = region_means$region_number)

unique(mydata2$region_number)

mydata2[which(mydata2$region_number==""),]$region

p8 <- ggplot(mydata2, aes(region_number, bs_pop, fill=region_number)) +
  geom_boxplot(outlier.size = 0.2, size=0.2) +
  stat_boxplot(geom="errorbar", width=0.2, size=0.2) +
  scale_fill_manual(values = region_colors) +  # 按 region 指定颜色
  
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red") +
  stat_summary(fun=mean, geom="text", aes(label=round(..y.., 1)), vjust=-0.5, size=2.2) +
  labs(x="Region", y="Surface per capita (m²)") +
  #scale_x_discrete(label =c("#1","#2","#3","#4","#5","#6","#7","#8"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(fill = FALSE)  # 关闭图例显

region_colors <- c(
  "#3 Land-Rich Developed Countries" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2 Europe and Japan" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1 East Asia and the Pacific" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4 Latin America and The Caribbean" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7 Sub-Saharan Africa" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8 West Asia and North Africa" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5 Southeast Asia" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6 South and Central Asia" = "#D1D759"            # Slightly Lighter Deep Lemon
)

bs2 <- ggplot(mydata2, 
              aes(log10(bs_2000),log10(bs_city),
                  colour=region2_new,
                  size=pop_city2/1000000)) +
  geom_point(shape=21,stroke = 0.4) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2) +
  scale_y_continuous(limits = c(0.5, 3.5)) +
  scale_x_continuous(limits = c(0.5, 3.5)) +
  labs( x = expression(paste(Log[10],"[Sum of Surface(", m^3,", 2000)]")),  
        y = expression(paste(Log[10],"[Sum of Surface(", m^3,", 2020)]"))) +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(size="none")  # 关闭SUM的图例显


bs3 <- ggplot(mydata2, 
              aes(log10(bspop_2000),log10(bs_pop),
                  colour=region2_new,
                  size=pop_city2/1000000)) +
  geom_point(shape=21,stroke = 0.4) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2) +
  scale_y_continuous(limits = c(0.5, 2.5)) +
  scale_x_continuous(limits = c(0.5, 2.5)) +
  labs( x = expression(paste(Log[10],"[Surface per capita (", m^3,", 2000)]")),  
        y = expression(paste(Log[10],"[Surface per capita (", m^3,", 2020)]"))) +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(size="none")  # 关闭SUM的图例显



tiff(file="图2 人均建筑面积.tiff", res = 600, width = 4000, height = 1500, compression = "lzw")
ggarrange(p8,
          bs2,
          bs3,
          nrow = 1,
          labels = c("b", "c", "d"),
          align = "h", 
          font.label = list(size = 11),
          label.x = c(0, -0.015, -0.03) , # 只调整 c 和 d 的位置
          common.legend = TRUE,legend="bottom")
dev.off()
getwd()

#============================Module 9: Gini Coefficient of BVPC=============================
region_colors <- c(
  "#3" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6" = "#D1D759"            # Slightly Lighter Deep Lemon
)

# 计算每个 region 的平均值
region_means <- mydata2 %>%
  group_by(region_number) %>%
  summarize(mean_gini_bs = mean(gini_bs)) %>%
  arrange(desc(mean_gini_bs))

# 按照平均值对 region 进行降序排列
mydata2$region_number <- factor(mydata2$region_number, levels = region_means$region_number)

unique(mydata2$region_number)

mydata2[which(mydata2$region_number==""),]$region

p7 <- ggplot(mydata2, aes(region_number, gini_bs, fill=region_number)) +
  geom_boxplot(outlier.size = 0.2, size=0.2) +
  stat_boxplot(geom="errorbar", width=0.2, size=0.2) +
  scale_fill_manual(values = region_colors) +  # 按 region 指定颜色
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red") +
  stat_summary(fun=mean, geom="text", aes(label=sprintf("%.2f", ..y..)), vjust=-0.5, size=2.2) +
  labs(x="Region", y="Gini Coefficient of\n surface per capita (2020)") +
  #scale_x_discrete(label =c("#1","#2","#3","#4","#5","#6","#7","#8"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title=element_text(size=8))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(fill = FALSE)  



region_colors <- c(
  "#3 Land-Rich Developed Countries" = "#8BCA8F",    # Slightly Lighter Deep Sea Green
  "#2 Europe and Japan" = "#E06F6F",                 # Slightly Lighter Deep Pink
  "#1 East Asia and the Pacific" = "#8B9FD6",        # Slightly Lighter Deep Light Blue
  "#4 Latin America and The Caribbean" = "#D08F9E",  # Slightly Lighter Deep Lavender Pink
  "#7 Sub-Saharan Africa" = "#228B22",               # Slightly Lighter Deep Aquamarine
  "#8 West Asia and North Africa" = "#E2966F",       # Slightly Lighter Deep Coral
  "#5 Southeast Asia" = "#6AB9D4",                   # Slightly Lighter Deep Sky Blue
  "#6 South and Central Asia" = "#D1D759"            # Slightly Lighter Deep Lemon
)

bs_gini <- ggplot(mydata2, aes(ginibs_2000,gini_bs,
                               colour=region2_new,
                               size=pop_city2/1000000)) +
  geom_point(shape=21,stroke = 0.4) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2)  +
  scale_y_continuous(limits = c(0.05, 0.65)) +
  scale_x_continuous(limits = c(0.05, 0.65)) +
  labs(
    x = expression(paste("Gini Coefficient of surface per capita(2000)")),  # 修改X轴名称
    y = "Gini Coefficient of\n surface per capita (2020)"  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title.y =element_text(size=7.5),
        axis.title.x =element_text(size=6.7))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(size="none")  # 关闭SUM的图例显


gini_2000 <- ggplot(mydata2, aes(ginibs_2000,ginibv_2000,
                                 colour=region2_new,
                                 size=pop_city2/1000000)) +
  geom_point(shape=21,stroke = 0.4) +
  scale_size_continuous(range = c(0.5,5),
                        breaks = c(0.1, 0.5,1,3,10,20))+
  geom_abline(intercept = 0, slope = 1, size=0.3, linetype=2)  +
  scale_y_continuous(limits = c(0.05, 0.7)) +
  scale_x_continuous(limits = c(0.05, 0.7)) +
  labs(
    x = expression(paste("Gini Coefficient of surface per capita(2000)")),  # 修改X轴名称
    y ="Gini Coefficient of\n volume per capita (2000)"  # 使用相同的形式设置Y轴名称
  ) +
  scale_color_manual(values = region_colors, breaks = region_order) +  # 按 region 指定颜色和顺序
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="white", color="black")) +
  theme(axis.text=element_text(size=7), 
        axis.title.y =element_text(size=7.5),
        axis.title.x =element_text(size=6.7))+
  theme(legend.title = element_blank(),
        legend.key.size=unit(4,'mm'),
        legend.text = element_text(size = 6))+
  guides(size="none")  # 关闭SUM的图例显


# 设置各个图的主题，减小边缘的间距


tiff(file="图4 gini人均建筑体量bs.tiff", res = 600, width = 4000, height = 1500, compression = "lzw")
ggarrange(p7,
          bs_gini,
          gini_2000,
          nrow = 1,
          labels = c("b", "c", "d"),
          align = "h", 
          font.label = list(size = 11),
          common.legend = TRUE,legend="bottom")
dev.off()
getwd()

#============================Module 10: Gini Coefficient of BVPC(Urban-Rural)=============================
value_colors <- c("UC" = "#D54846", "DUC" = "#DA8F87", "SUC" = "#F3DBD6", 
                  "SU" = "#D9E0E7", "RC" = "#98CADD", "LDR" = "#61AACF")

country_gini$VALUE <- factor(country_gini$VALUE, levels = c("UC", "DUC", "SUC", "SU", "RC", "LDR", "VLDR"))

country_bs <- ggplot(country_gini, 
                     aes(x = VALUE, 
                         y = gini_bs, 
                         group = VALUE, 
                         fill = VALUE)) +
  guides(fill=FALSE) +
  geom_boxplot(size = 0.2, outlier.size = 0.2) +
  stat_boxplot(geom = "errorbar",size=0.2, width=0.3)+  
  facet_wrap(~region,nrow=2)+
  scale_fill_manual(values = value_colors) +
  scale_x_discrete(labels = c("UC", "DU", "SU", "SC", "RC", "LC")) +  # Modify x-axis labels
  labs(x="Urban-rural typologies", 
       y="Gini Coefficient of \nsurface per capita in 2020")+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(    axis.text = element_text(size = 7),
            axis.title = element_text(size = 8),
            strip.background = element_blank(),  
            strip.text = element_text(color = "black",size=6) )



country_bs2 <- ggplot(country_gini,
                      aes(x = VALUE, 
                          y = gini_bsc, 
                          group = VALUE, 
                          fill = VALUE)) +
  guides(fill=FALSE) +
  geom_boxplot(size =0.2, outlier.size = 0.2) +
  stat_boxplot(geom = "errorbar", size = 0.2, width=0.3)+ 
  facet_wrap(~region,nrow=2)+
  scale_fill_manual(values = value_colors) +
  scale_x_discrete(labels = c("UC", "DU", "SU", "SC", "RC", "LC")) +  # Modify x-axis labels
  scale_y_continuous(limits = c(-0.25, 0.25))+
  geom_hline(yintercept = 0, color="blue", size=0.5,linetype = "dashed") +  # 添加红色虚线
  labs(x="Urban-rural typologies", #Date
       y="Changes of Gini Coefficient \nof surface per capita (2000-2020)")+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill="white",color="black"))+
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    strip.background = element_blank(),  
    strip.text = element_text(color = "black",size=6) )




tiff(file="图5 gini城乡梯度bs.tiff", res = 600, width = 3600, height =3200,  compression = "lzw")
ggarrange(country_bs,country_bs2,
          nrow = 2,
          ncol=1,
          labels=c("a","b"),
          align = "v",label.y = 1.02, label.x = 0.02,
          font.label = list(size = 12))
dev.off()
getwd()

#============================Module 11: Gini Coefficient of London=============================
library(ggplot2)
library(sf)
setwd ("D:/A大创/A建筑人口/输出图/1论文")
windowsFonts(B=windowsFont("Times New Roman"))

# 读取两组点矢量数据
points_2020 <- st_read("D:/A大创/A建筑人口/数据/建筑体量/全球/2020_bvld.shp")
points_2000 <- st_read("D:/A大创/A建筑人口/数据/建筑体量/2000/2000_bvld.shp")

# 去除 NA 值，确保数据有效
points_2020 <- points_2020[!is.na(points_2020$grid_code) & !is.na(points_2020$pop), ]
points_2000 <- points_2000[!is.na(points_2000$grid_code) & !is.na(points_2000$pop), ]

# 计算洛伦兹曲线函数
calculate_lorenz_data <- function(sorted_value, sorted_pop) {
  cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
  cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积建筑体量比例
  return(data.frame(cum_pop = cum_pop, cum_value = cum_value))
}

# 计算加权基尼系数函数
calculate_gini <- function(cum_pop, cum_value) {
  lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
  gini <- 1 - 2 * lorenz_area
  return(gini)
}

# 对2020年的数据进行排序和计算
ordered_indices_2020 <- order(points_2020$grid_code)
sorted_value_2020 <- points_2020$grid_code[ordered_indices_2020]
sorted_pop_2020 <- points_2020$pop[ordered_indices_2020]

lorenz_data_2020 <- calculate_lorenz_data(sorted_value_2020, sorted_pop_2020)
gini_2020 <- calculate_gini(lorenz_data_2020$cum_pop, lorenz_data_2020$cum_value)

# 对2000年的数据进行排序和计算
ordered_indices_2000 <- order(points_2000$grid_code)
sorted_value_2000 <- points_2000$grid_code[ordered_indices_2000]
sorted_pop_2000 <- points_2000$pop[ordered_indices_2000]

lorenz_data_2000 <- calculate_lorenz_data(sorted_value_2000, sorted_pop_2000)
gini_2000 <- calculate_gini(lorenz_data_2000$cum_pop, lorenz_data_2000$cum_value)

cat("2020年基尼系数: ", gini_2020, "\n")
cat("2000年基尼系数: ", gini_2000, "\n")

# 定义需要绘制虚线的点位置：Bottom 10%, Middle 10%, Top 10%
break_points <- c(0.1, 0.5, 0.9)

# 计算虚线点的位置
get_break_values <- function(cum_pop, cum_value, break_points) {
  sapply(break_points, function(p) {
    index <- which.min(abs(cum_pop - p))  # 找到最接近指定点的索引
    return(cum_value[index])  # 获取对应的累积建筑体量比例
  })
}

# 计算2020年虚线点
break_values_2020 <- get_break_values(lorenz_data_2020$cum_pop, lorenz_data_2020$cum_value, break_points)
break_data_2020 <- data.frame(cum_pop = break_points, cum_value = break_values_2020)

# 计算2000年虚线点
break_values_2000 <- get_break_values(lorenz_data_2000$cum_pop, lorenz_data_2000$cum_value, break_points)
break_data_2000 <- data.frame(cum_pop = break_points, cum_value = break_values_2000)

p <- ggplot() +
  # 绘制2020年和2000年的洛伦兹曲线
  geom_line(data = lorenz_data_2020, aes(x = cum_pop, y = cum_value), color = "red", size = 1) +
  geom_line(data = lorenz_data_2000, aes(x = cum_pop, y = cum_value), color = "orange", size = 1) +
  
  # 在关键位置绘制点
  geom_point(data = break_data_2020, aes(x = cum_pop, y = cum_value), color = "red", size = 3) +
  geom_point(data = break_data_2000, aes(x = cum_pop, y = cum_value), color = "orange", size = 3) +
  
  # 绘制虚线（从x轴和y轴延伸）
  geom_segment(data = break_data_2020, aes(x = cum_pop, xend = cum_pop, y = 0, yend = cum_value), 
               linetype = "dashed", color = "red") +
  geom_segment(data = break_data_2020, aes(x = 0, xend = cum_pop, y = cum_value, yend = cum_value), 
               linetype = "dashed", color = "red") +
  
  geom_segment(data = break_data_2000, aes(x = cum_pop, xend = cum_pop, y = 0, yend = cum_value), 
               linetype = "dashed", color = "orange") +
  geom_segment(data = break_data_2000, aes(x = 0, xend = cum_pop, y = cum_value, yend = cum_value), 
               linetype = "dashed", color = "orange") +
  
  # 添加关键点的标注
  geom_text(data = break_data_2020, aes(x = 0, y = cum_value, label = paste0(round(cum_value*100, 2), "%")),
            hjust = 0.45, color = "red", size = 1.8,vjust = 1.5) +
  geom_text(data = break_data_2000, aes(x = 0, y = cum_value, label = paste0(round(cum_value*100, 2), "%")),
            hjust = 0.45, color = "orange", size = 1.8,vjust = -0.5) +
  
  geom_text(data = break_data_2020, aes(x = cum_pop, y = 0, label = c("Bottom 10%", "Middle 50%", "Top 10%")),
            vjust = 1.5, color = "red", size = 1.8) +
  geom_text(data = break_data_2000, aes(x = cum_pop, y = 0, label = c("Bottom 10%", "Middle 50%", "Top 10%")),
            vjust = 1.5, color = "orange", size = 1.8, vjust = -1.5) +
  
  # 绝对平等线
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  
  # 设置坐标轴范围为0-100%，只在关键位置添加刻度
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), 
                     labels = c("0", "100%")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), 
                     labels = c("0", "100%")) +  # 修改 y 轴范围到 0-100
  
  # 设置背景为白色，轴线为黑色实线
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        axis.title.x = element_text(family = "B", size = 10),  # 设置x轴标题字体和大小
        axis.title.y = element_text(family = "B", size = 10),  # 设置y轴标题字体和大小
        axis.text.x = element_text( size = 7),   # 设置x轴刻度标签字体和大小
        axis.text.y = element_text( size = 7) ) +
  # x轴和y轴标签
  labs(x = "Cumulated Population (%)", 
       y = "Cumulated Building Volume per Capita\n of London (%)")+
  # 添加字母A和B
  annotate("text", x = 0.53, y = 0.45, label = "A", size = 4, color = "black") +
  annotate("text", x = 0.7, y = 0.2, label = "B", size = 4, color = "black")
# 打印图像
print(p)


# 读取两组点矢量数据
points_2020 <- st_read("D:/A大创/A建筑人口/数据/建筑体量/全球/2020_bsld.shp")
points_2000 <- st_read("D:/A大创/A建筑人口/数据/建筑体量/2000/2000_bsld.shp")

# 去除 NA 值，确保数据有效
points_2020 <- points_2020[!is.na(points_2020$grid_code) & !is.na(points_2020$pop), ]
points_2000 <- points_2000[!is.na(points_2000$grid_code) & !is.na(points_2000$pop), ]

# 计算洛伦兹曲线函数
calculate_lorenz_data <- function(sorted_value, sorted_pop) {
  cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
  cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积建筑体量比例
  return(data.frame(cum_pop = cum_pop, cum_value = cum_value))
}

# 计算加权基尼系数函数
calculate_gini <- function(cum_pop, cum_value) {
  lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
  gini <- 1 - 2 * lorenz_area
  return(gini)
}

# 对2020年的数据进行排序和计算
ordered_indices_2020 <- order(points_2020$grid_code)
sorted_value_2020 <- points_2020$grid_code[ordered_indices_2020]
sorted_pop_2020 <- points_2020$pop[ordered_indices_2020]

lorenz_data_2020 <- calculate_lorenz_data(sorted_value_2020, sorted_pop_2020)
gini_2020 <- calculate_gini(lorenz_data_2020$cum_pop, lorenz_data_2020$cum_value)

# 对2000年的数据进行排序和计算
ordered_indices_2000 <- order(points_2000$grid_code)
sorted_value_2000 <- points_2000$grid_code[ordered_indices_2000]
sorted_pop_2000 <- points_2000$pop[ordered_indices_2000]

lorenz_data_2000 <- calculate_lorenz_data(sorted_value_2000, sorted_pop_2000)
gini_2000 <- calculate_gini(lorenz_data_2000$cum_pop, lorenz_data_2000$cum_value)

cat("2020年基尼系数: ", gini_2020, "\n")
cat("2000年基尼系数: ", gini_2000, "\n")

# 定义需要绘制虚线的点位置：Bottom 10%, Middle 10%, Top 10%
break_points <- c(0.1, 0.5, 0.9)

# 计算虚线点的位置
get_break_values <- function(cum_pop, cum_value, break_points) {
  sapply(break_points, function(p) {
    index <- which.min(abs(cum_pop - p))  # 找到最接近指定点的索引
    return(cum_value[index])  # 获取对应的累积建筑体量比例
  })
}

# 计算2020年虚线点
break_values_2020 <- get_break_values(lorenz_data_2020$cum_pop, lorenz_data_2020$cum_value, break_points)
break_data_2020 <- data.frame(cum_pop = break_points, cum_value = break_values_2020)

# 计算2000年虚线点
break_values_2000 <- get_break_values(lorenz_data_2000$cum_pop, lorenz_data_2000$cum_value, break_points)
break_data_2000 <- data.frame(cum_pop = break_points, cum_value = break_values_2000)


p2 <- ggplot() +
  # 绘制2020年和2000年的洛伦兹曲线
  geom_line(data = lorenz_data_2020, aes(x = cum_pop, y = cum_value), color = "#6a0dad", size = 1) +
  geom_line(data = lorenz_data_2000, aes(x = cum_pop, y = cum_value), color = "#90ee90", size = 1) +
  
  # 在关键位置绘制点
  geom_point(data = break_data_2020, aes(x = cum_pop, y = cum_value), color = "#6a0dad", size = 3) +
  geom_point(data = break_data_2000, aes(x = cum_pop, y = cum_value), color = "#90ee90", size = 3) +
  
  # 绘制虚线（从x轴和y轴延伸）
  geom_segment(data = break_data_2020, aes(x = cum_pop, xend = cum_pop, y = 0, yend = cum_value), 
               linetype = "dashed", color = "#6a0dad") +
  geom_segment(data = break_data_2020, aes(x = 0, xend = cum_pop, y = cum_value, yend = cum_value), 
               linetype = "dashed", color = "#6a0dad") +
  
  geom_segment(data = break_data_2000, aes(x = cum_pop, xend = cum_pop, y = 0, yend = cum_value), 
               linetype = "dashed", color = "#90ee90") +
  geom_segment(data = break_data_2000, aes(x = 0, xend = cum_pop, y = cum_value, yend = cum_value), 
               linetype = "dashed", color ="#90ee90") +
  
  # 添加关键点的标注
  geom_text(data = break_data_2020, aes(x = 0, y = cum_value, label = paste0(round(cum_value*100, 2), "%")),
            hjust = 0.45, color = "#6a0dad", size = 1.8,vjust = 1.5) +
  geom_text(data = break_data_2000, aes(x = 0, y = cum_value, label = paste0(round(cum_value*100, 2), "%")),
            hjust = 0.45, color = "#90ee90", size = 1.8,vjust = -0.5) +
  
  geom_text(data = break_data_2020, aes(x = cum_pop, y = 0, label = c("Bottom 10%", "Middle 50%", "Top 10%")),
            vjust = 1.5, color = "#6a0dad", size = 1.8) +
  geom_text(data = break_data_2000, aes(x = cum_pop, y = 0, label = c("Bottom 10%", "Middle 50%", "Top 10%")),
            vjust = 1.5, color = "#90ee90", size = 1.8, vjust = -1.5) +
  
  # 绝对平等线
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  
  # 设置坐标轴范围为0-100%，只在关键位置添加刻度
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 1), 
                     labels = c("0", "100%")) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), 
                     labels = c("0", "100%")) +  # 修改 y 轴范围到 0-100
  # 设置背景为白色，轴线为黑色实线
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        axis.title.x = element_text(family = "B", size = 10),  # 设置x轴标题字体和大小
        axis.title.y = element_text(family = "B", size = 10),  # 设置y轴标题字体和大小
        axis.text.x = element_text( size = 7),   # 设置x轴刻度标签字体和大小
        axis.text.y = element_text( size = 7) ) +
  # x轴和y轴标签
  labs(x = "Cumulated Population (%)", 
       y = "Cumulated Building Surface per Capita\n of London (%)")+
  # 添加字母A和B
  annotate("text", x = 0.53, y = 0.45, label = "A", size = 4, color = "black") +
  annotate("text", x = 0.7, y = 0.2, label = "B", size = 4, color = "black")

# 打印图像
print(p2)

tiff(file="图2 lorenz_comparison_2000_2020.tif", res = 600, width = 3800, height = 1700, compression = "lzw")
ggarrange(p,
          p2,   
          nrow = 1,
          labels = c("e", "f"),
          #align = "h"
          font.label = list(size = 15),
          #common.legend = TRUE,
          #legend = "bottom",
          label.x = c(0.02, 0.02) ,label.y = c(1.03, 1.02) 
          # 只调整 c 和 d 的位置
)
dev.off()
getwd()

#============================Module 12: calculation of Gini Coefficient=============================

# 读取点矢量数据和面矢量数据
polygons <- st_read("D:/A大创/A毕设/下载数据/全球城市边界数据/GUB_Global_2018/2018_50_proj.shp")
points <- st_read("D:/A大创/A建筑人口/数据/建筑体量/全球/2020_bspop2.shp")
# 进行空间连接，将点与面结合
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$pop.x), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ORIG_FID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$pop.x[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/bs_2020_weighted_gini.csv", row.names = FALSE)



# 读取点矢量数据和面矢量数据
polygons <- st_read("D:/A大创/A毕设/下载数据/全球城市边界数据/GUB_Global_2018/2018_50_proj.shp")
points <- st_read("D:/A大创/A建筑人口/数据/建筑体量/2000/2000_bvpop2.shp")
# 进行空间连接，将点与面结合
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$pop.x), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ORIG_FID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$pop.x[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/bv_2000_weighted_gini.csv", row.names = FALSE)



# 读取点矢量数据和面矢量数据
polygons <- st_read("D:/A大创/A毕设/下载数据/全球城市边界数据/GUB_Global_2018/2018_50_proj.shp")
points <- st_read("D:/A大创/A建筑人口/数据/建筑体量/2000/2000_bspop2.shp")
# 进行空间连接，将点与面结合
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$pop.x), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ORIG_FID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$pop.x[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/bs_2000_weighted_gini.csv", row.names = FALSE)



polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_uc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bvpop_uc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_duc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bvpop_duc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_suc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bvpop_suc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_su20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bvpop_su_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_rc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bvpop_rc_weighted_gini.csv", row.names = FALSE)





polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_ldr20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bvpop_ldr_weighted_gini.csv", row.names = FALSE)





polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_uc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bspop_uc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_duc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bspop_duc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_suc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bspop_suc_weighted_gini.csv", row.names = FALSE)





polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_su20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bspop_su_weighted_gini.csv", row.names = FALSE)





polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_rc20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bspop_rc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_ldr20.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bspop_ldr_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_uc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bv2000_uc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_duc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bv2000_duc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_suc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bv2000_suc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_su00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bv2000_su_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_rc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bv2000_rc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bvpop_ldr00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bv2000_ldr_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_uc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bs2000_uc_weighted_gini.csv", row.names = FALSE)



polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_duc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bs2000_duc_weighted_gini.csv", row.names = FALSE)



polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_suc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bs2000_suc_weighted_gini.csv", row.names = FALSE)




polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_su00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bs2000_su_weighted_gini.csv", row.names = FALSE)



polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_rc00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bs2000_rc_weighted_gini.csv", row.names = FALSE)



polygons <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/country166p.shp")
points <- st_read("D:/A大创/A建筑人口/数据/重新分类/梯度/bspop_ldr00.shp")
# 进行空间连接
joined_data <- st_join(points, polygons)
# 去除 NA 值，确保值有效
joined_data <- joined_data[!is.na(joined_data$grid_code) & !is.na(joined_data$grid_cod_1), ]
# 按每个 polygon (ORIG_FID) 分组数据
grouped_data <- split(joined_data, joined_data$ID)
# 计算每个 polygon 内的加权基尼系数
gini_coefficients <- sapply(grouped_data, function(subset_data) {
  # 对 grid_code 按升序排序，同时相应地调整人口数据
  ordered_indices <- order(subset_data$grid_code)
  sorted_value <- subset_data$grid_code[ordered_indices]
  sorted_pop <- subset_data$grid_cod_1[ordered_indices]
  # 如果该区域至少有两个点，才进行基尼系数计算
  if (length(sorted_value) > 1 && sum(sorted_pop) > 0) {
    # 计算加权洛伦兹曲线
    cum_pop <- cumsum(sorted_pop) / sum(sorted_pop)  # 累积人口比例
    cum_value <- cumsum(sorted_value * sorted_pop) / sum(sorted_value * sorted_pop)  # 加权累积值比例
    # 计算基尼系数
    lorenz_area <- sum((cum_value[-1] + cum_value[-length(cum_value)]) * diff(cum_pop)) / 2
    gini <- 1 - 2 * lorenz_area
    return(gini)
  } else {
    return(NA)  # 如果数据不足返回 NA
  }
})
# 将基尼系数的结果转为数据框
result <- data.frame(ID = names(gini_coefficients), Gini = as.numeric(gini_coefficients))
# 打印结果
print(result)
# 将结果保存为CSV文件
write.csv(result, file = "D:/A大创/A建筑人口/数据/重新分类/梯度/excel/bs2000_ldr_weighted_gini.csv", row.names = FALSE)
