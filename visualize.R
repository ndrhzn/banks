library(ggplot2)

#process data-------------------------------------------------------------------

  data <- read.csv("~/R/banks/data.csv", stringsAsFactors = F)
  data$dateBankruted <- as.Date(data$dateBankruted, format = "%d.%m.%Y")
  data$dateLiquidated <- as.Date(data$dateLiquidated, format = "%d.%m.%Y")
  
  data <- data[!is.na(data$dateLiquidated), c(1, 2, 4)]
  
  for (i in seq_along(data$dateLiquidated)) {
    
    if (is.na(data$dateBankruted[i] == TRUE)) {
      
      data$dateBankruted[i] = data$dateLiquidated[i]
      
    }
    
  }
  
  
  
  data$difference <- data$dateLiquidated - data$dateBankruted
  
#set axis breaks and text elements----------------------------------------------

breaks = classInt::classIntervals(as.numeric(data$difference), n = 5, style = "jenks")$brks
  
title = "Хроніка чистки банківської системи України"

subtitle = "Початок лінії позначає дату визнання банку неплатоспроможним. Точка позначає дату прийняття рішення про ліквідацію банку. Довжина лінії позначає кількість днів між визнанням банку неплатоспроможним та прийняттям рішення про його ліквідацію"

caption = "Дані: НБУ | Візуалізація: textura.in.ua"

annotation1 = "Ліквідації банку зазвичай передує визнання його неплатоспроможним та запровадження тимчасової адміністрації. ТА запроваджується з метою збереження капіталу та активів банку, оцінки його стану, відновлення його платоспроможності та ліквідності, стабілізації діяльності банку"

annotation2 = "В разі неможливості приведення фінансового стану банку до вимог НБУ, відновленння його платоспроможності та ліквідності, Нацбанк приймає рішення про ліквідацію банку"


#generate plot------------------------------------------------------------------

png(filename = "banks.png", width = 1200, height = 1000, type = "cairo")
ggplot()+
  geom_curve(data = data[data$difference > 0, ], 
             aes(x = dateBankruted, xend = dateLiquidated, y = 0, yend = difference), 
              curvature = 0.1, alpha = 0.5, color = "darkred")+
  geom_point(data = data, aes(x = dateLiquidated, y = difference), 
             alpha = 0.5, color = "darkred", size = 2.5)+
  geom_text(data = data[data$difference>127, ],
            aes(x = dateLiquidated, y = difference, label = name,
                family = "Ubuntu Condensed", fontface = "bold"), color = "#3A3F4A",
            hjust = -0.15, size = 5)+
  geom_text(aes(x = as.Date("2014-07-01"), y = 214, family = "Ubuntu Condensed",
                label = stringr::str_wrap(annotation1, 40)),
            size = 5, color = "#3A3F4A")+
  geom_text(aes(x = as.Date("2016-07-01"), y = 275, family = "Ubuntu Condensed",
                label = stringr::str_wrap(annotation2, 40)),
            size = 5, color = "#3A3F4A")+
  scale_y_reverse(position = "right", breaks = breaks, expand = c(0.01, 0))+
  scale_x_date(position = "top", expand = c(0.05, 0.05))+
  labs(title = title,
       subtitle = stringr::str_wrap(subtitle, width = 125),
       caption = caption)+
  theme_minimal(base_family = "Ubuntu Condensed")+
  theme(text = element_text(family = "Ubuntu Condensed", color = "#3A3F4A", size = 14),
        legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.key.height = unit(3.5, "pt"),
        legend.key.width = unit(125, "pt"),
        axis.text.x = element_text(face = "plain", size = 16),
        axis.text.y = element_text(face = "plain", size = 16),
        axis.title = element_blank(),
        axis.ticks.length  = unit(0.1, "lines"),
        panel.grid.major = element_line(size = 0.35, linetype = "dotted", color = "#5D646F"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 40, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 20, margin = margin(b = 20), face = "plain"),
        plot.caption = element_text(size = 16, margin = margin(b = 10, t = 20), color = "#5D646F"),
        plot.background = element_rect(fill = "#EFF2F4"),
        plot.margin = unit(c(2, 2, 2, 2), "cm"))
dev.off()