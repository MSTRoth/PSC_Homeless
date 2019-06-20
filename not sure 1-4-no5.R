century <- read_csv("Century Link New Vehicles.csv")

century <- century %>% 
  mutate(sum = `Transaction Value`/1000000)

ggplot(century, aes(x = `Contract Vehicle`, y = sum, fill = `Funding Agency`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(sum, digits = 2)), size = 5, position = position_dodge(width=1), vjust = -.7)+
  #geom_text(aes(label = sprintf('%.0f%%', percent), y = sum.x), size = 4.5, position = position_dodge(width=1), vjust = -.7)+
  #scale_fill_manual(values = c("2016" = "royalblue3", "2017" = "gold3", "2018" = "deeppink4")) +
  labs(y = paste("Contract Obligations (in ", "millions)", sep = ""), x = "Contract Vehicle", 
       title = "FY18* Contract Obligations for Alliant and EIS by Funding Agency", subtitle = "*DoD data is incomplete for FY18Q4")+
  theme(axis.text = element_text(size = 12), plot.title = element_text(size = 28), plot.subtitle = element_text(size = 22, face = "italic"),
        axis.title.y = element_text(size = 14, face = "bold"), axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "gray"),
        legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 12),
        legend.spacing.x = unit(.25, 'cm'))



