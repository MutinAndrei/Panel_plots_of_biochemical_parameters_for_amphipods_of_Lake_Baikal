library(ggplot2)
library(ggtext)
library(glue)
library(patchwork)
library(openxlsx)


#download data
ATP <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 2)
ADP <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 3)
AMP <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 4)
AEC <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 5)
Glucose <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 6)
Glycogen <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 7)
POD <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 8)
CAT <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 9)
GST <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 10)
LDH <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments1.xlsx", sheet = 11)
#make data part is factors (ATP)
ATP$Temperature <- as.factor(ATP$Temperature)
ATP$Species <- as.factor(ATP$Species)
ATP$Parameter <- as.factor(ATP$Parameter)
ATP$Depth <- as.factor(ATP$Depth)
#make data part is factors (ADP)
ADP$Temperature <- as.factor(ADP$Temperature)
ADP$Species <- as.factor(ADP$Species)
ADP$Parameter <- as.factor(ADP$Parameter)
ADP$`Depth` <- as.factor(ADP$Depth)
#make data part is factors (AMP)
AMP$Temperature <- as.factor(AMP$Temperature)
AMP$Species <- as.factor(AMP$Species)
AMP$Parameter <- as.factor(AMP$Parameter)
AMP$Depth <- as.factor(AMP$Depth)
#make data part is factors (AEC)
AEC$Temperature <- as.factor(AEC$Temperature)
AEC$Species <- as.factor(AEC$Species)
AEC$Parameter <- as.factor(AEC$Parameter)
AEC$Depth <- as.factor(AEC$Depth)
#make data part is factors (Glucose)
Glucose$Temperature <- as.factor(Glucose$Temperature)
Glucose$Species <- as.factor(Glucose$Species)
Glucose$Parameter <- as.factor(Glucose$Parameter)
Glucose$Depth <- as.factor(Glucose$Depth)
#make data part is factors (Glycogen)
Glycogen$Temperature <- as.factor(Glycogen$Temperature)
Glycogen$Species <- as.factor(Glycogen$Species)
Glycogen$Parameter <- as.factor(Glycogen$Parameter)
Glycogen$Depth <- as.factor(Glycogen$Depth)
#make data part is factors (POD)
POD$Temperature <- as.factor(POD$Temperature)
POD$Species <- as.factor(POD$Species)
POD$Parameter <- as.factor(POD$Parameter)
POD$Depth <- as.factor(POD$Depth)
#make data part is factors (CAT)
CAT$Temperature <- as.factor(CAT$Temperature)
CAT$Species <- as.factor(CAT$Species)
CAT$Parameter <- as.factor(CAT$Parameter)
CAT$Depth <- as.factor(CAT$Depth)
#make data part is factors (GST)
GST$Temperature <- as.factor(GST$Temperature)
GST$Species <- as.factor(GST$Species)
GST$Parameter <- as.factor(GST$Parameter)
GST$Depth <- as.factor(GST$Depth)
#make data part is factors (LDH)
LDH$Temperature <- as.factor(LDH$Temperature)
LDH$Species <- as.factor(LDH$Species)
LDH$Parameter <- as.factor(LDH$Parameter)
LDH$Depth <- as.factor(LDH$Depth)
#split table at Species factor (ATP)
FlavusATP <- ATP[ATP$Species == "O. flavus", ]
AlbinusATP <- ATP[ATP$Species == "O. albinus", ]
#split table at Species factor (ADP)
FlavusADP <- ADP[ADP$Species == "O. flavus", ]
AlbinusADP <- ADP[ADP$Species == "O. albinus", ]
#split table at Species factor (AMP)
FlavusAMP <- AMP[AMP$Species == "O. flavus", ]
AlbinusAMP <- AMP[AMP$Species == "O. albinus", ]
#split table at Species factor (AEC)
FlavusAEC <- AEC[AEC$Species == "O. flavus", ]
AlbinusAEC <- AEC[AEC$Species == "O. albinus", ]
#split table at Species factor (Glucose)
FlavusGlucose <- Glucose[Glucose$Species == "O. flavus", ]
AlbinusGlucose <- Glucose[Glucose$Species == "O. albinus", ]
#split table at Species factor (Glycogen)
FlavusGlycogen <- Glycogen[Glycogen$Species == "O. flavus", ]
AlbinusGlycogen <- Glycogen[Glycogen$Species == "O. albinus", ]
#split table at Species factor (POD)
FlavusPOD <- POD[POD$Species == "O. flavus", ]
AlbinusPOD <- POD[POD$Species == "O. albinus", ]
#split table at Species factor (CAT)
FlavusCAT <- CAT[CAT$Species == "O. flavus", ]
AlbinusCAT <- CAT[CAT$Species == "O. albinus", ]
#split table at Species factor (GST)
FlavusGST <- GST[GST$Species == "O. flavus", ]
AlbinusGST <- GST[GST$Species == "O. albinus", ]
#split table at Species factor (LDH)
FlavusLDH <- LDH[LDH$Species == "O. flavus", ]
AlbinusLDH <- LDH[LDH$Species == "O. albinus", ]

#plot for ATP O. flavus
FlavusATP[,3] <- factor(FlavusATP[,3], levels=unique(FlavusATP[,3]))
FlavusATPcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
scaleFUN <- function(x) sprintf("%.1f", x)
p1 <- ggplot(FlavusATP, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusATPcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "ATP, µM/g wet weight",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue",'cornflowerblue', "darkblue")) +
  scale_shape_manual(values = c(3, 8, 17)) +
  ggtitle(substitute(paste(bolditalic('O. flavus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 3.5) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 2.85, yend = 2.85, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 2.85, yend = 2.85, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(3.42), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(2), y = c(3.13), label = "decrease", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(3.42), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(3.13), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(2.85), label = "Control", cex=4, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=25, family="Arial")) + 
  theme(legend.position = c(0.8, 1.0515),
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01),
        axis.title.y = element_text(vjust = +1.7)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) + scale_y_continuous(labels=scaleFUN)
#save plot ATP
ggsave("2023-12-20_АТФ flavus.png", width = 20, height = 11, units = "cm", dpi = 600)

#plot for ADP O. albinus
AlbinusATP[,3] <- factor(AlbinusATP[,3], levels=unique(AlbinusATP[,3]))
AlbinusATPcoluor <- c("gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p1

p2 <- ggplot(AlbinusATP, 
       aes(x = factor(Temperature), y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusATPcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue")) +
  scale_shape_manual(values = c(17)) +
  ggtitle(substitute(paste(bolditalic('O. albinus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 3.5) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 2.85, yend = 2.85, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(6), y = c(3.42), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(3.13), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(2.85), label = "Control", cex=4, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=25, family="Arial")) + 
  theme(legend.position = c(0.86, 1.0515),
        legend.direction = "horizontal", 
        legend.spacing.y = unit(1.0, 'cm')) +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
    scale_x_discrete(limits = c('0.5', '1', '2', '4', '6', '8', '10', '12', '14', '16', '18', '20', '22')) + 
  scale_y_continuous(labels=scaleFUN)
ggsave("2023-12-20_АТФ albinus.png", width = 20, height = 11, units = "cm", dpi = 600) 

p1/p2
ggsave("2023-12-20_АТФ albinus и flavus2.0.png", width = 20, height = 22, units = "cm", dpi = 600)

#plot for ADP O. flavus
FlavusADP[,3] <- factor(FlavusADP[,3], levels=unique(FlavusADP[,3]))
FlavusADPcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
p3 <- ggplot(FlavusADP, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusADPcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "ADP, µM/g wet weight",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = .55, yend = .55, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = .55, yend = .55, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(.65), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(2), y = c(.59), label = "decrease", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(.65), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(.59), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(.53), label = "Control", cex=4, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=25, family="Arial")) +
  ylim(0, .65) + 
  theme(legend.position = "None",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01),
        axis.title.y = element_text(vjust = +1.7)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black"))
#save plot ADP
ggsave("2023-12-20_АДФ flavus.png", width = 20, height = 11, units = "cm", dpi = 600)

#plot for ADP O. albinus
AlbinusADP[,3] <- factor(AlbinusADP[,3], levels=unique(AlbinusADP[,3]))
AlbinusADPcoluor <- c("gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p4 <- ggplot(AlbinusADP, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusADPcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue")) +
  scale_shape_manual(values = c(17)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 0.65) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = .55, yend = .55, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(6), y = c(.65), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(.59), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(.53), label = "Control", cex=4, col = "black") + 
  annotate("text", x = c(12), y = c(0.27), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=25, family="Arial")) + 
  theme(legend.position ="None",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  scale_x_discrete(limits = c('0.5', '1', '2', '4', '6', '8', '10', '12', '14', '16', '18', '20', '22'))
#save plot ADP
ggsave("2023-12-20_АДФ albinus.png", width = 20, height = 11, units = "cm", dpi = 600)


p3/p4
ggsave("2023-12-20_АДФ albinus и flavus2.0.png", width = 20, height = 22, units = "cm", dpi = 600)

#plot for AMP O. flavus
FlavusAMP[,3] <- factor(FlavusAMP[,3], levels=unique(FlavusAMP[,3]))
FlavusAMPcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
p5 <- ggplot(FlavusAMP, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusADPcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "AMP, µM/g wet weight",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = .55, yend = .55, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = .55, yend = .55, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(.65), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(2), y = c(.59), label = "decrease", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(.65), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(.59), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(.53), label = "Control", cex=4, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(size=25, family="Arial")) +
  ylim(0, .65) + 
  theme(legend.position = "None",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01),
        axis.title.y = element_text(vjust = +1.7)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black"))
#save plot AMP
ggsave("2023-12-20_АМФ flavus.png", width = 20, height = 11, units = "cm", dpi = 600)

#plot for AMP O. albinus
AlbinusAMP[,3] <- factor(AlbinusAMP[,3], levels=unique(AlbinusAMP[,3]))
AlbinusAMPcoluor <- c("gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p6 <- ggplot(AlbinusAMP, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusAMPcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue")) +
  scale_shape_manual(values = c(17)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 0.65) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = .55, yend = .55, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(6), y = c(.65), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(.59), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(.53), label = "Control", cex=4, col = "black") + 
  annotate("text", x = c(8), y = c(0.2), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(size=25, family="Arial")) + 
  theme(legend.position = "None",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  scale_x_discrete(limits = c('0.5', '1', '2', '4', '6', '8', '10', '12', '14', '16', '18', '20', '22'))
#save plot AMP
ggsave("2023-12-20_АМФ albinus.png", width = 20, height = 11, units = "cm", dpi = 600)

p5/p6
ggsave("2_p_2023-12-20_АМФ albinus и flavus.png", width = 20, height = 22, units = "cm", dpi = 600)

#################################################################################################################################################################

#plot for AEC O. flavus
FlavusAEC[,3] <- factor(FlavusAEC[,3], levels=unique(FlavusAEC[,3]))
FlavusAECcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
p7 <- ggplot(FlavusAEC, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusAECcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "AEC, A.U.",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 1.26, yend = 1.26, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 1.26, yend = 1.26, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(1.5), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(2), y = c(1.38), label = "decrease", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(1.5), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(1.38), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(1.25), label = "Control", cex=4, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(size=25, family="Arial")) +
  ylim(0, 1.5) + 
  theme(legend.position = "None",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01),
        axis.title.y = element_text(vjust = +1.7)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black"))
#save plot AEC
ggsave("2023-12-19_АEC flavus.png", width = 20, height = 11, units = "cm", dpi = 600)

#plot for AEC O. albinus
AlbinusAEC[,3] <- factor(AlbinusAEC[,3], levels=unique(AlbinusAEC[,3]))
AlbinusAECcoluor <- c("gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p8 <- ggplot(AlbinusAEC, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusAECcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue")) +
  scale_shape_manual(values = c(17)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 1.5) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 1.26, yend = 1.26, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(6), y = c(1.5), label = "Temperature", cex=6, col = "black") +
  annotate("text", x = c(6), y = c(1.38), label = "increase", cex=6, col = "black") +
  annotate("text", x = c(4), y = c(1.25), label = "Control", cex=4, col = "black") + 
  annotate("text", x = c(8), y = c(1.1), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=25, family="Arial")) + 
  theme(legend.position = "none",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  scale_x_discrete(limits = c('0.5', '1', '2', '4', '6', '8', '10', '12', '14', '16', '18', '20', '22'))
#save plot AEC
ggsave("2023-12-18_АEC albinus.png", width = 20, height = 11, units = "cm", dpi = 600)

p7/p8
#save plot AEC
ggsave("2_p_2023-12-20_АEC albinus и flavus.png", width = 20, height = 22, units = "cm", dpi = 600)

#################################################################################################################################################################

#plot for Glucose O. flavus
FlavusGlucose[,3] <- factor(FlavusGlucose[,3], levels=unique(FlavusGlucose[,3]))
FlavusGlucosecoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                         "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                         "darkorange")
p9 <- ggplot(FlavusGlucose, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusGlucosecoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "Glucose, µM/g wet weight",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(substitute(paste(bolditalic('O. flavus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 2.6, yend = 2.6, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 2.6, yend = 2.6, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(2.9), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(2.75), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(2.9), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(2.75), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(2.3), label = "Control", cex=3, col = "black") + 
  annotate("text", x = c(13), y = c(2.6), label = substitute(paste(bold("*"))), cex=10, col = "black") + 
  annotate("text", x = c(12), y = c(2.4), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(11), y = c(2.4), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(10), y = c(1.4), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(9), y = c(1.2), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = c(0.7, 1.0515),
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0,3)
#save plot Glucose
ggsave("2023-12-20_Glucose flavus.png", width = 20, height = 11, units = "cm", dpi = 600)


#plot for Glucose O. albinus
AlbinusGlucose[,3] <- factor(AlbinusGlucose[,3], levels=unique(AlbinusGlucose[,3]))
AlbinusGlucosecoluor <- c("gray40", "floralwhite", "floralwhite",
                          "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                          "floralwhite")
p10 <- ggplot(AlbinusGlucose, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusGlucosecoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue")) +
  scale_shape_manual(values = c(17)) +
  ggtitle(substitute(paste(bolditalic('O. albinus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 2.6, yend = 2.6, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(6), y = c(2.9), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(2.75), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(2.3), label = "Control", cex=3, col = "black") + 
  annotate("text", x = c(13), y = c(2.9), label = substitute(paste(bold("*"))), cex=10, col = "black") + 
  annotate("text", x = c(12), y = c(2.8), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(11), y = c(2.5), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(10), y = c(2.2), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  theme(text=element_text(size=18, family="Arial")) + 
  theme(legend.position = c(0.83, 1.0515),
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) + 
  ylim(0, 3) +
  scale_x_discrete(limits = c('0.5', '1', '2', '4', '6', '8', '10', '12', '14', '16', '18', '20', '22'))
#save plot Glucose
ggsave("2023-12-20_Glucose albinus.png", width = 20, height = 11, units = "cm", dpi = 600)

p9/p10
ggsave("2023-12-20_Glucose albinus и flavus.png", width = 20, height = 22, units = "cm", dpi = 600)


#plot for Glycogen O. flavus
FlavusGlycogen[,3] <- factor(FlavusGlycogen[,3], levels=unique(FlavusGlycogen[,3]))
FlavusGlycogencoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                          "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                          "darkorange")
p11 <- ggplot(FlavusGlycogen, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusGlycogencoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "Glycogen, µM/g wet weight",
  ) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 36, yend = 36, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 36, yend = 36, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(39.5), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(37.5), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(39.5), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(37.5), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(35), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = "none",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) + 
  ylim(0, 40) 

#save plot Glucose
ggsave("2023-12-20_Glycogen flavus.png", width = 20, height = 11, units = "cm", dpi = 600)

#plot for Glycogen O. albinus
AlbinusGlycogen[,3] <- factor(AlbinusGlycogen[,3], levels=unique(AlbinusGlycogen[,3]))
AlbinusGlycogencoluor <- c("gray40", "floralwhite", "floralwhite",
                           "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                           "floralwhite")
p12 <- ggplot(AlbinusGlycogen, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusGlycogencoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue")) +
  scale_shape_manual(values = c(17)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 36, yend = 36, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(6), y = c(39.5), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(37.5), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(35), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) + 
  theme(legend.position ="none",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) + 
  ylim(0, 40) +
  scale_x_discrete(limits = c('0.5', '1', '2', '4', '6', '8', '10', '12', '14', '16', '18', '20', '22'))

#save plot Glucose
ggsave("2023-12-20_Glycogen albinus.png", width = 20, height = 11, units = "cm", dpi = 600)

p11/p12
ggsave("2023-12-20_Glycogen albinus и flavus.png", width = 20, height = 22, units = "cm", dpi = 600)


#plot for POD O. flavus
FlavusPOD[,3] <- factor(FlavusPOD[,3], levels=unique(FlavusPOD[,3]))
FlavusPODcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
p13 <- ggplot(FlavusPOD, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusPODcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "POD, nkat/mg protein",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(substitute(paste(bolditalic('O. flavus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = .22, yend = .22, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = .22, yend = .22, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(.25), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(.235), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(.25), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(.235), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(0.2), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = c(0.72, 1.0515),
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0, 0.25)
#save plot POD
ggsave("2023-12-20_POD flavus.png", width = 20, height = 11, units = "cm", dpi = 600)



#plot for POD O. albinus
AlbinusPOD[,3] <- factor(AlbinusPOD[,3], levels=unique(AlbinusPOD[,3]))
AlbinusPODcoluor <- c("floralwhite", "floralwhite", "floralwhite", "gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p14 <- ggplot(AlbinusPOD, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusPODcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(17, 19)) +
  ggtitle(substitute(paste(bolditalic('O. albinus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = .22, yend = .22, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = .22, yend = .22, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(.25), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(.235), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(.25), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(.235), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(0.2), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = c(0.78, 1.0515),
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0, 0.25)

#save plot POD
ggsave("2023-12-20_POD albinus.png", width = 20, height = 11, units = "cm", dpi = 600)

p13/p14
ggsave("2_p_2023-12-20_POD albinus и flavus.png", width = 20, height = 22, units = "cm", dpi = 600)



#plot for CAT O. flavus
FlavusCAT[,3] <- factor(FlavusCAT[,3], levels=unique(FlavusCAT[,3]))
FlavusCATcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
p15 <- ggplot(FlavusCAT, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusCATcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "CAT, nkat/mg protein",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 1720, yend = 1720, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 1720, yend = 1720, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(1975), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(1850), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1975), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1850), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(1650), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = "none",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0, 2000)
#save plot CAT
ggsave("2023-12-20_CAT flavus.png", width = 20, height = 11, units = "cm", dpi = 600)

#plot for CAT O. albinus
AlbinusCAT[,3] <- factor(AlbinusCAT[,3], levels=unique(AlbinusCAT[,3]))
AlbinusCATcoluor <- c("floralwhite", "floralwhite", "floralwhite", "gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p16 <- ggplot(AlbinusCAT, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusCATcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(17, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 1720, yend = 1720, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 1720, yend = 1720, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(1975), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(1850), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1975), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1850), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(1650), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = "none",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black"))
#save plot CAT
ggsave("2023-12-20_CAT albinus.png", width = 20, height = 11, units = "cm", dpi = 600)

p15/p16
ggsave("2_p_2023-12-20_CAT albinus и flavus_test.png", width = 20, height = 22, units = "cm", dpi = 600)


#plot for GST O. flavus
FlavusGST[,3] <- factor(FlavusGST[,3], levels=unique(FlavusGST[,3]))
FlavusGSTcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
p17 <- ggplot(FlavusGST, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusGSTcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "GST, nkat/mg protein",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(substitute(paste(bolditalic('O. flavus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 29, yend = 29, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 29, yend = 29, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(33), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(31), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(33), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(31), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(27), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = c(0.72, 1.0515),
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0, 33)
#save plot GST
ggsave("2023-12-20_GST flavus.png", width = 20, height = 11, units = "cm", dpi = 600)


#plot for GST O. albinus
AlbinusGST[,3] <- factor(AlbinusGST[,3], levels=unique(AlbinusGST[,3]))
AlbinusGSTcoluor <- c("floralwhite", "floralwhite", "floralwhite", "gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p18 <- ggplot(AlbinusGST, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusGSTcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(17, 19)) +
  ggtitle(substitute(paste(bolditalic('O. albinus')))) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 29, yend = 29, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 29, yend = 29, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(33), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(31), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(33), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(31), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(27), label = "Control", cex=3, col = "black") + 
  annotate("text", x = c(1), y = c(20), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(2), y = c(20), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(11), y = c(18), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = c(0.78, 1.0515),
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0, 33)
#save plot GST
ggsave("2023-12-20_GST albinus.png", width = 20, height = 11, units = "cm", dpi = 600)

p17/p18

ggsave("2_p_2023-12-20_GST albinus and flavus.png", width = 20, height = 22, units = "cm", dpi = 600)

#plot for LDH O. flavus
FlavusLDH[,3] <- factor(FlavusLDH[,3], levels=unique(FlavusLDH[,3]))
FlavusLDHcoluor <- c("darkorange", "darkorange", "darkorange", "gray40", "darkorange", "darkorange",
                     "darkorange", "darkorange", "darkorange", "darkorange", "darkorange", "darkorange",
                     "darkorange")
p19 <- ggplot(FlavusLDH, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = FlavusLDHcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = "LDH, nkat/mg protein",
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("deepskyblue", "cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(3, 8, 19, 3, 8, 19, 8, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 1070, yend = 1070, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 1070, yend = 1070, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(1225), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(1140), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1225), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1140), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(950), label = "Control", cex=3, col = "black") + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = "none",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0, 1250)
#save plot LDH
ggsave("2023-12-20_LDH flavus.png", width = 20, height = 11, units = "cm", dpi = 600)


#plot for LDH O. albinus
AlbinusLDH[,3] <- factor(AlbinusLDH[,3], levels=unique(AlbinusLDH[,3]))
AlbinusLDHcoluor <- c("floralwhite", "floralwhite", "floralwhite", "gray40", "floralwhite", "floralwhite",
                      "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite", "floralwhite",
                      "floralwhite")
p20 <- ggplot(AlbinusLDH, 
       aes(x = Temperature, y = Value, ymin = 0)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.5, fill = AlbinusLDHcoluor, width = 0.6) +  
  geom_point(size = 2, alpha = 1, aes(group = Temperature, col = Depth, shape = Depth), 
             position = position_jitterdodge(jitter.width = 0.5)) +
  labs(fill = "Температура",
       col = "Depth, m:",
       shape = "Depth, m:",
       x = "Temperature, °C",
       y = NULL,
  ) +
  theme(title  = ggtext::element_markdown()) +
  scale_color_manual(values = c("cornflowerblue", "darkblue")) +
  scale_shape_manual(values = c(17, 19)) +
  ggtitle(NULL) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(3.5,4.5), linetype = "dashed", linewidth = 0.6) +
  annotate("segment", x = 5, xend = 7, y = 1070, yend = 1070, colour = "firebrick2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 1, y = 1070, yend = 1070, colour = "dodgerblue2", size=1.2, arrow=arrow(length = unit(0.2, "cm"))) +
  annotate("text", x = c(2), y = c(1225), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(2), y = c(1140), label = "decrease", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1225), label = "Temperature", cex=4.5, col = "black") +
  annotate("text", x = c(6), y = c(1140), label = "increase", cex=4.5, col = "black") +
  annotate("text", x = c(4), y = c(950), label = "Control", cex=3, col = "black") + 
  annotate("text", x = c(7), y = c(220), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  annotate("text", x = c(10), y = c(220 ), label = substitute(paste(bold("*"))), cex=10, col = "black") +
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) + 
  theme(text=element_text(color="black"),axis.text=element_text(color="black")) +
  theme(text=element_text(size=18, family="Arial")) +
  theme(legend.position = "none",
        legend.direction = "horizontal") +
  theme(plot.title = element_text(hjust = 0.01)) + theme(legend.background=element_rect(fill = alpha("white", 0))) + 
  theme(
    axis.line = element_line(linewidth = 0.4, color="black")) +
  ylim(0, 1250)
#save plot LDH
#adenilats
p20
(p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8)
ggsave("allmethabolits_fla_alb.png", width = 50, height = 46, units = "cm", dpi = 600)
#
(p9 + p10) / (p11 + p12)
ggsave("allmethabolits2_fla_alb.png", width = 33, height = 25, units = "cm", dpi = 600)
p16

(p13 + p14) / (p15 + p16) 
ggsave("CAT_POD.png", width = 36, height = 25, units = "cm", dpi = 600)


(p17 + p18) / (p19 + p20)
ggsave("GST_LDH.png", width = 36, height = 25, units = "cm", dpi = 600)
