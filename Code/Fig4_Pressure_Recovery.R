######################################
######################################
### MULLANE ET AL. (2023) FIGURE 4 ###
######################################
######################################

### Packages

packages <- c(
  "readr", "tidyverse", "grid", 
  "gridExtra", "svglite"
)

funlist <-  lapply(packages, function(x) {
  if (x %in% rownames(installed.packages())) {
    require(x, character.only = T)
  }else{
    install.packages(x, character.only = T); require(x, character.only = T)
  }
})

### Download files from GitHub

SS_10BA_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/10BA_Recovery_Swimming_Speed.csv"
PM_10BA_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/10BA_Recovery_Percent_Motile.csv"
SS_18_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/18_Recovery_Swimming_Speed.csv"
PM_18_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/18_Recovery_Percent_Motile.csv"
SS_36_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/36_Recovery_Swimming_Speed.csv"
PM_36_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/36_Recovery_Percent_Motile.csv"

### Read in CSV files
  
SS_10BA <- read_csv(url(SS_10BA_url))
PM_10BA <- read_csv(url(PM_10BA_url))
SS_18 <- read_csv(url(SS_18_url))
PM_18 <- read_csv(url(PM_18_url))
SS_36 <- read_csv(url(SS_36_url))
PM_36 <- read_csv(url(PM_36_url))

### Figure settings

theme_set(theme_bw(base_size = 10, base_family = "Arial"))
swimming_speed <- expression(paste(bold('Swimming Speed ('~mu*'m/sec )')))


#################
## STRAIN 10BA ##
#################

# Find the mean & standard deviation of the percentage of motile cells 

PM_10BA_Avg <- group_by_at(PM_10BA, vars(Time, Period)) %>%
  summarize(Mean = mean(Value), SD = sd(Value))

PM_10BA_Avg_DF <- as.data.frame(PM_10BA_Avg)

# Find the mean & standard deviation of the swimming speed

SS_10BA_Avg <- group_by_at(SS_10BA, vars(Time, Period)) %>%
  summarize(Mean = mean(Value), SD = sd(Value))

SS_10BA_Avg_DF <- as.data.frame(SS_10BA_Avg)


# Percent motile cells plot

A <- ggplot(data = PM_10BA_Avg_DF, aes(x = Time, y = Mean)) +
  geom_point(size = 2) +
  geom_line(aes(group = Period), linewidth = 1.5) +
  geom_errorbar(data = PM_10BA_Avg_DF, 
                aes(x = Time, ymin = Mean-SD, ymax = Mean+SD), 
                width = 1, linewidth = 0.5) +
  scale_color_manual(name = NULL, 
                     breaks = c("Before", "During", "After"), 
                     labels = c("Before Exposure", "During Exposure - 100 MPa", "Post-Exposure")) +
  scale_x_continuous(name="Time Post-Exposure (Seconds)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 30, 60, 90), 
                     label = c("0", "30", "60", "240")) +
  scale_y_continuous(name = "Percentage of Motile Cells", 
                     expand = c(0, 0),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  coord_cartesian(xlim = c(-40, 100), ylim = c(0, 100)) +
  annotate("rect", xmin = -20, xmax = 0, ymin = 0, ymax = 100,
           alpha = 0.1, fill = "black") +
  annotate("rect", xmin = -40, xmax = -20, ymin = 0, ymax = 100,
           alpha = 0.3, fill = "black") +
  labs(tag = "A.") +
  theme(axis.title.x = element_text(hjust = 0.8, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none", 
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_rect(colour = "black", linewidth = 1))

A

# Swimming speed plot

B <- ggplot(data = SS_10BA_Avg_DF, aes(x = Time, y = Mean)) +
  geom_point(size = 2) +
  geom_line(aes(group = Period), linewidth = 1.5) +
  geom_errorbar(data = SS_10BA_Avg_DF, 
                aes(x = Time, ymin = Mean-SD, ymax = Mean+SD),
                width = 1, linewidth = 0.5) +
  scale_color_manual(name = NULL,
                     breaks = c("Before", "During", "After"), 
                     labels = c("Before Exposure", "During Exposure - 100 MPa", "Post-Exposure")) +
  scale_x_continuous(name = "Time Post-Exposure (Seconds)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 30, 60, 90), 
                     label = c("0", "30", "60", "240")) +
  scale_y_continuous(name = swimming_speed, 
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(-40, 100),
                  ylim = c(0, 60)) +
  annotate("rect", xmin = -20, xmax = 0, ymin = 0, ymax = 60,
           alpha = 0.1, fill = "black") +
  annotate("rect", xmin = -40, xmax = -20, ymin = 0, ymax = 60,
           alpha = 0.3, fill = "black") +
  labs(tag = "B.") +
  theme(axis.title.x = element_text(hjust = 0.8, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none", 
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_rect(colour = "black", linewidth = 1))

B


## Combine the plots for strain 10BA ##

# We'll add a legend later

Title_10BA <- expression(paste(bolditalic('Halomonas'), bold(" sp. strain 10BA GOM-1509m")))

combo_10BA <- grid.arrange(A, B, top = textGrob(Title_10BA, vjust = 1), nrow = 1)


###############
## STRAIN 18 ##
###############

# Find the mean & standard deviation of the percentage of motile cells 

PM_18_Avg <- group_by_at(PM_18, vars(Time, Period)) %>%
  summarize(Mean = mean(Value), SD = sd(Value))

PM_18_Avg_DF <- as.data.frame(PM_18_Avg)

# Find the mean & standard deviation of the swimming speed

SS_18_Avg <- group_by_at(SS_18, vars(Time, Period)) %>%
  summarize(Mean = mean(Value), SD = sd(Value))

SS_18_Avg_DF <- as.data.frame(SS_18_Avg)


# Percent motile cells plot

C <- ggplot(data = PM_18_Avg_DF, aes(x = Time, y = Mean)) +
  geom_point(size = 2) +
  geom_line(aes(group = Period), linewidth = 1.5) +
  geom_errorbar(data = PM_18_Avg_DF, 
                aes(x = Time, ymin = Mean-SD, ymax = Mean+SD), 
                width = 1, linewidth = 0.5) +
  scale_color_manual(name = NULL, 
                     breaks = c("Before", "During", "After"), 
                     labels = c("Before Exposure", "During Exposure - 100 MPa", "Post-Exposure")) +
  scale_x_continuous(name="Time Post-Exposure (Seconds)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 30, 60, 90), 
                     label = c("0", "30", "60", "220")) +
  scale_y_continuous(name = "Percentage of Motile Cells", 
                     expand = c(0, 0),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  coord_cartesian(xlim = c(-40, 100), ylim = c(0, 100)) +
  annotate("rect", xmin = -20, xmax = 0, ymin = 0, ymax = 100,
           alpha = 0.1, fill = "black") +
  annotate("rect", xmin = -40, xmax = -20, ymin = 0, ymax = 100,
           alpha = 0.3, fill = "black") +
  labs(tag = "C.") +
  theme(axis.title.x = element_text(hjust = 0.8, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none", 
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_rect(colour = "black", linewidth = 1))

C

# Swimming speed plot

D <- ggplot(data = SS_18_Avg_DF, aes(x = Time, y = Mean)) +
  geom_point(size = 2) +
  geom_line(aes(group = Period), linewidth = 1.5) +
  geom_errorbar(data = SS_18_Avg_DF, 
                aes(x = Time, ymin = Mean-SD, ymax = Mean+SD),
                width = 1, linewidth = 0.5) +
  scale_color_manual(name = NULL,
                     breaks = c("Before", "During", "After"), 
                     labels = c("Before Exposure", "During Exposure - 100 MPa", "Post-Exposure")) +
  scale_x_continuous(name = "Time Post-Exposure (Seconds)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 30, 60, 90), 
                     label = c("0", "30", "60", "220")) +
  scale_y_continuous(name = swimming_speed, 
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(-40, 100),
                  ylim = c(0, 60)) +
  annotate("rect", xmin = -20, xmax = 0, ymin = 0, ymax = 60,
           alpha = 0.1, fill = "black") +
  annotate("rect", xmin = -40, xmax = -20, ymin = 0, ymax = 60,
           alpha = 0.3, fill = "black") +
  labs(tag = "D.") +
  theme(axis.title.x = element_text(hjust = 0.8, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none", 
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_rect(colour = "black", linewidth = 1))

D


## Combine the plots for strain 18 ##

# We'll add a legend later

Title_18 <- expression(paste(bolditalic('Alcanivorax'), bold(" sp. strain 18 GOM-1509m")))

combo_18 <- grid.arrange(C, D, top = textGrob(Title_18, vjust = 1), nrow = 1)


###############
## STRAIN 36 ##
###############

# Find the mean & standard deviation of the percentage of motile cells 

PM_36_Avg <- group_by_at(PM_36, vars(Time, Period)) %>%
  summarize(Mean = mean(Value), SD = sd(Value))

PM_36_Avg_DF <- as.data.frame(PM_36_Avg)

# Find the mean & standard deviation of the swimming speed

SS_36_Avg <- group_by_at(SS_36, vars(Time, Period)) %>%
  summarize(Mean = mean(Value), SD = sd(Value))

SS_36_Avg_DF <- as.data.frame(SS_36_Avg)

# Percent motile cells plot

E <- ggplot(data = PM_36_Avg_DF, aes(x = Time, y = Mean)) +
  geom_point(size = 2) +
  geom_line(aes(group = Period), linewidth = 1.5) +
  geom_errorbar(data = PM_36_Avg_DF, 
                aes(x = Time, ymin = Mean-SD, ymax = Mean+SD), 
                width = 1, linewidth = 0.5) +
  scale_color_manual(name = NULL, 
                     breaks = c("Before", "During", "After"), 
                     labels = c("Before Exposure", "During Exposure - 100 MPa", "Post-Exposure")) +
  scale_x_continuous(name="Time Post-Exposure (Seconds)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 30, 60, 90), 
                     label = c("0", "30", "60", "220")) +
  scale_y_continuous(name = "Percentage of Motile Cells", 
                     expand = c(0, 0),
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c("0%", "25%", "50%", "75%", "100%")) +
  coord_cartesian(xlim = c(-40, 100), ylim = c(0, 100)) +
  annotate("rect", xmin = -20, xmax = 0, ymin = 0, ymax = 100,
           alpha = 0.1, fill = "black") +
  annotate("rect", xmin = -40, xmax = -20, ymin = 0, ymax = 100,
           alpha = 0.3, fill = "black") +
  labs(tag = "E.") +
  theme(axis.title.x = element_text(hjust = 0.8, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none", 
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_rect(colour = "black", linewidth = 1))

E

# Swimming speed plot

F <- ggplot(data = SS_36_Avg_DF, aes(x = Time, y = Mean)) +
  geom_point(size = 2) +
  geom_line(aes(group = Period), linewidth = 1.5) +
  geom_errorbar(data = SS_36_Avg_DF, 
                aes(x = Time, ymin = Mean-SD, ymax = Mean+SD),
                width = 1, linewidth = 0.5) +
  scale_color_manual(name = NULL,
                     breaks = c("Before", "During", "After"), 
                     labels = c("Before Exposure", "During Exposure - 100 MPa", "Post-Exposure")) +
  scale_x_continuous(name = "Time Post-Exposure (Seconds)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 30, 60, 90), 
                     label = c("0", "30", "60", "240")) +
  scale_y_continuous(name = swimming_speed, 
                     expand = c(0, 0)) +
  coord_cartesian(xlim = c(-40, 100),
                  ylim = c(0, 60)) +
  annotate("rect", xmin = -20, xmax = 0, ymin = 0, ymax = 60,
           alpha = 0.1, fill = "black") +
  annotate("rect", xmin = -40, xmax = -20, ymin = 0, ymax = 60,
           alpha = 0.3, fill = "black") +
  labs(tag = "F.") +
  theme(axis.title.x = element_text(hjust = 0.8, face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none", 
        plot.margin = unit(c(0.5, 0.1, 0.1, 0.1), "cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.background = element_rect(colour = "black", linewidth = 1))

F


## Combine the plots for strain 18 ##

# We'll add a legend later

Title_36 <- expression(paste(bolditalic('Shewanella'), bold(" sp. strain 36 GOM-46m")))

combo_36 <- grid.arrange(E, F, top = textGrob(Title_36, vjust = 1), nrow = 1)


#######################
## COMBINE THE PLOTS ##
#######################

combo_all <- grid.arrange(combo_10BA, combo_18, combo_36, ncol = 1, nrow = 3)

# Save the final plots 

ggsave("Fig4_Pressure_Recovery.png", plot = combo_all, 
       width = 8.5, height = 11, unit = 'in', 
       dpi = 600, bg = "white", 
       path = "~/Desktop")

ggsave("Fig4_Pressure_Recovery.svg", plot = combo_all, 
       width = 8.5, height = 11, unit = 'in', 
       dpi = 600, bg = "white", 
       path = "~/Desktop")

