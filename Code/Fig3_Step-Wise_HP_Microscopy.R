######################################
######################################
### MULLANE ET AL. (2023) FIGURE 3 ###
######################################
######################################

### Packages

packages <- c(
  "readr", "ggplot2", "grid", "gridExtra", 
  "ragg", "svglite", "scales"
)

funlist <-  lapply(packages, function(x) {
  if (x %in% rownames(installed.packages())) {
    require(x, character.only = T)
  }else{
    install.packages(x, character.only = T); require(x, character.only = T)
  }
})

### Download files from GitHub

SS_10BA_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/10BA_Swimming_Speed.csv"
PM_10BA_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/10BA_Percent_Motile_Cells.csv"
SS_18_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/18_Swimming_Speed.csv"
PM_18_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/18_Percent_Motile_Cells.csv"
SS_36_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/36_Swimming_Speed.csv"
PM_36_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/36_Percent_Motile_Cells.csv"
  
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

# Percent motile cells plot

A <- ggplot(data = PM_10BA, 
            aes(x = Pressure, y = Percent, col = ID)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(data = PM_10BA,
                aes(x = Pressure, ymin = Percent-StDev, ymax = Percent+StDev), 
                width = 1, linewidth = 0.5) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 20, 40, 60, 80), 
                     label = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name = "Percent Motile Cells", 
                     expand = c(0, 0),
                     breaks = c(0, 25, 50, 75, 100),
                     label = c("0%", "25%", "50%", "75%", "100%")) +
  coord_cartesian(xlim = c(0, 85), ylim = c(0, 100)) +
  scale_color_manual(name = NULL,
                     breaks = c("RT_F1", "RT_F2", "7C_F1", "7C_F2"),
                     values = c("#ef3b2c", "#67000d", "#4292c6", "#08306b")) +
  labs(tag = "A.") +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))

A

# Swimming speed plot

B <- ggplot(data = SS_10BA, 
            aes(x = Pressure, y = Speed, col = ID)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(data = SS_10BA, 
                aes(x = Pressure, ymin = Speed-StDev, ymax = Speed+StDev), 
                width = 1, linewidth = 0.5) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 20, 40, 60, 80),
                     label = c("0", "20", "40", "60", "80")) +
  scale_y_continuous(name = swimming_speed, 
                     expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 85), ylim = c(0, 40)) +
  scale_color_manual(name = NULL,
                     breaks = c("RT_F1", "RT_F2", "7C_F1", "7C_F2"),
                     values = c("#ef3b2c", "#67000d", "#4292c6", "#08306b")) +
  labs(tag = "B.")+
  theme(axis.title = element_text(face = "bold"), 
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

B


## Combine the plots for strain 10BA ##

# We'll add a legend later

Title_10BA <- expression(paste(bolditalic('Halomonas'), bold(" sp. strain 10BA GOM-1509m")))

combo_10BA <- grid.arrange(A, B, top = textGrob(Title_10BA, vjust = 1), nrow = 1)


###############
## STRAIN 18 ##
###############

# Percent motile cells plot

C <- ggplot(data = PM_18, 
            aes(x = Pressure, y = Percent, col = ID)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(data = PM_18,
                aes(x = Pressure, ymin = Percent-StDev, ymax = Percent+StDev), 
                width = 1, linewidth = 0.5) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 20, 40, 60, 80, 100, 120), 
                     label = c("0", "20", "40", "60", "80", "100", "120")) +
  scale_y_continuous(name = "Percent Motile Cells", 
                     expand = c(0, 0),
                     breaks = c(0, 25, 50, 75, 100),
                     label = c("0%", "25%", "50%", "75%", "100%")) +
  coord_cartesian(xlim = c(0, 125), ylim = c(0, 100)) +
  scale_color_manual(name = NULL,
                     breaks = c("RT_F1", "RT_F2"),
                     values = c("#ef3b2c", "#67000d")) +
  labs(tag = "C.") +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))

C

# Swimming speed plot

D <- ggplot(data = SS_18, 
            aes(x = Pressure, y = Speed, col = ID)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(data = SS_18, 
                aes(x = Pressure, ymin = Speed-StDev, ymax = Speed+StDev), 
                width = 1, linewidth = 0.5) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 20, 40, 60, 80, 100, 120),
                     label = c("0", "20", "40", "60", "80", "100", "120")) +
  scale_y_continuous(name = swimming_speed, 
                     expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 125), ylim = c(0, 60)) +
  scale_color_manual(name = NULL,
                     breaks = c("RT_F1", "RT_F2"),
                     values = c("#ef3b2c", "#67000d")) +
  labs(tag = "D.")+
  theme(axis.title = element_text(face = "bold"), 
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

D


## Combine the plots for strain 10BA ##

# We'll add a legend later

Title_18 <- expression(paste(bolditalic('Alcanivorax'), bold(" sp. strain 18 GOM-1509m")))

combo_18 <- grid.arrange(C, D, top = textGrob(Title_18, vjust = 1), nrow = 1)


###############
## STRAIN 36 ##
###############

# Percent motile cells plot

E <- ggplot(data = PM_36, 
            aes(x = Pressure, y = Percent, col = ID)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(data = PM_36,
                aes(x = Pressure, ymin = Percent-StDev, ymax = Percent+StDev), 
                width = 1, linewidth = 0.5) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 20, 40, 60, 80, 100, 120), 
                     label = c("0", "20", "40", "60", "80", "100", "120")) +
  scale_y_continuous(name = "Percent Motile Cells", 
                     expand = c(0, 0),
                     breaks = c(0, 25, 50, 75, 100),
                     label = c("0%", "25%", "50%", "75%", "100%")) +
  coord_cartesian(xlim = c(0, 125), ylim = c(0, 100)) +
  scale_color_manual(name = NULL,
                     breaks = c("RT_F1", "RT_F2", "7C_F1", "7C_F2"),
                     values = c("#ef3b2c", "#67000d", "#4292c6", "#08306b")) +
  labs(tag = "E.") +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"))

E

# Swimming speed plot

F <- ggplot(data = SS_36, 
            aes(x = Pressure, y = Speed, col = ID)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(data = SS_36, 
                aes(x = Pressure, ymin = Speed-StDev, ymax = Speed+StDev), 
                width = 1, linewidth = 0.5) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 20, 40, 60, 80, 100, 120),
                     label = c("0", "20", "40", "60", "80", "100", "120")) +
  scale_y_continuous(name = swimming_speed, 
                     expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 125), ylim = c(0, 60)) +
  scale_color_manual(name = NULL,
                     breaks = c("RT_1", "RT_2", "7C_1", "7C_2"),
                     values = c("#ef3b2c", "#67000d", "#4292c6", "#08306b")) +
  labs(tag = "F.")+
  theme(axis.title = element_text(face = "bold"), 
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

F


## Combine the plots for strain 10BA ##

# We'll add a legend later

Title_36 <- expression(paste(bolditalic('Shewanella'), bold(" sp. strain 36 GOM-46m")))

combo_36 <- grid.arrange(E, F, top = textGrob(Title_36, vjust = 1), nrow = 1)

#######################
## COMBINE THE PLOTS ##
#######################

# We need a legend, so let's re-plot one of the datasets with the legend

B2 <- ggplot(data = SS_10BA, 
             aes(x = Pressure, y = Speed, col = ID)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(data = SS_10BA, 
                aes(x = Pressure, ymin = Speed-StDev, ymax = Speed+StDev), 
                width = 1, linewidth = 0.5) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0), 
                     breaks = c(0, 20, 40, 60, 80, 100, 120),
                     label = c("0", "20", "40", "60", "80", "100", "120")) +
  scale_y_continuous(name = swimming_speed, 
                     expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 85), ylim = c(0, 40)) +
  scale_color_manual(name = NULL,
                     breaks = c("RT_F1", "RT_F2", "7C_F1", "7C_F2"),
                     labels = c("23°C Pressurization", "23°C Depressurization", "7°C Pressurization", "7°C Depressurization"),
                     values = c("#ef3b2c", "#67000d", "#4292c6", "#08306b")) +
  labs(tag = "B.")+
  theme(axis.title = element_text(face = "bold"), 
        legend.position = "bottom",
        plot.margin = unit(c(0.5,0.5,0,0.5), "cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

B2

# Extract the legend

legend <- function(a.gplot){
  if (!gtable::is.gtable(a.gplot))
    a.gplot <- ggplotGrob(a.gplot)
  leg <- which(sapply(a.gplot$grobs, function(c) c$name) == "guide-box")
  a.gplot$grobs[[leg]]
}

Bottom_Legend <- legend(B2)

# Then combine them all together

combo_all <- grid.arrange(combo_10BA, combo_18, combo_36, Bottom_Legend, ncol = 1, nrow = 4, heights = c(4,4,4,0.4))

# Save the final plots 

ggsave("Fig3_Step-Wise_HP_Microscopy.png", plot = combo_all, 
       width = 8.5, height = 11, unit = 'in', 
       dpi = 600, bg = "white", 
       path = "~/Desktop")

ggsave("Fig3_Step-Wise_HP_Microscopy.svg", plot = combo_all, 
       width = 8.5, height = 11, unit = 'in', 
       dpi = 600, bg = "white", 
       path = "~/Desktop")

