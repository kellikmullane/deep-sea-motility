######################################
######################################
### MULLANE ET AL. (2023) FIGURE 2 ###
######################################
######################################

### Packages

packages <- c(
  "readr", "ggplot2", "ggtrendline", "ggpubr", "grid", 
  "gridExtra", "tidyverse", "ggforce", "svglite", "SciViews"
)

funlist <-  lapply(packages, function(x) {
  if (x %in% rownames(installed.packages())) {
    require(x, character.only = T)
  }else{
    install.packages(x, character.only = T); require(x, character.only = T)
  }
})

### Download files from GitHub

GR_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/Verhulst_Model_Growth_Rates.csv"
ES_10BA_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/10BA_Expansion_Speed.csv"
ES_18_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/18_Expansion_Speed.csv"
ES_36_url = "https://raw.githubusercontent.com/kellikmullane/deep-sea-motility/main/Data_Files/36_Expansion_Speed.csv"

### Read in CSV files

df_GR <- read_csv(url(GR_url))
df_10BA_ES <- read_csv(url(ES_10BA_url))
df_18_ES <- read_csv(url(ES_18_url))
df_36_ES <- read_csv(url(ES_36_url))

### Figure settings

theme_set(theme_bw(base_size = 10, base_family = "Arial"))
growth_rate <- expression(bold(paste("Growth Rate (", h^-1, ")")))
expansion_speed <- expression(bold(paste("Expansion Speed (μm/s)")))


#################################
## DATA WRANGLING & STATISTICS ##
#################################

# Subset the growth rate data by study strain

df_10BA_GR <- subset(df_GR, Strain == "10BA")
df_18_GR <- subset(df_GR, Strain == "18")
df_36_GR <- subset(df_GR, Strain == "36")

# Calculate expansion speed (mm/h)

df_10BA_ES$Speed_mm_h <- (df_10BA_ES$Distance_TE - df_10BA_ES$Distance_T0)/(df_10BA_ES$TE - df_10BA_ES$T0)
df_18_ES$Speed_mm_h <- (df_18_ES$Distance_TE - df_18_ES$Distance_T0)/(df_18_ES$TE - df_18_ES$T0)
df_36_ES$Speed_mm_h <- (df_36_ES$Distance_TE - df_36_ES$Distance_T0)/(df_36_ES$TE - df_36_ES$T0)

# Convert from mm/h to um/s

df_10BA_ES$Speed_um_s <- (df_10BA_ES$Speed_mm_h)/3.6
df_18_ES$Speed_um_s <- (df_18_ES$Speed_mm_h)/3.6
df_36_ES$Speed_um_s <- (df_36_ES$Speed_mm_h)/3.6

# Find the mean & standard deviation of the expansion speed

df_10BA_ES <- df_10BA_ES %>%
  group_by(Temperature, Pressure) %>%
  summarise(ES_Avg = mean(Speed_um_s), ES_SD = sd(Speed_um_s))

df_18_ES <- df_18_ES %>%
  group_by(Temperature, Pressure) %>%
  summarise(ES_Avg = mean(Speed_um_s), ES_SD = sd(Speed_um_s))

df_36_ES <- df_36_ES %>%
  group_by(Temperature, Pressure) %>%
  summarise(ES_Avg = mean(Speed_um_s), ES_SD = sd(Speed_um_s))

#################
## STRAIN 10BA ##
#################

# Combine the strain 10BA growth rate and expansion speed dataframes

df_10BA <- merge(df_10BA_GR, df_10BA_ES)

df_10BA$GR_SD <- df_10BA$GR_StDev
df_10BA$GR_Avg <- df_10BA$GR

df_10BA <- subset(df_10BA, select=-c(Strain, GR, GR_StDev, Mp, Mp_StDev))

#Combine the Temperature & Pressure into a new column called "ID" - this will make plotting easier

df_10BA$ID <- paste(df_10BA$Temperature, df_10BA$Pressure, sep = "_")

# Pivot the DF longer

df_10BA <- df_10BA %>%
  pivot_longer(cols = -c(ID, Pressure, Temperature), names_to = c("Names"), values_to = "Value")

# Split the "Names" column into 2

df_10BA <- df_10BA %>%
  separate(col = "Names", into = c("ES_GR", "Avg_SD"), "_")

# Combine the ID & "ES_GR" columns into a new column called "ID_Type"

df_10BA$ID_Temp <- paste(df_10BA$Temperature, df_10BA$ES_GR, sep = "_")

# And finally, pivot wider for plotting

df_10BA <- df_10BA %>%
  select(-ID, -ES_GR, -Temperature) %>%
  pivot_wider(names_from = Avg_SD, values_from = Value)

df_10BA <- replace(df_10BA, is.na(df_10BA), 0)

# Plot strain 10BA data

A <- ggplot(data = df_10BA, aes(linetype = as.factor(ID_Temp))) + 
  geom_point(aes(x = Pressure, y = Avg, col = as.factor(ID_Temp)), size = 2.5) +
  geom_errorbar(aes(x = Pressure, ymin = Avg - SD, ymax = Avg + SD, col = as.factor(ID_Temp)), 
                width = 1, size = 0.5, linetype = 1) + 
  stat_smooth(aes(x = Pressure, y = Avg, color = as.factor(ID_Temp)), se = FALSE, method = "lm",
              show.legend = FALSE) +
  stat_regline_equation(aes(x = Pressure, y = Avg, 
                        color = as.factor(ID_Temp), 
                        label =  paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
                        label.x = c(29, 29, 29, 29),
                        label.y = c(0.46, 0.415, 0.37, 0.325),
                        show.legend = FALSE,
                        size = 2) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
  scale_x_continuous(name = "Pressure (MPa)", 
                     expand = c(0.003, 0),
                     breaks = c(0, 10, 20, 30, 40, 50), 
                     label = c("0", "10", "20", "30", "40", "50")) + 
  scale_y_continuous(name = growth_rate, 
                     expand = c(0,0),
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     label = c("0.00", "0.10", "0.20", "0.30", "0.40", "0.50"),
                     sec.axis = dup_axis(name = expansion_speed)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.5)) + 
  labs(tag = "A.") +
  scale_color_manual(name = NULL,
                     breaks = c("30_GR", "30_ES", "4_GR", "4_ES"),
                     values = c("#e31a1c", "#fd8d3c", "#225ea8", "#1d91c0"),
                     label = c("Growth Rate - 30°C", "Expansion Speed - 30°C", "Growth Rate - 4°C", "Expansion Speed - 4°C")) +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) 

A


###############
## STRAIN 18 ##
###############

# Combine the strain 18 growth rate and expansion speed dataframes

df_18 <- merge(df_18_GR, df_18_ES)

df_18$GR_SD <- df_18$GR_StDev
df_18$GR_Avg <- df_18$GR

df_18 <- subset(df_18, select=-c(Strain, GR, GR_StDev, Mp, Mp_StDev))

#Combine the Temperature & Pressure into a new column called "ID" - this will make plotting easier

df_18$ID <- paste(df_18$Temperature, df_18$Pressure, sep = "_")

# Pivot the DF longer

df_18 <- df_18 %>%
  pivot_longer(cols = -c(ID, Pressure, Temperature), names_to = c("Names"), values_to = "Value")

# Split the "Names" column into 2

df_18 <- df_18 %>%
  separate(col = "Names", into = c("ES_GR", "Avg_SD"), "_")

# Combine the ID & "ES_GR" columns into a new column called "ID_Type"

df_18$ID_Temp <- paste(df_18$Temperature, df_18$ES_GR, sep = "_")

# And finally, pivot wider for plotting

df_18 <- df_18 %>%
  select(-ID, -ES_GR, -Temperature) %>%
  pivot_wider(names_from = Avg_SD, values_from = Value)

df_18 <- replace(df_18, df_18 == 0, NA)

# Plot strain 18 data 

B <- ggplot(data = df_18, aes(linetype = as.factor(ID_Temp))) + 
  geom_point(aes(x = Pressure, y = Avg, col = as.factor(ID_Temp)), size = 2.5) +
  geom_errorbar(aes(x = Pressure, ymin = Avg - SD, ymax = Avg + SD, col = as.factor(ID_Temp)), 
                width = 1, size = 0.5, linetype = 1) + 
  stat_smooth(aes(x = Pressure, y = Avg, color = as.factor(ID_Temp)), se = FALSE, method = "lm",
              show.legend = FALSE) +
  stat_regline_equation(aes(x = Pressure, y = Avg, 
                        color = as.factor(ID_Temp), 
                        label =  paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
                        label.x = c(29, 29),
                        label.y = c(0.46, 0.415),
                        show.legend = FALSE,
                        size = 2) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(name = "Pressure (MPa)",
                     expand = c(0.003, 0),
                     breaks = c(0, 10, 20, 30, 40, 50), 
                     label = c("0", "10", "20", "30", "40", "50")) +
  scale_y_continuous(name = growth_rate,
                     expand = c(0,0),
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     label = c("0.00", "0.10", "0.20", "0.30", "0.40", "0.50"),
                     sec.axis = dup_axis(name = expansion_speed)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.5)) + 
  labs(tag = "B.") +
  scale_color_manual(name = NULL,
                     breaks = c("30_GR", "30_ES"),
                     values = c("#e31a1c", "#fd8d3c"),
                     label = c("Growth Rate - 30°C", "Expansion Speed - 30°C")) +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) 

B

###############
## STRAIN 36 ##
###############

# Combine the strain 36 growth rate and expansion speed dataframes

df_36 <- merge(df_36_GR, df_36_ES)

df_36$GR_SD <- df_36$GR_StDev
df_36$GR_Avg <- df_36$GR

df_36 <- subset(df_36, select=-c(Strain, GR, GR_StDev, Mp, Mp_StDev))
 
# Combine the Temperature & Pressure into a new column called "ID" - this will make plotting easier

df_36$ID <- paste(df_36$Temperature, df_36$Pressure, sep = "_")

# Pivot the DF longer

df_36 <- df_36 %>%
  pivot_longer(cols = -c(ID, Pressure, Temperature), names_to = c("Names"), values_to = "Value")

# Split the "Names" column into 2

df_36 <- df_36 %>%
  separate(col = "Names", into = c("ES_GR", "Avg_SD"), "_")

# Combine the ID & "ES_GR" columns into a new column called "ID_Type"

df_36$ID_Temp <- paste(df_36$Temperature, df_36$ES_GR, sep = "_")

# And finally, pivot wider for plotting

df_36 <- df_36 %>%
  select(-ID, -ES_GR, -Temperature) %>%
  pivot_wider(names_from = Avg_SD, values_from = Value)

# Plot strain 10BA data

C <- ggplot(data = df_36, aes(linetype = as.factor(ID_Temp))) + 
  geom_point(aes(x = Pressure, y = Avg, col = as.factor(ID_Temp)), size = 2.5) +
  geom_errorbar(aes(x = Pressure, ymin = Avg - SD, ymax = Avg + SD, col = as.factor(ID_Temp)), 
                width = 1, size = 0.5, linetype = 1) + 
  stat_smooth(aes(x = Pressure, y = Avg, color = as.factor(ID_Temp)), se = FALSE, method = "lm",
              show.legend = FALSE) +
  stat_regline_equation(aes(x = Pressure, y = Avg, 
                        color = as.factor(ID_Temp), 
                        label =  paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
                        label.x = c(29, 29, 29, 29),
                        label.y = c(0.55, 0.5, 0.45, 0.40),
                        show.legend = FALSE,
                        size = 2) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
  scale_x_continuous(name = "Pressure (MPa)",
                     expand = c(0.003, 0),
                     breaks = c(0, 10, 20, 30, 40, 50),
                     label = c("0", "10", "20", "30", "40", "50")) +
  scale_y_continuous(name = growth_rate, 
                     expand = c(0,0),
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                     label = c("0.00", "0.10", "0.20", "0.30", "0.40", "0.50", "0.60"),
                     sec.axis = dup_axis(name = expansion_speed)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.6)) + 
  labs(tag = "C.") +
  scale_color_manual(name = NULL,
                     breaks = c("30_GR", "30_ES", "4_GR", "4_ES"),
                     values = c("#e31a1c", "#fd8d3c", "#225ea8", "#1d91c0"),
                     label = c("Growth Rate - 30°C", "Expansion Speed - 30°C", "Growth Rate - 4°C", "Expansion Speed - 4°C")) +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "none",
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) 

C


#######################
## COMBINE THE PLOTS ##
#######################

# Make pretty titles for each strain

Title_10BA <- expression(paste(bolditalic('Halomonas'), bold(" sp. strain 10BA GOM-1509m")))
Title_18 <- expression(paste(bolditalic('Alcanivorax'), bold(" sp. strain 18 GOM-1509m")))
Title_36 <- expression(paste(bolditalic('Shewanella'), bold(" sp. strain 36 GOM-46m")))

# Combine the plots & titles by strain (this will make it easier to combine them all later)

AA <- grid.arrange(A, top = textGrob(Title_10BA))
BB <- grid.arrange(B, top = textGrob(Title_18))
CC <- grid.arrange(C, top = textGrob(Title_36))

# We need a legend, so let's re-plot one of the strains with the legend

A2 <- ggplot(data = df_10BA, aes(linetype = as.factor(ID_Temp))) + 
  geom_point(aes(x = Pressure, y = Avg, col = as.factor(ID_Temp)), size = 2.5) +
  geom_errorbar(aes(x = Pressure, ymin = Avg - SD, ymax = Avg + SD, col = as.factor(ID_Temp)), 
                width = 1, size = 0.5, linetype = 1) + 
  stat_smooth(aes(x = Pressure, y = Avg, color = as.factor(ID_Temp)), se = FALSE, method = "lm",
              show.legend = FALSE) +
  stat_regline_equation(aes(x = Pressure, y = Avg, 
                        color = as.factor(ID_Temp), 
                        label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
                        label.x = c(35, 35, 35, 35),
                        label.y = c(0.46, 0.415, 0.37, 0.325),
                        show.legend = FALSE,
                        size = 2.5) +
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed")) +
  scale_x_continuous(name = "Pressure (MPa)",
                     expand = c(0.003, 0),
                     breaks = c(0, 10, 20, 30, 40, 50),
                     label = c("0", "10", "20", "30", "40", "50")) + 
  scale_y_continuous(name = growth_rate,
                     expand = c(0,0),
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     label = c("0.00", "0.10", "0.20", "0.30", "0.40", "0.50"),
                     sec.axis = dup_axis(name = expansion_speed)) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.5)) +
  labs(tag = "A.") +
  scale_color_manual(name = NULL,
                     breaks = c("30_GR", "30_ES", "4_GR", "4_ES"),
                     values = c("#e31a1c", "#fd8d3c", "#225ea8", "#1d91c0"),
                     label = c("Growth Rate - 30°C", "Expansion Speed - 30°C", "Growth Rate - 4°C", "Expansion Speed - 4°C")) +
  guides(linetype = "none",
         color = guide_legend(nrow = 2)) +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom",
        plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()) 

A2

# Extract the legend

legend <- function(a.gplot){
  if (!gtable::is.gtable(a.gplot))
    a.gplot <- ggplotGrob(a.gplot)
  leg <- which(sapply(a.gplot$grobs, function(c) c$name) == "guide-box")
  a.gplot$grobs[[leg]]
}

Bottom_Legend <- legend(A2)

# Then combine them all together

combo <- grid.arrange(AA, BB, CC, Bottom_Legend, nrow = 4, layout_matrix = rbind(c(1), c(2), c(3), c(4)), heights = c(3, 3, 3, 0.5))

# Save the final plots 

ggsave("Fig2_Growth_Rate_Expansion_Speed.png", plot = combo, 
       width = 4.6, height = 9.5, unit = 'in', 
       dpi = 600, bg = "white", 
       path = "~/Desktop")

ggsave("Fig2_Growth_Rate_Expansion_Speed.svg", plot = combo, 
       width = 4.6, height = 9.5, unit = 'in', 
       dpi = 600, bg = "white", 
       path = "~/Desktop")

