library(tidyverse)
library(ggplot2)
library(ggpubr)

# Set working directory for Trackman csv
setwd("~/Desktop/Wareham/Data/Games")

# CHANGE
df = read.csv("20240731-CCBLBourne-1_unverified.csv")

# Catcher's name in Last, First Format
name <- "Jarrell, Jacob" 
date <- "2024_07_31"
opp <- "Bourne"
last_name <- str_trim(str_extract(name, "^[^,]+"))

#-------------------------
df_who= filter(df, Catcher == name)

#Build Plate dimensions
d <- 2.92/12 # diameter of baseball
#rulebook zone
plate_x_dim <- c(-9.95/12, 9.95/12, 9.95/12, -9.95/12, -9.95/12)
plate_y_dim <- c(1.622, 1.622, 3.37, 3.37, 1.622) 
plate_dim_df <- data.frame(x=plate_x_dim, y=plate_y_dim)
plate_edge_x <- c(plate_x_dim[1] - 2.94/12, plate_x_dim[2] + 2.94/12, 
                  plate_x_dim[3] + 2.94/12, plate_x_dim[4] - 2.94/12, 
                  plate_x_dim[5] - 2.94/12) #accounts for radius of baseball
plate_edge_y <- c(1.622 - 2.94/12, 1.622 - 2.94/12, 3.37 + 2.94/12, 
                  3.37 + 2.94/12, 1.622 - 2.94/12) 
plate_edges <- data.frame(x=plate_edge_x, y=plate_edge_y)
strikezone <- c(plate_edge_x[1], plate_edge_x[2], plate_edge_y[1], plate_edge_y[3])
plate <- data.frame(x = c(0, 8.5/12, 8.5/12, -8.5/12, -8.5/12),
                    y = c(0, -sqrt((12**2)-(8.5**2))/12/3, 0-(sqrt((12**2)-(8.5**2))+8.5)/12/2.5,
                          0-(sqrt((12**2)-(8.5**2))+8.5)/12/2.5, -sqrt((12**2)-(8.5**2))/12/3))

#Function to build plot
final_plot <- function(df) {
  plot <- ggplot() + 
    geom_polygon(data = plate, aes(x=x, y=y), fill = NA, color = "black") +
    geom_path(data = plate_edges, aes(x=x, y=y), linetype = "dashed") +
    geom_path(data=plate_dim_df, aes(x=x, y=y)) +
    geom_point(data = df, aes(x=PlateLocSide, y=PlateLocHeight, color=PitchCall, shape=TaggedPitchType)) +
    lims(x=c(-2, 2), y=c(-0.5656846, 5)) +
    theme_bw() +
    coord_equal() +
    theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
          axis.title=element_blank(),
          legend.title = element_blank(), 
          legend.position = "bottom", legend.box = "vertical") +
    scale_color_manual(labels = c("Ball Called", "Strike Called"), 
                       values = c("blue", "red"), drop = TRUE) 
  
  return(plot)}

for (i in 1:nrow(df_who)) {
  x = df_who$PlateLocSide[i]
  z = df_who$PlateLocHeight[i]
}

# Filter for called pitches, determine whether in rule book zone 
# or not and whether correct call was made
calls <- df_who %>% 
  filter(PitchCall %in% c("BallCalled", "StrikeCalled")) %>% 
  select(PitchNo, Pitcher, Batter, BatterTeam, Inning, Balls, Strikes, Outs, PitchCall, PlateLocSide, PlateLocHeight) 

calls2 <- calls %>% mutate(
  InZone = ifelse(abs(PlateLocSide) <= 0.83 & PlateLocHeight > 1.479 & PlateLocHeight <= 3.5, 1, 0),
  CallNum = ifelse(PitchCall == "StrikeCalled", 1, 0),
  CallScore = InZone - CallNum
)

misses <- calls2 %>% 
  filter(CallScore != 0)

# Calculate overall accuracy
accuracy <- 1 - dim(misses)[1] / dim(calls2)[1]

# Calculate ball and strike accuracy
true_strikes <- sum(calls2$InZone, na.rm = TRUE)
true_balls <- dim(calls2)[1] - true_strikes
true_balls_called_strikes <- sum(misses$CallScore == -1)
true_strikes_called_balls <- sum(misses$CallScore == 1)
ball_acc <- 1 - true_balls_called_strikes / true_balls
strike_acc <- 1 - true_strikes_called_balls / true_strikes

df_filtered <- df_who %>% filter(PitchCall %in% c("BallCalled", "StrikeCalled"))
lhh <- final_plot(df_filtered %>% filter(BatterSide == "Left"))
rhh <- final_plot(df_filtered %>% filter(BatterSide == "Right"))
overall <- final_plot(df_filtered)

fig <- ggarrange(lhh + labs(title = "LHH") + theme(plot.title = element_text(hjust = .5)), 
                 overall + labs(title = "Overall") + theme(plot.title = element_text(hjust = .5)), 
                 rhh + labs(title = "RHH") + theme(plot.title = element_text(hjust = .5)), 
                 ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")

ann_fig <- annotate_figure(fig, top = text_grob(paste0(date, "-", opp, "-", name)), 
                           bottom = text_grob(paste0("Total Acc: ", round(accuracy, digits = 4)*100,
                                                     "%, In Zone Acc: ", round(strike_acc, digits = 4)*100, 
                                                     "%, Out Zone Acc: ", round(ball_acc, digits = 4)*100, "%")))

# Open a pdf file to create plot
dir.create(paste0("~/Desktop/Wareham/Data/Reports/", date, "_", opp), recursive = TRUE)
setwd(paste0("~/Desktop/Wareham/Data/Reports/", date, "_", opp))
pdf(paste0(date, "_", name, "_", "_Umpire_Report.pdf")) 
# Create a plot
ann_fig
# Close the pdf file
dev.off()
