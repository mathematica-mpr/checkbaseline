#### STANDARDIZED BIAS PLOT WITHOUT ARROWS [i.e. no matching]
sbplot_nm <- function(plot.df = non_matched.tb) {

 a <- -0.5; b <- 0.5
 plot.df$NameNumber <- as.numeric(rownames(plot.df))
 min_x = round(min(plot.df$Standardized.bias),1) -.1
 max_x = round(max(plot.df$Standardized.bias),1) +.1
 if(min_x > a) { min_x <- a}; if(max_x < b) { max_x <- b}
 absolute_max <- max(c(abs(min_x),abs(max_x)))
 min_x <- -1*absolute_max; max_x <- absolute_max
 range = max(abs(min_x),abs(max_x))
 ncovs <- length(plot.df$Name)

 df.bars <- data.frame(xmin = c(-range,-0.25,-0.05,0.05,0.25),
                       xmax = c(-0.25, -0.05, 0.05,0.25,range),
                       ymin = 0.5,
                       ymax = ncovs +.5,
                       balance = factor(c("Unbalanced","Good balance","Great balance","Good balance","Unbalanced"),
                       levels = c("Great balance","Good balance","Unbalanced")))

 #print(paste0("ncovs = ", ncovs, ";range = ", -range, ",", range))
 sb.plot_nm <- ggplot() +

  # background bars
  geom_rect(data=df.bars,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                fill = balance), alpha = 0.9) +
  #scale_fill_manual(values = c("#6C6F70","#D7D3C8","#C7BE71","#D7D3C8","#6C6F70")) +
  #scale_fill_manual(values = c("#999967", "#CCCC9A","#666666")) +
   scale_fill_manual(values = c("#f0f0f0", "#bdbdbd","#636363")) +
  scale_alpha(guide = 'none') +

  # scatters
  geom_point(data = plot.df, aes(x = Standardized.bias, y = NameNumber), size = 4) +
  xlab("Effect size") +
  scale_x_continuous(breaks = c(-.9, -.7, -.5, -.25, -.05, .05, .25, .5, .7, .9),
                     limits = c(-range,range)) +
  ylab(" ") +
  scale_y_continuous(breaks = c(1:ncovs),
                     labels = as.character(unique(plot.df$Name)),
                     limits = c(0.5,ncovs+.5)) +
  # theme_bw(base_size = 14) +
  # theme(legend.position = "right",
  #       legend.title = element_text(size = 2, colour = "white"),
  #       legend.key = element_blank(), #<-removes boxes around key symbols
  #       axis.ticks = element_blank(),
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       panel.grid.minor.y = element_blank(),
  #       panel.grid.major.y = element_line(colour = "gray30", linetype = 2 ),
  #       panel.border = element_blank()) +
  # guides(fill = FALSE) + #<-removes rectangles dataset legend
  geom_vline(xintercept = 0, linetype = "longdash", colour="gray") +
  coord_cartesian(xlim = c(min_x,max_x)) +
  theme_mpr() +
  theme(legend.title=element_blank())
 return(sb.plot_nm)

}

#### STANDARDIZED BIAS PLOT WITH ARROWS [i.e. with matched and un-matched data]*
sbplot_wm <- function(plot.df, mytitle = "Effect size") {

 # obtain parameters
 a <- -0.5; b <- 0.5
 plot.df$NameNumber <- as.numeric(rownames(plot.df))
 min_x = round(min(plot.df$Standardized.bias),1) -.1
 max_x = round(max(plot.df$Standardized.bias),1) +.1
 if(min_x > a) { min_x <- a}; if(max_x < b) { max_x <- b}
 absolute_max <- max(c(abs(min_x),abs(max_x)))
 min_x <- -1*absolute_max; max_x <- absolute_max
 range = max(abs(min_x),abs(max_x))
 ncovs <- length(unique(plot.df$Name))
 df.bars <- data.frame(xmin = c(-range,-0.25,-0.05,0.05,0.25),
                      xmax = c(-0.25, -0.05, 0.05,0.25,range),
                      ymin = 0.5,
                      ymax = ncovs +.5,
                      balance = factor(c("Unbalanced","Good balance","Great balance","Good balance","Unbalanced"),
                      levels = c("Great balance","Good balance","Unbalanced")))

 # constructing arrows
 plot.df <- arrange(plot.df, Matching, desc(sortorder)) %>%
  tbl_df %>%
  group_by(Matching) %>%
  dplyr::mutate(NameNumber = row_number())
 plot.df <- arrange(plot.df, Matching, NameNumber)
 plot.df <- merge(
  plot.df,
  plot.df[plot.df$Matching == 'None', c('NameNumber', 'Standardized.bias')],
  by = c('NameNumber'),
  suffixes = c('', '_unmatched'),
  all = TRUE)
 plot.df$dirsign <- ifelse(
  plot.df$Matching == 'Matched',
  -.01 * sign(plot.df$Standardized.bias_unmatched - plot.df$Standardized.bias),
  NA)

 sb.plot_wm <- ggplot() +

  # background bars
  geom_rect(data=df.bars,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                fill = balance), alpha = 0.9) +
     scale_x_continuous(breaks = c(-.9, -.7, -.5, -.25, -.05, .05, .25, .5, .7, .9),
                        limits = c(-range,range)) +
#  scale_fill_manual(values = c("#6C6F70","#D7D3C8","#C7BE71","#D7D3C8","#6C6F70")) +
  scale_alpha(guide = 'none') +

  # scatters
  geom_point(data = plot.df, aes(x = Standardized.bias, y = NameNumber, colour = Matching), size = 4) +
  xlab("Effect size") +
  scale_colour_manual(values = c(None = "#34B6E4", Matched = "#002E5F"),
                      labels = c(None = "Before matching", Matched = "After matching"),
                      limits = c("None","Matched")) +
  ylab(" ") +
  scale_y_continuous(breaks = c(1:ncovs),
                     labels = as.character(unique(plot.df$Name)),
                     limits = c(0.5,ncovs+.5)) +
  # theme_bw(base_size = 14) +
  # theme(legend.position = "right",
  #       legend.title = element_text(size = 2, colour = "white"),
  #       legend.key = element_blank(), #<-removes boxes around key symbols
  #       axis.ticks = element_blank(),
  #       axis.text = element_text(size = 20),
  #       axis.title = element_text(size = 20),
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       panel.grid.minor.y = element_blank(),
  #       panel.grid.major.y = element_line(colour = "gray30", linetype = 2 ),
  #       panel.border = element_blank(),
  #       strip.background = element_rect(fill="white", color="white")) +
   theme_mpr() +
   theme(legend.title=element_blank()) +
   scale_fill_manual(values = c("#f0f0f0", "#bdbdbd","#636363")) +
  #guides(fill = FALSE) + #<-removes rectangles dataset legend
  geom_vline(xintercept = 0, linetype = "longdash", colour="gray") +

  # adding arrows
  geom_segment(
   data = plot.df[plot.df$Matching == 'Matched', ],
   aes(
    x = Standardized.bias_unmatched,
    xend = Standardized.bias,
    y = NameNumber,
    yend = NameNumber),
   colour = "#34B6E4",
   arrow = arrow(length = unit(0.3, "cm")))
 return(sb.plot_wm)

}

