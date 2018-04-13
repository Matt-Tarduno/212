rm(list=ls())

#  Setup ----------------------------------------------------------------

# Sebastien Annan-Phan, Alejandro Favela, Matthew Tadruno 

# Packages 
library(pacman)
library(ggpubr)

# p_load examples
p_load(dplyr, haven, readr)
p_load(ggplot2,extrafont,Matrix, reshape)

# Directories  
if (Sys.getenv("LOGNAME")=="AlexFavela") {
  dir_data <- "~/Google Drive/ARE 212 Psets/Pset 1/Data/"
}

if (Sys.getenv("LOGNAME")=="matthewtarduno") {
  dir_data <- "/Users/matthewtarduno/Desktop/212/pset2/"
  # If this doesn't work, set your gdrive path
}

if (Sys.getenv("LOGNAME")=="") {
  dir_data <- "C:/Users/sphan/Google Drive/ARE 212 Psets/Pset 2/"
}

# Q.1 "Read the data into R" ----------------------------------------------------------------

nerlove <- read_csv(paste0(dir_data, "nerlove.csv"))

clist <- list("PL","PF","PK","Q")

plot_list = list()
for (var in clist){
  attach(nerlove)
  y_var = get(var)
  gph = ggplot() + 
  geom_point(aes_(x = TC, y = y_var), col = "maroon",  alpha = .5, size = 2) +
  ggtitle(paste0(var)) + ylab(var) + xlab("Total Cost") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))
  assign(paste0("gph_",var), gph)
}

ggarrange(gph_PL, gph_PF, gph_PK, gph_Q, ncol = 2, nrow = 2, align = "hv")


### Clean PL ###
before <- ggplot(data = nerlove) + 
  geom_histogram(aes(x = PL), fill = "maroon", col = "white", bins = 15) +
  ggtitle("Raw data")

#Remove observation with value more than 10 times the mean
nerlove$PL[which(nerlove$PL > 100)] <- NA
 
after <- ggplot(data = nerlove) + 
  geom_histogram(aes(x = PL), fill = "maroon", col = "white", bins = 15) +
ggtitle("without outlier")

ggarrange(before, after, ncol = 2, nrow = 1, align = "h")

plot_list = list()
for (var in clist){
  attach(nerlove)
  y_var = get(var)
  gph = ggplot() + 
    geom_point(aes_(x = TC, y = y_var), col = "maroon",  alpha = .5, size = 2) +
    ggtitle(paste0(var)) + ylab(var) + xlab("Total Cost") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  assign(paste0("gph_",var), gph)
}

ggarrange(gph_PL, gph_PF, gph_PK, gph_Q, ncol = 2, nrow = 2, align = "hv")





  