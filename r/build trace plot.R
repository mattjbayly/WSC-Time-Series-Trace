########################################
# Build Trace Plots of WSC Time Series #
########################################
#######################################

# Set local directories
base_dir <- getwd()

# Load Libraries
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(purrr)
library(RColorBrewer)
library(grDevices)

# Load custom trace plot function
source("./r/functions/TracePlot.R")

# Load WSC station data - daily averages
dat <- fread(paste0(base_dir, "/data/daily.summary.data.csv"))


# Filter out records with less than 10 years of data
datyr <- dat %>%
  group_by(ID) %>%
  mutate(nyr = median(YRCOUNT, na.rm=TRUE)) %>%
  filter(nyr > 100)
unique(datyr$ID)
summary(datyr$nyr)

# Set date for x-axis - dummy yr and time
datyr$xval <- as.POSIXct(paste0("2000-", datyr$DATE, " 00:00:00"), format="%Y-%m-%d %H:%M:%S")
# Need to set to continuious number for plot
datyr$xval <- as.numeric(as.character(format(datyr$xval, "%j")))

# Build Color Palette for Howe Sound
mpal <- rev(c("#4d6a88ff", "#2b7ae0ff", "#5daae5ff", "#5eade4ff", "#b6e9f4ff",
              "#e7f5f9ff", "#cdecf1ff", "#b8e7edff", "#c7ebf1ff", "#d6f3f6ff", "#ffffffff"))
pal = colorRampPalette(mpal)
col_group <- pal(length(unique(datyr$ID)))
plot(1:length(col_group), col=col_group, pch=19)

  
#------------------------------------------
# Plot with fill
#pdf("FillOnly.pdf",width=20,height=9.25)
#png("FillOnly.png",width=10,height=4.6, units="in", res=300)

TracePlot(xvals=datyr$xval,
          yvals=datyr$MEDIAN+1,
          group=datyr$ID,
          overlap_scale=8,
          ytrans="sqrt",
          showraw=TRUE,
          mmcol=col_group,
          fill=TRUE)

dev.off()


#------------------------------------------
# Plot with lines only
#pdf("LinesOnly.pdf",width=20,height=9.25)
#png("LinesOnly.png",width=10,height=4.6, units="in", res=300)


TracePlot(xvals=datyr$xval,
          yvals=datyr$MEDIAN+1,
          group=datyr$ID,
          overlap_scale=8,
          ytrans="sqrt",
          showraw=TRUE,
          mmcol=NA,
          fill=FALSE)

dev.off()


