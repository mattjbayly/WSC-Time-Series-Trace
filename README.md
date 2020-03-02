# WSC-Time-Series-Trace
Artistic trace plots for BC Water Survey Canada Station data.

Stream discharge time series are ordered by regional flow regimes. Lower darker lines are rain-dominated stations (similar to those near the coast/VI), higher lighter lines are the interior/higher elevation snow-dominated systems with a large spring freshet.

This repository contains a draft TracePlot function (r/functions/TracePlot.R) to generate artistic displays of overlapping time series. The function takes in a time series and scales and orders them for plotting. An overlap scale and other features allow for some basic style manipulation. Wrote this in a hurry and the function needs some cleanup. I also want to rewrite for ggplot2. See examples below.

```r
# Load Libraries
library(dplyr); library(data.table); library(ggplot2); library(tidyr); library(purrr); library(RColorBrewer); library(grDevices)

# Load custom trace plot function
source("./r/functions/TracePlot.R")

# Load WSC station data - daily averages
dat <- fread(paste0(base_dir, "/data/daily.summary.data.csv"))

# Filter out records with less than 10 years of data
datyr <- dat %>% group_by(ID) %>% mutate(nyr = median(YRCOUNT, na.rm=TRUE)) %>% filter(nyr > 100)

# Set date for x-axis - dummy yr and time
datyr$xval <- as.POSIXct(paste0("2000-", datyr$DATE, " 00:00:00"), format="%Y-%m-%d %H:%M:%S")
# Need to set to continuious number for plot
datyr$xval <- as.numeric(as.character(format(datyr$xval, "%j")))

# Build Color Palette for Howe Sound
mpal <- rev(c("#4d6a88ff", "#2b7ae0ff", "#5daae5ff", "#5eade4ff", "#b6e9f4ff",
              "#e7f5f9ff", "#cdecf1ff", "#b8e7edff", "#c7ebf1ff", "#d6f3f6ff", "#ffffffff"))
pal = colorRampPalette(mpal); col_group <- pal(length(unique(datyr$ID)))
#------------------------------------------
# Plot with fill
TracePlot(xvals=datyr$xval,
          yvals=datyr$MEDIAN+1,
          group=datyr$ID,
          overlap_scale=8,
          ytrans="sqrt",
          showraw=TRUE,
          mmcol=col_group,
          fill=TRUE)
```
![FillOverlap](FillOnly.png?raw=true "Fill Overlap")

```r
#------------------------------------------
# Plot with Lines
TracePlot(xvals=datyr$xval,
          yvals=datyr$MEDIAN+1,
          group=datyr$ID,
          overlap_scale=8,
          ytrans="sqrt",
          showraw=TRUE,
          mmcol=NA,
          fill=FALSE)
```
![LineOverlap](LinesOnly.png?raw=true "Line Overlap")
