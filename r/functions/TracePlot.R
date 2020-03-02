# Build trace plot

TracePlot <- function(xvals=NA,
                      yvals=NA,
                      group=NA,
                      overlap_scale=5,
                      ytrans="sqrt",
                      showraw=FALSE,
                      mmcol=NA,
                      fill=FALSE
){
  
  
  
  
  # Make sure group is categorical
  group=as.character(group)
  yvals=as.numeric(yvals)
  
  # Build temp df
  tdf <- data.frame(group=group,xvals=xvals, yvals=yvals)
  
  # Scale y values globally
  if(ytrans == "sqrt"){
    tdf$yvals <- sqrt(tdf$yvals+1)
  }
  
  if(ytrans == "log"){
    tdf$yvals <- log(tdf$yvals)
  }
  
  # Scale all values within each group
  scale_y <- function(x){
    (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  }
  
  # Scale and adjust for min to be zero
  tdf2 <- tdf %>% group_by(group) %>%
    mutate(scale_y = scale_y(yvals)) %>% # Scale
    mutate(min_y = min(scale_y, na.rm=TRUE)) %>% # Find min for group
    mutate(min_y_adj = ifelse(min_y>0, 0, abs(min_y))) %>% # Find min for group
    mutate(adj_y = min_y_adj + scale_y) # Correct values by min
  
  # QA plot
  if(FALSE){
    ggplot(tdf2, aes(x = xvals, y = adj_y)) + 
      geom_line(aes(color = group)) +
      theme_minimal() + theme(legend.position = "none")
    
    ggplot(tdf2, aes(x = xvals, y = adj_y)) + 
      geom_smooth(aes(color = group), se = FALSE) +
      theme_minimal() + theme(legend.position = "none")
  }
  
  # Run stats snooth to get averaged value
  
  # Set grouping overlap factor.
  # Usually a multiple of the number of groups.
  # models <- tdf2 %>%
  #   tidyr::nest(-group) %>%
  #   dplyr::mutate(
  #     # Perform loess calculation on each CpG group
  #     m = purrr::map(data, loess,
  #                    formula = adj_y ~ xvals, span = .5),
  #     # Retrieve the fitted values from each model
  #     fitted = purrr::map(m, `[[`, "fitted")
  #   )
  # 
  # # Apply fitted y's as a new column
  # results <- models %>%
  #   dplyr::select(-m) %>%
  #   tidyr::unnest()
  # 
  # # Fix zero
  # results$fitted <- ifelse(results$fitted<0, 0, results$fitted)
  # 
  
  
  results <- tdf2
  
  # Overwrite with raw
  if(showraw){
    results$fitted <- results$adj_y
  }
  
  
  
  ## Plot with loess line for each group
  #ggplot(results, aes(x = xvals, y = adj_y, group = group, colour = group)) +
  #  geom_line(aes(y = fitted))+ theme(legend.position = "none")
  
  
  
  #==========================================================
  
  
  
  # Start to build plot --- 
  # How tall should it be - number of groups
  ngroups <- results$group %>% unique() %>% length()
  # Get the group height scale
  scale_height <- max(results$fitted, na.rm=TRUE)
  # from argument set overlap scale
  y_scale_adj <- overlap_scale/scale_height
  
  # Fix y value
  results$ystep1 <- results$fitted*y_scale_adj
  
  # ordger groups (or not)
  results <- results %>% 
    group_by(group) %>%
    mutate(maxy = max(ystep1))
  results$maxx2 <- ifelse(results$ystep1 == results$maxy, results$xvals, -9999)
  results <- results %>% 
    group_by(group) %>%
    mutate(maxx = max(maxx2))
  
  
  
  #========================================================
  # start plot
  # from top to bottom
  
  par(mar = c(1,1,2,1))
  
  plot(1, type="n", xlab="", ylab="", xlim=c(0, max(results$xvals)), ylim=c(0, ngroups+scale_height),
       axes=F)
  
  
  
  # loop through sort order to add lines
  plotting_o <- unique(paste0(results$maxx, "_", results$group))
  plotting_o <- sort(plotting_o)
  
  
  
  if(all(!(is.na(mmcol)))){
    print("Custom Col")
    print(mmcol[1:5])
    line_col <- mmcol
    if(fill){
      fill_col <- mmcol
    } else {
      fill_col <- "white"
    }
    
  } else {
    line_col <- "black"
    fill_col <- "white"
    
  }
  
  
  for(i in 1:length(plotting_o)){
    
    this_s <- strsplit(plotting_o[i], "_")
    this_g <- this_s[[1]][2]
    
    this_dat <- results[which(results$group == this_g),]
    
    adj_y <- (this_dat$ystep1+ngroups-i)
    adj_x <- this_dat$xvals
    
    lines(adj_x, adj_y, col=NA)
    
    
    m <- length(adj_x)                         
    x.poly <- c(adj_x, adj_x[m], adj_x[1])         # Adjoin two x-coordinates
    y.poly <- c(adj_y, -999, -999)                     # .. and the corresponding y-coordinates
    
    mfillcol <- adjustcolor(fill_col[i])
    if(is.na(fill_col[i])){
      mfillcol <- "white"
    }
    
    polygon(x.poly, y.poly, col="white", border=NA)          # Show the polygon fill only
    polygon(x.poly, y.poly, col=mfillcol, border=NA)          # Show the polygon fill only
    
    
    
    mfillcol <- adjustcolor(line_col[i])
    if(is.na(line_col[i])){
      mfillcol <- "black"
    }
    
    lines(adj_x, adj_y, col=mfillcol, lwd=1.5)
    
    
    
  }
  
  
  
  
}