
# nday <- 30; npop <- 5000; tpvac <- 1.0
# isa.dpevent(nday = 93, npop = 44081, tpevent = 0.60, replace = TRUE)


# ==============================================================================

isa.dpevent <- function(nday, npop, tpevent, replace = FALSE){

  # nday: number of days
  # npop: number of units
  # tpevent: total proportion of units that experience the event of interest over the nday period
   
  if(replace == TRUE){
    rval <- tpevent / nday
  } 
  
  else
    if(replace == FALSE){
      epevent <- tpevent / nday
      
      solve <- function(nday, npop, epevent, tpevent){
        
        # Data frame to collect results:
        rval.df <- data.frame(nday = 1:nday, epevent = epevent, npop = NA, neventp = NA, neventn = NA)
        head(rval.df)
        
        # Day 1:
        rval.df$npop[1] <- npop
        rval.df$neventp[1] <- round(rval.df$epevent[1] * rval.df$npop[1], digits = 0)
        rval.df$neventn[1] <- round(rval.df$npop[1] - rval.df$neventp[1], digits = 0)
        head(rval.df)
        
        # Loop through each day and calculate number of farms vaccinated and unvaccinated:
        for(i in 2:nrow(rval.df)){
          rval.df$npop[i] <- rval.df$neventn[i - 1]
          
          rval.df$neventp[i] <- round(rval.df$epevent[i] * rval.df$npop[i], digits = 0)
          rval.df$neventn[i] <- round(rval.df$npop[i] - rval.df$neventp[i], digits = 0)
        }
        head(rval.df); tail(rval.df)
        
        # Total farms vaccinated:
        nevent <- sum(rval.df$neventp); nevent
        
        # Observed proportion of farms vaccinated over nday:
        pevent <- nevent / npop; pevent
        
        # Difference between pvac and target proportion vaccinated (tpvac):
        delta <- abs(pevent - tpevent)
        return(delta)
      }
      
      # Find the value of epvac (daily probability of vaccination) that satisfies tpvac (the total proportion of farms vaccinated over nday):
      rval <- optimize(f = solve, interval = c(0,1), nday = nday, npop = npop, tpevent = tpevent, maximum = FALSE)$minimum
    }
  
  return(rval)
}


# ==============================================================================
# Test:

# Create a series of population vaccination proportions:
# rval.df <- data.frame(replace = "TRUE", tpvac = seq(from = 0.10, to = 1.0, by = 0.10), dpvac = NA)
# rval.df$tpvac[10] <- 0.99
# 
# for(i in 1:nrow(rval.df)){
#   rval.df$dpvac[i] <- dpvac(nday = 30, npop = 5000, tpvac = rval.df$tpvac[i], replace = TRUE)
# }
# 
# rval.rtdf <- rval.df
# 
# 
# rval.df <- data.frame(replace = "FALSE", tpvac = seq(from = 0.10, to = 1.0, by = 0.10), dpvac = NA)
# rval.df$tpvac[10] <- 0.99
# 
# for(i in 1:nrow(rval.df)){
#   rval.df$dpvac[i] <- dpvac(nday = 30, npop = 5000, tpvac = rval.df$tpvac[i], replace = FALSE)
# }
# 
# rval.rfdf <- rval.df
# 
# rval.df <- rbind(rval.rtdf,rval.rfdf)
# 
# windows(); ggplot(data = rval.df, aes(x = tpvac, y = dpvac, color = replace)) +
#   theme_bw() +
#   geom_point() +
#   geom_line(linewidth = 0.5) +
#   scale_color_manual(values = c("red", "dark blue"), breaks = c("TRUE", "FALSE"), labels = c("TRUE", "FALSE"), name = "Replacement") +
#   scale_x_continuous(limits = c(0,1), name = "Proportion of population to be vaccinated") +
#   scale_y_continuous(limits = c(0,0.15), name = "Proportion of population vaccinated each day") +
#   theme(legend.position = c(0.25,0.85)) + 
#   theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 14), axis.text.y = element_text(size = 12), strip.text = element_text(size = 12))
  

# ==============================================================================
# Check:

# tpvac == 0.10
# dpvac <- rval.df$dpvac[1]
# 
# rval.df <- data.frame(nday = 1:nday, npop = NA, dpvac = dpvac, nvacp = NA, nvacn = NA)
# head(rval.df)
# 
# # Day 1:
# rval.df$npop[1] <- npop
# rval.df$nvacp[1] <- round(rval.df$dpvac[1] * rval.df$npop[1], digits = 0)
# rval.df$nvacn[1] <- round(rval.df$npop[1] - rval.df$nvacp[1], digits = 0)
# head(rval.df)
# 
# # Loop through each day and calculate number of farms vaccinated and unvaccinated:
# for(i in 2:nrow(rval.df)){
#   rval.df$npop[i] <- rval.df$nvacn[i - 1]
#   
#   rval.df$nvacp[i] <- round(rval.df$dpvac[i] * rval.df$npop[i], digits = 0)
#   rval.df$nvacn[i] <- round(rval.df$npop[i] - rval.df$nvacp[i], digits = 0)
# }
# head(rval.df); tail(rval.df)
# 
# # Total farms vaccinated:
# nvact <- sum(rval.df$nvacp); nvact
# 
# # Observed proportion of farms vaccinated over nday:
# pvact <- nvact / npop; pvact


# ==============================================================================
