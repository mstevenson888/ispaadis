isa.epicurve <- function(dat, model, stats = FALSE, type = "cumulative", quant = c(0.25, 0.75)){
    # ISP input: itno, day, premises identifier.
    # AADIS input: itno, day, number of events.

  require(tidyr); require(dplyr)
  
  if(model == "aadis"){
    dat.df <- dat %>%
      group_by(run,day.infected)
    dat.df <- data.frame(summarise(dat.df, n = n()))
    
    dat.df <- dat.df %>%
      complete(run, day.infected = full_seq(x = day.infected, period = 1)) %>%
      replace_na(list(n = 0))
    dat.df <- data.frame(dat.df)
    names(dat.df) <- c("itno","day","n")
  }

    # Re-format ISP data to return the number of events per day (instead of one line for each event):
  else
  if(model == "isp"){
      names(dat) <- c("itno","day","src","itype","xcoord","ycoord")
      dat.df <- dat %>%
        group_by(itno,day)
      dat.df <- data.frame(summarise(dat.df, n = n()))
      
      dat.df <- dat.df %>%
        complete(itno, day = full_seq(x = day, period = 1)) %>%
        replace_na(list(n = 0))
      
      dat.df <- data.frame(dat.df)
    }
  
  nitno <- max(dat.df$itno)  
  day.seq <- seq(min(dat.df$day), max(dat.df$day), by = 1)
  
  q01.fun <- function(dat) {
    quantile(dat, probs = quant[1])
  }
  
  q02.fun <- function(dat) {
    quantile(dat, probs = quant[2])
  }
  
  if (stats == TRUE) {
    out <- matrix(rep(NA, times = length(day.seq) * nitno), ncol = nitno)
    
    for (i in 1:nitno) {
      id <- dat.df$itno == i
      tdat.df <- data.frame(dat.df[id,])
      
      for (j in 1:length(day.seq)) {
        if (type == "cumulative"){
          out[j,i] <- sum(tdat.df$n[tdat.df$day <= day.seq[j]])
          # n <- cumsum(tmp.01$evt)
        }
        
        if (type == "frequency") {
          out[j,i] <- ifelse(length(tdat.df$n[tdat.df$day == day.seq[j]]) == 0, 0, tdat.df$n[tdat.df$day == day.seq[j]])
        }
        
        if (type == "cumulative") {
          out <- out
        }
        
      }
    }
    
    rmean <- apply(out, MARGIN = 1, FUN = mean)
    rsd <- apply(out, MARGIN = 1, FUN = sd)
    rmed <- apply(out, MARGIN = 1, FUN = median)
    rq01 <- apply(out, MARGIN = 1, FUN = q01.fun)
    rq02 <- apply(out, MARGIN = 1, FUN = q02.fun)
    rmin <- apply(out, MARGIN = 1, FUN = min)
    rmax <- apply(out, MARGIN = 1, FUN = max)
    
    q01.n <- paste("pr.", quant[1] * 100, sep = "")
    q02.n <- paste("pr.", quant[2] * 100, sep = "")
    rval <- data.frame(day.seq, rmean, rsd, rmed, rq01, rq02, rmin, rmax)
    names(rval) <- c("day", "pr.mean", "pr.sd", "pr.median", q01.n, q02.n, "pr.min", "pr.max")
  }
  
  else if (stats == FALSE) {
    out <- matrix(rep(NA, times = length(day.seq) * (nitno + 1)), ncol = nitno + 1)
    out[,1] <- day.seq
    
    for (i in 1:nitno) {
      id <- dat.df$itno == i
      tdat.df <- data.frame(dat.df[id,])
      
      for (j in 1:length(day.seq)) {
        if (type == "cumulative"){
          out[j,i + 1] <- sum(tdat.df$n[tdat.df$day <= day.seq[j]])
          # n <- cumsum(tmp.01$evt)
        }
        
        if (type == "frequency") {
          out[j,i] <- tdat.df$n[tdat.df$day == day.seq[j]]
        }
        
        if (type == "cumulative") {
          out <- out
        }
      }
    }
    
    # Fix up the dimension names of rval:
    .itno <- 1:n.iterations
    .cname <- as.character(1:n.iterations)
    .cname[.itno < 10] <- paste("i000", .cname[.itno < 10], sep = "")
    .cname[.itno >= 10 & .itno < 100] <- paste("i00", .cname[.itno >= 10 & .itno < 100], sep = "")
    .cname[.itno >= 100 & .itno < 1000] <- paste("i0", .cname[.itno >= 100 & .itno < 1000], sep = "")
    .cname[.itno >= 1000] <- paste("i", .cname[.itno >= 1000], sep = "")
    colnames(out) <- c("day", .cname)
    rval <- out
  }
  
  return(rval)
}