##A1
  df <- read.csv("herpetozoa-033-095-s001.csv")
  good <- complete.cases(df)
  cdf <- df[good, ]
  
##A2
  class(cdf)
  str(cdf)
  
##A3
  elev <- cdf[, 5]
  mean(elev)
  sd(elev)
  range(elev)
  
##A4
  species <- cdf[, 7:33]
  loop <- 0
  for (i in seq_len(ncol(species))){
    loop[i] <- (sum(species[i]))
  }
  table <- matrix(loop, nrow = 27, ncol = 1)
  dimnames(table) <- list(names(species), "Observation sums")
  table
  
  cloop <- sapply(species, sum)
  cloop
  max(cloop)
  min(cloop)
  which(cloop == max(cloop))
  which(cloop == min(cloop))
  
##A5
  df03 <- cdf[c("Htypes", "Phlo", "Babu", "Geat", "Hety", "Bosc", "Drru", "Apbo")]
  ASG <- df03[df03$Htypes == "ASG", -1]
  CVT <- df03[df03$Htypes == "CVT", -1]
  ESG <- df03[df03$Htypes == "ESG", -1]
  OGF <- df03[df03$Htypes == "OGF", -1]
  ASG_mean <- sapply(ASG, mean)
  CVT_mean <- sapply(CVT, mean)
  ESG_mean <- sapply(ESG, mean)
  OGF_mean <- sapply(OGF, mean)
  table_mean <- cbind(ASG_mean, CVT_mean, ESG_mean, OGF_mean)
  table_mean
  ASG_sum <- sapply(ASG, sum)
  CVT_sum <- sapply(CVT, sum)
  ESG_sum <- sapply(ESG, sum)
  OGF_sum <- sapply(OGF, sum)
  table_sum <- cbind(ASG_sum, CVT_sum, ESG_sum, OGF_sum)
  table_sum
  
##B1
  df032<- cdf[, 4:6]
  df032
  df032_col <- c(ASG = "red", CVT = "green", ESG = "purple", OGF = "orange")
  plot(
    df032$Elev,
    df032$SPPTotal, 
    col = df032_col[df032$Htypes],
    pch = 19, 
    main = "Total Species Count per Elevation Level",
    xlab = "Elevation level", 
    ylab = "Total Species Count"
  )
  
##B2
  ASG32 <- sum(df032[df032$Htypes == "ASG", -(1:2)])
  CVT32 <- sum(df032[df032$Htypes == "CVT", -(1:2)])
  ESG32 <- sum(df032[df032$Htypes == "ESG", -(1:2)])
  OGF32 <- sum(df032[df032$Htypes == "OGF", -(1:2)])
  bardf <- c(ASG32, CVT32, ESG32, OGF32)
  bardf
  barplot(
    bardf, 
    names.arg = c("ASG", "CVT", "ESG", "OGF"),
    main = "Total Species Count per Habitat", 
    xlab = "Habitat Types",
    ylab = "Total Species Count", 
    col = df032_col
  )
  
##B3
  hist(
    df032$SPPTotal, 
    main = "Total Species Count",
    xlab = "Total Species Count",
    ylab = "Frequency",
    col = "pink"
  )
  
##C1
##Based on the summary statistics, the elevation range is 60 to 1020 meters. 
##The mean of the elevation is about 379 meters. Based on the scatter plot, lower elevation level has a higher species count. 
##The bar plot shows that habitat ASG has the highest total species count, while CVT has the lowest.   
  