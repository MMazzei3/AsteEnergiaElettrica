
ENERGETICO <- function(AllData, giorno, lambda){
  
  pippo <- as.Date(giorno)
  
  dt_actual <- subset(AllData, subset = AllData$BID_OFFER_DATE_DT == pippo)
  dt_pred <- subset(AllData, subset = AllData$BID_OFFER_DATE_DT < pippo) 
  
  dt_actual <- AllData[AllData$BID_OFFER_DATE_DT == pippo, ]
  
  df <- data.frame(DATA = sort(unique(dt_pred$BID_OFFER_DATE_DT), decreasing = T))
  
  Feste <- function(pippo){
    pippo <- as.Date(pippo)
    
    ANNO <- year(pippo)
    
    
    MESE <- sapply(pippo, 
                   function(x) switch(months(x), 
                                      gennaio = 1, febbraio = 2, marzo = 3, 
                                      aprile = 4, maggio = 5, giugno = 6, 
                                      luglio = 7, agosto = 8, settembre = 9, 
                                      ottobre = 10, novembre = 11, dicembre = 12))
    GIORNO <- day(pippo)
    
    GIORNOWEEK <- sapply(pippo, 
                         function(x) switch(weekdays(x), 
                                            lunedì = 1, martedì = 2, mercoledì = 3, 
                                            giovedì = 4, venerdì = 5, 
                                            sabato = 6, domenica = 7))
    WE <- ifelse(GIORNOWEEK >= 6, 1, 0)
    DOM <- ifelse(GIORNO == 7, 1, 0)
    CAPO <- ifelse(MESE == 1 & GIORNO == 1, 1, 0)
    EPIF <- ifelse(MESE == 1 & GIORNO == 6, 1, 0)
    CARNE <- ifelse(as.character(pippo) %in% c("2015-02-17", 
                                               "2016-02-09", 
                                               "2017-02-28", 
                                               "2018-02-13", 
                                               "2019-03-05", 
                                               "2020-02-25"), 
                    1, 0)
    PASQ <- ifelse(as.character(pippo) %in% c("2015-04-05", 
                                              "2016-02-27", 
                                              "2017-04-16", 
                                              "2018-04-01", 
                                              "2019-04-21", 
                                              "2020-04-12"), 
                   1, 0)
    PASQUET <- ifelse(as.character(pippo) %in% c("2015-04-06", 
                                                 "2016-02-28", 
                                                 "2017-04-17", 
                                                 "2018-04-02", 
                                                 "2019-04-22", 
                                                 "2020-04-13"), 
                      1, 0)
    LIB <- ifelse(MESE == 4 & GIORNO == 25, 1, 0)
    LAV <- ifelse(MESE == 5 & GIORNO == 1, 1, 0)
    REP <- ifelse(MESE == 6 & GIORNO == 2, 1, 0)
    FERAGO <- ifelse(MESE == 8 & GIORNO == 15, 1, 0)
    SANTI <- ifelse(MESE == 11 & GIORNO == 1, 1, 0)
    IMMA <- ifelse(MESE == 12 & GIORNO == 8, 1, 0)
    NATALE <- ifelse(MESE == 12 & GIORNO == 25, 1, 0)
    SANSTE <- ifelse(MESE == 12 & GIORNO == 26, 1, 0)
    
    
    data.frame(pippo, ANNO, MESE, 
               GIORNO, GIORNOWEEK,
               WE, DOM, CAPO, EPIF, CARNE, PASQ, PASQUET,
               LIB, LAV, REP, FERAGO, SANTI,
               IMMA, NATALE, SANSTE)
    
  }
  
  df <- Feste(df$DATA)
  df.prev <- Feste(pippo)
  
  df.checks <- switch(as.character(df.prev$GIORNOWEEK), 
                      "1" = subset(df, subset = df$GIORNOWEEK == 1 | 
                                     df$GIORNOWEEK == 5)[1:2, ], 
                      "2" = subset(df, subset = df$GIORNOWEEK < 2 | 
                                     df$GIORNOWEEK == 5)[1:2, ], 
                      "3" = subset(df, subset = df$GIORNOWEEK < 3)[1:2, ], 
                      "4" = subset(df, subset = df$GIORNOWEEK < 4)[1:2, ], 
                      "5" = subset(df, subset = df$GIORNOWEEK < 5)[1:2, ], 
                      "6" = subset(df, subset = df$GIORNOWEEK %in% c(5, 6))[1:2, ], 
                      "7" = subset(df, subset = df$WE == 1)[1:2, ])
  df.checks2 <- NULL
  if(df.prev$CAPO == 1){
    df.checks2 <- subset(df, subset = df$pippo=="2019-12-31")[1, ]
  }else if(df.prev$pippo == "2020-01-02"){
    df.checks2 <- subset(df, subset = df$pippo=="2019-12-30" | df$pippo=="2019-12-29" )[1:2, ]
    # # }else if(df.prev$CARNE == 1){
    # #   df.checks2 <- subset(df, subset = df$CARNE == 1)[1, ]
    # }else if(df.prev$PASQ == 1){
    #   df.checks2 <- subset(df, subset = df$WE == 1)[1:2, ]
  }else if (df.prev$PASQUET == 1){
    df.checks2 <- subset(df, df$PASQ == 1)[1, ]
  }
  # }else if(df.prev$LIB == 1){
  #   df.checks2 <- subset(df, df$PASQUET == 1 | df$PASQ == 1)[1:2, ]
  # }
  
  if(!is.null(df.checks2) > 0){
    df.checks <- df.checks2
  }
  
  dt_pred <- subset(dt_pred, subset = dt_pred$BID_OFFER_DATE_DT %in% df.checks$pippo)
  
  CurvaOfferta <- function(Prezzo, Quantita){
    data.frame(P = sort(Prezzo), Q = cumsum(Quantita[order(Prezzo)]))
  }
  
  x <- seq(0, min(max(dt_actual$ENERGY_PRICE_NO[dt_actual$ENERGY_PRICE_NO < 500]), 
                  max(dt_pred$ENERGY_PRICE_NO[dt_pred$ENERGY_PRICE_NO < 500])), by = lambda)
  ActualOffer <- CurvaOfferta(dt_actual$ENERGY_PRICE_NO, dt_actual$QUANTITY_NO)
  
  giorni <- df.checks$pippo 
  y_hat <- NULL
  for (i in 1:NROW(giorni)) {
    
    tmpOffer <- subset(dt_pred, subset = dt_pred$BID_OFFER_DATE_DT == as.character(giorni[i]))
    tmpOffer <- CurvaOfferta(tmpOffer$ENERGY_PRICE_NO, tmpOffer$QUANTITY_NO)
    f <- approxfun(tmpOffer$P, tmpOffer$Q, ties = "ordered")
    # points(x, f(x), type = "l", lty = 2, lwd = 0.5, col = "gray70")
    y_hat <- rbind(y_hat, data.frame(x = x, y = f(x), Giorno = as.character(giorni[i])))
  }
  
  
  if(NROW(giorni) > 1){
    yHat <- aggregate(y_hat$y, by = list(x = y_hat$x), median)
    colnames(yHat) <- c("x", "y")
    for(i in 2:(NROW(yHat))){
      if(yHat$y[i] < yHat$y[i - 1]){
        yHat$y[i] <- yHat$y[i - 1]
      }
    }
  }else{
    yHat <- data.frame(x = y_hat$x, y = y_hat$y)
    colnames(yHat) <- c("x", "y")
  }
  
  # points(Q ~ P, ActualOffer, type = "l", col = "red", lwd = 2)
  # points(y ~ x, data = yHat, type = "l", col = "blue", lwd = 2)
  
  f.Real <- approxfun(ActualOffer$P, ActualOffer$Q, ties = "ordered")
  y <- f.Real(x)
  MAE <- mean(abs(y - yHat$y))
  MAE200 <- mean(abs(y[1:(NROW(x)/2)] - yHat$y[1:(NROW(x)/2)]))
  MAPE <- mean(abs((y - yHat$y)/y))
  MAPE200 <- mean(abs((y[1:(NROW(x)/2)] - yHat$y[1:(NROW(x)/2)])/y[1:(NROW(x)/2)]))
  list(GiorniSel = giorni,
       MAE = data.frame(MAE = MAE, MAE200 = MAE200), 
       MAPE = data.frame(MAPE = MAPE, MAPE200 = MAPE200), 
       RealCurve = data.frame(Price = x, Quantity = y), 
       PredCurve = data.frame(Price = x, Quantity = yHat$y), 
       Splines = y_hat, 
       FakePrezzi = x)
  
}

ENERGY <- function(AllData, AnnoMeseGiorno, DaysBefore = 10){
  
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(forecast)
  
  giorno <- as.Date(AnnoMeseGiorno)
  giorno_num <- as.numeric(giorno)
  
  AllData %>% 
    filter(BID_OFFER_DATE_DT == giorno) -> dt_actual
  
  dt_actual <- dt_actual[order(dt_actual$ENERGY_PRICE_NO, decreasing = F), ] 
  
  f <- approxfun(x = dt_actual$ENERGY_PRICE_NO, 
                 y = cumsum(dt_actual$QUANTITY_NO),
                 ties = "ordered")
  
  priceFake <- seq(0,500,by = 2)
  yFake <- f(priceFake)
  dA <- data.frame(priceFake,yFake)
  
  tot <- DaysBefore
  giorni <- sort(seq(giorno-1, giorno-tot,-1))
  
  dF <- NULL
  for (g in giorni) {
    AllData %>% 
      filter(BID_OFFER_DATE_DT == g) -> dt_actual
    
    dt_actual <- dt_actual[order(dt_actual$ENERGY_PRICE_NO, decreasing = F), ] 
    
    f <- approxfun(x = dt_actual$ENERGY_PRICE_NO, 
                   y = cumsum(dt_actual$QUANTITY_NO),
                   #y = (dt_actual$QUANTITY_NO),
                   ties = "ordered")
    yFake <- f(priceFake)
    dtemp <- cbind.data.frame(priceFake,yFake)
    dF <- rbind.data.frame(dF,dtemp)
    
  }
  
  pred <- vector(length = NROW(priceFake))
  begin <- date()
  for(i in 1:NROW(priceFake)){
    #cat(i, "su", NROW(priceFake), "\n")
    dF %>% filter(priceFake == priceFake[i]) -> tmp
    ds <- ts(tmp[, 2],start = 1, end=tot, frequency = NROW(priceFake))
    fit_LOESS <- stlf(ds, method = "ets", h = 1,
                      robust = TRUE,t.window = 1,
                      biasadj = TRUE)
    
    sink("nul")
    s <- summary(fit_LOESS) 
    sink()
    pred[i] <- s$`Point Forecast`
  }
  end <- date()
  
  pred2 <- pred
  for(i in 2:(NROW(pred))){
    if(pred2[i] < pred2[i - 1]){
      pred2[i] <- pred2[i - 1]
    }
  }
  
  # plot(dA,type="l",col="black")
  # points(priceFake, pred2, type = "l", col = "red")
  
  MAE <- mean(abs(dA$yFake - pred2))
  MAE200 <- mean(abs(dA$yFake[1:(NROW(priceFake)/2)] - pred2[1:(NROW(priceFake)/2)]))
  MAPE <- mean(abs((dA$yFake - pred2)/dA$yFake))
  MAPE200 <- mean(abs((dA$yFake[1:(NROW(priceFake)/2)] - pred2[1:(NROW(priceFake)/2)])/dA$yFake[1:(NROW(priceFake)/2)]))
  time <- rbind(begin, end)
  colnames(time) <- "Time"
  list(MAE = data.frame(MAE = MAE, MAE200 = MAE200), 
       MAPE = data.frame(MAPE = MAPE, MAPE200 = MAPE200), 
       DaysBefore = tot,
       WeekDay = weekdays(giorno),
       Time = time, 
       RealCurve = data.frame(Price = priceFake, Quantity = dA$yFake), 
       PredCurve = data.frame(Price = priceFake, Quantity = pred2))
  
}

ENERGY.HELLO <- function(AllData, AnnoMeseGiorno){
  
  giorno <- as.Date(AnnoMeseGiorno)
  giorno_num <- as.numeric(giorno)
  AllData %>% 
    filter(BID_OFFER_DATE_DT == giorno) -> dt_actual
  
  dt_actual <- dt_actual[order(dt_actual$ENERGY_PRICE_NO, decreasing = F), ] 
  
  f <- approxfun(x = dt_actual$ENERGY_PRICE_NO, 
                 y = cumsum(dt_actual$QUANTITY_NO),
                 ties = "ordered")
  
  priceFake <- seq(0,500,by = 2)
  yFake <- f(priceFake)
  dA <- data.frame(priceFake,yFake)
  
  
  giorni <- as.Date("2017-01-01"):(giorno - 1)#sort(seq(giorno-1, giorno-tot,-1))
  
  dF <- NULL
  for (g in 1:NROW(giorni)) {
    
    AllData %>% 
      filter(BID_OFFER_DATE_DT == giorni[g]) -> tmp
    
    tmp <- tmp[order(tmp$ENERGY_PRICE_NO, decreasing = F), ] 
    
    f <- approxfun(x = tmp$ENERGY_PRICE_NO, 
                   y = cumsum(tmp$QUANTITY_NO),
                   ties = "ordered")
    yFake <- f(priceFake)
    dtemp <- data.frame(priceFake, yFake)
    dF <- rbind.data.frame(dF,dtemp)
    
  }
  
  pred <- vector(length = NROW(priceFake))
  begin <- date()
  for(i in 1:NROW(priceFake)){
    
    #cat(i, "su", NROW(priceFake), "\n")
    
    dF %>% filter(priceFake == priceFake[i]) -> tmp
    ds <- ts(tmp[, 2], start = c(2017), frequency = 365)
    
    fit_LOESS <- stlf(ds, method = "ets", h = 1, robust = TRUE, 
                      t.window = 1, s.window = "periodic")
    
    sink("nul")
    s <- summary(fit_LOESS) 
    sink()
    pred[i] <- s$`Point Forecast`
  }
  end <- date()
  
  pred2 <- pred
  for(i in 2:(NROW(pred))){
    if(pred2[i] < pred2[i - 1]){
      pred2[i] <- pred2[i - 1]
    }
  }
  
  # plot(dA,type="l",col="black")
  # points(priceFake, pred2, type = "l", col = "red")
  
  MAE <- mean(abs(dA$yFake - pred2))
  MAE200 <- mean(abs(dA$yFake[1:(NROW(priceFake)/2)] - pred2[1:(NROW(priceFake)/2)]))
  MAPE <- mean(abs((dA$yFake - pred2)/dA$yFake))
  MAPE200 <- mean(abs((dA$yFake[1:(NROW(priceFake)/2)] - pred2[1:(NROW(priceFake)/2)])/dA$yFake[1:(NROW(priceFake)/2)]))
  time <- rbind(begin, end)
  colnames(time) <- "Time"
  list(MAE = data.frame(MAE = MAE, MAE200 = MAE200), 
       MAPE = data.frame(MAPE = MAPE, MAPE200 = MAPE200), 
       WeekDay = weekdays(giorno),
       Time = time, 
       RealCurve = data.frame(Price = priceFake, Quantity = dA$yFake), 
       PredCurve = data.frame(Price = priceFake, Quantity = pred2))
  
}

