library(xgboost)
library(scatterplot3d)

BSMPrice<-function(PutCallFlag,S,T,K,r,sigma){
  if(PutCallFlag=="Call"){
    d1<-(log(S/K)+(r+sigma^2/2)*T) / (sigma*sqrt(T))
    d2<-d1 - sigma * sqrt(T)
    return(S*pnorm(d1, mean=0, sd=1) - K*exp(-r*T)*pnorm(d2, mean=0, sd=1))
  }
  if(PutCallFlag=="Put") return(-BSMPrice("Call",S,T,K,r,-1*sigma))
}

# Load data
load("EurexOptionsDaxPlus.RData")
Handelstage <- sort(unique(EurexOptionsDax$Handelstag))

### Select a Handelstag (day)
i <- 113

### Get data
MarketData <- subset(EurexOptionsDax, OptionType=='Call' & Handelstag==Handelstage[i] & Moneyness>=0.8 & Moneyness<=1.2 & t_delta>=1/12 & t_delta<=1)
### XGBoost
set.seed(0)
xgb_data <- xgb.DMatrix(data=as.matrix(MarketData[, c("Moneyness", "t_delta")]), label=as.matrix(MarketData$ImpliedVola))
xgboost_fit <- xgb.train(data=xgb_data, objective="reg:squarederror", nrounds=1000, max.depth=100, eta=0.2)

MarketData$ModelVola <- predict(xgboost_fit, xgb_data)

### Create Plots
  S <- MarketData$SchlusspreisBasiswert[1]
  t_delta <- seq(1/52, 1, length.out=29)
  Moneyness <- seq(1.2, 0.8,length.out=31)
  StrikePrices <- S/Moneyness
  
  IV <- Moneyness %o% t_delta
  for(n in 1:length(Moneyness)){
    for(m in 1:length(t_delta)){
      IV[n, m] <- predict(object=xgboost_fit, newdata=as.matrix(data.frame(Moneyness=Moneyness[n], t_delta=t_delta[m])))
    }
  }
  
  ### Calc model prices
  for(j in 1:nrow(MarketData)){
    MarketData$ModelPrice[j] <- BSMPrice(PutCallFlag=MarketData$OptionType[j], S=MarketData$SchlusspreisBasiswert[j], T=MarketData$t_delta[j], K=MarketData$StrikePrice[j], r=log(1+MarketData$EONIA[j]), sigma=MarketData$ModelVola[j])
  }
  MarketData$deltaPrices <-  MarketData$ModelPrice - MarketData$TaeglicherAbrechnungspreis
  
  layout(matrix(c(1,1,1,1,1,2,2,2,2),1))
  ### Create Surface Plot
  p <- persp(StrikePrices, t_delta, IV, theta=30, phi=30, col="lightgrey", expand=0.5, shade=0.2, xlab="Strike", ylab="Maturity", zlab="Volatility", ticktype="detailed")
  obs <- trans3d(MarketData$StrikePrice, MarketData$t_delta, MarketData$ImpliedVola, p)
  pred <- trans3d(MarketData$StrikePrice, MarketData$t_delta, MarketData$ModelVola, p)
  
  points(obs, col="red", pch=16)
  segments(obs$x, obs$y, pred$x, pred$y)

  ### Plot deltas prices
  scatterplot3d(MarketData$StrikePrice, MarketData$t_delta, MarketData$deltaPrices, pch=16, highlight.3d=TRUE, angle=45, type="h", box=FALSE, main=NULL, xlab="Strike", ylab="t", zlab="Delta Model- vs Observed Price")
  
  ### Add title and subtitle    
  mtext(paste("Handelstag: ", MarketData$Handelstag[i], sep=""), side=3, line=-2, outer=TRUE, cex=1)
