require(reshape2)
require(ggplot2)
require(zoo)
require(data.table)
require(lubridate)
require(xts)

setwd("C:/Users/gargn/Documents/Finance_in_R_Jupyter _Notebook/InterestRateModelling_usingPCA")

libor <- fread("LIBOR USD.csv", sep=",")
head(libor,20)
libor[,c(1,5,7,8)][,Date:=dmy(Date)][,yearmonth:=year(Date)*1e2+month(Date)][,c(2:4):=lapply(.SD, mean), .SDcols=c(2:4),by=yearmonth] -> libor
libor[,c(5,2:4)][order(yearmonth)] -> libor
unique(libor) -> libor
libor[yearmonth>=200007 & yearmonth<=201609] -> libor
colnames(libor)[2:4] <- paste0("libor_",c(1,3,6),"m")
as.zoo(libor[,c(2:4)], as.yearmon(as.character(libor$yearmonth), format="%Y%m")) -> libor

swap <- fread("FRB_H15.csv", sep=",")
swap <- swap[6:nrow(swap)]
colnames(swap) <- c("date",paste0("swap_",as.character(c(1:5,7,10,30)),"yr"))
swap[,c(2:9):=lapply(.SD, FUN = function(x){as.double(x)}), .SDcols=c(2:9)]
swap[,date:=as.yearmon(date,format="%Y-%m")]
swap[complete.cases(swap)] -> swap
swap <- as.zoo(swap[,c(2:9)], swap$date)

rates<- merge(libor,swap,all=T)
tsRainbow = rainbow(ncol(rates))
plot(x=rates,col=tsRainbow, main="Historical Interest Rates", ylab="Rates", xlab="Time", plot.type="single", xaxt ="n", yaxt="n")
axis(1)
pts<- pretty(rates)
axis(2, at=pts, labels=paste(pts, "%", sep=""))
legend(x="topright", legend=colnames(rates), lty=1, col=tsRainbow, cex=.64,bty="n")

avail_tenors <- function(header) {
  txtStart = regexpr("_",header)
  txtEnd = regexpr("yr",header)
  
  tenors <- as.integer(substr(header,txtStart+1,txtEnd-1))
  result = data.table(col = seq(1,length(header)), tenor = tenors)
  return(result)
}

at <- avail_tenors(colnames(swap))

highlow <- function(yr) {
   
  if(yr %in% at$tenor) {
    return (NULL)
  }
  else {
    for(i in 1:nrow(at)) {
      if(at$tenor[i] > yr) return (at[(i-1):i,])
    }
  }
}
highlow(1.5)

interpolateSwaps <- function(swap,yr) {
  hl <- highlow(yr)
  
  if(is.null(hl)) {
    return (NULL)
  }
  
  else{
    return ((swap[,hl$col[1]]*(hl$tenor[2]-yr) + swap[,hl$col[2]]*(yr-hl$tenor[1]))/(hl$tenor[2] - hl$tenor[1]))
  }
}

interpolateSwaps(swap,1.5)
interpolateSwaps(swap,10)

interpolateSwapsperiods <- function(yrLow,yrHigh,freq,swap) {
  result <- swap
  for (i in seq(from = yrLow, to = yrHigh, by=freq)) {
    if(i %in% at$tenor) {
      
    }
    else {
      result$newcol <- interpolateSwaps(result,i)
      colnames(result)[length(colnames(result))] <- paste0("swap_",i,"yr") 
    }
  }
  return (result)
}

swap <- interpolateSwapsperiods(1,30,0.5,swap)

at <- avail_tenors(colnames(swap))
at <- at[order(tenor)]
swap[,at$col] -> swap

rates<- merge(libor, swap, all=FALSE)
tsRainbow <- rainbow(ncol(rates))
plot(x=rates,col=tsRainbow, main="Historical Interest Rates", ylab="Rates", xlab="Time", plot.type="single", xaxt ="n", yaxt="n")
axis(1)
pts<- pretty(rates)
axis(2, at=pts, labels=paste(pts, "%", sep=""))
legend(x="topright", legend=colnames(rates), lty=1, col=tsRainbow, cex=.64,bty="n")

bootstrap<-function(freq,libor, swap){
  at <- avail_tenors(colnames(swap))
  max <- at$tenor[NCOL(swap)]
  result <- libor[,match("libor_6m", colnames(libor))]/100
  df <- (1+result[,NCOL(result)])^-.5
  for(i in seq(from=1, to=max, by=freq)){
    txt <- paste0("swap_", i, "yr")
    coupon <- swap[,match(txt, colnames(swap))]/100/2
    newcol <- ((1+coupon)/(1-annuityDF(df)*coupon))^(1/i)-1
    result <- cbind(result,newcol)
    txt <- paste0("spot_", i, "yr")
    colnames(result)[NCOL(result)] <- txt
    txt <- paste0("df_", i, "yr")
    newcol <- (1+result[,NCOL(result)])^-i
    df <- cbind(df,newcol)
    colnames(df)[NCOL(df)] <- txt
  }
  result <- na.omit(result)
  return(result)
}

annuityDF<-function(df)
{
  max = NCOL(df)
  for(i in 1:max){
    if(i==1){
      result <-  df[,1]
    }else{
      result <- result + df[,i]
    }
  }
  return(result)
}


spot <- bootstrap(.5, libor, swap)
colnames(spot)[1] <- "spot_.5yr"

libor <- libor[,c(1:2)]/100
spot <- merge(libor, spot)
spot <- na.omit(spot)
write.zoo(spot, file="spot.csv")


spotPctDiff <- spot/lag(spot,1) - 1
spotPctDiff <- unclass(spotPctDiff)[2:length(spotPctDiff[,1]),1:length(colnames(spotPctDiff))]
scaledSpotPctDiff <- scale(spotPctDiff)
pca <- eigen(cor(scaledSpotPctDiff))
write(pca$values, file="eigenvalues.csv", sep=",", ncolumns=1)
write(t(pca$vectors), file="eigenvectors.csv", sep=",", ncolumns=length(colnames(spot)))
vol.df <- data.frame(attr(scaledSpotPctDiff, "scaled:scale"))
colnames(vol.df)<-c("vol")
write(vol.df$vol, file="vol.csv", sep=",", ncolumns=1)

plot(pca$values/length(pca$values), main = "Percentage of Variance explained by various Principal Components", ylab="Prop.Variance explained", xlab = "Components")

pca_long <- data.table(index = 1:62, coredata(pca$vectors[,1:3]))
colnames(pca_long)[2:4] <- c("PCA_1","PCA_2","PCA_3")
melt(pca_long, id="index", variable.name = "Component") -> pca_long

ggplot(pca_long, aes(x=index)) +
  geom_line(aes(y=value, col=Component), size=1.25) +
  labs(title = "Principal Component Analysis from 1m LIBOR to 30Yr Swap", x = "Term", y = "Value")

eigenGoodForm <- NULL
stdevRateGoodForm <- NULL
lastRateGoodForm <- NULL
curvePoints <- ncol(scaledSpotPctDiff)
for(i in 1:curvePoints)
{
  eigenGoodForm <- c(eigenGoodForm, c(rep(pca$values[i], curvePoints)))  
  stdevRateGoodForm <- c(stdevRateGoodForm, c(attr(scaledSpotPctDiff, "scaled:scale")))
  lastRateGoodForm <- c(lastRateGoodForm, c((unclass(last(spot))[1,])))
}
# Develop the shocked pc's and the rateShocks
pcaShockUp <- pca$vectors * eigenGoodForm^.5 * qnorm(.995)
rateShockUp <- (1+pcaShockUp*stdevRateGoodForm *sqrt(12))* lastRateGoodForm

pcaShockUp <- subset(data.frame(pcaShockUp), select=-c(4:length(colnames(data.frame(pcaShockUp)))))
colnames(pcaShockUp) <- c("PCA_1_Up", "PCA_2_Up", "PCA_3_Up")
rateShockUp <-  subset(data.frame(rateShockUp), select=-c(4:length(colnames(data.frame(rateShockUp)))))
colnames(rateShockUp) <- c("rate_PCA_1_Up", "rate_PCA_2_Up", "rate_PCA_3_Up")

pcaShockDown <- pca$vectors * eigenGoodForm^.5 * -qnorm(.995)
rateShockDown <- (1+pcaShockDown*stdevRateGoodForm * sqrt(12))* lastRateGoodForm 

pcaShockDown <- subset(data.frame(pcaShockDown), select=-c(4:length(colnames(data.frame(pcaShockDown)))))
colnames(pcaShockDown) <- c("PCA_1_Down", "PCA_2_Down", "PCA_3_Down")
rateShockDown <-  subset(data.frame(rateShockDown), select=-c(4:length(colnames(data.frame(rateShockDown)))))
colnames(rateShockDown) <- c("rate_PCA_1_Down", "rate_PCA_2_Down", "rate_PCA_3_Down")

pcaShock <- cbind(pcaShockUp, pcaShockDown)
pcaShock$Term <- c(1/12, 3/12, seq(from=.5, t=30, by=.5))
col_id <- grep(c("Term"), names(pcaShock))
pcaShock <- pcaShock[,c(col_id, (1:ncol(pcaShock))[-col_id])]

rateShock <- cbind(rateShockUp, rateShockDown)
rateShock$Term <- c(1/12, 3/12, seq(from=.5, t=30, by=.5))
rateShock$Base <- unclass(last(spot))[1,]
col_id <- grep(c("Base"), names(rateShock))
rateShock <- rateShock[,c(col_id, (1:ncol(rateShock))[-col_id])]
col_id <- grep(c("Term"), names(rateShock))
rateShock <- rateShock[,c(col_id, (1:ncol(rateShock))[-col_id])]
rateShock <- as.data.table(rateShock)

rateShock_long <- melt.data.table(rateShock, id = "Term")
colnames(rateShock_long)[2:3] <- c("Curve","Rate")

ggplot(rateShock_long, aes(x=Term)) +
  geom_line(aes(y=Rate, col=Curve), size=1.25) +
  labs(title="Shocked Rate Curves") +
  scale_y_continuous(n.breaks = 4, limits = c(0,0.04)) +
  scale_x_continuous(n.breaks = 3, limits = c(0,30)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

write.csv(pcaShock, "PCA_Shocks.csv")
write.csv(rateShock, "Rate_Shocks.csv")
save.image("InterestRateModelling_usingPCA.RData")
