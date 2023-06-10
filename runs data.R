balls = read.csv("C:\\Users\\obind\\Downloads\\Stats 100\\Project\\deliveries.csv")
balls[balls$player_dismissed == "", ]$player_dismissed = NA
#Batsman next runs
ballsBat = balls[, c("match_id", "inning", "batsman", "batsman_runs", "player_dismissed")]
ballsBat$next_runs = NA
ballsBat$balls_until_next = NA
rows = nrow(ballsBat)
for (i in 1:rows){
  for (j in i+1:rows){
    if (ballsBat[i, "batsman"] == ballsBat[j, "batsman"]){
      if (ballsBat[])
        ballsBat[i, "next_runs"] = ballsBat[j, "batsman_runs"]
        ballsBat[i, "balls_until_next"] = j
        j = rows + 1
    }
  }
}


balls$bowler_runs = balls$wide_runs + balls$noball_runs + balls$batsman_runs
#Create histogram
library(ggplot2)
ggplot(data = balls, aes(x = bowler_runs)) + geom_histogram() +
  scale_x_continuous(n.breaks=10) + ggtitle("Histogram of runs allowed per ball, IPL 2008-2019")

table(balls$bowler_runs)

#Two-way conditional historgram
ballsAnly = balls[,c("match_id", "inning", "bowler", "bowler_runs")]
ballsAnly$next_runs = NA
for (i in 1:(nrow(ballsAnly)-1)){
  if (ballsAnly[i, "bowler"] == ballsAnly[i+1, "bowler"] &&
      ballsAnly[i, "match_id"] == ballsAnly[i+1, "match_id"] &&
      ballsAnly[i, "inning"] == ballsAnly[i+1, "inning"]){
    
    ballsAnly[i, "next_runs"] = ballsAnly[i+1, "bowler_runs"]
  }
}

freq_table = table(ballsAnly$bowler_runs, ballsAnly$next_runs, dnn=c("ball i", "ball i+1"))
freq_table_norm = t(apply(freq_table, 1, function(x) x/sum(x)))
freq_table_norm
library(fields)
heatmap(freq_table_norm[c(1:5, 7), c(1:5, 7)],
        Rowv=NA, Colv=NA, scale="none", col = heat.colors(256))
image.plot(legend.only=TRUE, side=3, col=heat.colors(256), zlim = range(freq_table_norm, na.rm=TRUE))
mtext("runs on ball i+1", side = 1, line = 2.5)
mtext("ball on ball i", side = 2, line = 2.5)

#Expected conditional runs
exp_runs = rowSums(freq_table_norm)

library(ggplot2)
ggplot(data=ballsAnly, aes(x = bowler_runs, y = next_runs)) + geom_tile()

freqTable = table(ballsAnly$bowler_runs, ballsAnly$next_runs)
heatmap(freqTable, scale="none", Rowv=NA, Colv=NA,)

noInt = abline(0, 0.5, col=c("blue"),
               add.expr = abline(0, 1, col=c("blue"))

heatmap(freqTable[c(1:5, 7), c(1:5, 7)], Rowv=NA, Colv=NA,
        add.expr = abline(0, 0.436548, col=c("blue"))
        
abline(0, 0.436548, col=c("blue"))
abline(0, 1, col=c("blue"))

lm(ballsAnly$total_runs~ballsAnly$next_runs)
4*0.436548

ggplot(ballsAnly, aes(total_runs, next_runs)) + stat_bin_2d()
