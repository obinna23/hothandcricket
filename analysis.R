library(dplyr)

balls = read.csv("C:\\Users\\obind\\Downloads\\Stats 100\\Project\\deliveries.csv")
balls[balls$player_dismissed == "", ]$player_dismissed = NA

library(ggplot2)
ggplot(data = balls, aes(x = batsman_runs)) + geom_histogram()

batsmenInns = balls %>% group_by(match_id, batsman)

#Streaks vs expected streaks
expected_streaks = batsmenInns %>% summarize(
    (2 * sum(batsman_runs < 1) * sum(batsman_runs >= 1) / n()),          
    (2 * sum(batsman_runs < 4) * sum(batsman_runs >= 4) / n()),
    (2 * sum(batsman_runs < 6) * sum(batsman_runs >= 6) / n())
)
colnames(expected_streaks) = c("match_id", "batsman", "score_streaks", "four_streaks", "six_streaks")

count_runs = function(data, num){
  streaks = 0
  score = 0
  not_score = 0
  if (length(data) >= 3){
    for (i in 1:(length(data)-1)){
      if (data [i] < num){not_score = not_score + 1}
      else{score = + 1}
      if ((data[i] < num) != (data[i+1] < num)){
        streaks = streaks + 1
      }
    }
    if (data[length(data)] < num){not_score = not_score + 1}
    else{score = score + 1}
    
    return(c(score, not_score, streaks))
  }
  else{return(c(NA, NA, NA))}
}

wolf_warditz_test = function(data, num){
  obv_array = count_runs(data, num)
  N = obv_array[1] + obv_array[2]
  R = 2 * obv_array[1] * obv_array[2] / N + 1
  r = obv_array[3]
  V = (R - 1) * (R - 2) / (N - 1)
  z = (r - R) / sqrt(V)
  return(pnorm(q = z, lower.tail=TRUE))
}

wolf_warditz = batsmenInns %>% summarize(
  n(),
  wolf_warditz_test(batsman_runs, 1), 
  wolf_warditz_test(batsman_runs, 4), 
  wolf_warditz_test(batsman_runs, 6)
)

#Balls vs geometric
balls_counted = batsmenInns %>% summarize(
  sum(wide_runs + noball_runs > 0),
  sum(player_dismissed==batsman, na.rm=TRUE),
  n())

colnames(balls_counted) = c("match_id", "batsman", "extra_balls", "dismissed", "total_balls")
balls_counted$inning_length = balls_counted$total_balls - balls_counted$extra_balls

#Find wicket rate
wide_noball = sum(balls$wide_runs + balls$noball_runs > 0)
wicket_total = sum(balls$player_dismissed == balls$batsman, na.rm=TRUE)
wicket_rate = wicket_total / (nrow(balls) - wide_noball)

ggplot(data = balls_counted[balls_counted$dismissed == 1 & balls_counted$inning_length > 0, ], aes(x = inning_length)) +
  geom_histogram(aes(y=..density..), binwidth=1) + ylim(0, 0.06) +
  scale_x_continuous(limits = c(0, 72), breaks = seq(0, 72, 6)) +
  geom_function(fun = function(x) (1-wicket_rate)**(x-1)*wicket_rate, color="blue") +
  geom_function(fun = function(x) (wicket_rate)*exp(-wicket_rate*x), color="red") +
  ggtitle("Balls before dismissal vs geometric distribution")

obvBalls = table(balls_counted[balls_counted$inning_length > 0,]$inning_length)
obvBalls = as.data.frame(obvBalls)
obvBalls$Var1 = as.numeric(obvBalls$Var1)
obvBalls$geom = (1 - wicket_rate)**obvBalls$Var1*wicket_rate
chisq.test(obvBalls$Freq, obvBalls$geom)
