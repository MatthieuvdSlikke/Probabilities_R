library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

#The death_prob data frame from the dslabs package contains information about the estimated probability of death within 1 year (prob) for different ages and sexes.
#Use death_prob to determine the death probability of a 50 year old female, p.
p_F <- filter(death_prob,sex =='Female' & age ==50)
p_F

#The loss in the event of the policy holder's death is -$150,000 and the gain if the policy holder remains alive is the premium $1,150.
#What is the expected value of the company's net profit on one policy for a 50 year old female?
expected_value_F_50 <- p_F$prob*(-150000)+(1-p_F$prob)*1150
expected_value_F_50

#Calculate the standard error of the profit on one policy for a 50 year old female.
standard_error_F_50 <- abs(-150000-1150)*sqrt(p_F$prob*(1-p_F$prob))
standard_error_F_50

#What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
expected_value_F_50_1000pol <- expected_value_F_50*1000
expected_value_F_50_1000pol

#What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
standard_error_F_50_1000pol <- standard_error_F_50*sqrt(1000)
standard_error_F_50_1000pol

#Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
pnorm(0, expected_value_F_50_1000pol,standard_error_F_50_1000pol)


#Use death_prob to determine the probability of death within one year for a 50 year old male.
p_M <- filter(death_prob,sex =='Male' & age ==50)
p_M

#Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000. Use the formula for expected value of the sum of draws with the following values and solve for the premium  𝑏 :
#What premium should be charged?
premium <- (700000/1000+p_M$prob*150000)/(1-p_M$prob)
premium

#Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
standard_error_M_50_1000pol <- abs(-150000-premium)*sqrt(p_M$prob*(1-p_M$prob))*sqrt(1000)
standard_error_M_50_1000pol

#What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(0, 700000, standard_error_M_50_1000pol)


#In this 6-part question, we'll look at a scenario in which a lethal pandemic disease increases the probability of death within 1 year for a 50 year old to .015. Unable to predict the outbreak, the company has sold 1,000 $150,000 life insurance policies for $1,150.
#What is the expected value of the company's profits over 1,000 policies?
expected_value_pandemic <- 1000 * (-150000*0.015 + 1150*(1-0.015))

#What is the standard error of the expected value of the company's profits over 1,000 policies?
standard_error_pandemic <- sqrt(1000)*abs(-150000-1150)*sqrt(0.015*(1-0.015))
standard_error_pandemic

#What is the probability of the company losing money?
pnorm(0,expected_value_pandemic,standard_error_pandemic)

#Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go out of business.
#What is the probability of losing more than $1 million?
pnorm(-1000000,expected_value_pandemic,standard_error_pandemic)

#Investigate death probabilities p <- seq(.01, .03, .001).
#What is the lowest death probability for which the chance of losing money exceeds 90%?
expect_val <- function(x){
  1000 * (-150000*x + 1150*(1-x))
}
stand_err <- function(x){
  sqrt(1000)*abs(-150000- 1150)*sqrt(x*(1-x))
}
calc_pnorm <- function(x){
  pnorm(0,expect_val(x),stand_err(x))
}
lowest_prop_0 <- data.frame(lowest_prob=p,pnorm=sapply(p,calc_pnorm)) %>%filter(pnorm>=0.9)
head(lowest_prop_0 ,1)

#Investigate death probabilities p <- seq(.01, .03, .0025).
#What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
calc_pnorm_1mil <- function(x){
  pnorm(-1000000,expect_val(x),stand_err(x))
  }

lowest_prop_1mil <- data.frame(lowest_prob=p,pnorm=sapply(p,calc_pnorm_1mil)) %>%filter(pnorm>=0.9)
head(lowest_prop_1mil ,1)

#Define a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, loss of -$150,000 on a claim, and profit of $1,150 when there is no claim. Set the seed to 25, then run the model once.
#What is the reported profit (or loss) in millions (that is, divided by  10^6 )?
set.seed(25, sample.kind = "Rounding")
p_loss <- .015
profit <- sum(sample(c(-150000,1150),n, replace=TRUE,prob=c(p_loss,1-p_loss)))/10^6

#Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
set.seed(27, sample.kind = "Rounding")
#What is the observed probability of losing $1 million or more?
B <- 10000
S <- replicate(B, {
  profit <- sum(sample(c(-150000,1150),n, replace=TRUE,prob=c(p_loss,1-p_loss)))/10^6
  profit
})
mean(S < -1)


#Suppose that there is a massive demand for life insurance due to the pandemic, and the company wants to find a premium cost for which the probability of losing money is under 5%, assuming the death rate stays stable at \(p=0.015\).
z <- qnorm(0.05)
p <- 0.015
x<- 150000*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

#What is the expected profit per policy at this rate?
exp_profit <-x*(1-p)+(-150000)*p
exp_profit

#What is the expected profit over 1,000 policies?
exp_profit*1000

#Run a Monte Carlo simulation with B=10000to determine the probability of losing money on 1,000 policies given the new premium x, loss on a claim of $150,000, and probability of claim \(p=.015\). Set the seed to 28 before running your simulation.
#What is the probability of losing money here?
set.seed(28, sample.kind = "Rounding")

B <- 10000
S <- replicate(B, {
  profit <- sum(sample(c(-150000,x),n, replace=TRUE,prob=c(p,1-p)))/10^6
  profit
})
mean(S < 0)


#The company cannot predict whether the pandemic death rate will stay stable. Set the seed to 29, then write a Monte Carlo simulation that for each of \(B = 10000\) iterations:
  #randomly changes \(p\) by adding a value between -0.01 and 0.01 with sample(seq(-0.01, 0.01, length = 100), 1)
  #uses the new random \(p\) to generate a sample of \(n = 1,000 \) policies with premium x and loss per claim \(l = -150000\)
  #returns the profit over \(n\) policies (sum of random variable)
#The outcome should be a vector of \(B\) total profits. Use the results of the Monte Carlo simulation to answer the following three questions.

#What is the expected value over 1,000 policies?
set.seed(29, sample.kind = "Rounding")
profit <- replicate(B,{
  new_p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  profit <- sum(sample(c(-150000,x),n, replace=TRUE,prob=c(new_p,1-new_p)))
  sum(profit)
})
mean(profit)

#What is the probability of losing money?
mean(profit<0)

#What is the probability of losing more than $1 million?
mean(profit< -1000000)

