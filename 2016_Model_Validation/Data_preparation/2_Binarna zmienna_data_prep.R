N <- 5000

income <- rpois(N, 5) * 1000 %>% pmax(2000)
loan_size <- abs(round(rnorm(N, 30,20) * 5000 + abs(round(rnorm(N,0.4,0.1) * income)*10))*100)/100

cor(loan_size,income)

z <- -3 - 0.6/100*income+0.2/2000*loan_size + rnorm(N)
pr <- 1/(1+exp(-z))
default <- rbinom(N,1,pr)
table(default)
df <- data.frame(income,loan_size, default)

write.csv(df, '2_retail_credits.csv',row.names = F)



N <- 5000

income <- rpois(N, 5) * 1000 %>% pmax(2000)
loan_size <- abs(round(rnorm(N, 30,20) * 5000 + round(rnorm(N,0,0.02) * income/10)*10)*100)/100

cor(loan_size,income)

z <- 20 + 0.5/1000*income-0.2/100*loan_size + rnorm(N)
pr <- 1/(1+exp(-z))
default <- rbinom(N,1,pr)
table(default)
df <- data.frame(income,loan_size, default)

write.csv(df, 'new_retail_credits.csv',row.names = F)