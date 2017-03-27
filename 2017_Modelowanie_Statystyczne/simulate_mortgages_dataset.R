N <- 3000

income <- rpois(N, 5) * 1000 %>% pmax(2000)
loan_size <- abs(round(rnorm(N, 20,7) * 15000 + abs(round(rnorm(N,0.2,0.1) * income/3)*10))*100)/100

loan_size <- round(abs(round(rnorm(N, 30,10) * 15000))/100)*100
collateral <- round((abs(round(rnorm(N,1,0.3) * loan_size)) + abs(round(rnorm(N, 3,2) * 10000)))/100)*100
city <- c(rep('CA.Los.Angeles',900),rep('MI.Detroit',600),rep('IL.Chicago',1500))

            
            
cor(loan_size,collateral)
plot(loan_size,collateral)

(collateral/loan_size) %>% hist

mortgages <- data.frame(income,loan_size,collateral,city) %>% mutate(LTV = loan_size/collateral)
mortgages <- data.frame(mortgages, ran = runif(N)) %>% arrange(ran) %>% select(-ran) %>%
  mutate(PD = runif(N,max = LTV/20))


write.csv(mortgages, 'mortgages.csv',row.names = F)
