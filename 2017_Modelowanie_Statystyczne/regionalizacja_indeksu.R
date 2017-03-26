# Problem 1 - regionalizacja indexu 

# wczytanie danych
HPI <- read.csv("E:/1TB_disk/Dane/Projekty/R_projects/Warsztaty_UJ/Modelowanie_statystyczne/HPI.csv")

# przejrzenie danych
str(HPI)
head(HPI)

# wczytanie pakietow
library(dplyr)
library(tidyr)
library(ggplot2)

# zamiana typu zmiennej Date
HPI$Date <- HPI$Date %>% as.Date()

# dodajmy zmienna miesi¹c
HPI2 <- HPI %>% mutate(month = substr(Date,6,7))

# przejscie na dlugi format tabeli
HPI_long <- HPI2 %>% gather('Index','Value', AZ.Phoenix:National.US)

# wizualizacja danych
HPI_long %>% ggplot() + geom_line(aes(x = Date, y = Value, group = Index, colour = Index))

# tylko kilka wybranych miast i index dla US
HPI_long %>% filter(Index %in% c('CA.Los.Angeles','MI.Detroit','IL.Chicago','National.US')) %>% 
  ggplot() + geom_line(aes(x = Date, y = Value, Group = Index, colour = Index))

# policzenie stop zwrotu
HPI_long_2<- HPI_long %>% group_by(Index) %>% mutate(simple_quarterly_return = (Value-lag(Value,3))/lag(Value,3)) %>%
  filter(month %in% c('03','06','09','12'))

HPI_short <- HPI_long_2 %>% select(-Value) %>% spread(key = Index, value = simple_quarterly_return)
                                
# model dla trzech wybranych stanow (w naszym przykladzie to w tych stanach mamy skoncentrowany nasz portfel)
HPI_short <- HPI_short %>% select(Date,National.US,CA.Los.Angeles,MI.Detroit,IL.Chicago)
HPI_short <- HPI_short[complete.cases(HPI_short),]



model_Los_Angeles <- lm(CA.Los.Angeles ~ National.US,data = HPI_short)
model_Detroit <- lm(MI.Detroit ~ National.US,data = HPI_short)
model_Chicago <- lm(IL.Chicago ~ National.US,data = HPI_short)

summary(model_Los_Angeles)
plot(y = HPI_short$CA.Los.Angeles, x = HPI_short$National.US, xlim = c(-0.1,0.1), ylim = c(-0.1,0.1))
abline(model_Los_Angeles$coefficients, lwd = 2)
grid()

# cwiczenie 1
# A) dodac do wykresu tytu³ (parametr main)


# B) stworzyc analogiczny wykres dla Detroit


# C) ograniczyc probkê, na ktorej jest zbudowany model do danych po 2003 i podac wp³yw na model
# mozna na przyklad wyznaczyc rok dla ka¿dej obserwacji i wyfiltrowac obserwaje po 2003
# mozna rowniez wyfiltrowac po datcie porownujac siê na przyklad z as.Date('2003-01-01')




portfel <- a


# model Loss Given Default (LGD)


portfel %>% mutate(shock_for_given_state = 1.82281 * national_index_quarterly_return,
                   value_under_scenario = current_value * (1 + shock_for_given_state),
                   load_to_value = exposure/value_under_scenario,
                   LGD = )

