# WPROWADZENIE

data <- read.csv("countries of the world.csv", dec = ',', sep = ',')

data <- data[complete.cases(data),]
 
colnames(data) <- c("Country", "Region", "Pop", "Area", "PopDens", "Coast", "Migration", "InfMort",
                    "GDP", "Literacy", "Phones", "Arable", "Crops", "Other", "Climate", "Birth", "Death",
                    "Argi", "Industry", "Service")

data <- data %>% mutate(Country = str_trim(Country), Region = str_trim(Region))

data %>% filter(Country == "Poland")

data %>% filter(Migration > 0) %>% group_by(Region) %>% summarise(meanGDP = mean(GDP), sumPop = sum(Pop)) %>% arrange(meanGDP) %>%
  mutate(GDPPop = meanGDP * sumPop) %>% select(Region, sumPop, meanGDP)

data1 <- data %>% filter(Coast > 0) %>% group_by(Region) %>% arrange(desc(Pop)) %>% select(Country, Region, Pop)
data1 %>% filter(Pop == max(Pop))

# EKONOMETRIA

# Model regresji liniowej:
# y = a + b*x + E

data2 <- data[,3:20]

model <- lm(log(GDP) ~ PopDens + Migration + InfMort + Phones + Arable + Crops +
              Other + Death + Argi + Service, data2)
summary(model)

# normalność reszt

mean(model$residuals)
shapiro.test(model$residuals)
hist(model$residuals, breaks = 50)

# heteroskedastyczność

plot(model$fitted.values, model$residuals)
bptest(model)

# autokorelacja

dwtest(model)

# rozwiązywanie problemów

data %>% ggplot(aes(Service, GDP)) + geom_point() + geom_smooth(method = 'lm')

# badanie korealcji

cor(data$GDP, data$Migration)
cor(data$GDP, data$Migration)^2

dataCor <- cor(data2)
library(corrplot)
corrplot(dataCor, order = "hclust")

# dobór zmiennych

data_num <- data[,-c(1:2)]
data_num <- data %>% select(-Country, -Region)
model <- lm(GDP ~ ., data_num)
summary(model)

# metoda Hellwiga

k <- c(1, 3, 5)
R0 <- dataCor[7, -7]
R <- dataCor[-7, -7]

#h1 <- R0[k[1]]^2 / sum(abs(R[1, k]))
#h2 <- R0[k[2]]^2 / sum(abs(R[3, k]))
#h3 <- R0[k[3]]^2 / sum(abs(R[5, k]))

compute_H <- function(k)
{
  H <- 0
  for(i in k)
  {
    H <- H + R0[i]^2 / sum(abs(R[i, k]))
  }
  return(H)
}

compute_H(c(1))

# ilość kombinacji

m <- length(R0)
comb <- expand.grid(replicate(m, c(T, F), simplify = F))
k <- c(1:m)[as.logical(comb[300,])]

for(i in (length(comb) - 1))
{
  k <- c(1:m)[as.logical(comb[i,])]
  compute_H(k)
}

############## METODA HELLWIGA cd ###########################

comb <- expand.grid(replicate(m, c(T, F), simplify = F))
k <- c(1:m)[as.logical(comb[1200,])]

H_max <- 0
best_k <- NULL
for(i in 1:(nrow(comb)-1))
{
  k <- c(1:m)[as.logical(comb[i,])]
  H <- compute_H(k)
  if(H > H_max)
  {
    H_max <- H
    best_k <- k
  }
}

best_k
H_max

#############################################################

model <- lm(GDP ~ Phones, data_num)
summary(model)

####################### PREDYKCJA ###########################
set.seed(100)
train <- data %>% slice_sample(prop = 0.8)
test <- data %>% filter(!(Country %in% train$Country)) %>% select(-Country, -Region)
train <- train %>% select(-Country, -Region)

model <- lm(GDP ~ . -Industry -Area -Literacy, train)
summary(model)

prediction <- predict(model, test)
error <- test$GDP - prediction
plot(test$GDP, prediction)

RMSE <- sqrt(mean(error^2))
MAE <- mean(abs(error))
MAPE <- mean(abs((test$GDP - prediction)/test$GDP))

######### ZMIANA DANYCH KATEGORYCZNYCH ######################

data$Region <- as.factor(data$Region)
model <- lm(GDP ~ Region, data)
summary(model)

# y = a + b1*x1 + b2*x2 + b3*x3
# x = 2: y = a + b2
# x = 3: y = a + b3
# x = 1: y = a

mean_GDP <- data %>% group_by(Region) %>% 
  summarise(mean_GDP = mean(GDP)) %>%
  arrange(mean_GDP)

levels(data$Region) <- mean_GDP$Region

model <- lm(GDP ~ Region, data)
summary(model)




