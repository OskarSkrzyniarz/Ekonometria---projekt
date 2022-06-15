###################### CZĘŚĆ PIERWSZA: WCZYTYWANIE DANYCH ######################

# Wczytywanie danych z pliku csv
data <- read.csv('data.csv')
summary(data)

# Zmiana nazw niektórych zmiennych w celu wygodniejszej pracy na nich
for(i in 1:2921)
{
  len <- nchar(data$Nation[i])
  data$Nation[i] <- substr(data$Nation[i], len-2, len)
  
  len <- nchar(data$Player[i])
  data$Player[i] <- substr(data$Player[i], 1, len-9)
  
  if(data$League[i] == "eng Premier League")
    data$League[i] <- substr(data$League[i], 5, nchar(data$League[i]))
  else
    data$League[i] <- substr(data$League[i], 4, nchar(data$League[i]))
}

# Szukamy braków we wczytanych danych
summary(data)

# Podmieniamy brakujące dane
data[1532, 8] <- 2002
data[1532, 7] <- 2022 - 2002
data[1532, 3] <- "ESP"


formation <- c(1:2921)
for(i in 1:2921)
{
  if(data$Position[i] == "GK" | data$Position[i] == "DF")
    formation[i] <- "Def"
  else
    formation[i] <- "Off"
}
data$Formation <- formation
data <- data %>% relocate(Formation, .after = Position)

# Wykres pudełkowy rozbieżności danych pod względem ligi
plot1 <- ggplot(data, aes(x = League, y = Asissts, fill = League)) +
         geom_boxplot() + labs(x="Liga", y = "Asysyty") + 
         theme(legend.position="none") + scale_fill_brewer(palette="RdBu")
plot1

# Wyrkes kołowy ilości reprezentantów poszczególnych krajów
france <- 0
spain <- 0
england <- 0
italy <- 0
germany <- 0

countOfCoutnries <- length(unique(data$Nation))

for(x in data$Nation)
{
  if(x == "ESP")
    spain <- spain + 1
  if(x == "FRA")
    france <- france + 1
  if(x == "ENG")
    england <- england + 1
  if(x == "GER")
    germany <- germany + 1
  if(x == "ITA")
    italy <- italy + 1
}

others <- 2921 - france - spain - england - italy - germany

plot2Data <- data.frame(
             values <- c(france, spain, england, italy, germany, others),
             lbl <- c("Francja", "Hiszpania", "Anglia",
                      "Włochy", "Niemcy", "Pozostałe kraje (103)"),
             percent <- paste(as.character(round(values / 2921 * 100,
                                              digits = 1)), '%'))

plot2 <- ggplot(plot2Data, aes(x = "", y = values, fill = lbl)) +
         geom_col(color = "black") +
         geom_text(aes(label = percent), color = "black",
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
         guides(fill = guide_legend(title = "Państwo")) +
         scale_fill_viridis_d() +
         coord_polar(theta = "y") + 
         scale_fill_brewer(palette="Blues") +
         theme_void()
plot2

# Analiza asyst na konkretnych pozycjach
gkAss <- 0
dfAss <- 0
mfAss <- 0
frAss <- 0
totalAss <- sum(data$Asissts)
for(i in 1:2921)
{
  if(substr(data$Position[i], 1, 1) == 'G')
    gkAss <- gkAss + data$Asissts[i]
  else if(substr(data$Position[i], 1, 1) == 'D')
    dfAss <- dfAss + data$Asissts[i]
  else if(substr(data$Position[i], 1, 1) == 'M')
    mfAss <- mfAss + data$Asissts[i]
  else if(substr(data$Position[i], 1, 1) == 'F')
    frAss <- frAss + data$Asissts[i]
}

plot3Data <- data.frame(
  values <- c(gkAss, dfAss, mfAss, frAss),
  lbl <- c("Bramkarze", "Obrońcy", "Pomocnicy", "Napastnicy"),
  percent <- paste(as.character(round(values / 2921 * 100, digits = 1)), '%'))

plot3 <- ggplot(plot3Data, aes(x = "", y = values, fill = lbl)) +
         geom_col(color = "black") +
         geom_text(aes(label = percent), color = "black",
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
         guides(fill = guide_legend(title = "Pozycja")) +
         scale_fill_viridis_d() +
         coord_polar(theta = "y") + 
         scale_fill_brewer(palette="RdBl") +
         theme_void()
plot3

data <- data[!(data$Position == "GK"),]

# Usuwamy zbędne kolumny: kraj pochodzenia, liczbę porządkową i ligę
rawData <- subset(data, select = c(-Player, -Nation, -Position,
                                   -Team, -League, -Born))

########################## CZĘŚĆ DRUGA: PODZIAŁ DANYCH #########################

# Wykres podziału danych ze względu na formację zawodnika
offensive <- sum(rawData$Formation == "Off")
defensive <- sum(rawData$Formation == "Def")

plot4Data <- data.frame(
  values <- c(offensive, defensive),
  lbl <- c("Ofensywni", "Defensywni"),
  percent <- paste(as.character(round(values / 2705 * 100,
                                      digits = 1)), '%'))

plot4 <- ggplot(plot4Data, aes(x = "", y = values, fill = lbl)) +
  geom_col(color = "black") +
  geom_text(aes(label = percent), color = "black",
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  guides(fill = guide_legend(title = "Zawodnicy")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="OrRd") +
  theme_void()
plot4

# Sprawdzamy liczność zbiorów przy podziale 70% - 30%
trainQuantity <- round(2705 * 0.7, digits = 0)
testQuantity <- 2705 - trainQuantity

# Dobieramy wielkości prób tak, aby w próbie uczącej podział ze względu na
# formację był zbliżony do: 35% - defensywni, 65% - ofensywni.
trainDefQuantity <- round(trainQuantity * 0.35, digits = 0)
trainOffQuantity <- trainQuantity - trainDefQuantity

offPlayer <- rawData %>% filter(rawData$Formation == "Off")
defPlayer <- rawData %>% filter(rawData$Formation == "Def")

offFreq <- trainOffQuantity / nrow(offPlayer)
defFreq <- trainDefQuantity / nrow(defPlayer)

set.seed(213742069)

trainA <- offPlayer %>% dplyr::sample_frac(offFreq)
testA  <- dplyr::anti_join(offPlayer, trainA, by = 'X.')

trainB <- defPlayer %>% dplyr::sample_frac(defFreq)
testB  <- dplyr::anti_join(defPlayer, trainB, by = 'X.')

train <- rbind(trainA, trainB)
test <- rbind(testA, testB)

# Wykresy sprawdzające prawidłowość podziału
plot5Data <- data.frame(
  values <- c(nrow(trainA), nrow(trainB)),
  lbl <- c("Ofensywni", "Defensywni"),
  percent <- paste(as.character(round(values / nrow(train) * 100,
                                      digits = 1)), '%'))
plot6Data <- data.frame(
  values <- c(nrow(testA), nrow(testB)),
  lbl <- c("Ofensywni", "Defensywni"),
  percent <- paste(as.character(round(values / nrow(test) * 100,
                                      digits = 1)), '%'))

plot5 <- ggplot(plot5Data, aes(x = "", y = values, fill = lbl)) +
  geom_col(color = "black") +
  geom_text(aes(label = percent), color = "black",
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  guides(fill = guide_legend(title = "Zawodnicy")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="GnBu") +
  theme_void()

plot6 <- ggplot(plot6Data, aes(x = "", y = values, fill = lbl, binwidth = 1)) +
  geom_col(color = "black") +
  geom_text(aes(label = percent), color = "black",
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  guides(fill = guide_legend(title = "Zawodnicy")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="PuRd") +
  theme_void()

plotMixed <- ggarrange(plot5, plot6, 
             labels = c("TRAIN", "TEST"),
             ncol = 2, nrow = 1)
plotMixed

# Usuwamy kolumnę z indeksem
train <- subset(train, select = c(-X.))
test <- subset(test, select = c(-X.))

######################### CZĘŚĆ TRZECIA: DOBÓR ZMIENNYCH #######################

# Faktoryzacja zmiennej jakościowej
for(i in 1:nrow(train))
{
  if(train$Formation[i] == "Def")
    train$Formation[i] <- 0
  else train$Formation[i] <- 1
}

for(i in 1:nrow(test))
{
  if(test$Formation[i] == "Def")
    test$Formation[i] <- 0
  else test$Formation[i] <- 1
}

# Model regresji liniowej
model <- lm(Asissts ~ Formation + Age + X90s + TotPass + PassAtt + TotDist +
              PrgDist + ShortPass + ShortPassAtt + MedPass + MedPassAtt +
              LongPass + LongPassAtt + Goals + Shoots + SoT + FreeKicks + SCA +
              PassLiveToShoot + PassDeadToShoot + DribToShoot + ShootToShoot +
              FoulToShoot + DefToShoot + GCA + PassLiveToGoal + PassDeadToGoal +
              DribToGoal + ShootToGoal + FoulToGoal + DefToGoal, train)
summary(model)
rVec <- round(summary(model)$adj.r.squared * 100, 2)

# R-squared = 84,21%
# -DefToShoot (N/A)
# -DefToGoal (N/A)
model <- lm(Asissts ~ Formation + Age + X90s + TotPass + PassAtt + TotDist +
            PrgDist + ShortPass + ShortPassAtt + MedPass + MedPassAtt +
            LongPass + LongPassAtt + Goals + Shoots + SoT + FreeKicks + SCA +
            PassLiveToShoot + PassDeadToShoot + DribToShoot + ShootToShoot +
            FoulToShoot + GCA + PassLiveToGoal + PassDeadToGoal +
            DribToGoal + ShootToGoal + FoulToGoal, train)
summary(model)
rVec <- append(rVec, round(summary(model)$adj.r.squared * 100, 2))

# R-squared = 84,21%
# -PrgDist (0.914578)
model <- lm(Asissts ~ Formation + Age + X90s + TotPass + PassAtt + TotDist +
              ShortPass + ShortPassAtt + MedPass + MedPassAtt +
              LongPass + LongPassAtt + Goals + Shoots + SoT + FreeKicks + SCA +
              PassLiveToShoot + PassDeadToShoot + DribToShoot + ShootToShoot +
              FoulToShoot + GCA + PassLiveToGoal + PassDeadToGoal +
              DribToGoal + ShootToGoal + FoulToGoal, train)
summary(model)
rVec <- append(rVec, round(summary(model)$adj.r.squared * 100, 2))

# R-squared = 84,21%
# -TotDist (0.808383)
model <- lm(Asissts ~ Formation + Age + X90s + TotPass + PassAtt +
              ShortPass + ShortPassAtt + MedPass + MedPassAtt +
              LongPass + LongPassAtt + Goals + Shoots + SoT + FreeKicks + SCA +
              PassLiveToShoot + PassDeadToShoot + DribToShoot + ShootToShoot +
              FoulToShoot + GCA + PassLiveToGoal + PassDeadToGoal +
              DribToGoal + ShootToGoal + FoulToGoal, train)
summary(model)
rVec <- append(rVec, round(summary(model)$adj.r.squared * 100, 2))

# R-squared = 84,22%
# -SoT (0.634297)
model <- lm(Asissts ~ Formation + Age + X90s + TotPass + PassAtt +
              ShortPass + ShortPassAtt + MedPass + MedPassAtt +
              LongPass + LongPassAtt + Goals + Shoots + FreeKicks + SCA +
              PassLiveToShoot + PassDeadToShoot + DribToShoot + ShootToShoot +
              FoulToShoot + GCA + PassLiveToGoal + PassDeadToGoal +
              DribToGoal + ShootToGoal + FoulToGoal, train)
summary(model)
rVec <- append(rVec, round(summary(model)$adj.r.squared * 100, 2))

# R-squared = 84,23%
# -ShootToGOal (0.200574)
model <- lm(Asissts ~ Formation + Age + X90s + TotPass + PassAtt +
              ShortPass + ShortPassAtt + MedPass + MedPassAtt +
              LongPass + LongPassAtt + Goals + Shoots + FreeKicks + SCA +
              PassLiveToShoot + PassDeadToShoot + DribToShoot + ShootToShoot +
              FoulToShoot + GCA + PassLiveToGoal + PassDeadToGoal +
              DribToGoal + FoulToGoal, train)
summary(model)
rVec <- append(rVec, round(summary(model)$adj.r.squared * 100, 2))

# R-squared = 84,22%
# +ShootToGOal (0.200574)
model <- lm(Asissts ~ Formation + Age + X90s + TotPass + PassAtt +
              ShortPass + ShortPassAtt + MedPass + MedPassAtt +
              LongPass + LongPassAtt + Goals + Shoots + FreeKicks + SCA +
              PassLiveToShoot + PassDeadToShoot + DribToShoot + ShootToShoot +
              FoulToShoot + GCA + PassLiveToGoal + PassDeadToGoal +
              DribToGoal + ShootToGoal + FoulToGoal, train)
summary(model)
rVec <- append(rVec, round(summary(model)$adj.r.squared * 100, 2))

# MAX(R-squared = 84,23%)
# Liczba zmiennych w modelu: 26

# Wykres zmiany wskaźnika R-squared)
plot7Data <- data.frame(
  values <- rVec,
  lbl <- c("Default", "-NA", "-PrgDist", "-TotDist", "-SoT"
           , "-ShootToGoal", "+ShootToGoal"))

plot7 <- ggplot(plot7Data, aes(x = lbl, y = values, group = 1, color = "red")) +
  scale_x_discrete(limits = lbl) +
  labs(y = "Skorygowany wsp. determinacji", x = "Usunięcie zmiennej") +
  geom_line(linetype = "dashed", color = "red") +
  geom_point() +
  theme_light() +
  theme(legend.position="none")
plot7

######################### CZĘŚĆ CZWARTA: POSTAĆ MODELU #########################

# wypisać wzór z summary(model)

########################### CZĘŚĆ PIĄTA: DIAGNOSTYKA ###########################

# normalność reszt
mean(model$residuals)
shapiro.test(model$residuals)
hist(model$residuals, breaks = 50)

# heteroskedastyczność
plot(model$fitted.values, model$residuals)
bptest(model)

# autokorelacja
dwtest(model)


################### CZĘŚĆ SZÓSTA: PROGNOZA I BŁĘDY PROGNOZY ####################

prediction <- round(predict(model, test), digits = 0)
error <- test$Asissts - prediction # Zamienić wartości -1 na 0
plot(test$Asissts, prediction)  # Sprawdzić poprawność predyckji z rzeczywistością

RMSE <- sqrt(mean(error^2))
MAE <- mean(abs(error))
MAPE <- mean(abs((test$GDP - prediction)/test$GDP))

################## CZĘŚĆ SIÓDMA: PODSUMOWANIE I INTERPRETACJA ##################










