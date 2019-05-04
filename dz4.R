# загрузка пакетов

library('GGally')       # графики совместного разброса переменных

library('lmtest')       # тесты остатков регрессионных моделей

library('FNN')          # алгоритм kNN

library('MASS')

data(Boston)            # открываем данные

?Boston   

# константы

my.seed <- 12345

train.percent <- 0.85

# преобразуем категориальные переменные в факторы



Boston$chas <- as.factor(Boston$chas)
str(Boston)

# обучающая выборка

set.seed(my.seed)
bost <- Boston[, -c(2,5,6,7,8,9,11,13,14)]
inTrain <- sample(seq_along(bost$crim), 
                  
                  nrow(bost) * train.percent)

df.train <- bost[inTrain, c(colnames(bost)[-1], colnames(bost)[1])]

df.test <- bost[-inTrain, -1]


# описательные статистики по переменным

summary(df.train)




model.1 <- lm(crim ~ . + chas:tax + chas:black + chas:indus,
              
              data = df.train)

summary(model.1)
# взаимодействие chas:tax исключаем оно не значимо

model.2 <- lm(crim ~ . + chas:black + chas:indus,
              
              data = df.train)

summary(model.2)
# взаимодействие chas:black исключаем

model.3 <- lm(crim ~ . + chas:indus,
              
              data = df.train)

summary(model.3)

#Коэффициент при indus имеет наибольшее р-значение, его исключим

model.4 <- lm(crim ~ chas + tax + black + chas:indus,
              
              data = df.train)

summary(model.4)

#Коэффициент при chas имеет наибольшее р-значение, его исключим

model.5 <- lm(crim ~ tax + black + chas:indus,
              
              data = df.train)

summary(model.5)

# взаимодействие chas:indus исключаем оно не значимо

model.6 <- lm(crim ~ tax + black,
              
              data = df.train)

summary(model.6)

#Очевидно, стоит остановиться на модели без взаимодействий. Проверим её остатки.

# тест Бройша-Пагана

bptest(model.6)



# статистика Дарбина-Уотсона

dwtest(model.6)



# графики остатков

par(mar = c(4.5, 4.5, 2, 1))

par(mfrow = c(1, 3))

plot(model.6, 1)

plot(model.6, 4)

plot(model.6, 5)

par(mfrow = c(1, 1))


# фактические значения y на тестовой выборке

y.fact <- bost[-inTrain, 1]

y.model.lm <- predict(model.6, df.test)

MSE.lm <- sum((y.model.lm - y.fact)^2) / length(y.model.lm)



# kNN требует на вход только числовые переменные

df.train.num <- as.data.frame(apply(df.train, 2, as.numeric))

df.test.num <- as.data.frame(apply(df.test, 2, as.numeric))



for (i in 2:50){
  
  model.knn <- knn.reg(train = df.train.num[, !(colnames(df.train.num) %in% 'crim')], 
                       
                       y = df.train.num[, 'crim'], 
                       
                       test = df.test.num, k = i)
  
  y.model.knn <- model.knn$pred
  
  if (i == 2){
    
    MSE.knn <- sum((y.model.knn - y.fact)^2) / length(y.model.knn)
    
  } else {
    
    MSE.knn <- c(MSE.knn, 
                 
                 sum((y.model.knn - y.fact)^2) / length(y.model.knn))
    
  }
  
}



# график

par(mar = c(4.5, 4.5, 1, 1))

# ошибки kNN

plot(2:50, MSE.knn, type = 'b', col = 'darkgreen',  ylim = c(165, 200),
     
     xlab = 'значение k', ylab = 'MSE на тестовой выборке')

# ошибка регрессии

lines(2:50, rep(MSE.lm, 49), lwd = 2, col = grey(0.2), lty = 2)

legend('bottomright', lty = c(1, 2), pch = c(1, NA), 
       
       col = c('darkgreen', grey(0.2)), 
       
       legend = c('k ближайших соседа', 'регрессия (все факторы)'), 
       
       lwd = rep(2, 2))


{r, include = F}

frml.to.text.01 <- paste0('$\\frac {\\sqrt{MSE_{TEST}}}{\\bar{y}_{TEST}} = ',
                          
                          round(sqrt(MSE.lm) / mean(y.fact) * 100, 1),
                          
                          '\\%$')
