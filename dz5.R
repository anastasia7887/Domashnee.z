library('GGally')            # матричные графики
library('boot') 
library('MASS')
library('FNN')          # алгоритм kNN
data(Boston)     

Boston$chas <- as.factor(Boston$chas)

bost <- Boston[, -c(2,5,6,7,8,9,11,13,14)]

n <- nrow(bost)

# доля обучающей выборки
train.percent <- 0.5

# выбрать наблюдения в обучающую выборку
set.seed(my.seed)
attach(bost)
inTrain <- sample(n, n*train.percent)

lm.1 <- lm(crim ~ ., subset = inTrain, data = bost)
mean((crim[-inTrain] - predict(lm.1, bost[-inTrain, ]))^2)

detach(bost)

attach(bost)

lm.2 <- lm(crim ~ tax + black + indus, subset = inTrain, data = bost)
mean((crim[-inTrain] - predict(lm.2, bost[-inTrain, ]))^2)

detach(bost)


fit.glm.1 <- glm(crim ~ ., data = bost)
cv.err <- cv.glm(bost, fit.glm.1)

# результат: первое число -- по формуле LOOCV-ошибки,
#  второе -- с поправкой на смещение
cv.err$delta

fit.glm.2 <- glm(crim ~ tax + black + indus, data = bost)
cv.err <- cv.glm(bost, fit.glm.2)

# результат: первое число -- по формуле LOOCV-ошибки,
#  второе -- с поправкой на смещение
cv.err$delta

# k-кратная перекрёстная проверка ==============================================
#Модель со всеми предикторами
#k=10

fit.glm.1 <- glm(crim ~ ., data = bost)
cv.err.k.fold <- cv.glm(bost, fit.glm.1, K = 10)$delta[1]

cv.err.k.fold
#k=5

fit.glm.1 <- glm(crim ~ ., data = bost)
cv.err.k.fold <- cv.glm(bost, fit.glm.1, K = 5)$delta[1]

cv.err.k.fold

#Модель без факторов

#k=10

fit.glm.2 <- glm(crim ~ tax + black + indus, data = bost)
cv.err.k.fold <- cv.glm(bost, fit.glm.1, K = 10)$delta[1]

cv.err.k.fold
#k=5

fit.glm.2 <- glm(crim ~ tax + black + indus, data = bost)
cv.err.k.fold <- cv.glm(bost, fit.glm.1, K = 5)$delta[1]

cv.err.k.fold

# Оценивание точности линейной регрессионной модели ----------------------------

# оценить стандартные ошибки параметров модели 
#  mpg = beta_0 + beta_1 * horsepower с помощью бутстрепа,
#  сравнить с оценками ошибок по МНК

# функция для расчёта коэффициентов ПЛР по выборке из данных
boot.fn <- function(data, index){
  coef(lm(crim ~ ., data = data, subset = index))
}
boot.fn(bost, 1:n)    



# пример применения функции к бутстреп-выборке
set.seed(my.seed)
boot.fn(bost, sample(n, n, replace = T))


# применяем функцию boot для вычисления стандартных ошибок параметров
#  (1000 выборок с повторами)
boot(bost, boot.fn, 1000)

# сравним с МНК
attach(bost)
summary(lm(crim ~ ., data = bost))$coef
detach(bost)

# оценки не отличаются 

