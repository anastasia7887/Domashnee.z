library('leaps')             # функция regsubset() -- отбор оптимального 
#  подмножества переменных
library('glmnet')            # функция glmnet() -- лассо
library('pls')               # регрессия на главные компоненты -- pcr()
#  и частный МНК -- plsr()
library('MASS')
my.seed <- 1
train.percent <- 0.85
fix(Boston)
names(Boston)
dim(Boston)

# считаем пропуски
sum(is.na(Boston$crim))

# Пошаговое исключение =========================================================

regfit.bwd <- regsubsets(crim ~ ., Boston, nvmax = 13, method = 'backward')

summary(regfit.bwd)

#тестовая выборка
inTrain <- sample(seq_along(Boston$crim), 
                  
                  nrow(Boston) * train.percent)

df.train <- Boston[inTrain, c(colnames(Boston)[-1], colnames(Boston)[1])]

df.test <- Boston[-inTrain,]

# отбираем 10 блоков наблюдений
k <- 10
set.seed(my.seed)
folds <- sample(1:k, nrow(Boston), replace = T)

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# заготовка под матрицу с ошибками
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))

# заполняем матрицу в цикле по блокам данных
for (j in 1:k){
  best.fit <- regsubsets(crim ~ ., Boston[folds !=j, ], nvmax = 13)
  
  # теперь цикл по количеству объясняющих переменных
  for (i in 1:13){
    # модельные значения 
    pred <- predict(best.fit, Boston[folds !=j, ], id = i)
    # вписываем ошибку в матрицу
    cv.errors[j, i] <- mean((Boston[folds == j, 'crim'] - pred)^2)
  }
} 

# усредняем матрицу по каждому столбцу (т.е. по блокам наблюдений), 
#  чтобы получить оценку MSE для каждой модели с фиксированным 
#  количеством объясняющих переменных
mean.cv.errors <- apply(cv.errors, 2, mean)
round(mean.cv.errors, 0)

# на графике
plot(mean.cv.errors, type = 'b')
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)],
       col = 'red', pch = 20, cex = 2)

# перестраиваем модель с 1 объясняющей переменной на тестовой выборке

reg.best <- regsubsets(crim ~ ., df.test, nvmax = 13)
round(coef(reg.best, 1), 3)

pred <- predict(reg.best, df.test, id = 1)
#Считаем ошибку
cv.error <- mean((df.test[, 'crim'] - pred)^2)
cv.error

# из-за синтаксиса glmnet() формируем явно матрицу объясняющих...

x <- model.matrix(crim ~ ., Boston)[, -1]



# и вектор значений зависимой переменной

y <- Boston$crim
set.seed(my.seed)

train <- sample(1:nrow(x), nrow(x)/2)

test <- -train

y.test <- y[test]

set.seed(my.seed)
pls.fit <- plsr(crim ~ ., data =Boston,  subset = train, scale = T,
                validation = 'CV')
summary(pls.fit)

# теперь подгоняем модель для найденного оптимального M = 10 

#  и оцениваем MSE на тестовой

pls.pred <- predict(pls.fit, x[test, ], ncomp = 2)

round(mean((pls.pred - y.test)^2), 0)



# подгоняем модель на всей выборке

pls.fit <- plsr(crim ~ ., data = Boston, scale = T, ncomp = 10)

summary(pls.fit)


# доля обучающей выборки
train.percent <- 0.5

# выбрать наблюдения в обучающую выборку
set.seed(my.seed)

inTrain <- sample(n, n*train.percent)

lm.1 <- regsubsets(crim ~ .,subset = inTrain, data = Boston, nvmax = 13)
mean((crim[-inTrain] - predict(lm.1, id = 1, Boston[-inTrain, ]))^2)



lm.2 <- plsr(crim ~ ., data = Boston, subset = inTrain, scale = T, ncomp = 10)
mean((crim[-inTrain] - predict(lm.2, Boston[-inTrain, ]))^2)


#второй метод дал лучший результат