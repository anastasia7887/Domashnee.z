# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# ..............................................................................
# Математическое моделирование: Практика 3
#   Параметрические классификаторы для бинарной Y
#      * логистическая регрессия
#      * линейный дискриминантный анализ (LDA)
#      * квадатичный дискриминантный анализ (QDA)
#      * ROC-кривая
# ..............................................................................
library('ISLR')
library('GGally')
library('MASS')
library('titanic')

my.seed <- 123
train.percent <- 0.75

# Исходные данные: набор titanic_train -----------------------------------------------
?titanic_train
head(titanic_train)

str(titanic_train)
titanic_train1 <- titanic_train[, -4]
titanic_train1$Ticket <- as.factor(titanic_train1$Ticket)
titanic_train1$Ticket <- as.numeric(titanic_train1$Ticket)
titanic_train1$Sex <- as.factor(titanic_train1$Sex)
titanic_train1$Cabin <- as.factor(titanic_train1$Cabin)
titanic_train1$Cabin <- as.numeric(titanic_train1$Cabin)
titanic_train1$Embarked <- as.factor(titanic_train1$Embarked)
titanic_train1$Survived <- as.factor(titanic_train1$Survived)
# графики разброса
ggpairs(titanic_train1)

# Отбираем наблюдения в обучающую выборку --------------------------------------

set.seed(my.seed)
inTrain <- sample(seq_along(titanic_train1$Survived),
                  nrow(titanic_train1) * train.percent)
df <- titanic_train1[inTrain, ]

# фактические значения на обучающей выборке
Факт <- df$Survived


# Строим модели, чтобы спрогнозировать ---------------------------------

# Логистическая регрессия ======================================================
model.logit <- glm(Survived ~ Pclass+PassengerId+Sex+Age+SibSp+Parch+Ticket+Fare+Cabin+Embarked,
                   data = df, family = 'binomial')
    
    
summary(model.logit)

# прогноз: вероятности принадлежности классу '1' (выжил)
p.logit <- predict(model.logit, df, type = 'response')
    
Прогноз <- factor(ifelse(p.logit > 0.5, 1, 0), levels = c(0 ,1),
                  labels = c('0', '1'))
    
    

# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])
# специфичность
conf.m[1, 1] / sum(conf.m[1, ])
# верность
sum(diag(conf.m)) / sum(conf.m)


# LDA ==========================================================================
model.lda <- lda(Survived ~ Pclass+PassengerId+Sex+Age+SibSp+Parch+Ticket+Fare+Cabin+Embarked, data = df)
    
model.lda

# прогноз: вероятности принадлежности классу '1' (выжил)
p.lda <- predict(model.lda, df, 
                 type = 'response')
Прогноз <- factor(ifelse(p.lda$posterior[, '1'] > 0.5, 
                         1, 0),
                  levels = c(0, 1),
                  labels = c('0', '1'))

# матрица неточностей
conf.m <- table(Факт, Прогноз)
conf.m

# чувствительность
conf.m[2, 2] / sum(conf.m[2, ])
# специфичность
conf.m[1, 1] / sum(conf.m[1, ])
# верность
sum(diag(conf.m)) / sum(conf.m)



# Подбор границы отсечения вероятностей классов --------------------------------

# ROC-кривая для LDA ===========================================================
# считаем 1-SPC и TPR для всех вариантов границы отсечения
x1 <- NULL    # для (1 - SPC)
y1 <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.No', 'fact.Yes')
colnames(tbl) <- c('predict.No', 'predict.Yes')

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 501)){
  # прогноз
  Прогноз <- factor(ifelse(p.logit > p, 
                           1, 0),
                    levels = c(0, 1),
                    labels = c('0', '1'))
  
  
  # фрейм со сравнением факта и прогноза
  df.compare <- data.frame(Факт = Факт, Прогноз = Прогноз)
  
  # заполняем матрицу неточностей
  # TN
  tbl[1, 1] <- nrow(df.compare[df.compare$Факт == '0' & df.compare$Прогноз == '0', ])
  
  # TP
  tbl[2, 2] <- nrow(df.compare[df.compare$Факт == '1' & df.compare$Прогноз == '1', ])
  
  # FP
  tbl[1, 2] <- nrow(df.compare[df.compare$Факт == '0' & df.compare$Прогноз == '1', ])
  
  # FN
  tbl[2, 1] <- nrow(df.compare[df.compare$Факт == '1' & df.compare$Прогноз == '0', ])
  
  
  # считаем характеристики
  TPR <- tbl[2, 2] / sum(tbl[2, ])
  y1 <- c(y1, TPR)
  SPC <- tbl[1, 1] / sum(tbl[1, ])
  x1 <- c(x1, 1 - SPC)
}
# строим ROC-кривую
par(mar = c(5, 5, 1, 1))
# кривая
plot(x1, y1, 
     type = 'l', col = 'blue', lwd = 3,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
# считаем 1-SPC и TPR для всех вариантов границы отсечения
x <- NULL    # для (1 - SPC)
y <- NULL    # для TPR

# заготовка под матрицу неточностей
tbl <- as.data.frame(matrix(rep(0, 4), 2, 2))
rownames(tbl) <- c('fact.No', 'fact.Yes')
colnames(tbl) <- c('predict.No', 'predict.Yes')

# цикл по вероятностям отсечения
for (p in seq(0, 1, length = 501)){
    # прогноз
    Прогноз <- factor(ifelse(p.lda$Syrvived[, '1'] > p, 
                                        1, 0),
                                 levels = c(0, 1),
                                 labels = c('0', '1'))
        
    
    # фрейм со сравнением факта и прогноза
    df.compare <- data.frame(Факт = Факт, Прогноз = Прогноз)
    
    # заполняем матрицу неточностей
    # TN
    tbl[1, 1] <- nrow(df.compare[df.compare$Факт == '0' & df.compare$Прогноз == '0', ])
    
    # TP
    tbl[2, 2] <- nrow(df.compare[df.compare$Факт == '1' & df.compare$Прогноз == '1', ])
    
    # FP
    tbl[1, 2] <- nrow(df.compare[df.compare$Факт == '0' & df.compare$Прогноз == '1', ])
    
    # FN
    tbl[2, 1] <- nrow(df.compare[df.compare$Факт == '1' & df.compare$Прогноз == '0', ])
        
    
    # считаем характеристики
    TPR <- tbl[2, 2] / sum(tbl[2, ])
    y <- c(y, TPR)
    SPC <- tbl[1, 1] / sum(tbl[1, ])
    x <- c(x, 1 - SPC)
}

# строим ROC-кривую
par(mar = c(5, 5, 1, 1))
# кривая
plot(x, y, 
     type = 'l', col = 'blue', lwd = 3,
     xlab = '(1 - SPC)', ylab = 'TPR', 
     xlim = c(0, 1), ylim = c(0, 1))
# прямая случайного классификатора
abline(a = 0, b = 1, lty = 3, lwd = 2)
# точка для вероятности 0.5
points(x[p.vector == 0.5], y[p.vector == 0.5], pch = 16)
text(x[p.vector == 0.5], y[p.vector == 0.5], 'p = 0.5', pos = 4)
# точка для вероятности 0.2
points(x[p.vector == 0.2], y[p.vector == 0.2], pch = 16)
text(x[p.vector == 0.2], y[p.vector == 0.2], 'p = 0.2', pos = 4)


