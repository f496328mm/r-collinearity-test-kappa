



library(data.table);
library(ade4);
library(dplyr);


## corrplot 0.84 loaded
x = c('柴犬','米克斯','哈士奇')
# 製造類別變數
data = data.table(matrix(sample(x,100,replace = T)))

ont_hot_3dim = acm.disjonctif( data )


x1 <- ont_hot_3dim[,1]
x2 <- ont_hot_3dim[,2]
x3 <- ont_hot_3dim[,3]   
mm12 <- model.matrix(~ x1 + x2)        # normal model, two indep. regressors
mm123 <- model.matrix(~ x1 + x2 + x3)  # bad model with near collinearity
print( kappa(mm12) )                   # a 'low' kappa is good
print( kappa(mm123) )                  # a 'high' kappa indicates trouble

# 做迴歸
data2 = ont_hot_3dim %>% cbind(.,rnorm(100)) %>% data.table
colnames(data2) = paste('x',c(1:ncol(data2)),sep='')
fit <- lm(x4~., data=data2)
print(fit)


# 做迴歸
data3 = ont_hot_3dim[,1:2] %>% cbind(.,rnorm(100)) %>% data.table
colnames(data3) = paste('x',c(1:ncol(data3)),sep='')
fit2 <- lm(x3~., data=data3)
print(fit2)

#----------------------------------------------------------










