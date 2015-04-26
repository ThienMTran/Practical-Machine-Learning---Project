train = read.csv("./pml-training.csv")
test = read.csv("./pml-testing.csv")

# Dependent Variables
# Class A: exactly according to specification, Class B: throwing the elbows to the front,
# Class C: lifting the dumbbell only halfway, Class D: lowering the dumbbell only halfway,
# Class E: throwing the hips to the front.

# Predictor Variables

# For data recording, the authors used 4 inertial measurement units (IMU), which provide three-axes: acceleration,
# gyroscope and magnetometer data.
# The sensors are mounted in the users' glove, armbarnd, lumbar belt, and dumdbell.

# The author also used "a sliding window approach with diifferent lengths from 0.5 second to 
# 2.5 seconds, with 0.5 second overlap. In each step of the sliding window approach they calculated features
# on the features on the Euler angles (roll, pitch and yaw), as well as the raw accelerometer, gyroscope and
# magnetometer readings. For the Euler angles of each of the 4 sensors they calculated 8 features: mean, 
# variance, standard deviation, max, min, amplitude, kurtosis and skewness.

# Exploration
table(apply(is.na(train),2,sum))
which(apply(is.na(train),2,sum) == 0)
train1 = subset(train, select=which(apply(is.na(train),2,sum) == 0)) # remove variables with NA's

#####
summary(train$classe) #this is the 5 classes that we want to predict.

# Plot hidden
#####
par(mfrow = c(7,7), mar=c(1,4,1,1))
for(i in 2:50){
  plot(train1[,i], col=train1$classe, ylab=colnames(train1[i]))
}
legend("topleft", legend = unique(train1$classe), col = unique(train1$classe), pch=1)

par(mfrow = c(7,7), mar=c(1,4,1,1))
for(i in 51:92){
  plot(train1[,i], col=train1$classe, ylab=colnames(train1[i]))
}
legend("topleft", legend = unique(train1$classe), col = unique(train1$classe), pch=1)

# Plot
#####
usedVariables = c("roll_belt","pitch_belt","total_accel_belt","gyros_belt_x","gyros_belt_y",
                  "gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x",
                  "magnet_belt_y","magnet_belt_z","gyros_arm_x","accel_dumbbell_x",
                  "accel_dumbbell_y","pitch_forearm")
par(mfrow = c(4,4), mar=c(5,4,1,1))
for(i in usedVariables){
  plot(train1[,i], col=train1$classe, ylab=colnames(train1[i]))
}

# Modelling
#####
# After visually examine the plots of all the variables, we choose 16 variables that show the most variations
# accross 5 class (i.e., A, B, C, D, E).

train2 = subset(train1, select = c(usedVariables,"classe"))
test2 = subset(test, select = c(usedVariables))

######
library(caret)
modFitTree = train(classe ~ ., data=train2, method="treebag") # We choose the "rpart"treebag" package as its computation advantage against the ramdom forest

# prediction
# test predict
predTree = predict(modFitTree, newdata=test2)
test$predict = predTree

# Answer
answer = as.character(predTree)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answer)
