#install libraries
library(tidyverse)
library(caret)
library(corrplot)
install.packages("caret")
install.packages("corrplot")

#check working directory
getwd()

#load data
car_price <- read.csv("Car Details.csv", header = TRUE, stringsAsFactors = TRUE)
View(car_price)
str(car_price)

#splitting name column into car_brand and car_name
car_price[c('car_brand', 'car_name')] <- str_split_fixed(car_price$name, ' ', 2)

#drop name and car_name column after split
car_price$name <- NULL
car_price$car_name <- NULL
str(car_price)

#checking distr of car_brands in the data set
table(car_price$car_brand)

#checking for null values in dataset
sapply(car_price, function(x) sum(is.na(x))) #No null value exists

#Plotting data to view distr of car brands
ggplot(data = car_price, aes(x = car_brand, fill = car_brand)) +
  geom_bar() + labs(x='car_brand') + labs(title = "Distr of Car Brands") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#converting factors to numerical variables
#factor1 - fuel
levels(car_price$fuel) #checking name of factors
table(car_price$fuel) #checking distr of fuel
car_price$fuel <- str_replace(car_price$fuel, 'Diesel', "0")
car_price$fuel <- str_replace(car_price$fuel, 'Petrol', "1")
car_price$fuel <- str_replace(car_price$fuel, 'CNG', "2")
car_price$fuel <- str_replace(car_price$fuel, 'LPG', "3")
car_price$fuel <- str_replace(car_price$fuel, 'Electric', "4")
car_price$fuel <- as.numeric(car_price$fuel)

#factor2 - seller_type
levels(car_price$seller_type) #checking name of factors
table(car_price$seller_type) #checking distr of seller_type
car_price$seller_type <- str_replace(car_price$seller_type, 'Individual', "0")
car_price$seller_type <- str_replace(car_price$seller_type, 'Dealer', "1")
car_price$seller_type <- str_replace(car_price$seller_type, 'Trustmark Dealer',"2")
car_price$seller_type <- as.numeric(car_price$seller_type)

#factor3  - transmission
levels(car_price$transmission) #checking name of factors
table(car_price$transmission) #checking distr of transmission
car_price$transmission <- str_replace(car_price$transmission, 'Manual', "0")
car_price$transmission <- str_replace(car_price$transmission, 'Automatic', "1")
car_price$transmission <- as.numeric(car_price$transmission)

#factor4 - owner
table(car_price$transmission) #checking distr of owner
levels(car_price$owner) #checking name of factors
table(car_price$owner) #checking distr of fuel factor
car_price$owner <- str_replace(car_price$owner, 'First Owner', "0")
car_price$owner <- str_replace(car_price$owner, 'Second Owner', "1")
car_price$owner <- str_replace(car_price$owner, 'Third Owner', "2")
car_price$owner <- str_replace(car_price$owner, 'Fourth & Above Owner', "3")
car_price$owner <- str_replace(car_price$owner, 'Test Drive Car', "4")
car_price$owner <- as.numeric(car_price$owner)


#factor5 - car_brand
table(car_price$car_brand) #checking distr of car_brand
car_price$car_brand <- str_replace(car_price$car_brand, 'Maruti', '0')
car_price$car_brand <- str_replace(car_price$car_brand, 'Skoda', '1')
car_price$car_brand <- str_replace(car_price$car_brand, 'Honda', '2')
car_price$car_brand <- str_replace(car_price$car_brand, 'Hyundai', '3')
car_price$car_brand <- str_replace(car_price$car_brand, 'Toyota', '4')
car_price$car_brand <- str_replace(car_price$car_brand, 'Ford', '5')
car_price$car_brand <- str_replace(car_price$car_brand, 'Renault', '6')
car_price$car_brand <- str_replace(car_price$car_brand, 'Mahindra', '7')
car_price$car_brand <- str_replace(car_price$car_brand, 'Tata', '8')
car_price$car_brand <- str_replace(car_price$car_brand, 'Chevrolet', '9')
car_price$car_brand <- str_replace(car_price$car_brand, 'Fiat', '10')
car_price$car_brand <- str_replace(car_price$car_brand, 'Datsun', '11')
car_price$car_brand <- str_replace(car_price$car_brand, 'Jeep', '12')
car_price$car_brand <- str_replace(car_price$car_brand, 'Mercedes-Benz', '13')
car_price$car_brand<- str_replace(car_price$car_brand, 'Mitsubishi', '14')
car_price$car_brand<- str_replace(car_price$car_brand, 'Audi', '15')
car_price$car_brand<- str_replace(car_price$car_brand, 'Volkswagen', '16')
car_price$car_brand<- str_replace(car_price$car_brand, 'BMW', '17')
car_price$car_brand<- str_replace(car_price$car_brand, 'Nissan', '18')
car_price$car_brand<- str_replace(car_price$car_brand, 'Lexus', '19')
car_price$car_brand<- str_replace(car_price$car_brand, 'Jaguar', '20')
car_price$car_brand<- str_replace(car_price$car_brand, 'Land', '21')
car_price$car_brand<- str_replace(car_price$car_brand, 'MG', '22')
car_price$car_brand<- str_replace(car_price$car_brand, 'Volvo', '23')
car_price$car_brand<- str_replace(car_price$car_brand, 'Daewoo', '24')
car_price$car_brand<- str_replace(car_price$car_brand, 'Kia', '25')
car_price$car_brand<- str_replace(car_price$car_brand, 'Force', '26')
car_price$car_brand<- str_replace(car_price$car_brand, 'Ambassador', '27')
car_price$car_brand <- str_replace(car_price $car_brand, 'Ashok', '28')
car_price$car_brand <- str_replace(car_price$car_brand ,'Isuzu', '29')
car_price$car_brand <- str_replace(car_price$car_brand, 'Opel', '30')
car_price$car_brand <- str_replace(car_price$car_brand, 'Peugeot', '31')
car_price$car_brand <- as.numeric(car_price$car_brand)
table(car_price$car_brand)

str(car_price)

#numerical features (corrplot only works for numerical variables)
car_price_num <- car_price %>%
  select(year, selling_price, km_driven, fuel, transmission, owner)
corrplot(cor(car_price_num), type="full", 
         method ="color", title = "Correlation Plot", 
         mar=c(0,0,1,0), tl.cex= 0.8, outline= T, tl.col="indianred4")

round(cor(car_price_num),2) #no high correlations

#spliting dataset into training 80% and test 20%
set.seed(5)

train_ind<-sample(1:nrow(car_price),0.8*nrow(car_price))
train_data<-car_price[train_ind,]

test_data <-car_price[-train_ind,]

#ML Model 1 - Linear Regression
model_1 <- lm(selling_price ~ year+km_driven+fuel+seller_type+transmission+owner+car_brand, data = train_data)
summary(model_1)
plot(model_1)

#Using Model for predictions
pred_lr <- predict(model_1, newdata = test_data)
pred_lr
error_lr <- test_data$selling_price - pred_lr
error_lr
RMSE_lr <- sqrt(mean(error_lr^2))
RMSE_lr


plot(test_data$selling_price,pred_lr, main="Scatterplot",
     col = c("red","blue"), xlab = "Actual Selling Price", 
     ylab = "Predicted Selling Price")

