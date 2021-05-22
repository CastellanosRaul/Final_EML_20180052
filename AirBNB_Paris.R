library(readr)
library(dplyr)
library(writexl)
library(mltools)
library(data.table)
library(dummies)


abnb <- read.csv("airbnb-listings.csv", sep = ";") 
unique(abnb$Room.Type)

airbnb <- abnb %>% select(Price,Host.Response.Rate, Room.Type, Property.Type,
                            Accommodates, Bathrooms, Bedrooms, Beds, 
                            Cleaning.Fee, Guests.Included, Review.Scores.Rating,
                            Cancellation.Policy)


airbnb$Cleaning.Fee <- ifelse(is.na(airbnb$Cleaning.Fee), 0, 1)

colnames(airbnb) <- c("Price", "Host_Response_Rate", "Room_Type",
                      "Property_Type", "Accommodates", "Bathrooms",
                      "Bedrooms", "Beds", "Cleaning_Fee", "Guests_Included",
                      "Review_Scores_Rating", "Cancellation_Policy")

airbnb1 <- na.omit(airbnb)

airbnb4 <- as.data.frame(airbnb1$Room_Type)
colnames(airbnb4) <- c("Room_Type")

#write_xlsx(airbnb1, "airbnb_paris.xlsx")

airbnb2 <- airbnb1 %>% select(-c(Room_Type, Property_Type, Cancellation_Policy))

airbnb3 <- airbnb1 %>% select(Property_Type, Cancellation_Policy, Cleaning_Fee)



x <- airbnb3 %>% group_by(Room_Type) %>% summarise(Conteo = n(), 
                                                   Porcentaje = round(n()/nrow(airbnb3)*100,2))

x2 <- airbnb3 %>% group_by(Property_Type) %>% summarise(Conteo = n(), 
                                                   Porcentaje = round(n()/nrow(airbnb3)*100,2))

x3 <- airbnb3 %>% group_by(Cancellation_Policy) %>% summarise(Conteo = n(), 
                                                   Porcentaje = round(n()/nrow(airbnb3)*100,2))

x4 <- airbnb3 %>% group_by(Cleaning_Fee) %>% summarise(Conteo = n(), 
                                                   Porcentaje = round(n()/nrow(airbnb3)*100,2))

# write_xlsx(x, "RoomType.xlsx")
# write_xlsx(x2, "PropType.xlsx")
# write_xlsx(x3, "CancelPo.xlsx")
# write_xlsx(x4, "CleanFee.xlsx")

# airbnb3$ID <- seq.int(nrow(airbnb3))
# airbnb3 <- airbnb3[,c(5,1:4)]
airbnb3$Property_Type <- as.factor(airbnb3$Property_Type)
airbnb3$Cancellation_Policy <- as.factor(airbnb3$Cancellation_Policy)
airbnb3$Cleaning_Fee <- as.factor(airbnb3$Cleaning_Fee)

airdummy1 <- data.table(dummy(airbnb3$Property_Type))
airdummy2 <- data.table(dummy(airbnb3$Cancellation_Policy))
airdummy3 <- data.table(dummy(airbnb3$Cleaning_Fee))

airdummy1 <- airdummy1[,c(1,2,9,14,16,17)]
airdummy2 <- airdummy2[,-4]

colnames(airdummy1) <- c("Apartment", "Bed & Breakfast", "Condominium",
                         "House", "Loft", "Other")
colnames(airdummy2) <- c("Cancelation: Flexible", "Cancelation: Moderate",
                         "Cancelation: Strict")
colnames(airdummy3) <- c("Cleaning Fee: No", "Cleaning Fee: Yes")

airbnb1 <- airbnb1[,-c(3,4,9,12)]

airbnb1 <- cbind.data.frame(airbnb4,airbnb1, airdummy1, airdummy2, airdummy3)
airbnb1 <- airbnb1[,-19]

#write_csv(airbnb1,"airbnb_final_eml.csv")

trial <- airbnb1[c(sample(x=30000, 800)),]
#write_csv(trial,"airbnb_trial_eml.csv")
