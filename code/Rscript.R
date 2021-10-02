install.packages("readxl")
library(readxl)
data<-read_excel("C:/Users/ERICK/Downloads/hotel_bookings_miss.xlsx",col_names = TRUE)
View(data)
names(data)
str(data)
summary(data)
sapply(data,anyNA)
data_sin_NA<-na.omit(data)
View(data_sin_NA)

fix_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x  
}

boxplot(data_sin_NA$children)
sin_outliers<-fix_outliers(data_sin_NA$children)
par(mfrow = c(1,2))
boxplot(data_sin_NA$children, main = "Children con Outliers")
boxplot(sin_outliers,main="Children sin Outliers")

boxplot(data_sin_NA$arrival_date_year)
sin_outliers<-fix_outliers(data_sin_NA$arrival_date_year)
par(mfrow = c(1,2))
boxplot(data_sin_NA$arrival_date_year, main = "Arrival date year con Outliers")
boxplot(sin_outliers,main="Arrival date year sin Outliers")

boxplot(data_sin_NA$adr)
sin_outliers_adr<-fix_outliers(data_sin_NA$adr)
par(mfrow = c(1,2))
boxplot(data_sin_NA$adr, main = "adr con Outliers")
boxplot(sin_outliers_adr,main="adr sin Outliers")

boxplot(data_sin_NA$total_of_special_requests)
sin_outliers<-fix_outliers(data_sin_NA$total_of_special_requests)
par(mfrow = c(1,2))
boxplot(data_sin_NA$total_of_special_requests, main = "total of special request con Outliers")
boxplot(sin_outliers,main="total of special request sin Outliers")

boxplot(data_sin_NA$is_repeated_guest)
sin_outliers<-fix_outliers(data_sin_NA$is_repeated_guest)
par(mfrow = c(1,2))
boxplot(data_sin_NA$is_repeated_guest, main = "is repeated guest con Outliers")
boxplot(sin_outliers,main="is repeated guest sin Outliers")

boxplot(data_sin_NA$is_canceled)
sin_outliers<-fix_outliers(data_sin_NA$is_canceled)
par(mfrow = c(1,2))
boxplot(data_sin_NA$is_canceled, main = "is canceled con Outliers")
boxplot(sin_outliers,main="is canceled guest sin Outliers")

boxplot(data_sin_NA$is_canceled)
sin_outliers<-fix_outliers(data_sin_NA$is_canceled)
par(mfrow = c(1,2))
boxplot(data_sin_NA$is_canceled, main = "is canceled con Outliers")
boxplot(sin_outliers,main="is canceled guest sin Outliers")

boxplot(data_sin_NA$arrival_date_week_number)
sin_outliers<-fix_outliers(data_sin_NA$arrival_date_week_number)
par(mfrow = c(1,2))
boxplot(data_sin_NA$arrival_date_week_number, main = "arrival date week number con Outliers")
boxplot(sin_outliers,main="arrival date week number sin Outliers")

boxplot(data_sin_NA$arrival_date_day_of_month)
sin_outliers<-fix_outliers(data_sin_NA$arrival_date_day_of_month)
par(mfrow = c(1,2))
boxplot(data_sin_NA$arrival_date_day_of_month, main = "arrival_date_day_of_month con Outliers")
boxplot(sin_outliers,main="arrival_date_day_of_month guest sin Outliers")

boxplot(data_sin_NA$previous_cancellations)
sin_outliers<-fix_outliers(data_sin_NA$previous_cancellations)
par(mfrow = c(1,2))
boxplot(data_sin_NA$previous_cancellations, main = "previous_cancellations con Outliers")
boxplot(sin_outliers,main="previous_cancellations guest sin Outliers")

boxplot(data_sin_NA$previous_bookings_not_canceled)
sin_outliers<-fix_outliers(data_sin_NA$previous_bookings_not_canceled)
par(mfrow = c(1,2))
boxplot(data_sin_NA$previous_bookings_not_canceled, main = "previous_bookings_not_canceled con Outliers")
boxplot(sin_outliers,main="previous_bookings_not_canceled guest sin Outliers")

boxplot(data_sin_NA$days_in_waiting_list)
sin_outliers<-fix_outliers(data_sin_NA$days_in_waiting_list)
par(mfrow = c(1,2))
boxplot(data_sin_NA$days_in_waiting_list, main = "days_in_waiting_list con Outliers")
boxplot(sin_outliers,main="days_in_waiting_list guest sin Outliers")

data_limpia<-data_sin_NA
write.csv(data_limpia,"dataSetLimpia.csv",na="NA",row.names=FALSE)

library(dplyr)
hotel_reserva<-filter(data_limpia,is_canceled==0)
View(hotel_reserva)
count_Hotel=count(hotel_reserva,hotel_reserva$hotel)

library(ggplot2)
ggplot(count_Hotel,aes(x=count_Hotel$`hotel_reserva$hotel`,y=count_Hotel$n))+
  geom_bar(stat="identity",fill="blue")+labs(x="HOTELES REGISTRADOS",y="NUMERO TOTAL DE RESERVAS")+
  ggtitle("RESERVAS CONFIRMADAS POR HOTEL")

count_Hotel_Tiempo = count(hotel_reserva,hotel_reserva$hotel,hotel_reserva$arrival_date_year) 
ggplot(count_Hotel_Tiempo,aes(x=count_Hotel_Tiempo$`hotel_reserva$arrival_date_year`,y=count_Hotel_Tiempo$n))+
  geom_line(aes(color=count_Hotel_Tiempo$`hotel_reserva$hotel`,linetype=count_Hotel_Tiempo$`hotel_reserva$hotel`))+
  labs(x="AÑOS REGISTRADOS",y="TOTAL RESERVAS REGISTRADAS")+
  ggtitle("RESERVAS ANUALES REALIZADAS")

hotel_meses<-data.frame(table(data_limpia$arrival_date_month))
hotel_meses$Var1=factor(hotel_meses$Var1,levels = month.name)
hotel_meses$Demanda<-cut(hotel_meses$Freq,breaks = 3,labels = c("Baja","Media","Alta"))
ggplot(data=hotel_meses,aes(x=hotel_meses$Var1,y=hotel_meses$Freq,fill=Demanda))+
  geom_bar(stat="identity")+
  labs(x="MES",y="CANTIDAD DE RESERVAS")+
  ggtitle("CANTIDAD DE RESERVAS POR MESES CON TEMPORADAS DE DEMANDA")


table(data_limpia$arrival_date_month)
meses<-data.frame(M= table(data_limpia$arrival_date_month))
meses<-meses[order(meses$M.Freq),]
barplot(meses$M.Freq,xlab = "MESES",ylab="NUMERO TOTAL DE RESERVAS",main="DISTRIBUCION POR MES DE DEMANDA DE RESERVAS",names.arg = meses$M.Var1)

data_limpia$niñosYbebes=(data_limpia$babies>0|data_limpia$children>0)        
graf=table(data_limpia$niñosYbebes)
options(scipen=50)
barplot(graf,col=c("red","blue"),legend=c("sin bebes y/o niños","con bebes y/o niños"),main = "Reservaciones con niños y/o bebes")

        