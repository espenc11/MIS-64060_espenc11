mean(Cereals$calories)
mean(Cereals$sugars)
mean(Cereals$rating)
median(Cereals$rating)
sd(Cereals$calories)
sd(Cereals$sugars)
sd(Cereals$rating)
IQR(Cereals$calories)
IQR(Cereals$sugars)
IQR(Cereals$rating)

plot(Cereals$sugars,Cereals$rating)
plot(Cereals$sugars,Cereals$rating,xlab="Sugars",ylab="Rating",main="Cereal rating based on sugar content")

transform(Cereals$weight-1)
round(Cereals$rating, digits=0)

