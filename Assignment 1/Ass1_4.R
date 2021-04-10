df = read.table("owid-covid-data.csv", sep=";",
                header = TRUE, stringsAsFactors = FALSE)

df$date = as.Date(df$date, "%d-%m-%y")

dec = subset(df, df$date == as.Date("20-12-2020", "%d-%m-%y") &
            df$continent == "Europe", 
            select = c(iso_code, new_cases_smoothed_per_million))
jan = subset(df, df$date == as.Date("20-01-2021", "%d-%m-%y") &
            df$continent == "Europe", 
            select = c(iso_code, new_cases_smoothed_per_million))

dec$new_cases_smoothed_per_million = 
  as.numeric(gsub(",", ".", dec$new_cases_smoothed_per_million))
jan$new_cases_smoothed_per_million = 
  as.numeric(gsub(",", ".", jan$new_cases_smoothed_per_million))

dec_cases = dec$new_cases_smoothed_per_million
jan_cases = jan$new_cases_smoothed_per_million

summary(dec_cases)
sd(dec_cases)

summary(jan_cases)
sd(jan_cases)

par(mfrow=c(1,2))

plot(sort(dec_cases), main="Plot of sorted dec_cases vs. countries",
     xlab="Sorted index of country", ylab="dec_cases")
plot(sort(jan_cases), main="Plot of sorted jan_cases vs. countries",
     xlab="Sorted index of country", ylab="jan_cases")

hist(dec_cases, ylim=c(0,25))
hist(jan_cases, ylim=c(0,25))

boxplot(dec_cases, main="Boxplot of dec_cases", ylab="dec_cases", ylim=c(0,1050))
boxplot(jan_cases, main="Boxplot of jan_cases", ylab="jan_cases", ylim=c(0,1050))

par(mfrow=c(1,1))

plot(dec_cases, jan_cases)

cor(dec_cases, jan_cases)
cor(dec_cases, jan_cases, method="spearman")
cor(dec_cases, jan_cases, method="kendall")
