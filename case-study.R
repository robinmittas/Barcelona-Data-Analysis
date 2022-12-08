library(ggplot2)
library(data.table)
library(magrittr) # Needed for %>% operator
library(tidyr)
require(dplyr) # needed to rename columns
library(grid) # to put two plots die by side


## This chunk of code and output do not need to be included in the pdf document so we set echo=FALSE and results='hide'
## PLS MAKE SURE TO SET THE WORKINGDIRECTORY
## Load the file(s)
births  <- fread("./data/births.csv")
population <- fread("./data/population.csv")
immigrants <- fread("./data/immigrants_by_nationality.csv")
immigrants_sex <- fread("./data/immigrants_emigrants_by_sex.csv")
unemployment <- fread("./data/unemployment.csv")
life_expect <- fread("./data/life_expectancy.csv")

# Rename some column names (easier for joining and further processing)
births = rename(births, "district_code"="District Code", "gender"="Gender", "year"="Year")
unemployment = rename(unemployment, "year" = "Year", "gender"="Gender", "district_code"="District Code")
population = rename(population, "year"="Year", "gender"="Gender", "age"="Age", "district_code"="District.Code")
immigrants = rename(immigrants, "year"="Year", "nationality"="Nationality", "district_code"="District Code")
immigrants_sex = rename(immigrants_sex, "year"="Year", "gender"="Gender", "district_code"="District Code")


# Aggregate DATA per District
unemployment <- unemployment[, .(Number=sum(Number)), by = c("year", "Month", "district_code", "District Name", "gender", "Demand_occupation")]

ggplot(unemployment[Demand_occupation=="Registered unemployed"], aes(gender, Number)) + geom_boxplot() + facet_wrap(~year)

ggplot(unemployment[Demand_occupation=="Registered unemployed", .(Number_Unemployed=sum(Number)), by = c("gender", "year")], aes(gender, Number_Unemployed)) + geom_bar(stat='identity') + facet_wrap(~year) +scale_y_continuous(labels = scales::comma)

# Calculate the difference per year in the gender groups

diff_gender_unemployed_per_year <- unemployment[Demand_occupation=="Registered unemployed", .(aggregated=sum(Number)), by = c("gender", "year")] 
diff_gender_unemployed_per_year <- dcast(diff_gender_unemployed_per_year, ... ~gender, value.var = "aggregated")
diff_gender_unemployed_per_year[, diff_gender_groups_female_minus_male := Female-Male]
diff_gender_unemployed_per_year = rename(diff_gender_unemployed_per_year, "male_unemployed"="Male", "female_unemployed"="Female")



ggplot(unemployment[Demand_occupation=="Registered unemployed", .(Number_Unemployed=sum(Number)), by = c("gender", "year")], aes(gender, Number_Unemployed)) + geom_bar(stat='identity') + facet_wrap(~year) +scale_y_continuous(labels = scales::comma) +labs( y = "Number Unemployed citizens", title = "1. Distribution of Unemployed citizens over all years") + theme(plot.title = element_text(size=10))
diff_gender_unemployed_per_year


ggplot(unemployment[Demand_occupation=="Registered unemployed"], aes(factor(district_code), Number, fill=gender)) + geom_boxplot() +
  labs(x = "District Code", y = "Number Unemployed citizens", title = "2. Distribution of Unemployed citizens within each district") + theme(plot.title = element_text(size=10))


#This is also supported by the difference in the medians of the male and female citizens over all years of unemployed citizens.
medians_gender <- unemployment[Demand_occupation=="Registered unemployed", median(Number, na.rm = T), by=gender]
diff_median_gender <- medians_gender[gender=="Female", V1] - medians_gender[gender=="Male", V1]
diff_median_gender 



ggplot(unemployment, aes(factor(unemployment$Month, levels = month.name), Number, fill=gender)) + geom_boxplot() +
  labs(x = "Month", y = "Number Unemployed citizens", title = "Distribution of Unemployed citizens within each month over all years") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(plot.title = element_text(size=10))


summer_months <- c("May", "June", "July", "August", "September")
unemployment[, summer_month:=Month %in% summer_months]
unemployment_summer <- unemployment[, .(number_unemployed=sum(Number)), by=c("summer_month", "year", "Month")]

ggplot(unemployment_summer, aes(summer_month, number_unemployed)) + geom_boxplot() + labs(x = "Summer Month", y = "Number Unemployed citizens", title = "3. Number of Unemployed People from 2013-2017 against summer months")+
  facet_wrap(~year) + scale_y_continuous(labels = scales::comma) + theme(plot.title = element_text(size=10))


t_stat_for_specific_year <- function(test_year){
  year_data <- unemployment_summer[year==test_year]
  t_statistic <- t.test(number_unemployed ~ summer_month, data=year_data)
  t_statistic
}

t_stat_for_specific_year(2017)

t_stat_for_specific_year(2016)
T_star <- sapply(1:N_permu, function(x){
  median_diff_summer(unemployment_summer[, Month := sample(Month), by=year]) })

ggplot(data.table(T_star = T_star), aes(T_star)) + geom_histogram(bins=30) +
  geom_vline(aes(xintercept=T_obs, color="T_ref")) + labs(title="4. Permutation Test on Summer months, metric: difference in medians") + xlim(-15000,15000) + theme(plot.title = element_text(size=10))



population <- population[, .(number_citizens=sum(Number)), by=c("district_code", "age", "year", "gender")]
population_total <- population[, .(number_citizens_total = sum(number_citizens)), by=c( "year", "district_code", "gender")]
population_workers <- population[age %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")]
population_workers <- population_workers[, .(number_workers_total = sum(number_citizens)), by=c("year", "district_code", "gender")]

population_youth <- population[age %in% c("15-19", "20-24", "25-29"), .(number_citizens_youth = sum(number_citizens)), by = c("year", "district_code", "gender")]

immigrants <- immigrants[nationality!="Spain", .(number_immigrants=sum(Number)), by=c("district_code", "year")]
immigrants_sex <- immigrants_sex[, .(number_immigrants=sum(Immigrants)), by=c("district_code", "year", "gender")]
unemployment <- unemployment[Demand_occupation=="Registered unemployed", .(number_unemployed=mean(Number)), by=c("year", "district_code", "gender")]
births <- births[, .(number_births=sum(Number)), by=c("district_code", "year", "gender")]

# Replace values for joins
births[gender=="Boys", "gender"] <- "Male"
births[gender=="Girls", "gender"] <- "Female"

unemployment <- merge(unemployment, population_total, by=c("district_code", "year", "gender"), all.x=T)
unemployment <- merge(unemployment, population_workers, by=c("district_code", "year", "gender"), all.x=T)
unemployment <- merge(unemployment, population_youth, by=c("district_code", "year", "gender"), all.x=T)
unemployment <- merge(unemployment, births, by=c("district_code", "year", "gender"), all.x=T)
unemployment <- merge(unemployment, immigrants_sex, by = c("district_code", "year", "gender"), all.x = T)

unemployment[, rate_unemployed:=number_unemployed/number_workers_total]
unemployment[, share_youth:=number_citizens_youth/number_workers_total]
unemployment[, share_immigrants:=number_immigrants/number_citizens_total]


ggplot(unemployment, aes(factor(district_code), rate_unemployed, fill=gender)) + geom_boxplot() +
  labs(x = "District Code", y=" Rate unemployed", title="5. Rate of unemployed people per district code and gender" ) + theme(plot.title = element_text(size=10))



#Initially, this plot could be interpreted as an indication that with an increasing number of births among births/immigrants/youth the number of unemployed citizens also rises.  
ggplot(unemployment, aes(number_births, number_unemployed)) + geom_point() + geom_smooth(method="lm")
ggplot(unemployment, aes(number_immigrants, number_unemployed)) + geom_point() + geom_smooth(method="lm")
ggplot(unemployment, aes(number_citizens_youth, number_unemployed)) + geom_point() + geom_smooth(method="lm")



ggplot(unemployment, aes(number_citizens_youth, number_unemployed)) + geom_point() + geom_smooth(method="lm") + labs(x="Number of Young citizens", y = "Number of unemployed Citizens", title = "6. Correlation of Number of young citizens/ unemployed citizens") + theme(plot.title = element_text(size=10))


ggplot(unemployment, aes(share_youth, rate_unemployed)) + geom_point() + labs( x= "Share youth", y= "Rate unemployed", title= "7. Correlation of rate of unemployment with share of youth")+
  geom_smooth(method="lm") + theme(plot.title = element_text(size=10))



ggplot(unemployment, aes(number_immigrants, number_unemployed)) + geom_point() + geom_smooth(method="lm") + labs(x="Number of Immigrants", y = "Number of unemployed Citizens", title = "8. Correlation of Number of Immigrants/ unemployed citizens") + theme(plot.title = element_text(size=10))


ggplot(unemployment, aes(share_immigrants, rate_unemployed)) + geom_point() + labs( x= "Share immigrants", y= "Rate unemployed", title= "9. Correlation of rate of unemployment with share of immigrants")+ geom_smooth(method="lm") + theme(plot.title = element_text(size=10))

cor.test(unemployment$share_youth, unemployment$rate_unemployed, method="spearman", exact=FALSE)$estimate
cor.test(unemployment$share_immigrants, unemployment$rate_unemployed, method="spearman", exact=FALSE)$estimate
cor.test(unemployment$number_births, unemployment$rate_unemployed, method="spearman", exact=FALSE)

unemployment[, male_indicator:=gender=="Male"]
t_statistic <- t.test(number_unemployed ~ gender, data=unemployment)
t_statistic$p.value


###
# drop na for linear reg
unemployment <- unemployment[!is.na(number_citizens_total),]

lin_formula <- as.formula("number_unemployed ~ number_citizens_total + number_workers_total + number_citizens_youth + number_immigrants")

linear_reg <- lm(lin_formula, data = unemployment)


unemployment[, predict_lin_reg := predict(linear_reg, newdata = unemployment)]
unemployment[, residuals := number_unemployed-predict_lin_reg]

ggplot(unemployment, aes(predict_lin_reg, residuals)) + geom_point() + labs( x= "Prediction", y= "Residuals", title= "10. Prediction-Resdiual Plot for linear regression Model")+ theme(plot.title = element_text(size=10))
ggplot(unemployment, aes(sample = residuals)) + geom_qq() + geom_qq_line() +  labs(title= "11. QQ-Plot of Residuals")+ theme(plot.title = element_text(size=10))



life_expect <- melt(life_expect, id.vars = c("Neighborhood", "Gender"), variable.name = "year", value.name = "life_expectancy")

#Let us make some tests
#For that we calculate the medians of both groups and their difference (= T_ref):
#First of all we define a function, which calculates the difference in medians:

median_diff <- function(dt){
  dt[Gender=="Female", median(life_expectancy, na.rm=T)] -
    dt[Gender=="Male", median(life_expectancy, na.rm=T)]
}

T_ref <- median_diff(life_expect)
#Now lets permute the data for n=1000
life_expect_copy <- copy(life_expect)



N_permu = 1000
T_star <- sapply(1:N_permu, function(x){
  median_diff(life_expect[, Gender := sample(Gender)]) })
#Lets plot our result

ggplot(data.table(T_star = T_star), aes(T_star)) + geom_histogram(bins=30) +
  geom_vline(aes(xintercept=T_ref, color="T_ref")) + labs(title = "13. Difference in medians after repeating permutation 1000 times") + theme(plot.title = element_text(size=10))
