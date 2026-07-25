# Number of people in the study 
n <- 1000
# create a patient ID 1:n means sequence 1:1000
Patient_ID <- 1:n
# put it into a data frame 
outbreak_data <- data.frame(Patient_ID)
head(outbreak_data)
str(outbreak_data)
dim(outbreak_data)
# Add Age to the data set 
outbreak_data$Age <- sample(1:90, size = n, replace = TRUE)
head(outbreak_data)
summary(outbreak_data)
hist(outbreak_data$Age, main = "Age Distribution of Participants", xlab = "Age (Years)",
     ylab = "Number of Participants",
     col = "skyblue",
     border = "black",)
# add gender to the dataset
outbreak_data$Gender <- sample(c("MALE","FEMALE"), n, replace = TRUE)
summary(outbreak_data)
head(outbreak_data)
# ADD state to the dataset 
States <- c("lagos","oyo","ogun","osun","kano","Delta","Enugu","Rivers","Abuja","Kaduna")
outbreak_data$State <- sample(States, n, replace = TRUE)
summary(outbreak_data)
head(outbreak_data)
table(outbreak_data$State)
outbreak_data$Ate_fish <- sample(c("YES","NO"), n, replace = TRUE, prob = c(0.40,0.60))
outbreak_data$Ate_Meat <- sample(c("YES","NO"), n, replace = TRUE, prob = c(0.65,0.35))
outbreak_data$Ate_Sea_food <- sample(c("YES","NO"), n, replace = TRUE, prob = c(0.30,0.70))
outbreak_data$Eggs <- sample(c("YES","NO"), n, replace = TRUE, prob = c(0.55,0.45))
outbreak_data$Raw_vegetable <- sample(c("YES","NO"), n, replace = TRUE, prob = c(0.45,0.55))
head(outbreak_data)
infection_prob = 0.50  # Seafood
infection_prob = 0.10  # No seafood
outbreak_data$infection_prob <- ifelse(outbreak_data$Ate_Sea_food == "YES", 0.80,0.02)
outbreak_data$infection_status <- ifelse(runif(n) < outbreak_data$infection_prob, "YES","NO")
names(outbreak_data)
#inspect the general dataset
head(outbreak_data)
tail(outbreak_data)
dim(outbreak_data)
str(outbreak_data)
summary(outbreak_data)
boxplot(outbreak_data$Age)
# table all the categorical data 
table(outbreak_data$Gender)
prop.table(table(outbreak_data$Gender))

table(outbreak_data$State)
prop.table(table(outbreak_data$State))

table(outbreak_data$Ate_fish)
prop.table(table(outbreak_data$Ate_fish))

table(outbreak_data$Ate_Meat)
prop.table(table(outbreak_data$Ate_Meat))

table(outbreak_data$Ate_Sea_food)
prop.table(table(outbreak_data$Ate_Sea_food))

table(outbreak_data$Ate_Eggs)
prop.table(table(outbreak_data$Eggs))

table(outbreak_data$Raw_vegetable)
prop.table(table(outbreak_data$Raw_vegetable))

table(outbreak_data$infection_status)
prop.table(table(outbreak_data$infection_status))
#plot a chart
barplot(table(outbreak_data$Gender))
barplot(table(outbreak_data$State))

barplot(table(outbreak_data$infection_status))
# check for missing values
colSums(is.na(outbreak_data))
# make tables for bivariate analysis
seafood_table <- table(outbreak_data$Ate_Sea_food,outbreak_data$infection_status)
prop.table(seafood_table, margin = 1)
barplot(seafood_table,
        beside = TRUE,
        legend = TRUE,
        col = c("skyblue","tomato"))
chisq.test(seafood_table)
meat_table <- table(outbreak_data$Ate_Meat,outbreak_data$infection_status)
chisq.test(meat_table)
# Fish
fish_table <- table(outbreak_data$Ate_fish, outbreak_data$infection_status)
chisq.test(fish_table)
# Eggs
egg_table <- table(outbreak_data$Eggs,outbreak_data$infection_status)
chisq.test(egg_table)

# Raw Vegetables
veg_table <- table(outbreak_data$Raw_vegetable,outbreak_data$infection_status)
chisq.test(veg_table)
# Sex
sex_table <- table(outbreak_data$Gender, outbreak_data$infection_status)
chisq.test(sex_table)
# State
state_table <- table(outbreak_data$State, outbreak_data$infection_status)
chisq.test(state_table)
# Age(this is ttest bcos age is numerical variable)
t.test(Age ~ infection_status,
       data = outbreak_data)
# multivariate analysis
table(outbreak_data$infection_status)
str(outbreak_data$infection_status)
outbreak_data$infection_status <- factor(outbreak_data$infection_status,levels = c("NO", "YES"))
model <- glm(infection_status ~ Age + Gender + State + Ate_Sea_food + Ate_Meat + Ate_fish + Eggs + Raw_vegetable,
  family = binomial,data = outbreak_data)
summary(model)
exp(coef(model))
exp(cbind(Odds_Ratio = coef(model), confint(model)))
getwd()

# CREATE SOME CHart
barplot(table(outbreak_data$Ate_Sea_food,
              outbreak_data$infection_status),
        beside = TRUE,
        legend = TRUE,
        main = "Seafood Consumption by Infection Status",
        xlab = "Seafood Consumption",
        ylab = "Number of Participants",
        col = c("tomato", "steelblue"))
barplot(table(outbreak_data$State),
        main = "Distribution of Participants by State",
        xlab = "State",
        ylab = "Number of Participants",
        col = "darkgreen",
        las = 2)
barplot(table(outbreak_data$Gender),
        main = "Gender Distribution of Participants",
        xlab = "Gender",
        ylab = "Number of Participants",
        col = c("steelblue", "orange"))
png("Infection_Status.png", width = 800, height = 600)

barplot(table(outbreak_data$infection_status),
        main = "Distribution of Infection Status",
        xlab = "Infection Status",
        ylab = "Number of Participants",
        col = c("tomato", "steelblue"),
        border = "black")
png("Infection_Status.png", width = 800, height = 600)
summary(outbreak_data)
sd(outbreak_data$Age,na.rm = FALSE )
