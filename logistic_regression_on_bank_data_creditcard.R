
# logistixc regression for bank credit card 

library(readr)
df_bank <- read_csv("BankCreditCard.csv")
View(df_bank)

str(df_bank)

summary(df_bank)


table(df_bank$Default_Payment)

df_bank$`Customer ID` = NULL

df_bank

cols.to.factors <- c("Gender", "Academic_Qualification","Marital", 
                     "Repayment_Status_Jan","Repayment_Status_Feb", 
                     "Repayment_Status_March","Repayment_Status_April", 
                     "Repayment_Status_May","Repayment_Status_June", 
                     "Default_Payment")


df_bank[cols.to.factors] <- lapply(df_bank[cols.to.factors], factor)
str(df_bank)

# converting gender into male and female

levels(df_bank$Gender)[levels(df_bank$Gender) == "1"] <- "Male" 
levels(df_bank$Gender)[levels(df_bank$Gender) == "2"] <- "Female"

str(df_bank)
head(df_bank)


# converting acadamic_qalification into othen ( 1,2,3,4,5,6)
# 1=Undergraduate, 2=Graduate, 3=Postgraduate, 4=Professional, 5=Others, 6=Unknown

levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "1"] <- "Undergraduate"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "2"] <- "Graduate"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "3"] <- "Postgraduate"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "4"] <- "Professional"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "5"] <- "Others"
levels(df_bank$Academic_Qualification)[levels(df_bank$Academic_Qualification) == "6"] <- "Unknown"

head(df_bank)
summary(df_bank)

str(df_bank)


# scaling of the data
# converting the Default_ payment column in factor

df_bank$Default_Payment <- factor(df_bank$Default_Payment, levels = c(0,1))
str(df_bank)

# converting martial into other (1,2, 3)
# 1 = single, 2 = married and 3 = others

levels(df_bank$Marital)[levels(df_bank$Marital) == "1"] <- "Single"
levels(df_bank$Marital)[levels(df_bank$Marital) == "2"] <- "Married"
levels(df_bank$Marital)[levels(df_bank$Marital) == "3"] <- "Others"

head(df_bank)
str(df_bank)
summary(unique(df_bank))

library(caTools)

set.seed(123)

split_dfbank <- sample.split(df_bank$Default_Payment, SplitRatio = 0.75)
train_dfbank <- subset(df_bank, split_dfbank == T)
test_dfbank <- subset(df_bank, split_dfbank == F)


# creating modal

modal_dfbank <- glm(formula = Default_Payment~., family = "binomial", data = train_dfbank)
summary(modal_dfbank)

# prediction 

prediction <- predict(modal_dfbank, test_dfbank, type = "response")
head(prediction)

str(test_dfbank$Default_Payment)


#  we can ifelse function use for if greater then 0.5 then eqaul to 1 and less then equal to 0

test_dfbank$probabilityAnswer <- ifelse(prediction <= 0.5, 0 , 1)

head(test_dfbank$probabilityAnswer)
unique(test_dfbank$probabilityAnswer)


# create the table

table(test_dfbank$Default_Payment, test_dfbank$probabilityAnswer)
table(test_dfbank$probabilityAnswer)

sum(5543 + 582)

# total accuracy is 6125 = 5543 + 582 (overall 7500)
# and this is called confussion matrix