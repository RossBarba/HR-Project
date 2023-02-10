install.packages("mice")
install.packages("VIM")
install.packages("PRROC")

library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(mice)
library(VIM)
library(ROCR)
library(PRROC)

data <- IMB553_XLS_ENG


#1
#a) How many rows and columns is the data comprised of?
str(data)
#There are 17 columns and 12,333 rows

#b) Are there missing values in the data? Summarize the number of missing values by variable.
data %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))

#There are 2,719 missing values in the 'Duration to accept offer' column
#There are 747 missing values in the 'percent hike expected in CTC' column
#There are 596 missing values in the 'percent hike offered in CTC' column
#There are 851 missing values in the 'percent difference CTC' column
#The rest of the columns do not have any missing values

#Since there is a large mass of missing values, we are using the mice package to 
#inpute data with plausible values to the four columns.
md.pattern(data)
data <- data %>%
  rename(candidate_ref = `Candidate Ref`, 
         doj_extended = `DOJ Extended`,
         duration_to_accept_offer = `Duration to accept offer`,
         notice_period = `Notice period`,
         offered_band = `Offered band`,
         percent_hike_expected_in_ctc = `Pecent hike expected in CTC`,
         percent_hike_offered_in_ctc = `Percent hike offered in CTC`,
         percent_difference_ctc = `Percent difference CTC`,
         joining_bonus = `Joining Bonus`,
         candidate_relocation_actual = `Candidate relocate actual`,
         gender = `Gender`,
         candidate_source = `Candidate Source`,
         rex_in_yrs = `Rex in Yrs`,
         lob = `LOB`,
         location = `Location`,
         age = `Age`,
         status = `Status`)

data2 <- mice(data, m=5, maxit=50, meth="pmm", seed=500)
data3 <- complete(data2, 1)


#c) Are there any outliers in the data? Explain your outlier identification process and summarize the
#number of outliers by variable.

#now we check the distribution of relevant variables
summary(data3$duration_to_accept_offer)
hist(data3$duration_to_accept_offer)
#it looks like someone accidentally inputed a negative waiting duration, so we will eliminate this entry
#There is also some outliers on the higher end, so we will also filter that
data4 <- data3
data4 <- data3 %>%
  filter(duration_to_accept_offer >= 0, duration_to_accept_offer < 150)
#this eliminated 48 outliers
summary(data4$duration_to_accept_offer)
hist(data4$duration_to_accept_offer)

#Given the non normal distribution of many of the numeric variables, we feel it is
#not advisable to use Tukeys method because it would get rid of thousands of observations.
#We will just examine histograms for the rest of the numeric variables besides percent difference ctc

summary(data4$notice_period)
hist(data4$notice_period)
#There are many observations with a notice period of 120 days. Given the frequency of that notice period,
#we feel that it is relevant to the case and not by error

summary(data4$percent_hike_expected_in_ctc)
hist(data4$percent_hike_expected_in_ctc)
#There appears to be outliers on the higher end. We will use 200 as a cuttoff point
data4 <- data4 %>%
  filter(percent_hike_expected_in_ctc <= 200)
#This got rid of 42 outliers
summary(data4$percent_hike_expected_in_ctc)
hist(data4$percent_hike_expected_in_ctc)

summary(data4$percent_hike_offered_in_ctc)
hist(data4$percent_hike_offered_in_ctc)
#We are going to use 200 as a cutoff here as well
data4 <- data4 %>%
  filter(percent_hike_offered_in_ctc <= 200)
#This got rid of 65 outliers
summary(data4$percent_hike_offered_in_ctc)
hist(data4$percent_hike_offered_in_ctc)

summary(data4$rex_in_yrs)
hist(data4$rex_in_yrs)
#We do not want to get rid of any of the far observations for rex years. We believe
#that there are valuable insights to be gained from the more experienced individuals
#and that these observations are not by error

summary(data4$age)
hist(data4$age)
#We do not want to eliminate entries based on age either for the same reasoning as rex years

#Now we examine the frequency of the various categorical variables to see if there is anything atypical
offered_band_means <- data4 %>% group_by(offered_band) %>% summarize(freq = n())
#Even though there is a very low frequency of observations with the bands e4 - e6, we still feel
#that those entries are relevant and not done by error

candidate_source_means <- data4 %>% group_by(candidate_source) %>% summarize(freq = n())
lob_means <- data4 %>% group_by(lob) %>% summarize(freq = n())
location_means <- data4 %>% group_by(location) %>% summarize(freq = n())
#nothing looks strange here, so we get rid of no entries

#d) Is there a relationship between whether a candidate asked for an extension in date of joining and whether they actually joined?
chisq.test(data4$doj_extended, data4$status)
#it appears that there is a relationship between whether a candidate asked for an 
#extension in date of joining and whether they actually joined. This relationship is
#significant far beyond the alpha value of 0.05

#e) Create a variable called difference in hike expectation as the difference between the expected percentage hike and the percentage hike offered by the company.
#Is there a relationship between whether a candidate actually joined and the difference in hike expectation received by the candidate?
data4 <- data4 %>%
  mutate(difference_in_hike_expectation = percent_hike_expected_in_ctc - percent_hike_offered_in_ctc)
data4$status <- as.factor(data4$status)
t.test(data4$difference_in_hike_expectation ~ data4$status, var.equal = TRUE)
#It does appear that there is a relationship between the hike difference and whether or not a person
#actually joined the company, but it is not a large one. People who did not end up joining the
#company had an average 9.73% difference while the ones who joined had an average of 3.69%
###################################################################################################
#2)
set.seed(123)
train_index <- sample(2, 12178, prob = c(0.7, 0.3), replace = TRUE)

train <- data4[train_index == 1, ]
test <- data4[train_index == 2, ]
summary(train)
#There are 6016 people who joined and 2479 who did not join
summary(test)
#There are 2598 people who joined and 1085 who did not join

#They are nearly identical. In the training data, 71% ended up joining and 29% did not join.
#In the testing data, 70% joined and 30% did not join

##################################################################################################
#3)
#a)
tree_equation <- status ~ . - candidate_ref - percent_difference_ctc
tree <- rpart(tree_equation, data = train)
rpart.plot(tree)
tree

#b)
tree2 <- rpart(tree_equation, data = train, control = rpart.control(minbucket= 500))

#c)
rpart.plot(tree2)

#d)
trainpreds <- predict(tree2, train, type = "prob")[,2]
testpreds <- predict(tree2, test, type = "prob")[,2]

trainweights <- as.numeric(train$status) - 1
testweights <- as.numeric(test$status) - 1

roc_train <- roc.curve(scores.class0 = trainpreds,
                       weights.class0 = trainweights,
                       curve = T)

prcurve_train <- pr.curve(scores.class0 = trainpreds,
                          weights.class0 = trainweights,
                          curve = T)


roc_test <- roc.curve(scores.class0 = testpreds,
                       weights.class0 = testweights,
                       curve = T)

prcurve_test <- pr.curve(scores.class0 = testpreds,
                          weights.class0 = testweights,
                          curve = T)

plot(roc_train)
plot(prcurve_train)
plot(roc_test)
plot(prcurve_test)
#e)
trainpreds_binary <- ifelse(trainpreds > 0.6, 1, 0)
testpreds_binary <- ifelse(testpreds > 0.6, 1, 0)

confusion_matrix <- table(actual = train$status, predicted = trainpreds_binary)
cm_tree <- confusion_matrix

recall <- confusion_matrix[2,2] / (confusion_matrix[2,1] + confusion_matrix[2,2])
precision <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])

#f)
rpart.plot(tree2)
#################################################################################
#4)
#a)
glm_model <- glm(tree_equation, data = train, family = "binomial")
summary(glm_model)

#b)
testpreds <- predict(glm_model, test, type = "response")
trainpreds  <- glm_model$fitted.values

roc_train <- roc.curve(scores.class0 = trainpreds,
                       weights.class0 = trainweights,
                       curve = T)

prcurve_train <- pr.curve(scores.class0 = trainpreds,
                          weights.class0 = trainweights,
                          curve = T)

roc_test <- roc.curve(scores.class0 = testpreds,
                      weights.class0 = testweights,
                      curve = T)

prcurve_test <- pr.curve(scores.class0 = testpreds,
                         weights.class0 = testweights,
                         curve = T)

plot(roc_train)
plot(prcurve_train)
plot(roc_test)
plot(prcurve_test)

#c)
trainpreds_binary <- ifelse(trainpreds > 0.6, 1, 0)
testpreds_binary <- ifelse(testpreds > 0.6, 1, 0)

confusion_matrix <- table(actual = train$status, predicted = trainpreds_binary)
cm_glm <- confusion_matrix

recall <- confusion_matrix[2,2] / (confusion_matrix[2,1] + confusion_matrix[2,2])
precision <- confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])

cm_glm
cm_tree
