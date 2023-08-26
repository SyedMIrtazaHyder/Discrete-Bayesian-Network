#c)
#reading Excel File
library(readxl)
#saving data read from excel into dataframe
survey <- read_excel("C:\\Users\\drnaj\\OneDrive\\Desktop\\Bayesian Network\\Project 1\\survey.xlsx")
#Testing to see if data is read
head(survey, 5)
total_rows <- nrow(survey)

#d)
age <- factor(survey$A, levels = c("young", "adult", "old"))
sex <- factor(survey$S, levels = c('M', 'F'))
education <- factor(survey$E, levels = c("high", "uni"))
occupation <- factor(survey$O, levels = c("self", "emp"))
residence <- factor(survey$R, levels = c("small", "big"))
transport <- factor(survey$T, levels = c("car", "train", "other"))

#A
young <- length(which(age == "young"))/total_rows
adult <- length(which(age == "adult"))/total_rows
old <- 1 - young - adult
age_cpt <- c(young, adult, old)
names(age_cpt) <- levels(age)

#S
male <- length(which(sex == "M"))/total_rows
female <- 1 - male
sex_cpt <- c(male, female)
names(sex_cpt) <- levels(sex)

#E
#As A and S are independent based on BN, Joint Probabilites
high_given_young_male <- length(which(age == "young" & sex == "M" & education == "high"))/length(which(age == "young" & sex == "M"))
uni_given_young_male <- 1 - high_given_young_male
high_given_adult_male <- length(which(age == "adult" & sex == "M" & education == "high"))/length(which(age == "adult" & sex == "M"))
uni_given_adult_male <- 1 - high_given_adult_male
high_given_old_male <- length(which(age == "old" & sex == "M" & education == "high"))/length(which(age == "old" & sex == "M"))
uni_given_old_male <- 1 - high_given_old_male

high_given_young_female <- length(which(age == "young" & sex == "F" & education == "high"))/length(which(age == "young" & sex == "F"))
uni_given_young_female <- 1 - high_given_young_female
high_given_adult_female <- length(which(age == "adult" & sex == "F" & education == "high"))/length(which(age == "adult" & sex == "F"))
uni_given_adult_female <- 1 - high_given_adult_female
high_given_old_female <- length(which(age == "old" & sex == "F" & education == "high"))/length(which(age == "old" & sex == "F"))
uni_given_old_female <- 1 - high_given_old_female

combs <- expand.grid(level1 = levels(sex), level2 = levels(age))
education_cpt <- array(
  data = c(high_given_young_male, high_given_young_female,
           high_given_adult_male, high_given_adult_female,
           high_given_old_male, high_given_old_female,
           uni_given_young_male, uni_given_young_female,
           uni_given_adult_male, uni_given_adult_female,
           uni_given_old_male, uni_given_old_female),
  dim = c(6, 2),
  dimnames = list(
    paste(combs$level1, combs$level2, sep = " & "),
    levels(education)
  )
)

#R
small_given_high <- length(which(residence == "small" & education == "high"))/length(which(education == "high"))
big_given_high <- 1 - small_given_high
small_given_uni <- length(which(residence == "small" & education == "uni"))/length(which(education == "uni"))
big_given_uni <- 1 - small_given_uni

residence_cpt <- array(
  data = c(small_given_high, small_given_uni,
           big_given_high, big_given_uni),
  dim = c(2, 2),
  dimnames = list(
    levels(education),
    levels(residence)
  )
)

#O
self_given_high <- length(which(education == "high" & occupation == "self"))/length(which(education == "high"))
emp_given_high <- 1 - self_given_high
self_given_uni <- length(which(education == "uni" & occupation == "self"))/length(which(education == "uni"))
emp_given_uni <- 1 - self_given_uni

occupation_cpt <- array(
  data = c(self_given_high, self_given_uni,
           emp_given_high, emp_given_uni),
  dim = c(2, 2),
  dimnames = list(
    levels(education),
    levels(occupation)
  )
)

#T
car_given_self_and_small <- length(which(occupation == "self" & residence == "small" & transport == "car"))/length(which(occupation == "self" & residence == "small"))
train_given_self_and_small <- length(which(occupation == "self" & residence == "small" & transport == "train"))/length(which(occupation == "self" & residence == "small"))
other_given_self_and_small <- 1 - car_given_self_and_small - train_given_self_and_small

car_given_self_and_big <- length(which(occupation == "self" & residence == "big" & transport == "car"))/length(which(occupation == "self" & residence == "big"))
train_given_self_and_big <- length(which(occupation == "self" & residence == "big" & transport == "train"))/length(which(occupation == "self" & residence == "big"))
other_given_self_and_big <- 1 - car_given_self_and_big - train_given_self_and_big

car_given_emp_and_small <- length(which(occupation == "emp" & residence == "small" & transport == "car"))/length(which(occupation == "emp" & residence == "small"))
train_given_emp_and_small <- length(which(occupation == "emp" & residence == "small" & transport == "train"))/length(which(occupation == "emp" & residence == "small"))
other_given_emp_and_small <- 1 - car_given_emp_and_small - train_given_emp_and_small


car_given_emp_and_big <- length(which(occupation == "emp" & residence == "big" & transport == "car"))/length(which(occupation == "emp" & residence == "big"))
train_given_emp_and_big <- length(which(occupation == "emp" & residence == "big" & transport == "train"))/length(which(occupation == "emp" & residence == "big"))
other_given_emp_and_big <- 1 - car_given_emp_and_big - train_given_emp_and_big

combs2 <- expand.grid(level1 = levels(residence), level2 = levels(occupation))
transport_cpt <- array(
  data = c(car_given_self_and_small, car_given_self_and_big, car_given_emp_and_small, car_given_emp_and_big,
           train_given_self_and_small, train_given_self_and_big, train_given_emp_and_small, train_given_emp_and_big,
           other_given_self_and_small, other_given_self_and_big, other_given_emp_and_small, other_given_emp_and_big),
  dim = c(4, 3),
  dimnames = list(
    paste(combs2$level1, combs2$level2, sep = " & "),
    levels(transport)
  )
)

#e
#P(A = young , S = M, E = university , O = self-employed, R = big , T = car ) = 0.001611453
as.double(age_cpt[1] * sex_cpt[1] * education_cpt[7] * residence_cpt[4] * occupation_cpt[2] * transport_cpt[2])
#P(A = old, S = F , E = high school, O = employed, R = big , T = other ) = 0.009935257
as.double(age_cpt[3] * sex_cpt[2] * education_cpt[6] * residence_cpt[3] * occupation_cpt[3] * transport_cpt[12])
#P(A = adult, S = M, E = university , O = self-employed, R = small, T = train) = 0
as.double(age_cpt[2] * sex_cpt[1] * education_cpt[9] * residence_cpt[2] * occupation_cpt[2] * transport_cpt[5])
