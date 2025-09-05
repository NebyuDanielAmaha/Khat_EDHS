library(haven)
library(dplyr)
#read the data and filter out non-pregnant women
data <- read_dta("ETCR71FL.DTA") %>%  filter(v213 == 1)    
#Cleaning
#maternal age v013
# Collapse maternal age (v013) into 3 categories
data$maternal_age3 <- cut(data$v013,
                          breaks = c(0, 2, 4, 7),   # category cut points
                          labels = c("15-24", "25-34", "35-49"),
                          right = TRUE)

# Check distribution
table(data$maternal_age3)

# Residence
# Rename and convert v025 into a factor called residence
data$residence <- factor(data$v025,
                         levels = c(1, 2),
                         labels = c("Urban", "Rural"))


#religion
# Recode religion variable
data$religion4 <- NA
data$religion4[data$v130 == 1] <- "Orthodox"
data$religion4[data$v130 == 3] <- "Protestant"
data$religion4[data$v130 == 4] <- "Muslim"
data$religion4[data$v130 %in% c(2, 5, 96)] <- "Others"

# Convert to factor with set levels
data$religion4 <- factor(data$religion4,
                         levels = c("Orthodox", "Protestant", "Muslim", "Others"))

# Check results
table(data$religion4)

#religion with 2 categories
data$religion2 <- NA
data$religion2[data$v130 == 4] <- "Muslim"
data$religion2[data$v130 %in% c(1,2,3, 5, 96)] <- "Others"

# Convert to factor with set levels
data$religion2 <- factor(data$religion2,
                         levels = c("Muslim", "Others"))
# Check results
table(data$religion2)

#education
data$education3 <- NA
data$education3[data$v106 == 0] <- "No education"
data$education3[data$v106 == 1] <- "Primary"
data$education3[data$v106 %in% c(2, 3)] <- "Secondary+"

data$education3 <- factor(data$education3,
                          levels = c("No education", "Primary", "Secondary+"))

# Check distribution
table(data$education3)

#wealth
data$wealth5 <- factor(data$v190,
                       levels = 1:5,
                       labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

table(data$wealth5)

#wealth3
#wealth
data$wealth3 <- factor(
  ifelse(data$v190 %in% c(1, 2), "Poor",
         ifelse(data$v190 == 3, "Middle", "Rich")),
  levels = c("Poor", "Middle", "Rich")
)
table(data$wealth3)

#working status
data$working <- factor(data$v714,
                       levels = c(0, 1),
                       labels = c("No", "Yes"))

# Check distribution
table(data$working)

#mother chews khat
table(data$s1107a)

#mother drinks alcohol
data$drinks_alcohol <- factor(data$s1107c,
                       levels = c(0, 1),
                       labels = c("No", "Yes"))

#mother smokes cigarettes
data$smoking_status <- factor(data$v463a,
                              levels = c(0, 1),
                              labels = c("No", "Yes"))

#partner education
table(data$mv106)
# Manual recode husband's education into 3 levels
data$husband_edu <- NA
data$husband_edu[data$mv106 == 0] <- "No education"
data$husband_edu[data$mv106 == 1] <- "Primary"
data$husband_edu[data$mv106 %in% c(2, 3)] <- "Secondary+"

# Convert to factor with ordered levels
data$husband_edu <- factor(data$husband_edu,
                           levels = c("No education", "Primary", "Secondary+"))

# Check distribution
table(data$husband_edu)

#household chores
# Recode husband's help with household chores
data$husband_help <- NA
data$husband_help[data$s924a %in% c(0, 3)] <- "No"   # 0 = No, 3 = Not living = No
data$husband_help[data$s924a == 1] <- "Yes"          # 1 = Yes

# Convert to factor
data$husband_help <- factor(data$husband_help, levels = c("No", "Yes"))

# Check distribution
table(data$husband_help)

#husband drinks alcohol
data$husband_alcohol <- NA
data$husband_alcohol[data$sm815cc == 0] <- "No"
data$husband_alcohol[data$sm815cc == 1] <- "Yes"

# Convert to factor
data$husband_alcohol <- factor(data$husband_alcohol,
                               levels = c("No", "Yes"))

# Check distribution
table(data$husband_alcohol)


#husband chews khat
data$husband_chat_ever <- NA
data$husband_chat_ever[data$sm815aa == 0] <- "No"
data$husband_chat_ever[data$sm815aa == 1] <- "Yes"

# Convert to factor
data$husband_chat_ever <- factor(data$husband_chat_ever,
                                 levels = c("No", "Yes"))

# Check distribution
table(data$husband_chat_ever)

