library(readxl)
library(ggplot)
library(arules)

# Dedomena askisis: https://drive.google.com/file/d/13eNcxcH99Amu3tFqXdpzrybo0UQGXGLS/view
# Ekfonisi askisis: https://docs.google.com/document/d/1jSWLjNt5yBwDC1-5v-DjamPV8Qrxk-hJ1w4JLsPZ6QM/edit
#I just named every dataframe I read 'op', don't know why
op <- read_excel("ODEP_Exam_Project_Data.xlsx")

# BIMA 1
op1 <- op[op$course==c("C4") & op$year == 2018 & op$exam == 'S1',]

# BIMA 2
summary(op1$grade)

# BIMA 3
op2 <- subset(op1, op1$grade >= 5 )
grades <- op2$grade


# This function adds d_value column to a data frame
d_grading <- function(op, grades) {
  grade_values = c('Failed', 'Medium', 'Good', 'Very Good', 'Excellent')
  d_grades <- factor(grade_values)
  d_grade <- c()
  for (grade in grades) {
    if (grade < 5) d_grade <- c(d_grade, grade_values[1])
    else if (grade>=5 && grade < 6.25) d_grade <- c(d_grade, grade_values[2])
    else if (grade>= 6.25 && grade <7.5) d_grade <- c(d_grade, grade_values[3])
    else if (grade>= 7.5 && grade <8.75) d_grade <- c(d_grade, grade_values[4])
    else d_grade <- c(d_grade, grade_values[5])
  }
  op <- cbind(op, d_grade)
  return(op)
}

op2 <- d_grading(op2,op2$grade)
op2

# BIMA 4
op3 <- op[op$year == 2018 & op$exam == 'S1',]
op3 <- d_grading(op3,op3$grade)
op3$course <- as.factor(op3$course) 

prop.table(table(op3$course, op3$d_grade))*100

# BIMA 5
op4 = op3[op3$grade >= 5, ]

# BAR PLOT
ggplot(op4, aes(x=course, fill=d_grade)) + geom_bar()

#PIE PLOT
pp <- ggplot(op4, aes(x=course, fill=d_grade)) + geom_bar(position='fill') + ylab("proportions")
pp + coord_polar("y", start = 0)

#BIMA 6
rm(list=ls())

# This function adds d_value column to a data frame
d_grading <- function(op, grades) {
  grade_values = c('Failed', 'Medium', 'Good', 'Very Good', 'Excellent')
  d_grades <- factor(grade_values)
  d_grade <- c()
  for (grade in grades) {
    if (grade < 5) d_grade <- c(d_grade, grade_values[1])
    else if (grade>=5 && grade < 6.25) d_grade <- c(d_grade, grade_values[2])
    else if (grade>= 6.25 && grade <7.5) d_grade <- c(d_grade, grade_values[3])
    else if (grade>= 7.5 && grade <8.75) d_grade <- c(d_grade, grade_values[4])
    else d_grade <- c(d_grade, grade_values[5])
  }
  op <- cbind(op, d_grade)
  return(op)
}

Data <- read_excel("ODEP_Exam_Project_Data.xlsx")
Data_passed <- subset(Data, Data$grade >= 5)
Data_passed_slim <-Data_passed[c("student","course","grade")]
Data_passed_slim <- d_grading(Data_passed_slim, Data_passed_slim$grade)
Data_passed_slim$has <- paste(Data_passed_slim$course, "-", Data_passed_slim$d_grade)
write.csv(Data_passed_slim[c("student","has")], 'united_data_passed.csv' )

# BIMA 7
my_transactions <- my_transactions <- read.transactions(
  file = "united_data_passed.csv",
  sep = ",",
  format="single",
  cols=c("student","has"),
  header=TRUE
)

# BIMA 8
#get frequency of course count
frequency<-summary(my_transactions)@lengths
frequency<-as.data.frame(frequency)

#plot it out
ggplot(data=frequency, aes(x=sizes, y=Freq)) +
  geom_bar(stat="identity")

# BIMA 9
itemfr = itemFrequency(my_transactions,type='relative')
itemfr = sort(itemfr,decreasing = TRUE)[1:5]
fr_dataframe <- as.data.frame(itemfr5)
x_axis<-reorder(rownames(fr_dataframe), -itemfr5)
ggplot(fr_dataframe,aes(x=x_axis,y=itemfr))+geom_bar(stat='identity')

# BIMA 10
rules <- apriori(my_transactions, parameter=list(supp=0.05, conf=.63, minlen=2, 
                                                 target='rules'))
inspect(sort(rules, by="lift")[1:10])

# BIMA 11
reduntant_rules <- is.redundant(rules)
r_rules <- rules[reduntant_rules]
r_rules
nr_rules <- rules[!reduntant_rules]

# BIMA 12
inspect(sort(nr_rules, by="lift")[1:10])
selected <- subset(nr_rules, rhs %in% "C10 - Good" | rhs %in% "C10 - Very Good")
inspect(sort(selected, by="lift"))
selected <- subset(nr_rules, lhs %pin% "Excellent" | rhs %pin% "Excellent") 
inspect(sort(selected, by="lift"))

# BIMA 14
selected <- subset(nr_rules,(
                       (lhs %in%  'C4 - Very Good' | lhs %in%  'C4 - Excellent') 
                       | (lhs %in%  'C6 - Very Good' | lhs %in%  'C6 - Excellent')
                        )
                      & (rhs %pin%  'Very Good' | rhs %pin%  'Excellent'))
rules_with_conviction <- cbind(as(selected, "data.frame"), conviction=interestMeasure(selected, "conviction", trans))
please <- rules_with_conviction[order(rules_with_conviction$conviction,decreasing = TRUE),]
View(please)