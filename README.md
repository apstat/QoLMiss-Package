## QOLPackage
R package for calculating the domain-wise scale scores from Quality of Life questionnaire. There are three functions: qol, miss_qol and miss_patient takes input of the data set containing the answers of QOL questionnaire. It will compute the three types of domain based scale scores: Global, Functional, and Symptoms. In case of missing data, the miss_qol and miss_patient functions will make the required changes and then calculate the domain-wise scale scores. Finally, provide an output replacing the question columns with the domain-based scale scores in the original data set.

+ **qol(x)** - qol function first inputs a dataset containing the data of 30 questions from QOL questionnaire. It extracts only the columns named 'Q1','Q2',...,'Q30'.
+ **miss_qol(x)** - miss_qol function inputs a dataset containing missing information, represented as, 9 or 99 or NA. It extracts only the columns named 'Q1','Q2',...,'Q30' and replaces the missing data with the minimum value of the particular question.
+ **miss_patient(x)** - miss_patient function inputs a dataset in which the information of some patients are completely missing. The information of these patients are omitted from the data and only the columns named 'Q1','Q2',...,'Q30' are extracted.

x - A data frame with ID, Q1, Q2,..., Q30 columns along with other columns if data is available.

Using each of the 30 columns, the Raw Score is computed, and one column is obtained containing the Raw Score for each patient. Further, using each of the Raw Scores, three domain-based Scale Scores are computed, they are, Global Scales Score, Functional Scales Score and Symptoms Scales Score. Thus, the columns 'Q1','Q2',...,'Q30' are replaced by the domain-based scale scores, which is obtained as the output.
