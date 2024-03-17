# Mental Health Prediction in Teens

## Executive Summary
This project aims to develop a predictive model for mental health disorders in minors under 18, focusing on ADHD, Anxiety, and Depression. By analyzing demographic and medical characteristics, schools and districts can identify students at higher risk of developing these disorders, facilitating early intervention and resource allocation. The project utilized Client Level Data from SAMHSA and implemented various machine learning techniques for model training and evaluation.

## Project Background
Mental health disorders in children are increasingly prevalent and often overlooked. The project seeks to address this issue by enabling the implementation of school-based prevention programs. SAMHSA provided crucial data on demographics and mental health characteristics, guiding the development of predictive models.

## Data Description
### Data Cleaning/Preprocessing
The original dataset contained 6.9 million records and 40 categorical variables, which were refined to focus on minors and relevant predictors. Important variables related to age, education, ethnicity, race, and service records were retained, while irrelevant variables and mental health disorders were excluded. Null values were managed using a combination of rule-based and domain knowledge approaches.

### Important Predictors
1. AGE
2. EDUCATION
3. ETHNICITY
4. RACE
5. GENDER
6. SPHSERVICE
7. CMPSERVICE
8. OPISERVICE
9. RTCSERVICE
10. IJSSERVICE
11. SUB
12. SAP
13. SMISED

## Model Training and Evaluation
A variety of machine learning models, including logistic regression, decision trees, and gradient boosting, were trained and evaluated using precision and recall metrics. Random forests emerged as the best performer for predicting ADHD, Anxiety, and Depression.
