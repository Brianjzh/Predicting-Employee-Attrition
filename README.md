# Employee Attrition Analysis

## About
This project uses logistic regression models to analyze and predict employee attrition. The analysis evaluates the impact of various factors such as age, job satisfaction, and marital status on attrition, using metrics like AUC, precision, and recall to compare model performance.

---

## Tools & Technologies
- **Programming Language**: R
- **Libraries**:
  - Data manipulation and visualization: `tidyverse`, `GGally`
  - Statistical modeling and diagnostics: `caret`, `broom`
  - ROC and AUC metrics: `pROC`
  - Marginal effects: `margins`
  - Data reading: `readxl`

---

## Features
### **1. Data Preparation**
- Cleaned and processed data to handle missing values and appropriate data types.
- Created categorical variables and partitioned the dataset into training, validation, and test sets.

### **2. Logistic Regression Models**
- Built multiple logistic regression models:
  - **Model 1**: Age as a predictor.
  - **Model 2**: Age + Gender.
  - **Model 3**: Age + Gender + JobSatisfaction.
  - **Model 4**: Interaction model with Age, Gender, JobSatisfaction, and Income.
- Compared models using AIC, confusion matrices, and ROC curves.

### **3. Marginal Effects Analysis**
- Assessed the marginal effects of key predictors (e.g., Age) on attrition probability.
- Generated visualizations for marginal effects.

### **4. Model Evaluation**
- Evaluated models using:
  - Confusion matrices for precision, recall, and overall accuracy.
  - ROC curves and AUC values to assess discriminatory power.
- Compared models on validation and test sets.

---

## Results
### **Key Insights**
- **Significant Predictors**:
  - Age: Younger employees are more likely to leave.
  - JobSatisfaction: Higher satisfaction reduces attrition probability.
  - Marital Status and Business Travel: Significant impact on attrition.
- **Non-Significant Predictors**:
  - Income, Gender, DistanceFromHome, and Education had no significant impact.
- **Best Model**: Model 3 (Age + Gender + JobSatisfaction) had the highest AUC value on test data (0.6497).

### **Evaluation Metrics**
| Model                          | AUC (Test Data) | Precision | Recall  |
|--------------------------------|-----------------|-----------|---------|
| Model 1: Age                   | 0.608           | X         | X       |
| Model 2: Age + Gender          | 0.602           | X         | X       |
| Model 3: Age + Gender + JobSatisfaction | 0.632           | X         | X       |
| Model 4: Interaction Model     | 0.646           | X         | X       |

---

## Sample Visualization
- **ROC Curves for All Models**:
  ![ROC Curves](AUC)


---

## How to Use
1. Clone the repository:
   ```bash
   git clone https://github.com/username/employee-attrition-analysis.git
