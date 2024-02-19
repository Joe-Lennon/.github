# -*- coding: utf-8 -*-
"""
Created on Thu Feb  1 14:19:43 2024

@author: joeml
"""

# Import necessary libraries
from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import train_test_split
from sklearn.ensemble import BaggingClassifier, GradientBoostingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score

# Load dataset
data = load_breast_cancer()
X = data.data
y = data.target

# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Bagging with Decision Trees
bagging_clf = BaggingClassifier(
    base_estimator=DecisionTreeClassifier(),
    n_estimators=100,
    max_samples=0.5,  # Max samples for each base estimator
    max_features=0.5,  # Max features for each base estimator
    random_state=42
)
bagging_clf.fit(X_train, y_train)
bagging_train_acc = accuracy_score(y_train, bagging_clf.predict(X_train))
bagging_test_acc = accuracy_score(y_test, bagging_clf.predict(X_test))

print("Bagging with Decision Trees")
print(f"Training Accuracy: {bagging_train_acc:.2f}")
print(f"Testing Accuracy: {bagging_test_acc:.2f}")

# Boosting with Decision Trees
boosting_clf = GradientBoostingClassifier(
    n_estimators=100,
    learning_rate=0.1,
    random_state=42
)
boosting_clf.fit(X_train, y_train)
boosting_train_acc = accuracy_score(y_train, boosting_clf.predict(X_train))
boosting_test_acc = accuracy_score(y_test, boosting_clf.predict(X_test))

print("\nBoosting with Decision Trees")
print(f"Training Accuracy: {boosting_train_acc:.2f}")
print(f"Testing Accuracy: {boosting_test_acc:.2f}")



###
# Import necessary libraries
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier
from sklearn.metrics import accuracy_score

# Load Titanic dataset
titanic_data = pd.read_csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

# Data preprocessing
# Drop columns that are not relevant for prediction
titanic_data = titanic_data.drop(['Name', 'Ticket', 'Cabin', 'Embarked'], axis=1)
# Fill missing values
titanic_data['Age'].fillna(titanic_data['Age'].median(), inplace=True)
titanic_data['Fare'].fillna(titanic_data['Fare'].median(), inplace=True)
# Convert categorical variables to numerical
titanic_data['Sex'] = titanic_data['Sex'].map({'male': 0, 'female': 1})

# Define features and target variable
X = titanic_data.drop('Survived', axis=1)
y = titanic_data['Survived']

# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Random Forest Classifier
rf_clf = RandomForestClassifier(n_estimators=100, random_state=42)
rf_clf.fit(X_train, y_train)

# XGBoost Classifier
xgb_clf = XGBClassifier(n_estimators=100, learning_rate=0.1, random_state=42)
xgb_clf.fit(X_train, y_train)

# Evaluate Random Forest model
rf_train_acc = accuracy_score(y_train, rf_clf.predict(X_train))
rf_test_acc = accuracy_score(y_test, rf_clf.predict(X_test))

print("Random Forest Classifier")
print(f"Training Accuracy: {rf_train_acc:.2f}")
print(f"Testing Accuracy: {rf_test_acc:.2f}")

# Evaluate XGBoost model
xgb_train_acc = accuracy_score(y_train, xgb_clf.predict(X_train))
xgb_test_acc = accuracy_score(y_test, xgb_clf.predict(X_test))

print("\nXGBoost Classifier")
print(f"Training Accuracy: {xgb_train_acc:.2f}")
print(f"Testing Accuracy: {xgb_test_acc:.2f}")


###
# Import necessary libraries
# Define features and target variable
# Import necessary libraries
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier
from sklearn.metrics import accuracy_score

# Load Adult Income dataset
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
column_names = ['age', 'workclass', 'fnlwgt', 'education', 'education-num', 'marital-status', 
                'occupation', 'relationship', 'race', 'sex', 'capital-gain', 'capital-loss', 
                'hours-per-week', 'native-country', 'income']
data = pd.read_csv(url, names=column_names, na_values=" ?", skipinitialspace=True)

# Drop rows with missing values
data.dropna(inplace=True)

# Encode categorical variables
data_encoded = pd.get_dummies(data, columns=['workclass', 'education', 'marital-status', 'occupation', 
                                             'relationship', 'race', 'sex', 'native-country', 'income'],
                              drop_first=True)

# Define features and target variable
data_encoded.dtypes
X = data_encoded.drop('income_>50K', axis=1)
y = data_encoded['income_>50K']

# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Random Forest Classifier
rf_clf = RandomForestClassifier(n_estimators=100, random_state=42)
rf_clf.fit(X_train, y_train)

# XGBoost Classifier
xgb_clf = XGBClassifier(n_estimators=100, learning_rate=0.1, random_state=42)
xgb_clf.fit(X_train, y_train)

# Evaluate Random Forest model
rf_train_acc = accuracy_score(y_train, rf_clf.predict(X_train))
rf_test_acc = accuracy_score(y_test, rf_clf.predict(X_test))

print("Random Forest Classifier")
print(f"Training Accuracy: {rf_train_acc:.2f}")
print(f"Testing Accuracy: {rf_test_acc:.2f}")

# Evaluate XGBoost model
xgb_train_acc = accuracy_score(y_train, xgb_clf.predict(X_train))
xgb_test_acc = accuracy_score(y_test, xgb_clf.predict(X_test))

print("\nXGBoost Classifier")
print(f"Training Accuracy: {xgb_train_acc:.2f}")
print(f"Testing Accuracy: {xgb_test_acc:.2f}")
