import matplotlib.pyplot as plt
import sys
import numpy as np
from sklearn import datasets, linear_model, ensemble
from sklearn.metrics import mean_squared_error, r2_score
from sqlalchemy import false

# Load the diabetes dataset
X, diabetes_y = datasets.load_diabetes(return_X_y=True)
names = datasets.load_diabetes().feature_names

# Sorts list of sublists based on the n'th element
# Code provided by GeeksForGeeks, found at
# https://www.geeksforgeeks.org/python-sort-list-according-second-element-sublist/
def Sort(sub_li,n):
    l = len(sub_li)
    for i in range(0, l):
        # skip first sublist and last sublist
        if(n == 2):
            for j in range(1, l-i-2):
                if (float(sub_li[j][n]) > float(sub_li[j + 1][n])):
                    tempo = sub_li[j]
                    sub_li[j]= sub_li[j + 1]
                    sub_li[j + 1]= tempo
        else:
            for j in range(1, l-i-2):
                if (float(sub_li[j][n]) < float(sub_li[j + 1][n])):
                    tempo = sub_li[j]
                    sub_li[j]= sub_li[j + 1]
                    sub_li[j + 1]= tempo
    return sub_li

# title:  Title of the used regression
# method: The function call of the regression
# num:    The number of the model used (make it unique)
def runModel(title, method, num):
    print("\n\n " + title.upper())
    plt.figure(num)
    A = [['Metric', 'Coefficients', 'Mean Squared Error', 'Coeff. of Det.'],
        [],
        [],
        [],
        [],
        [],
        [],
        [],
        [],
        [],
        [],
        ['Averages']]

    for i in range(0,10):
        # Use only one feature
        diabetes_X = X[:, np.newaxis, i]

        # Split the data into training/testing sets
        diabetes_X_train = diabetes_X[:-20]
        diabetes_X_test = diabetes_X[-20:]

        # Split the targets into training/testing sets
        diabetes_y_train = diabetes_y[:-20]
        diabetes_y_test = diabetes_y[-20:]

        # Create linear regression object
        regr = method

        # Train the model using the training sets
        regr.fit(diabetes_X_train, diabetes_y_train)

        # Make predictions using the testing set
        diabetes_y_pred = regr.predict(diabetes_X_test)

        # Plot outputs

        A[i+1].append(str(names[i]))
        A[i+1].append(str(round(regr.coef_[0],2)))
        A[i+1].append(str(round(mean_squared_error(diabetes_y_test, diabetes_y_pred),2)))
        A[i+1].append(str(round(r2_score(diabetes_y_test, diabetes_y_pred),2)))

        plt.subplot(2,5,i+1)
        plt.title("Metric: %s" % (names[i]))
        plt.scatter(diabetes_X_test, diabetes_y_test, color="black")
        plt.plot(diabetes_X_test, diabetes_y_pred, color="blue", linewidth=3)

        plt.xticks(())
        plt.yticks(())
    mean_co = 0
    mean_squared = 0
    mean_co_det = 0
    for j in range(0,10):
        mean_co += float(A[j+1][1])
        mean_squared += float(A[j+1][2])
        mean_co_det += float(A[j+1][3])
    A[-1].append(round(mean_co/10, 2))
    A[-1].append(round(mean_squared/10, 2))
    A[-1].append(round(mean_co_det/10, 2))
    A = Sort(A, n)
    print('\n'.join([''.join(['{:^20}'.format(item) for item in row])
        for row in A]))

    plt.suptitle(title, fontsize=14)

# Metric to measure by
if(len(sys.argv) < 3):
    print("\nSyntax:    python3 script.py <n> <true/false>\
          \n   - <n>:          Number of metric to sort by [1,2,3] \
          \n   - <true\\false>: 'true' to show plots, 'false' to not show\n")
    exit(1)
else:
    try:
        n = int(sys.argv[1])
    except:
        print("<n> must be an integer.")
        exit(1)

# Run the following models on the dataset and print results
runModel("Linear Regression", linear_model.LinearRegression(), 1)
runModel("Kernel Ridge CV Regression", linear_model.RidgeCV(), 2)
runModel("SGD Regressor", linear_model.SGDRegressor(max_iter=100000), 3)
runModel("LASSO CV Regression", linear_model.LassoCV(), 4)
# runModel("Bayesian Ridge Regression", linear_model.BayesianRidge(), 5)
# runModel("Elastic Net CV Regression", linear_model.ElasticNetCV(), 6)

met = ['Coefficients', 'Mean Squared Error', 'Coefficients of detemination']

# Print some messages and show the plotted graphs
print("\n\nCoefficients: Higher is better")
print("Mean squared error: Lower is better")
print("Coefficient of determination: Higher is better")
print(('\033[1m' + "Sorting by %s\n" + '\033[0m') % met[n-1])

if(sys.argv[2] == "true"):
    plt.show()
