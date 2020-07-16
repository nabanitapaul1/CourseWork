# -*- coding: utf-8 -*-
"""
Created on Thu Jul  9 18:44:55 2020

@author: Nabanita
"""
# For reading datasets
# importing necessary libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import pylab
import scipy.stats as st
#1.) Calories_consumed-> predict weight gained using calories consumed
# reading a csv  file using pandas library

cal_wt = pd.read_csv("C:\\EXCELR\\ASSIGNMENTS\\SimpleLinearRegression\\calories_consumed.csv")

cal_wt.columns

# EDA
# Graphical Representation

# For calories consumed variable
plt.hist(cal_wt["Calories Consumed"]);plt.xlabel("Calories Consumed")

plt.boxplot(cal_wt["Calories Consumed"],vert= False );plt.xlabel("Calories Consumed")

st.probplot(cal_wt["Calories Consumed"],  dist="norm",plot=pylab)

# For  weight Gained variable

plt.hist(cal_wt["Weight gained (grams)"]);plt.xlabel("Weight gained (grams)")

plt.boxplot(cal_wt["Weight gained (grams)"],vert= False );plt.xlabel("Weight gained (grams)")

st.probplot(cal_wt["Weight gained (grams)"],  dist="norm",plot=pylab)

# Scatter Plot

plt.plot(cal_wt["Calories Consumed"],cal_wt["Weight gained (grams)"],"ro");plt.xlabel("Calories Consumed");plt.ylabel("Weight gained (grams)")

# Co-realation Coefficient

cal_wt.corr()
cal_wt["Calories Consumed"].corr(cal_wt["Weight gained (grams)"])
np.corrcoef(cal_wt["Calories Consumed"],cal_wt["Weight gained (grams)"])

cal_wt =  cal_wt.rename(columns= {"Calories Consumed": "Calories_Consumed", "Weight gained (grams)": "Weight_Gained"})

# Model Buillding

import statsmodels.formula.api as smf

# Model 1
cal_wt_lm=smf.ols("Weight_Gained~Calories_Consumed",data=cal_wt).fit()

# For getting coefficiets of the variables used in equation
cal_wt_lm.params

# p-values for the variable and R-squared value for prepared model
cal_wt_lm.summary()
lm_Rsquared = cal_wt_lm.rsquared
cal_wt_lm.conf_int(0.05) # 95% confidence interval

pred = cal_wt_lm.predict(cal_wt.iloc[:,1]) # Predicted values of  Weight_Gained using the linear model

# Visualization of regresion line over the scatter plot of Calorie_Consumed and Weight_Gained
# For visualization we need to import matplotlib.pyplot
import matplotlib.pylab as plt
plt.scatter(x=cal_wt['Calories_Consumed'],y=cal_wt['Weight_Gained'],color='red');plt.plot(cal_wt['Calories_Consumed'],pred,color='black');plt.xlabel('Calories_Consumed');plt.ylabel('Weight_Gained')

pred.corr(cal_wt['Weight_Gained']) # 0.9469

#RMSE
from sklearn.metrics import mean_squared_error

from math import sqrt

rmse_lm = sqrt(mean_squared_error(cal_wt['Weight_Gained'], pred))

print(rmse_lm)

# Tranformation
# Model 2

# Square root model

np.sqrt(cal_wt.Calories_Consumed).corr(cal_wt.Weight_Gained)

cal_wt_sqrt = smf.ols('Weight_Gained~np.sqrt(Calories_Consumed)',data=cal_wt).fit()
cal_wt_sqrt.params
cal_wt_sqrt.summary()
sqrt_Rsquared = cal_wt_sqrt.rsquared
pred = cal_wt_sqrt.predict(cal_wt.iloc[:,1]) # Predicted values of  Weight_Gained using the square root model
rmse_sqrt = sqrt(mean_squared_error(cal_wt.Weight_Gained,pred))
pred.corr(cal_wt.Weight_Gained) # 0.92
plt.scatter(x=cal_wt.Calories_Consumed, y= cal_wt.Weight_Gained, color="red");plt.plot(cal_wt.Calories_Consumed,pred, color="black");plt.xlabel("Calories_Consumed");plt.ylabel("Weight_Gained")

#  Logarithmic Model
# Model 3

np.log(cal_wt.Calories_Consumed).corr(cal_wt.Weight_Gained)

cal_wt_log = smf.ols('Weight_Gained~np.log(Calories_Consumed)',data=cal_wt).fit()
cal_wt_log.params
cal_wt_log.summary()
log_Rsquared = cal_wt_log.rsquared
pred = cal_wt_log.predict(cal_wt.iloc[:,1]) # Predicted values of  Weight_Gained using the logarithmic  model
rmse_log = sqrt(mean_squared_error(cal_wt.Weight_Gained,pred))
pred.corr(cal_wt.Weight_Gained) # 0.8987
plt.scatter(x=cal_wt.Calories_Consumed, y= cal_wt.Weight_Gained, color="red");plt.plot(cal_wt.Calories_Consumed,pred, color="black");plt.xlabel("Calories_Consumed");plt.ylabel("Weight_Gained")

# Quardratic Model
# Model 4

cal_wt["Calories_Consumed_sq"] = cal_wt.Calories_Consumed * cal_wt.Calories_Consumed

cal_wt.Weight_Gained.corr(cal_wt.Calories_Consumed_sq)

cal_wt_quad = smf.ols("Weight_Gained~Calories_Consumed+Calories_Consumed_sq",data=cal_wt).fit()
cal_wt_quad.params
cal_wt_quad.summary()
pred = cal_wt_quad.predict(cal_wt[["Calories_Consumed","Calories_Consumed_sq"]])

quad_Rsquared = cal_wt_quad.rsquared
rmse_quad =sqrt(mean_squared_error(cal_wt.Weight_Gained,pred))

plt.scatter(cal_wt.Calories_Consumed,cal_wt.Weight_Gained,c="b");plt.plot(cal_wt.Calories_Consumed,pred,"r");plt.xlabel("Calories_Consumed");plt.ylabel("Weight_Gained")

plt.scatter(np.arange(14),cal_wt_quad.resid_pearson);plt.axhline(y=0,color='red');plt.xlabel("Observation Number");plt.ylabel("Standardized Residual")

plt.hist(cal_wt_quad.resid_pearson)

# Creating data frame

cal_wt = pd.DataFrame([["Linear",lm_Rsquared,rmse_lm],["Square Root ",sqrt_Rsquared,rmse_sqrt],["Logarithmic",log_Rsquared,rmse_log],["Quadratic",quad_Rsquared,rmse_quad]],columns= ["Model","R-Square","RMSE"])
cal_wt.columns

#2.) Delivery_time -> Predict delivery time using sorting time 

# Importing Data

st_dvlt = pd.read_csv("C:\\EXCELR\\ASSIGNMENTS\\SimpleLinearRegression\\delivery_time.csv")

st_dvlt.columns

# EDA
# Graphical Representation

# For Sorting Time variable
plt.hist(st_dvlt["Sorting Time"]);plt.xlabel("Sorting Time")

plt.boxplot(st_dvlt["Sorting Time"],vert= False );plt.xlabel("Sorting Time")

st.probplot(st_dvlt["Sorting Time"],  dist="norm",plot=pylab)

# For  Delivery Time variable

plt.hist(st_dvlt["Delivery Time"]);plt.xlabel("Delivery Time")

plt.boxplot(st_dvlt["Delivery Time"],vert= False );plt.xlabel("Delivery Time")

st.probplot(st_dvlt["Delivery Time"],  dist="norm",plot=pylab)

# Scatter Plot

plt.plot(st_dvlt["Sorting Time"],st_dvlt["Delivery Time"],"ro");plt.xlabel("Sorting Time");plt.ylabel("Delivery Time")

# Co-realation Coefficient

st_dvlt.corr()
st_dvlt["Sorting Time"].corr(st_dvlt["Delivery Time"])
np.corrcoef(st_dvlt["Sorting Time"],st_dvlt["Delivery Time"])

st_dvlt =  st_dvlt.rename(columns= {"Sorting Time": "Sorting_Time", "Delivery Time": "Delivery_Time"})

# Model Buillding

import statsmodels.formula.api as smf

# Model 1
st_dvlt_lm=smf.ols("Delivery_Time~Sorting_Time",data=st_dvlt).fit()

# For getting coefficiets of the variables used in equation
st_dvlt_lm.params

# p-values for the variable and R-squared value for prepared model
st_dvlt_lm.summary()
lm_Rsquared = st_dvlt_lm.rsquared
st_dvlt_lm.conf_int(0.05) # 95% confidence interval

pred = st_dvlt_lm.predict(st_dvlt.iloc[:,1]) # Predicted values of  Delivery_time using the linear model

# Visualization of regresion line over the scatter plot of Sorting_time and Delivery_time
# For visualization we need to import matplotlib.pyplot
import matplotlib.pylab as plt
plt.scatter(x=st_dvlt['Sorting_Time'],y=st_dvlt['Delivery_Time'],color='red');plt.plot(st_dvlt['Sorting_Time'],pred,color='black');plt.xlabel('Sorting_Time');plt.ylabel('Delivery_Time')

pred.corr(st_dvlt['Delivery_Time']) # 0.9469

#RMSE
from sklearn.metrics import mean_squared_error

from math import sqrt

rmse_lm = sqrt(mean_squared_error(st_dvlt['Delivery_Time'], pred))

print(rmse_lm)

# Tranformation
# Model 2

# Square root model

np.sqrt(st_dvlt.Sorting_Time).corr(st_dvlt.Delivery_Time)

st_dvlt_sqrt = smf.ols('Delivery_Time~np.sqrt(Sorting_Time)',data=st_dvlt).fit()
st_dvlt_sqrt.params
st_dvlt_sqrt.summary()
sqrt_Rsquared = st_dvlt_sqrt.rsquared
pred = st_dvlt_sqrt.predict(st_dvlt.iloc[:,1]) # Predicted values of  Delivery  time using the square root model
rmse_sqrt = sqrt(mean_squared_error(st_dvlt.Delivery_Time,pred))
pred.corr(st_dvlt.Delivery_Time) # 0.92
plt.scatter(x=st_dvlt.Sorting_Time, y= st_dvlt.Delivery_Time, color="red");plt.plot(st_dvlt.Sorting_Time,pred, color="black");plt.xlabel("Sorting_Time");plt.ylabel("Delivery_Time")

#  Logarithmic Model
# Model 3

np.log(st_dvlt.Sorting_Time).corr(st_dvlt.Delivery_Time)

st_dvlt_log = smf.ols('Delivery_Time~np.log(Sorting_Time)',data=st_dvlt).fit()
st_dvlt_log.params
st_dvlt_log.summary()
log_Rsquared = st_dvlt_log.rsquared
pred = st_dvlt_log.predict(st_dvlt.iloc[:,1]) # Predicted values of  Delivery time using the logarithmic model
rmse_log = sqrt(mean_squared_error(st_dvlt.Delivery_Time,pred))
pred.corr(st_dvlt.Delivery_Time) # 0.8339
plt.scatter(x=st_dvlt.Sorting_Time, y= st_dvlt.Delivery_Time, color="red");plt.plot(st_dvlt.Sorting_Time,pred, color="black");plt.xlabel("Sorting_Time");plt.ylabel("Delivery_Time")

# Exponential Model
# Model 4


np.log(st_dvlt.Delivery_Time).corr(st_dvlt.Sorting_Time)

st_dvlt_expo = smf.ols("np.log(Delivery_Time)~Sorting_Time",data=st_dvlt).fit()
st_dvlt_expo.params
st_dvlt_expo.summary()
pred = st_dvlt_expo.predict(st_dvlt["Sorting_Time"])

expo_Rsquared = st_dvlt_expo.rsquared

pred= np.exp(pred)
rmse_expo =sqrt(mean_squared_error(st_dvlt.Delivery_Time,pred))

plt.scatter(st_dvlt.Sorting_Time,st_dvlt.Delivery_Time,c="b");plt.plot(st_dvlt.Sorting_Time,pred,"r");plt.xlabel("Sorting_Time");plt.ylabel("Delivery_Time")

plt.scatter(np.arange(21),st_dvlt_expo.resid_pearson);plt.axhline(y=0,color='red');plt.xlabel("Observation Number");plt.ylabel("Standardized Residual")

plt.hist(st_dvlt_expo.resid_pearson)

# Creating data frame

st_dvlt = pd.DataFrame([["Linear",lm_Rsquared,rmse_lm],["Square Root ",sqrt_Rsquared,rmse_sqrt],["Logarithmic",log_Rsquared,rmse_log],["Exponential",expo_Rsquared,rmse_expo]],columns= ["Model","R-Square","RMSE"])
st_dvlt.columns


# 3.) Emp_data -> Build a prediction model for Churn_out_rate

# Import Data

emp_data = pd.read_csv("C:\\EXCELR\\ASSIGNMENTS\\SimpleLinearRegression\\emp_data.csv")

emp_data.columns

# EDA
# Graphical Representation

# For Salary Hike  variable
plt.hist(emp_data["Salary_hike"]);plt.xlabel("Salary_hike")

plt.boxplot(emp_data["Salary_hike"],vert= False );plt.xlabel("Salary_hike")

st.probplot(emp_data["Salary_hike"],  dist="norm",plot=pylab)

# For Churn Out Rate variable

plt.hist(emp_data["Churn_out_rate"]);plt.xlabel("Churn_out_rate")

plt.boxplot(emp_data["Churn_out_rate"],vert= False );plt.xlabel("Churn_out_rate")

st.probplot(emp_data["Churn_out_rate"],  dist="norm",plot=pylab)

# Scatter Plot

plt.plot(emp_data["Salary_hike"],emp_data["Churn_out_rate"],"ro");plt.xlabel("Salary_hike");plt.ylabel("Churn_out_rate")

# Co-realation Coefficient

emp_data.corr()
emp_data["Salary_hike"].corr(emp_data["Churn_out_rate"])
np.corrcoef(emp_data["Salary_hike"],emp_data["Churn_out_rate"])

# Model Buillding

import statsmodels.formula.api as smf

# Model 1
emp_data_lm=smf.ols("Churn_out_rate~Salary_hike",data=emp_data).fit()

# For getting coefficiets of the variables used in equation
emp_data_lm.params

# p-values for the variable and R-squared value for prepared model
emp_data_lm.summary()
lm_Rsquared = emp_data_lm.rsquared
emp_data_lm.conf_int(0.05) # 95% confidence interval

pred = emp_data_lm.predict(emp_data.Salary_hike) # Predicted values of  Churn_out_rate using the linear model

# Visualization of regresion line over the scatter plot of Salary_hike and Churn_out_rate
# For visualization we need to import matplotlib.pyplot
import matplotlib.pylab as plt
plt.scatter(x=emp_data['Salary_hike'],y=emp_data['Churn_out_rate'],color='red');plt.plot(emp_data['Salary_hike'],pred,color='black');plt.xlabel('Salary_hike');plt.ylabel('Churn_out_rate')

pred.corr(emp_data['Churn_out_rate']) # 0.9117

#RMSE
from sklearn.metrics import mean_squared_error

from math import sqrt

rmse_lm = sqrt(mean_squared_error(emp_data['Churn_out_rate'], pred))

print(rmse_lm)

# Tranformation
# Model 2

# Square root model

np.sqrt(emp_data.Salary_hike).corr(emp_data.Churn_out_rate)

emp_data_sqrt = smf.ols('Churn_out_rate~np.sqrt(Salary_hike)',data=emp_data).fit()
emp_data_sqrt.params
emp_data_sqrt.summary()
sqrt_Rsquared = emp_data_sqrt.rsquared
pred = emp_data_sqrt.predict(emp_data.Salary_hike) # Predicted values of  Churn out rate using the square root model
rmse_sqrt = sqrt(mean_squared_error(emp_data.Churn_out_rate,pred))
pred.corr(emp_data.Churn_out_rate) # 0.92
plt.scatter(x=emp_data.Salary_hike, y= emp_data.Churn_out_rate, color="red");plt.plot(emp_data.Salary_hike,pred, color="black");plt.xlabel("Salary_hike");plt.ylabel("Churn_out_rate")

#  Logarithmic Model
# Model 3

np.log(emp_data.Salary_hike).corr(emp_data.Churn_out_rate)

emp_data_log = smf.ols('Churn_out_rate~np.log(Salary_hike)',data=emp_data).fit()
emp_data_log.params
emp_data_log.summary()
log_Rsquared = emp_data_log.rsquared
pred = emp_data_log.predict(emp_data.Salary_hike) # Predicted values of  Churn out rate using the logarithmic model
rmse_log = sqrt(mean_squared_error(emp_data.Churn_out_rate,pred))
pred.corr(emp_data.Churn_out_rate) # 0.921
plt.scatter(x=emp_data.Salary_hike, y= emp_data.Churn_out_rate, color="red");plt.plot(emp_data.Salary_hike,pred, color="black");plt.xlabel("Salary_hike");plt.ylabel("Churn_out_rate")

# Creating data frame

emp_data = pd.DataFrame([["Linear",lm_Rsquared,rmse_lm],["Square Root ",sqrt_Rsquared,rmse_sqrt],["Logarithmic",log_Rsquared,rmse_log]],columns= ["Model","R-Square","RMSE"])
emp_data.columns

#4) Salary_data -> Build a prediction model for Salary

# Importing data


salary_data = pd.read_csv("C:\\EXCELR\\ASSIGNMENTS\\SimpleLinearRegression\\Salary_Data.csv")

salary_data.columns

# EDA
# Graphical Representation

# For Years Experience variable
plt.hist(salary_data["YearsExperience"]);plt.xlabel("YearsExperience")

plt.boxplot(salary_data["YearsExperience"],vert= False );plt.xlabel("YearsExperience")

st.probplot(salary_data["YearsExperience"],  dist="norm",plot=pylab)

# For  Salary  variable

plt.hist(salary_data["Salary"]);plt.xlabel("Salary")

plt.boxplot(salary_data["Salary"],vert= False );plt.xlabel("Salary")

st.probplot(salary_data["Salary"],  dist="norm",plot=pylab)

# Scatter Plot

plt.plot(salary_data["YearsExperience"],salary_data["Salary"],"ro");plt.xlabel("YearsExperience");plt.ylabel("Salary")

# Co-realation Coefficient

salary_data.corr()
salary_data["YearsExperience"].corr(salary_data["Salary"])
np.corrcoef(salary_data["YearsExperience"],salary_data["Salary"])

# Model Buillding

import statsmodels.formula.api as smf

# Model 1
salary_data_lm=smf.ols("Salary~YearsExperience",data=salary_data).fit()

# For getting coefficiets of the variables used in equation
salary_data_lm.params

# p-values for the variable and R-squared value for prepared model
salary_data_lm.summary()
lm_Rsquared = salary_data_lm.rsquared
salary_data_lm.conf_int(0.05) # 95% confidence interval

pred = salary_data_lm.predict(salary_data.YearsExperience) # Predicted values of  Salary using the linear model

# Visualization of regresion line over the scatter plot of Years experience and Salary
# For visualization we need to import matplotlib.pyplot
import matplotlib.pylab as plt
plt.scatter(x=salary_data['YearsExperience'],y=salary_data['Salary'],color='red');plt.plot(salary_data['YearsExperience'],pred,color='black');plt.xlabel('YearsExperience');plt.ylabel('Salary')

pred.corr(salary_data['Salary']) # 0.978

#RMSE
from sklearn.metrics import mean_squared_error

from math import sqrt

rmse_lm = sqrt(mean_squared_error(salary_data['Salary'], pred))

print(rmse_lm)

# Tranformation
# Model 2

# Square root model

np.sqrt(salary_data.YearsExperience).corr(salary_data.Salary)

salary_data_sqrt = smf.ols('Salary~np.sqrt(YearsExperience)',data=salary_data).fit()
salary_data_sqrt.params
salary_data_sqrt.summary()
sqrt_Rsquared = salary_data_sqrt.rsquared
pred = salary_data_sqrt.predict(salary_data.YearsExperience) # Predicted values of  Weight_Gained using the square root model
rmse_sqrt = sqrt(mean_squared_error(salary_data.Salary,pred))
pred.corr(salary_data.Salary) # 0.9648
plt.scatter(x=salary_data.YearsExperience, y= salary_data.Salary, color="red");plt.plot(salary_data.YearsExperience,pred, color="black");plt.xlabel("YearsExperience");plt.ylabel("Salary")

#  Logarithmic Model
# Model 3

np.log(salary_data.YearsExperience).corr(salary_data.Salary)

salary_data_log = smf.ols('Salary~np.log(YearsExperience)',data=salary_data).fit()
salary_data_log.params
salary_data_log.summary()
log_Rsquared = salary_data_log.rsquared
pred = salary_data_log.predict(salary_data.YearsExperience) # Predicted values of  Weight_Gained using the logarithmic  model
rmse_log = sqrt(mean_squared_error(salary_data.Salary,pred))
pred.corr(salary_data.Salary) # 0.9240
plt.scatter(x=salary_data.YearsExperience, y= salary_data.Salary, color="red");plt.plot(salary_data.YearsExperience,pred, color="black");plt.xlabel("YearsExperience");plt.ylabel("Salary")


# Exponential Model
# Model 4


np.log(salary_data.Salary).corr(salary_data.YearsExperience)

salary_data_expo = smf.ols("np.log(Salary)~YearsExperience",data=salary_data).fit()
salary_data_expo.params
salary_data_expo.summary()
pred =salary_data_expo.predict(salary_data["YearsExperience"])

expo_Rsquared = salary_data_expo.rsquared

pred= np.exp(pred)
rmse_expo =sqrt(mean_squared_error(salary_data.Salary,pred))

plt.scatter(salary_data.YearsExperience,salary_data.Salary,c="b");plt.plot(salary_data.YearsExperience,pred,"r");plt.xlabel("YearsExperience");plt.ylabel("Salary")

plt.scatter(np.arange(30),salary_data_expo.resid_pearson);plt.axhline(y=0,color='red');plt.xlabel("Observation Number");plt.ylabel("Standardized Residual")

plt.hist(salary_data_expo.resid_pearson)



# Creating data frame

# Creating data frame

salary_data = pd.DataFrame([["Linear",lm_Rsquared,rmse_lm],["Square Root ",sqrt_Rsquared,rmse_sqrt],["Logarithmic",log_Rsquared,rmse_log],["Exponential",expo_Rsquared,rmse_expo]],columns= ["Model","R-Square","RMSE"])
salary_data.columns
