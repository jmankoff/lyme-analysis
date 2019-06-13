import pandas as pd
from scipy import stats

def is_normally_distributed(data:pd.DataFrame, columns:list=None, threshold:int=0.05)->pd.Series:
    """/
    tests normality of data under `columns` of dataframe `data` (all `data` columns, if `columns not explicitly 
    listed) and returns a series indexed by `columns` where the value for a column is True if its data is normal.
    
    test of normality evaluates the null hypothesis that values come from a normal distribution.
    - if p-value < threshold, we can reject the null hypothesis and conclude data is not normally distributed 
       ---> False return value
    - if p-value >= threshold, we fail to reject the null hypothesis and conclude data is normally distributed
    
    there are a lot of debates around the reliability of normality tests, given it heavily relies on p-value and
    makes conclusions based on the retention of the null hypothesis. basically:
     - p-value of small samples (N ~ 20) is usu. larger than the commonly used threshold values (e.g. 0.05) 
       despite the data being non-normal. 
     - p-value of large samples (N > 100) is usu. smaller that the commonly used threshold values (e.g. 0.05)
       despite the data being normal.
       
    plotting the data distribution is much more helpful in deciding normality
    """
    # null hypothesis: data[columns] comes from a normal distribution
    # if p-value < threshold we reject the null hypothesis
    if data is None:
        return None

    if columns is None:
        columns = data.columns

    k2, p = stats.normaltest(data[columns])
    nml_test = pd.DataFrame({'stats' : k2, 'p-value' : p}, index=columns)
    
    return nml_test['p-value'] >= threshold

def is_HOV_met(data1, data2, column, center='mean', threshold=0.05):
    # null hypothesis: data1[column] and data2[column] have equal variances
    w, p = stats.levene(data1[column], data2[column])
    if(p < threshold):
        # we reject the null hypothesis, i.e. the variances are unequal
        return False 
    # we maintain the null hypothesis, i.e. there is not enought evidence that the variances are unequal
    return True 

def between_comparison(independent_column_name, dependent_column_name, data):
    F, p =  stats.f_oneway(data[data[independent_column_name] == 'YES'][dependent_column_name], 
                           data[data[independent_column_name] == 'NO'][dependent_column_name])
    print('one-way ANOVA on {} for {}: F = {:.2f}, p = {:.3f}'.format(dependent_column_name, 
                                                                      independent_column_name,
                                                                      F, 
                                                                      p))
    return (F, p)