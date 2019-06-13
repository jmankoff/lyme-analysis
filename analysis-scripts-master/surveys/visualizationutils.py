import pandas as pd
import matplotlib.pyplot as plt

def distribution_graphs(independent_metrics:list, 
	                    dependent_metrics:list, 
	                    dependent:pd.DataFrame,
	                    width=12,
	                    height=4)->None:
	"""\
	plots the histogram of dependent variables over groups of independent variable.
	each row of graphs provides the distribution of a dependent variable for groups
	of an independet variable. the dependent dataframe contains both dependent
	variable columns and categorical independent variable columns.
	"""
	for iv in independent_metrics:
		grouped = dependent.groupby(by=iv)
		for ind_dv, dv in enumerate(dependent_metrics):
			fig, axes = plt.subplots(1, len(grouped), figsize=(width, height))
			fig.suptitle('Distribution of {} in Groups of {}'.format(dv, iv))
			for ind, group in enumerate(grouped.groups):
				ax = axes[ind]
				ax.set_title('{} = {}'.format(iv, group))
				ax.hist(grouped.get_group(group)[dv])


# TO-DO Q-Q plots of normality				