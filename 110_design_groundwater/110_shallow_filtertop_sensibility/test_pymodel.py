#!/usr/bin/env python3

import numpy as NP
import scipy.stats as STATS
import pandas as PD
import matplotlib as MP
import matplotlib.pyplot as PLT
import seaborn as SB
import pymc as PM
import pytensor as PP
import pytensor.tensor as PT

data = PD.read_csv("xg3obs_data.csv", sep = ";")
y_var = 'hg3_lcl'

# data cleansing
data = data.loc[:, ['loc_code', 'hydroyear', 'filtertop_depth', 'soilclass', y_var]]
data.dropna(inplace = True)
print(data.loc[data[y_var].values > 10, :])
data = data.loc[NP.logical_and(data[y_var].values < 2, data[y_var].values > -5), :]

# soilclass category
data['soilclass'] = PD.Categorical(data['soilclass'])
categories = data['soilclass'].cat.codes.values
n_categories = len(NP.unique(data['soilclass'].values))
cat_order = data['soilclass'].cat.categories
print(cat_order)

# data dimensions
n_observations = data.shape[0]
population_mean = data[y_var].mean()
population_sd = data[y_var].std()

# values
x = data['filtertop_depth'].values.ravel()#.reshape((-1,1))
y = data[y_var].values.ravel()#.reshape((-1, 1))


# model construction
with PM.Model() as model:
    a = PM.Normal('intercept', mu = population_mean, sigma = 2*population_sd, shape = n_categories)

    b = PM.Normal(f'class_slope', mu = 0, sigma = population_sd, shape = n_categories)
    d = PM.Data(f'xdata', x)
    estimator = a[categories] + d * b[categories]


    residual = PM.HalfNormal('residual')
    nu = PM.Normal('dof')

    posterior = PM.StudentT('y', mu = estimator, sigma = residual, nu = nu, observed = y)


# sampling
with model:
   trace = PM.sample(draws = 2**10 \
                     , tune = 2**10 \
                     , chains = 4 \
                     , progressbar = True \
                   )
   result = PM.summary(trace)
   print(PD.DataFrame(result))
   PM.plot_trace(trace, combined = True)
   PLT.show()

# data- and regression plot
dpi = 300
cm = 1/2.54
PLT.rcParams.update({"text.usetex": True})
fig = PLT.figure(figsize = (16*cm, 8*cm), dpi = dpi)
fig.subplots_adjust(top = 0.98 , right = 0.98, bottom = 0.16, left = 0.10 )

ax = fig.add_subplot(1, 1, 1)
x = NP.array([  NP.min(data['filtertop_depth'].values) \
              , NP.max(data['filtertop_depth'].values) \
             ])
colors = {'heavy': "#284971", 'light': "#8eafd7", 'peat': "#f0d70f"}

for nr, cat in enumerate(cat_order):
    # print(nr, cat)
    subdat = data.loc[data['soilclass'] == cat, :]
    ax.scatter(subdat['filtertop_depth'].values, subdat[y_var].values \
               , s = 1, alpha = 0.2, color = colors.get(cat, "#cccccc") \
               , zorder = 10)

    m = result.loc[f"class_slope[{nr}]", "mean"]
    n = result.loc[f"intercept[{nr}]", "mean"]
    y = m*x+n
    ax.plot(x, y, color = colors.get(cat, "#cccccc"), label = f"{cat}: \\(y={n:.3f} {m:+.3f}x\\) " \
            , zorder = 20)

ax.set_xlim(ax.get_xlim()[::-1])
ax.spines[['top', 'right']].set_visible(False)
ax.set_xlabel("filtertop depth (m)")
ax.set_ylabel("local HG3 (m)")
PLT.legend(loc = 'best', fontsize = 8)
fig.savefig(f"figures/python_model_fit_{y_var}.png", dpi = 300, transparent = False)
PLT.tight_layout()
PLT.show()
