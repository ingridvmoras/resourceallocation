import numpy as np
import pandas as pd
from scipy.integrate import odeint

# blood meal intake: periodic function
def my_sigma(t, T, delta, shift): #T: period of new blood meals (hours); delta: duration of blood feeding (hours): shift:time to first BM (hours)
    if delta > T/2:
        raise ValueError("duration of blood meal delta must be smaller than the period T/2")
    if ((t-shift) % T) < delta:
        x = 1
    else:
        x = 0
    return x

# function that runs the simulations, visualizes the results, and returns the data frame.
def run(state, params, model, tmin = 0, tend = 20, condition = ""):
    # set time vector
    t = np.arange(0, tend*24, 0.01)
    out = pd.DataFrame(odeint(model, state, t, args=(params,), method='impAdams'))
    out['cond'] = condition
    return out

# change parameters
def my_replace(p_old, p_new):
    p_old.update(p_new)
    return p_old

# functions that transform the obtained data sets
def add_type(df, category):
    df['type'] = category
    return df

def transform_df(df, cond):
    variables = ['B','R','E', "O","S", "beta_E"]
    new_df = df.melt(value_vars=variables)
    new_df['variable'] = pd.Categorical(new_df['variable'], categories=['B','R','E',"beta_E", "O","S"], ordered=True)
    
    resources = new_df[new_df['variable'] == "R"]
    resources = add_type(resources, "Reserves")
    
    storage = new_df[new_df['variable'] == "B"]
    storage = add_type(storage, "Blood")
    
    parasite = new_df[new_df['variable'].isin(["O", "S"])]
    parasite = add_type(parasite, "Parasite")
    
    eggs = new_df[new_df['variable'] == "E"]
    eggs = add_type(eggs, "Eggs")
    
    e20 = new_df[new_df['variable'] == "beta_E"]
    e20 = add_type(e20, "e20")
    
    df_complete = pd.concat([storage, e20, resources, eggs, parasite])
    df_complete['type'] = pd.Categorical(df_complete['type'], categories=["Blood","e20","Reserves", "Eggs", "Parasite"], ordered=True)
    df_complete['variable'] = pd.Categorical(df_complete['variable'], categories=["B", "beta_E", "R", "E", "O", "S"], ordered=True)
    df_complete['condition'] = cond
    return df_complete

# function to calculate fitness of egg and parasites
def localMaxima(params, data, tend):
    freq = params["T_sigma"]
    N = 24*tend/freq
    seq = np.arange(0, N+1)
    df = data[data['variable'] == "E"]
    out = []
    for i in seq:
        tmp = df[(df['time'] >= freq*i) & (df['time'] < freq*(i+1))]
        out.append(tmp['value'].max())
    localMaximum = sum(out)
    return localMaximum

# other functions are translated in a similar way