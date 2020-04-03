from scipy.optimize import minimize
from scipy.optimize import LinearConstraint
import numpy as np
from functools import partial

def guess_fit():

    def constraint1(pars, index):
        """
        ineq constraint -> return value >= 0
        Constraints to make sure a segment's parameter is lower than the next segments's
        """
        return pars[index] - pars[index+1]

    def errfcn(pars, value):
        """
        Update the objective function to calculate the difference -> value to be minimized
        """
        ll = np.sum([np.abs((pars[i]*value[i])-value[i]) for i in range(len(pars))])
        return ll

    # Segment Upper limits
    value = [0,
                1000000000,
                100000000,
                20000000,
                5000000,
                1000000,
                250000,
                100000
                ]

    # Set up initial guesses - past year values
    initial_guess = [1.00000,
                    1.00000,
                    0.99999,
                    0.99996,
                    0.99979,
                    0.99871,
                    0.99358,
                    0.98062
                    ]

    # Set up bounds - 1% difference with past year values
    bounds = [[x-0.01, min(1,x+0.01)] for x in initial_guess]

    # Add constraints to make sure a segment's parameter is lower than the next segments's
    cons = []
    for ii in range(len(initial_guess)-1):
        cons.append({'type': 'ineq', 'fun': partial(constraint1, index=ii)})

    # print([i['fun'](initial_guess) for i in cons])

    # Minimize
    out = minimize(errfcn
                   , initial_guess
                   , args=(value)
                   , bounds=bounds
                   , constraints=cons
                   , method='SLSQP')
    return out.x

guess_fit()