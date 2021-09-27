# Import packages
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

test = 1 # 0 if actual run


from ortools.sat.python import cp_model
from ortools.linear_solver import pywraplp
import pandas as pd
import numpy as np
# from mip.model import *

from timeit import default_timer as timer


# Read data
Jobs = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/Jobs.csv')
# JobTypes = pd.read_csv('C:/Projects/FSOx/DataInput/Data_Testing/JobTypes.csv')
Technicians = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/Techs.csv')
Locations = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/Locations.csv')
TechTimes = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/TechTimes.csv')


if test == 1:
    print("=========================== Test Run ==================================")
    # Subset Jobs for testing
    ntechs = 2#len(Technicians)
    njobs = 6#len(Jobs)
    Technicians = Technicians[0:ntechs]
    Jobs = Jobs[0:njobs]

timer_start = timer()
# Creating the model
# m = Model(solver_name='GRB') # By default it is set to minimize and the solver name is CBC
# m = cp_model.CpModel
m = cp_model.CpModel()
# cp_model.sat_parameters_pb2.SatParameters(num_search_workers=100)

# # If you want to change the objective to maximize
# m = Model(sense=MAXIMIZE, solver_name=CBC) # use GRB for Gurobi

# Jobs
Jobs_list = Jobs.JobID.tolist()
J_type = Jobs.JobType.to_numpy()
J_s = Jobs.StartWindow.tolist()
J_e = Jobs.EndWindow.tolist()

Jobs['new_col'] = list(zip(Jobs.StartWindow, Jobs.EndWindow))

# Technicians
Technicians.sort_values('TechID', inplace=True)
Technicians.reset_index(drop=True, inplace=True)
TechTimes.sort_values(['TechID', 'JobType'], inplace=True)
TechTimes.reset_index(drop=True, inplace=True)
Tech_list = Technicians.TechID.to_numpy()
T_s = Technicians.StartTime.tolist()
T_e = Technicians.EndTime.tolist()
T_bs = Technicians.StartBreakWindow.tolist()
T_be = Technicians.EndBreakWindow.tolist()
T_bd = Technicians.BreakDuration.tolist()
T_OT = Technicians.OverTimeAllowed.tolist()
#

# get jobs x tech times matrix basis the job types - T_times[tech][job]
T_times = np.array([[TechTimes['Duration'][(TechTimes['TechID'] == x) & (TechTimes['JobType'] == y)].values[0]
           for x in Technicians.TechID.tolist()]
           for y in Jobs.JobType.tolist()])

# Get number of jobs and techs
J, T = set(range(len(Jobs_list))), set(range(len(Tech_list)))

# Travel times
travel_t = np.random.randint(low=100, high=300, size=(njobs, njobs)).tolist()
for i in range(njobs):
    for j in range(njobs):
        if i == j:
            travel_t[i][j] = 0

### Add variables
# Job-Technician mapping
x = np.array([[m.NewIntVar(name='x({} ,{} )'.format(j, t), lb=0, ub=1) for t in T] for j in J])

# # Previous job variable
# last_job = np.array([[m.NewIntVar(name='last_job({} ,{} )'.format(j, t), lb=0, ub=10000) for t in T] for j in J])

# assigned_tech = np.array([m.NewBoolVar(name='assigned_tech({})'.format(j)) for j in J])

## Intermediate variables to enforce no overlap
a = np.array([[m.NewBoolVar(name='a({} ,{} )'.format(j, t)) for t in T] for j in J])
b = np.array([[[m.NewBoolVar(name='b({} ,{}, {} )'.format(jj, j, t)) for t in T] for j in J] for jj in J])
c = np.array([[m.NewBoolVar(name='c({} ,{} )'.format(j, t)) for t in T] for j in J])
d = np.array([m.NewBoolVar(name='d({})'.format(t)) for t in T])


# Job Start time mapping
st = np.array([m.NewIntVar(name='start_time', lb=700, ub=1700) for j in J])
duration = [m.NewIntVar(name='duration', lb=0, ub=500) for j in J]
end = [m.NewIntVar(name='end_time', lb=700, ub=1700) for j in J]

st_interval = np.array([[m.NewOptionalIntervalVar(name='interval{}'.format(j, t), start=st[j], end=end[j]
                                        , size=duration[j], is_present=x[j][t]) for t in T] for j in J])


# lunch Start time mapping - for each tech
# lunch = [m.NewIntVar(name='lunch_start', lb=700, ub=1700) for t in T]
lunch_st = [m.NewIntVar(name='lunch_start', lb=int(T_bs[t]*100), ub=int(T_be[t]*100)) for t in T]
lunch_duration = [int(T_bd[t]*100) for t in T]
lunch_end = [m.NewIntVar(name='lunch_end', lb=int(T_bs[t]*100), ub=int(T_be[t]*100)) for t in T]

lunch_interval = [m.NewIntervalVar(name='lunch_interval{}'.format(t), start=lunch_st[t], end=lunch_end[t], size=lunch_duration[t]) for t in T]

# Travel time variables
travel_st = [[[m.NewIntVar(name='travel_start', lb=700, ub=1700) for t in T] for j in J] for jj in J]
# travel_duration = [m.NewIntVar(name='travel_duration', lb=0, ub=500) for j in J]
travel_use = np.array([[[m.NewBoolVar(name='travel_use({} ,{}, {} )'.format(jj, j, t)) for t in T] for j in J] for jj in J])
travel_end = [[[m.NewIntVar(name='travel_end', lb=700, ub=1700) for t in T] for j in J] for jj in J]


travel_interval = np.array([[[m.NewOptionalIntervalVar(name='travel_interval({} ,{}, {} )'.format(jj, j, t), start=travel_st[jj][j][t]
                                             , end=travel_end[jj][j][t]
                                    , size=travel_t[jj][j]
                                    , is_present=travel_use[jj][j][t]) for t in T] for j in J] for jj in J])


len(travel_interval[:,:,0].tolist())

import itertools
flatten = itertools.chain.from_iterable


# Transpose the start time interval array and append lunch interval of the tech
st_interval = np.array(st_interval).T.tolist()
for t in T:
    st_interval[t].append(lunch_interval[t])
    st_interval[t].extend(list(flatten(travel_interval[:,:,t].tolist())))

# Transpose it back to get the shape to [job, tech]
st_interval = np.array(st_interval).T


### Add constraints

# Constraint on assigning every job to a technician
for j in J:
    m.Add(sum(x[j][t] for t in T) <= 1)

# # Constraint on assigning every job to some technician
# for t in T:
#     m.Add(sum(x[j][t] for j in J) == 1)


# Constraint on starting job between job start and end window
for t in T:
    for j in J:
        m.Add((st[j]) >= J_s[j]*100)  # start window
        m.Add((st[j]+int(T_times[j][t] * 100)) <= J_e[j]*100)  # end window to be updated with actual job duration

# Constraint to check any job doesn't start in another job
for t in T:
    interval_list = []
    # tech_index = x[j].index(1)  # gives the index of technician assigned
    for j in J:
        # Check if job is assigned to tech
        m.Add(x[j][t] == 1).OnlyEnforceIf(c[j][t])
        m.Add(x[j][t] == 0).OnlyEnforceIf(c[j][t].Not())

        # Update duration
        m.Add(duration[j] == int(T_times[j][t] * 100)).OnlyEnforceIf(c[j][t])

        for jj in J-{j}:
            if jj > j:
                m.Add(x[jj][t] == 1).OnlyEnforceIf(b[j][jj][t])
                m.Add(x[j][t] == 1).OnlyEnforceIf(b[j][jj][t])
                # m.Add(x[jj][t] == 0).OnlyEnforceIf(b[jj][j][t].Not())

                # Flag to use travel time or not
                m.Add(travel_use[j][jj][t] == 1).OnlyEnforceIf(b[j][jj][t])
                m.Add(travel_use[j][jj][t] == 0).OnlyEnforceIf(b[j][jj][t].Not())

                # Make sure next start time includes travel time
                m.Add(travel_st[j][jj][t] == end[j]).OnlyEnforceIf(b[j][jj][t])
                # m.Add(st[jj] == travel_end[j][jj][t]).OnlyEnforceIf(b[j][jj][t])

                # m.Add(st[jj] == 700).OnlyEnforceIf(b[jj][j][t].Not())

    # print(st_interval[:, t])
    m.AddNoOverlap(st_interval[:, t])#.OnlyEnforceIf(d[t])


### objective function: minimize the distance
# m.Maximize(sum(x[j][t] for t in T for j in J))
m.Maximize(x.sum())


# optimizing
print("======== Solving for %d jobs and %d techs =======" % (njobs, ntechs))
solver = cp_model.CpSolver()
# solver.parameters.num_search_workers = 2
status = solver.Solve(m)

print("Time taken to execute for %d jobs and %d techs: %f" % (njobs, ntechs, (timer()-timer_start)))

if status == cp_model.OPTIMAL:
    print('Success: ')
    solver.Value(x[0][0])
    output = [[0 for t in T]for j in J]
    op_travel_st = [[[0 for t in T] for j in J] for jj in J]
    op_start = [solver.Value(st[j]) for j in J]
    op_end = [solver.Value(end[j]) for j in J]
    op_lunch = [solver.Value(lunch_st[t]) for t in T]
    for j in J:
        for t in T:
            #if x[j][t].x >= 0.99:
            output[j][t] = solver.Value(x[j][t])
            # op_last[j][t] = solver.Value(last_job[j][t])
            # print('({} ,{} )'.format(j, t))
    for jj in J:
        for j in J:
            for t in T:
                op_travel_st[jj][j][t] = solver.Value(travel_st[jj][j][t])

    output_df = pd.DataFrame(output)
    output_travel_st_df = pd.DataFrame(op_travel_st)
    output_start_df = pd.DataFrame(op_start)
    output_end_df = pd.DataFrame(op_end)
    output_lunch_df = pd.DataFrame(op_lunch)

#
# test = np.array([[1,0,0],
#         [0,1,0],
#         [1,0,0]])
#
# [i for i, e in enumerate([1, 2, 1]) if e == 1]
# test[0].index(1)
# testslot = [[1,0,0,0],
#             [0,1,0,0],
#             [1,0,0,0]]
# for t in range(len(test[0])):
#     print('technician : %d' % t)
#     for h in range(len(testslot[0])):
#         print(sum([test[j][t]*testslot[j][h] for j in range(len(testslot))]))
#
#


#### Checks
print("=========================== Checks ==================================")
print('Number of technicians finally used to finish %d jobs: %d' % (output_df.sum(axis=0).sum(), output_df.loc[:,output_df.sum(axis=0) > 0].shape[1]))


# output_start_df[output_df[0]>0].sort_values([0])[0].tolist()
#
# T_times[output_df[0]>0,0]
#
#
# output_start_df[output_df[0]>0].sort_values([0])[0].index.to_numpy()
# np.where(output_start_df[output_df[0]>0].sort_values([0])[0].index.to_numpy() == 38)[0][0]-1
# output_start_df[output_df[0]>0].sort_values([0])[0].index.to_numpy()[np.where(output_start_df[output_df[0]>0].sort_values([0])[0].index.to_numpy() == 38)[0][0]-1]
#
# st_interval[x[j][t]]
#
testslot = np.array([[1,0,0,0],
            [0,1,0,0],
            [1,0,0,0]])
#
# np.sort(testslot[:,0])
# testslot[2,0]
#
# np.sort(st_interval[:,t])[0]