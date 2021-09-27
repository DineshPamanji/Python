import pandas as pd
import numpy as np
from algorithm import logger
import math
from algorithm.schedule import Schedule
from algorithm.data_classes import Data, Job, Tech
from algorithm.solver.solver_abc import Solver
from typing import List
from algorithm.travel.travel import Travel
import re
from functools import reduce
import pulp
import time


class MipSolver(Solver):
    techs_status: []
    techs: pd.DataFrame
    sorted_un_assigned_jobs: pd.DataFrame
    prev_obj_score = math.inf
    techs_able: List[Tech]
    missed_job_list: List[Job]

    def __init__(self, algorithm_settings: dict, data: Data, travel_timer: Travel):
        super().__init__(data, travel_timer)
        self.algorithm_settings = algorithm_settings

    def solve(self, schedule: Schedule) -> Schedule:

        t0total = time.time()

        # Number of jobs to complete
        numJobs = schedule.data.total_number_of_jobs
        numTechs = schedule.data.total_number_of_techs

        # dayEnd and dayStart are based on earlist possible tech start time and latest possible tech endTime

        # Change this so that it's not hardcoded
        dayStart = 7.5
        dayEnd = 18

        # Each time step is every 0.1 minutes
        dt = 0.1
        numTimes = int((dayEnd-dayStart)/dt)

        problem = pulp.LpProblem("FSOx", pulp.LpMaximize)

        # Declare binary decision variables

        # Assuming no overtime

        # Job can START any time within hours (hence can finish after hours)
        # Jobs need to be started between the start window time and end window time
        # Jobs can be started exactly on the start window time and up to but not including the end window time

        # Technicians can only start a job during their staffable hours.
        # Techs can start a jobs on their start time and up to but not including their end time
        # Techs can start a job ON their end time.

        t0dvs = time.time()

        print("Creating decision variables")

        # If technician, i, starts job, j, at time, t
        assignedDVNames = ["Tech_{}-Job_{}-Time_{}".format(tech_number, job_number, time_number)
                           for job_number in range(numJobs)
                           for tech_number in range(numTechs)
                           for time_number in range(getJobSWTN(schedule, job_number, dayStart, dt), getJobEWTN(schedule, job_number, dayStart, dt))
                           if (getTechSTN(schedule, tech_number, dayStart, dt)) <= time_number and (time_number < getTechETN(schedule, tech_number, dayStart, dt))]

        assignedDVs = pulp.LpVariable.dicts(
            name='Assigned', indexs=assignedDVNames, cat='Binary')

        # If technician, i, is available at time, t
        availableDVNames = ["Tech_{}-Time_{}".format(tech_number, time_number)
                            for tech_number in range(numTechs)
                            for time_number in range(getTechSTN(schedule, tech_number, dayStart, dt), getTechETN(schedule, tech_number, dayStart, dt))
                            ]
        availableDVs = pulp.LpVariable.dicts(
            name='Available', indexs=availableDVNames, cat='Binary')

        # May need further optimisations
        # If technician, i, starts traveling from job, j, to job, k, at time, t

        # travelDVNames = ["Tech_{}-JobFrom_{}-JobTo_{}-Time_{}".format(tech_number, job_from_number, job_to_number, time_number)
        #                  for tech_number in range(numTechs)
        #                  for job_from_number in range(numJobs)
        #                  for time_number in range(getJobSWTN(schedule, job_from_number, dayStart, dt)+getJobDN(schedule, tech_number, job_from_number, dt), getJobEWTN(schedule, job_from_number, dayStart, dt)+getJobDN(schedule, tech_number, job_from_number, dt))
        #                  if (getTechSTN(schedule, tech_number, dayStart, dt) <= time_number) and (time_number < getTechETN(schedule, tech_number, dayStart, dt))
        #                  for job_to_number in range(numJobs)
        #                  #  Could do further optimisations by incorporating preimported travel times
        #                  #  At the moment its just accounting for possibility of 0 traveltime
        #                  if getJobEWTN(schedule, job_to_number, dayStart, dt) > time_number
        #                  ]
        # travelDVs = pulp.LpVariable.dicts(
        #     name='Travel', indexs=travelDVNames, cat='Binary')

        print("Number of decision variables is:",
              len(assignedDVs)+len(availableDVs))

        t1dvs = time.time()
        t0cs = time.time()
        print("Creating decision variables took {} seconds".format(t1dvs-t0dvs))
        print("Defining constraints")

        print("Every technician can only be available or doing 1 job at all times")
        # Every technician can only be available or doing a maximum of 1 job at all times
        cs = [pulp.lpSum(
            [assignedDVs[r'Tech_{}-Job_{}-Time_{}'.format(tech_number, job_number, time_number)]
                for job_number in range(numJobs)
                if (getJobSWTN(schedule, job_number, dayStart, dt) <= time_number) and (time_number < getJobEWTN(schedule, job_number, dayStart, dt))]
        ) <= availableDVs[r'Tech_{}-Time_{}'.format(tech_number, time_number)]
            for tech_number in range(numTechs)
            for time_number in range(getTechSTN(schedule, tech_number, dayStart, dt), getTechETN(schedule, tech_number, dayStart, dt))
        ]

        for c in cs:
            problem += c

        # Every job can only be started once and done by one technician
        # SUBJECT TO CHANGE WHEN SOME JOBS NEED SEVERAL TECHNICIANS
        print("Every job can only be assigned at most once to at most one technician")
        cs = [pulp.lpSum(
            [assignedDVs[r'Tech_{}-Job_{}-Time_{}'.format(tech_number, job_number, time_number)]
                for tech_number in range(numTechs)
                for time_number in range(getJobSWTN(schedule, job_number, dayStart, dt), getJobEWTN(schedule, job_number, dayStart, dt))
                if (getTechSTN(schedule, tech_number, dayStart, dt)) <= time_number and (time_number < getTechETN(schedule, tech_number, dayStart, dt))]) <= 1
            for job_number in range(numJobs)
        ]

        for c in cs:
            problem += c

        # START CODING FROM HERE
        # Every tech assigned to a job is unavailable for any other jobs during the duration of the job
        # The tech becomes available immediately following the end of that duration
        print("A tech assigned to a job becomes unavailable for any other jobs during the duration of the job")

        # cs = [pulp.lpSum([assignedDVs[r'Tech_{}-Job_{}-Time_{}'.format(tech_number, job_number, time_number)],
        #                   availableDVs[r'Tech_{}-Time_{}'.format(tech_number, other_time_number)]])
        #       <= 1
        #       for job_number in range(numJobs)
        #       for tech_number in range(numTechs)
        #       for time_number in range(getJobSWTN(schedule, job_number, dayStart, dt), getJobEWTN(schedule, job_number, dayStart, dt))
        #       if ((getTechSTN(schedule, tech_number, dayStart, dt) <= time_number) and (time_number < getTechETN(schedule, tech_number, dayStart, dt)))
        #       for other_time_number in range(time_number+1, time_number + getJobDN(schedule, tech_number, job_number, dt))
        #       if (other_time_number < getTechETN(schedule, tech_number, dayStart, dt))
        #       ]

        # for c in cs:
        #     problem += c

        t1cs = time.time()

        t0solve = time.time()
        print("Constraint definitions took {} seconds".format(t1cs-t0cs))

        # Objective function. Maximise the profit. Each job completed gives same profit. So maximise number of jobs completed
        print("Defining objective function")
        problem += pulp.lpSum(assignedDVs)

        print("Algorithm solving")
        problem.solve()

        print("Number of jobs completed: ", sum(
            [assignedDVs[key].varValue for key in assignedDVNames]))

        t1solve = time.time()
        print("Objective function and solver took {} seconds".format(t1solve-t0solve))

        t1total = time.time()

        print("The whole algorithm took {} seconds".format(t1total-t0total))
        return schedule


def getJobDN(schedule, tech_number, job_number, delta):
    tech = getTech(schedule, tech_number)
    job = getJob(schedule, job_number)
    return round(tech._tech_times[job._job_type]/delta)


def getTech(schedule, tech_number):
    return schedule.data.tech_list[tech_number]


def getJob(schedule, job_number):
    return schedule.data.job_list[job_number]


def getJobSWTN(schedule, job_number, start, delta):
    job = getJob(schedule, job_number)
    job_start_window_number = round((job._start_window - start)/delta)
    return job_start_window_number


def getJobEWTN(schedule, job_number, start, delta):
    job = getJob(schedule, job_number)
    job_end_window_number = round((job._end_window - start)/delta)
    return job_end_window_number


def getTechSTN(schedule, tech_number, start, delta):
    tech = getTech(schedule, tech_number)
    tech_start_time_number = round((tech._start_time - start)/delta)
    return tech_start_time_number


def getTechETN(schedule, tech_number, start, delta):
    tech = getTech(schedule, tech_number)
    tech_end_time_number = round((tech._end_time - start)/delta)
    return tech_end_time_number
