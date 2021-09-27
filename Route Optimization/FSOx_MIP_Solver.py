from __future__ import print_function
from ortools.constraint_solver import routing_enums_pb2
from ortools.constraint_solver import pywrapcp
import numpy as np
import pandas as pd
from time import time
from datetime import date

from src.helpers.MIP_helper import time_matrix_prep, create_data_model, Callback, print_solution

# If testing
test = 1

# Input path
path = 'C:/Projects/FSOx/Data/Input/Data_Testing/'

# Read data
Jobs = pd.read_csv(path + 'Jobs.csv')
# JobTypes = pd.read_csv('JobTypes.csv')
Technicians = pd.read_csv(path + 'Techs.csv')
Locations = pd.read_csv(path + 'Locations.csv')
TechTimes = pd.read_csv(path + 'TechTimes.csv')
Times = pd.read_csv(path + 'WASH_LA_HERE_Updated.csv')

ntechs = len(Technicians)
njobs = len(Jobs)

if test == 1:
    print("=========================== Test Run ==================================")
    # Subset Jobs for testing
    ntechs = 4#len(Technicians)
    njobs = 4#len(Jobs)
    # Technicians = Technicians[0:ntechs]
    # Jobs = Jobs[0:njobs]
    Technicians = Technicians[-ntechs:]
    Jobs = Jobs[-njobs:]
    Technicians. reset_index(inplace=True, drop=True)
    Jobs. reset_index(inplace=True, drop=True)
    # Jobs['EndWindow'] = 19

# Get jobs * tech times matrix basis the job types - T_times[tech][job]
T_times = np.array([[TechTimes['Duration'][(TechTimes['TechID'] == x) & (TechTimes['JobType'] == y)].values[0]
                     for x in Technicians.TechID.tolist()]
                    for y in Jobs.JobType.tolist()])
T_times = T_times*100

# Tech end times - sum of overtime and tech end time
Tech_end = np.array((Technicians['EndTime']+Technicians['OverTimeAllowed']).tolist())*100

# Calculate time matrix
time_matrix_np,  all_id_dict, time_matrix = time_matrix_prep(Technicians, Jobs, Times)


def main():
    """Solve the vehicle routing problem with time windows, breaks, pick-ups and deliveries"""
    data = create_data_model(Technicians, Jobs, Tech_end, T_times, ntechs, njobs, time_matrix_np)
    start_time = time()
    print("----- Data ready")
    manager = pywrapcp.RoutingIndexManager(len(data['time_matrix_vehicle']),
                                           data['num_vehicles'], data['starts'], data['ends'])
    routing = pywrapcp.RoutingModel(manager)
    time_dimension = 'Time_dimension'

    # create a time call back function for each vehicle
    transit_callback_vehicle_index_all = []
    for vehicle_id in range(data['num_vehicles']):
        exec("transit%s = Callback(data, manager, vehicle_id, routing)" % str(vehicle_id))
        exec("transit_callback_vehicle_index_all.append((routing.RegisterTransitCallback(transit%s.time_callback)))" % (str(vehicle_id)))
        routing.SetArcCostEvaluatorOfVehicle(transit_callback_vehicle_index_all[vehicle_id], vehicle_id)

    routing.AddDimensionWithVehicleTransits(
        transit_callback_vehicle_index_all,
        0,  # allow waiting time
        1900,  # maximum time per vehicle
        False,  # Don't force start cumul to zero.
        time_dimension)
    time_dimension = routing.GetDimensionOrDie(time_dimension)
    # time_dimension.SetGlobalSpanCostCoefficient(100)

    # Add constraint on optional jobs
    # https://activimetrics.com/blog/ortools/counting_dimension/
    data['Optionals'] = [[4, 5, 8], [6, 7]]
    count_dimension_name = 'count'
    # assume some variable num_nodes holds the total number of nodes
    routing.AddConstantDimension(
        1,  # increment by one every time
        njobs+1,  # make sure the return to depot node can be counted
        True,  # set count to zero
        count_dimension_name)
    count_dimension = routing.GetDimensionOrDie(count_dimension_name)
    for opt_list in data['Optionals']:
        if len(opt_list) > 1:
            print(opt_list)
            routing.solver().AddConstraint(sum([count_dimension.CumulVar(manager.NodeToIndex(opt_list[i])) for i in range(len(opt_list))]) == 1)

    # Add time window constraints for each location except depot.
    for location_idx, time_window in enumerate(data['time_windows']):
        if location_idx in data['starts']:
            continue
        index = manager.NodeToIndex(location_idx)
        time_dimension.CumulVar(index).SetRange(time_window[0], time_window[1])
        # routing.solver().AddConstraint(counter_dimension.CumulVar(index) == 1)
        # time_dimension.SetCumulVarSoftLowerBound(index, time_window[0], 10)
        # time_dimension.SetCumulVarSoftUpperBound(index, time_window[1], 10)

    # Add time window constraints for each vehicle start node.
    solver = routing.solver()
    for vehicle_id in range(data['num_vehicles']):
        index = routing.Start(vehicle_id)
        time_dimension.CumulVar(index).SetRange(data['time_windows'][manager.IndexToNode(index)][0],
                                                data['time_windows'][manager.IndexToNode(index)][1])

        # # Penalize start time more to not start early in the day
        # time_dimension.SetCumulVarSoftLowerBound(index, data['time_windows'][manager.IndexToNode(index)][0], 100)
        # time_dimension.SetCumulVarSoftUpperBound(index, data['time_windows'][manager.IndexToNode(index)][1], 1)

        # Set hard-stop including the overtime allowed and end time of each tech
        solver.AddConstraint(time_dimension.CumulVar(routing.End(vehicle_id)) < int(data['tech_endtime'][vehicle_id]))

    for i in range(data['num_vehicles']):
        routing.AddVariableMinimizedByFinalizer(
            time_dimension.CumulVar(routing.Start(i)))
        routing.AddVariableMinimizedByFinalizer(
            time_dimension.CumulVar(routing.End(i)))

    # Lunch break intervals - update using actual data from Technicians
    for i in range(data['num_vehicles']):
        lunch = solver.FixedDurationIntervalVar(int(Technicians.StartBreakWindow[i])*100
                                                , int(Technicians.EndBreakWindow[i])*100
                                                , int(Technicians.BreakDuration[i]*100)
                                                , False
                                                , "Lunch for Technician "+ all_id_dict[i])
        tech_times = [0]*ntechs
        tech_times = np.array(tech_times+T_times[:, i].tolist())
        # travel_times = data['time_matrix'][i,:]#.tolist()
        # transit_times = np.add(tech_times, travel_times)
        # time_dimension.SetBreakIntervalsOfVehicle([lunch], i, tech_times+T_times[:, i].tolist())
        time_dimension.SetBreakIntervalsOfVehicle([lunch], i, tech_times)
        # solver.AddConstraint(time_dimension.CumulVar(routing.End(i)) < lunch.StartVar)
        # time_dimension.SetBreakIntervalsOfVehicle([lunch], i, data['time_matrix_vehicle'][i,:,i].tolist())

    # Allow to drop nodes.
    penalty = 1000
    for node in range(ntechs, len(data['time_matrix_vehicle'])):
        routing.AddDisjunction([manager.NodeToIndex(node)], penalty)


    # # For sequence jobs and parts-pickup
    # for request in data['pickups_deliveries']:
    #     pickup_index = manager.NodeToIndex(request[0])
    #     delivery_index = manager.NodeToIndex(request[1])
    #     routing.AddPickupAndDelivery(pickup_index, delivery_index)
    #     routing.solver().Add(
    #         routing.VehicleVar(pickup_index) == routing.VehicleVar(
    #             delivery_index))
    #
    #     # Pickup should be performed before delivery
    #     routing.solver().Add(
    #         time_dimension.CumulVar(pickup_index) <=
    #         time_dimension.CumulVar(delivery_index))
    #
    #     # Make sure delivery happens immediately after pickup
    #     routing.solver().Add(
    #         routing.NextVar(pickup_index) == manager.IndexToNode(delivery_index))



    # ###### Add other constraints
    # solver = routing.solver().Set
    # solver.parameters.num_search_workers = 2
    search_parameters = pywrapcp.DefaultRoutingSearchParameters()
    # search_parameters.workers
    # search_parameters.time_limit.seconds = 240
    # search_parameters.lns_time_limit.seconds = 0.5
    # search_parameters.first_solution_strategy = (routing_enums_pb2.FirstSolutionStrategy.PATH_MOST_CONSTRAINED_ARC)
    search_parameters.first_solution_strategy = (routing_enums_pb2.FirstSolutionStrategy.GLOBAL_CHEAPEST_ARC)
    # search_parameters.local_search_metaheuristic = (
    #     routing_enums_pb2.LocalSearchMetaheuristic.GUIDED_LOCAL_SEARCH)
    #     routing_enums_pb2.LocalSearchMetaheuristic.TABU_SEARCH)
    # search_parameters.log_search = True

    # routing.CloseModelWithParameters(search_parameters)

    assignment = routing.SolveWithParameters(search_parameters)
    if assignment:
        print('================= Success ================')
        global plan
        plan = print_solution(data, manager, routing, assignment, ntechs, T_times)
        plan.to_csv(path+'Plan_{}.csv'.format(date.today()), index=False)
        print("Time taken to optimize: %d seconds" % (time() - start_time))


if __name__ == '__main__':
  main()