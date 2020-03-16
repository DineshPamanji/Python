from __future__ import print_function
from ortools.constraint_solver import routing_enums_pb2
from ortools.constraint_solver import pywrapcp
import numpy as np
import pandas as pd
import random
import ast
from copy import deepcopy
from time import time
test = 1

# Read data
Jobs = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/Jobs.csv')
# JobTypes = pd.read_csv('C:/Projects/FSOx/DataInput/Data_Testing/JobTypes.csv')
Technicians = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/Techs.csv')
Locations = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/Locations.csv')
TechTimes = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/TechTimes.csv')
Times = pd.read_csv('C:/Projects/FSOx/Data/Input/Data_Testing/WASH_LA_HERE_Updated.csv')

ntechs = len(Technicians)
njobs = len(Jobs)

if test == 1:
    print("=========================== Test Run ==================================")
    # Subset Jobs for testing
    ntechs = 2#len(Technicians)
    njobs = 6#len(Jobs)
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

#### Get ntechs*jobs locations
# Get a list of all location ids
def time_matrix_prep(Technicians, Jobs, Times):
    """
    Function to create a universe of travel times from the existing locations and cache
    :param Technicians: Dataframe with details on Tech start, end locations and location ids
    :param Jobs: Dataframe with details on Jobs locations and location ids
    :param Times: Dataframe with cache travel times
    :return: Numpy array of time matrix of shape (techs+jobs, techs+jobs)
    """
    all_loc_ids = pd.DataFrame(Technicians.StartLocationID.astype('str').tolist()+Jobs.LocationID.astype('str').tolist(), columns=['LocationID'])

    # Add dummy column for cross join
    all_loc_ids['key'] = 1

    # Get all combinations
    locations_all = pd.merge(all_loc_ids[['LocationID', 'key']].drop_duplicates()
                             , all_loc_ids[['LocationID', 'key']].drop_duplicates()
                             , on='key')
    del locations_all['key'], all_loc_ids['key']

    # Concatenate start and end
    locations_all['Start_End'] = locations_all['LocationID_x'].astype('str')+'-'+locations_all['LocationID_y'].astype('str')
    Times['Start_End'] = Times['StartLocation'].astype('str')+'-'+Times['EndLocation'].astype('str')
    Times_sub = Times[Times['DateTime'] == 'weekday4-time8']

    # Join locations with travel times
    locations_all = pd.merge(locations_all, Times_sub[['Start_End', 'TravelTime', 'Distance']], how='left')

    # Fill with a high travel time wherever missing
    locations_all['TravelTime'].fillna(3, inplace=True)
    locations_all['TravelTime'] = locations_all['TravelTime']*100
    # locations_all['StartLocationID'][locations_all['TravelTime'] != 3].unique()
    # locations_all[locations_all['TravelTime'] != 3].shape

    # Pivot to create a matrix
    global time_matrix
    time_matrix = pd.pivot_table(locations_all[['LocationID_x', 'LocationID_y', 'TravelTime']], index='LocationID_x'
                          , columns='LocationID_y', values='TravelTime'
                          , aggfunc=sum)

    # Re-order indices
    time_matrix = time_matrix.reindex(all_loc_ids['LocationID'], axis=1)
    time_matrix = time_matrix.reindex(all_loc_ids['LocationID'], axis=0)

    # Convert to numpy array to be used in the algo
    time_matrix_numpy = time_matrix.to_numpy()
    return time_matrix_numpy

time_matrix_np = time_matrix_prep(Technicians, Jobs, Times)

def create_data_model():
    """Stores the data for the problem."""
    global data
    data = {}
    # data['time_matrix'] = np.random.randint(low=20, high=100, size=(njobs+ntechs, njobs+ntechs))
    data['time_matrix'] = time_matrix_np
    # Create a n*n*t matrix with zeroes
    data['time_matrix_vehicle'] = np.array([[[0 for t in range(ntechs)]
                                             for j in range(njobs+ntechs)]
                                            for i in range(njobs+ntechs)])
    # Add duration of every job for each technician - n*n*t matrix
    for i in range(ntechs+njobs):
        # print(i)
        for j in range(ntechs+njobs):
            for t in range(ntechs):
                if i == j:
                    data['time_matrix_vehicle'][i, j, t] = 0
                elif (i < ntechs) and (j < ntechs):
                    data['time_matrix_vehicle'][i, j, t] = 0
                elif (i < ntechs):
                    data['time_matrix_vehicle'][i, j, t] = data['time_matrix'][i, j]
                else:
                    data['time_matrix_vehicle'][i, j, t] = data['time_matrix'][i, j] + T_times[i-ntechs, t]
    data['time_windows'] = list(zip((Technicians.StartTime*100).astype(int), (Technicians.EndTime*100).astype(int)))\
                           +list(zip(Jobs.StartWindow*100, Jobs.EndWindow*100))
    data['num_vehicles'] = ntechs
    data['starts'] = data['ends'] = list(range(0, ntechs))  # Tech indices
    data['tech_endtime'] = Tech_end
    data['pickups_deliveries'] = [[ntechs+1, ntechs+2]]  # List of lists with pickup - delivery combinations

    return data

class Callback:
    """
    Class to define a different call back function for each vehicle
    """
    def __init__(self, data, manager, vehicle_id, routing):
        self.data = data
        self.manager = manager
        self.vehicle_id = vehicle_id
        self.routing = routing
        # self.time_dimension_name = time_dimension
        self.time_dimension = self.routing.GetDimensionOrDie("Time_dimension")

    def time_callback(self, from_index, to_index):
        """Returns the travel time + job duration between the two nodes for a particular vehicle."""
        # Convert from routing variable Index to time matrix NodeIndex.
        from_node = self.manager.IndexToNode(from_index)
        to_node = self.manager.IndexToNode(to_index)
        time_bin = self.time_dimension.CumulVar(from_index)/4

        return self.data['time_matrix_vehicle'][from_node, to_node, self.vehicle_id]+time_bin  # +data['demands'][from_node]

def print_solution(data, manager, routing, assignment):
    """Prints assignment on console."""
    time_dimension = routing.GetDimensionOrDie('Time_dimension')
    total_time = 0

    print('Breaks:')
    intervals = assignment.IntervalVarContainer()
    for i in range(intervals.Size()):
        brk = intervals.Element(i)
        brk.Var()
        if brk.PerformedValue() == 1:
            print('{}: Start({}) Duration({})'.format(
                brk.Var().Name(),
                brk.StartValue(),
                brk.DurationValue()))
            # print(brk.Var())
        else:
            print('{}: Unperformed'.format(brk.Var().Name()))

    # Display dropped nodes.
    dropped_nodes = 'Dropped nodes:'
    for node in range(routing.Size()):
        if routing.IsStart(node) or routing.IsEnd(node):
            continue
        if assignment.Value(routing.NextVar(node)) == node:
            dropped_nodes += ' {}'.format(manager.IndexToNode(node))
    print(dropped_nodes)
    # Display routes
    total_distance = 0
    total_load = 0
    techs_used = 0
    for vehicle_id in range(data['num_vehicles']):
        index = routing.Start(vehicle_id)
        plan_output = 'Route for vehicle {}:\n'.format(vehicle_id)
        route_distance = 0
        route_load = 0
        start = 1
        while not routing.IsEnd(index):
            node_load = 0
            node_index = manager.IndexToNode(index)
            time_var = time_dimension.CumulVar(index)

            # Add node load only if it isn't the start point
            if (start == 0) & (node_index >= ntechs):
                node_load += int(T_times[node_index-ntechs, vehicle_id])
                route_load += T_times[node_index-ntechs, vehicle_id]
            start = 0
            plan_output += '{0} Time({1},{2}) Duration({3})-> '.format(
                manager.IndexToNode(index), assignment.Min(time_var),
                assignment.Min(time_var)+int(node_load), route_load)

            # route_load += data['demands'][node_index]
            previous_index = index
            # prev_node_index = manager.IndexToNode(previous_index)
            index = assignment.Value(routing.NextVar(index))
            route_distance += routing.GetArcCostForVehicle(
                previous_index, index, vehicle_id)
        time_var = time_dimension.CumulVar(index)
        plan_output += '{0} Time({1},{2}) Duration({3}) \n'.format(
            manager.IndexToNode(index), assignment.Min(time_var),
            assignment.Max(time_var), route_load)
        plan_output += 'Time of the route: {}min\n'.format(
            assignment.Min(time_var))
        plan_output += 'Distance of the route: {}m\n'.format(route_distance)
        plan_output += 'Load of the route: {}\n'.format(route_load)
        print(plan_output)
        total_distance += route_distance
        total_load += route_load
        total_time += assignment.Min(time_var)
        if route_distance > 0:
            techs_used += 1
    print('Total Distance of all routes: {}m'.format(total_distance))
    print('Total Load of all routes: {}'.format(total_load))
    print('Total time of all routes: {}min'.format(total_time))
    print('Total techs used: {}'.format(techs_used))


def main():
    """Solve the VRP with time windows."""
    data = create_data_model()
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
        1700,  # allow waiting time
        1900,  # maximum time per vehicle
        False,  # Don't force start cumul to zero.
        time_dimension)
    time_dimension = routing.GetDimensionOrDie(time_dimension)
    # time_dimension.SetGlobalSpanCostCoefficient(100)

    # Add time window constraints for each location except depot.
    for location_idx, time_window in enumerate(data['time_windows']):
        if location_idx in data['starts']:
            continue
        index = manager.NodeToIndex(location_idx)
        time_dimension.CumulVar(index).SetRange(time_window[0], time_window[1])
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
                                                , "Lunch for Technician "+str(i))
        tech_times = [0]*ntechs
        tech_times = np.array(tech_times+T_times[:, i].tolist())
        # travel_times = data['time_matrix'][i,:]#.tolist()
        # transit_times = np.add(tech_times, travel_times)
        # time_dimension.SetBreakIntervalsOfVehicle([lunch], i, tech_times+T_times[:, i].tolist())
        time_dimension.SetBreakIntervalsOfVehicle([lunch], i, tech_times)
        # time_dimension.SetBreakIntervalsOfVehicle([lunch], i, data['time_matrix_vehicle'][i,:,i].tolist())

    # Allow to drop nodes.
    penalty = 1000
    for node in range(ntechs, len(data['time_matrix_vehicle'])):
        routing.AddDisjunction([manager.NodeToIndex(node)], penalty)

    # For sequence jobs and parts-pickup
    for request in data['pickups_deliveries']:
        pickup_index = manager.NodeToIndex(request[0])
        delivery_index = manager.NodeToIndex(request[1])
        routing.AddPickupAndDelivery(pickup_index, delivery_index)
        routing.solver().Add(
            routing.VehicleVar(pickup_index) == routing.VehicleVar(
                delivery_index))

        # Pickup should be performed before delivery
        routing.solver().Add(
            time_dimension.CumulVar(pickup_index) <=
            time_dimension.CumulVar(delivery_index))

        # Make sure delivery happens immediately after pickup
        routing.solver().Add(
            routing.NextVar(pickup_index) == manager.IndexToNode(delivery_index))

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
    search_parameters.log_search = True

    # routing.CloseModelWithParameters(search_parameters)

    assignment = routing.SolveWithParameters(search_parameters)
    if assignment:
        print('================= Success ================')
        print_solution(data, manager, routing, assignment)
        print("Time taken to optimize: %d seconds" % (time() - start_time))

if __name__ == '__main__':
  main()