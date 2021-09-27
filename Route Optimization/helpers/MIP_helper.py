import numpy as np
import pandas as pd
from copy import deepcopy


def time_matrix_prep(Technicians, Jobs, Times):
    """
    Function to create a universe of travel times from the existing locations and cache
    :param Technicians: Dataframe with details on Tech start, end locations and location ids
    :param Jobs: Dataframe with details on Jobs locations and location ids
    :param Times: Dataframe with cache travel times
    :return: Numpy array of time matrix of shape (techs+jobs, techs+jobs)
    """
    all_loc_ids = pd.DataFrame(Technicians.StartLocationID.astype('str').tolist()+Jobs.LocationID.astype('str').tolist(), columns=['LocationID'])

    global all_ids, all_id_dict
    all_ids = pd.DataFrame(Technicians.TechID.astype('str').tolist()+Jobs.JobID.astype('str').tolist(), columns=['ID'])
    all_id_dict = dict(zip(all_ids.index.tolist(), all_ids['ID']))

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
    Times_sub = Times[Times['DateTime'] == 'weekday4-time8']  # Select one of the bins

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
    return time_matrix_numpy, all_id_dict, time_matrix


def create_data_model(Technicians, Jobs, Tech_end, T_times, ntechs, njobs, time_matrix_np):
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

    # # Dummy bin level time matrix
    # data['time_matrix_vehicle_bins'] = np.array([[[[data['time_matrix_vehicle'][i, j, t] + b for b in range(4)]
    #                                                for t in range(ntechs)]
    #                                               for j in range(njobs + ntechs)]
    #                                              for i in range(njobs + ntechs)])

   # add time windows of technicians followed by job windows
    data['time_windows'] = list(zip((Technicians.StartTime*100).astype(int), (Technicians.EndTime*100).astype(int)))\
                           +list(zip(Jobs.StartWindow*100, Jobs.EndWindow*100))
    data['num_vehicles'] = ntechs
    data['starts'] = data['ends'] = list(range(0, ntechs))  # Tech indices
    data['tech_endtime'] = Tech_end

    data['pickups_deliveries'] = [[ntechs+1, ntechs+2]]  # List of lists with pickup - delivery combinations

    return data


class Callback:
    """
    Class to define a different call back function for each vehicle/technician
    """
    def __init__(self, data, manager, vehicle_id, routing):
        self.data = data
        self.manager = manager
        self.vehicle_id = vehicle_id
        self.routing = routing
        # self.time_dimension_name = time_dimension
        # self.time_dimension = self.routing.GetDimensionOrDie("Time_dimension")

    def time_callback(self, from_index, to_index):
        """Returns the travel time + job duration between the two nodes for a particular vehicle."""
        # Convert from routing variable Index to time matrix NodeIndex.
        from_node = self.manager.IndexToNode(from_index)
        to_node = self.manager.IndexToNode(to_index)
        # time_dimension = self.routing.GetDimensionOrDie('Time_dimension')
        # time_bin = min(round(self.routing.GetArcCostForVehicle(self.vehicle_id)/4*100)-2, 0)
        # print(time_bin)

        return self.data['time_matrix_vehicle'][from_node, to_node, self.vehicle_id] # +data['demands'][from_node]


def print_solution(data, manager, routing, assignment, ntechs, T_times):
    """Prints assignment on console."""
    time_dimension = routing.GetDimensionOrDie('Time_dimension')
    total_time = 0
    plan_output_df = pd.DataFrame()
    print('Breaks:')
    intervals = assignment.IntervalVarContainer()
    lunch = 1
    for i in range(intervals.Size()):
        brk = intervals.Element(i)
        brk.Var()
        if brk.PerformedValue() == 1:
            print('{}: Start({}) Duration({})'.format(
                brk.Var().Name(),
                brk.StartValue(),
                brk.DurationValue()))
            lunch_df = pd.DataFrame({'Tech': [brk.Var().Name()]
                                        , 'Node': [brk.Var().Name()], 'Start': [brk.StartValue()]
                                        , 'End': [brk.StartValue() + int(brk.DurationValue())]
                                        , 'Duration': [brk.DurationValue()]},
                                    columns=['Tech', 'Node', 'Start', 'End', 'Duration'])
            if lunch == 1:
                lunch_df_final = deepcopy(lunch_df)
                lunch = 0
            else:
                lunch_df_final = lunch_df_final.append(lunch_df)

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
    plan = 0
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

            plan_output += '{0} Time({1},{2}) Duration({3})-> '.format(
                manager.IndexToNode(index), assignment.Min(time_var),
                assignment.Min(time_var)+int(node_load), route_load)
            plan_output_df_sub = pd.DataFrame({'Tech': [vehicle_id]
                                                  , 'Node': [manager.IndexToNode(index)]
                                                  , 'Start':[assignment.Min(time_var)],
                                               'End': [assignment.Min(time_var) + int(node_load)], 'Duration':[route_load]},
                                              columns=['Tech', 'Node', 'Start', 'End', 'Duration'])
            if start == 1:
                plan_output_df = deepcopy(plan_output_df_sub)
            else:
                plan_output_df = plan_output_df.append(plan_output_df_sub)
            start = 0
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
        plan_output_df_sub = pd.DataFrame({'Tech': [vehicle_id]
                                              , 'Node': [manager.IndexToNode(index)], 'Start': [assignment.Min(time_var)],
                                           'End': [assignment.Max(time_var)],
                                           'Duration': [route_load]},
                                          columns=['Tech', 'Node', 'Start', 'End', 'Duration'])

        if start == 1:
            plan_output_df = deepcopy(plan_output_df_sub)
        else:
            plan_output_df = plan_output_df.append(plan_output_df_sub)

        if plan == 0:
            plan_final_df = deepcopy(plan_output_df)
            plan = 1
        else:
            plan_final_df = plan_final_df.append(plan_output_df)
        plan_output += 'End time of the route: {}hr(s)\n'.format(
            assignment.Min(time_var)/100)
        plan_output += 'Time spent on the route: {}hr(s)\n'.format(route_distance/100)
        plan_output += 'Job duration of the route: {}hr(s)\n'.format(round(route_load/100, 2))
        print(plan_output)
        total_distance += route_distance
        total_load += route_load
        total_time += assignment.Min(time_var)
        if route_distance > 0:
            techs_used += 1
    print('Total time spent on all routes: {}hr(s)'.format(total_distance/100))
    print('Total job duration of all routes: {}hr(s)'.format(round(total_load/100, 2)))
    # print('Total time of all routes: {}hr(s)'.format(total_time))
    print('Total techs used: {}'.format(techs_used))
    print(routing.GetArcCostForVehicle(vehicle=1, from_index=1, to_index=5))

    plan_final_df = plan_final_df.append(lunch_df_final)
    plan_final_df['Tech'].replace(all_id_dict, inplace=True)
    plan_final_df['Node'].replace(all_id_dict, inplace=True)

    return plan_final_df
