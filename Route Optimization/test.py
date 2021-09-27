import numpy as np
data = {}

data['time_matrix_vehicle'] = np.array([[[0+t for t in range(4)]
                                         for j in range(10 + 4)]
                                        for i in range(10 + 4)])

data['time_matrix_vehicle'][4, 10, 1]

data['time_matrix_vehicle_bins'] = np.array([[[[data['time_matrix_vehicle'][i, j, t] + b for b in range(4)]
                                          for t in range(4)]
                                         for j in range(10 + 4)]
                                        for i in range(10 + 4)])

[4, 10, 1, 0]

round(761/400)-2

data['time_matrix_vehicle_bins'][0, 2, 0, 0]
