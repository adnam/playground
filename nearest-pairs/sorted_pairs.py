"""
Find closest pairs of a list of coordinates
"""

from itertools import combinations

def squaredist(coords):
    """Calculate the square of the distance between a coordinate pair
    coords = ((X1, Y1), (X2, Y2))
    """
    return pow(coords[0][0]-coords[1][0], 2) + pow(coords[0][1]-coords[1][1], 2)

def dist(P1, P2):
    """Calculate the distance between a coordinate pair"""
    return sqrt(squaredist(P1, P2))

def sort_x_coords(coordinates):
    """ For a list of coordinates, sort according to their
    placement on the X-axis """
    return sorted(coordinates, key=lambda k: (k[0]))

def sort_y_coords(coordinates):
    """ For a list of coordinates, sort according to their
    placement on the Y-axis """
    return sorted(coordinates, key=lambda k: (k[1]))

def nearest(coordinates):
    sorted = sort_x_coords(coordinates)
    return nearest_sorted(sorted)

def nearest_brute_force(coordinates):
    (closest, shortest_dist) = (None, 0)
    for (P1, P2) in combinations(coordinates, 2):
        sqdist = squaredist((P1, P2))
        if closest is None or sqdist < shortest_dist:
            shortest_dist = sqdist
            closest = (P1, P2)
    return closest

def split_coords(coordinates):
    """For a list of coordinates ordered by X-axis, split them 
    into two groups along a central pivot line. Returns a tuple
    (left_list, right_list, pivot_point)"""
    pivot_point = abs(len(coordinates)/2)
    left_list = nearest_sorted(coordinates[0:pivot_point])
    right_list = nearest_sorted(coordinates[pivot_point:])
    return (left_list, right_list, pivot_point)

def nearest_sorted(coordinates):
    """For a set of coordinates which have been sorted by 
    X-axis coordinate, return the pair with the shortest
    distance between them"""
    if len(coordinates) <= 3:
        # Trivial case - three coordinates or less, use brute force
        return nearest_brute_force(coordinates)
    left, right, pivot = split_coords(coordinates)
    dist_left = squaredist(left)
    dist_right = squaredist(right)
    delta, nearest_pair = (dist_left, left) if dist_left < dist_right else (dist_right, right)
    marginal = marginal_points(left, right, pivot, delta)
    for i in xrange(len(marginal)):
        A = marginal[i]
        for j in xrange(i-1, len(marginal), -1):
            try:
                if marginal[i][1] - marginal[j][1] >= delta:
                    break
                pairdist = squaredist((marginal[i], marginal[j]))
                if pairdist < delta:
                    delta = pairdist
                    nearest_pair = (marginal[i], marginal[j])
            except IndexError: break

        for j in xrange(i+1, len(marginal)):
            try:
                if marginal[j][1] - marginal[i][1] >= delta:
                    break
                pairdist = squaredist((marginal[i], marginal[j]))
                if pairdist < delta:
                    delta = pairdist
                    nearest_pair = (marginal[i], marginal[j])
            except IndexError: break
    return nearest_pair

def marginal_points(left, right, pivot, delta):
    """For two lists of points "left" and "right", either
    side of a bisection line "pivot", return a list of all
    the coordinates within "delta" of that line, ordered
    by Y-coordinate"""
    marginal = []
    for l_point in left[-1::-1]:
        if pivot-l_point[0] < delta/2:
            marginal.append(l_point)
        else: break
    for r_point in right:
        if r_point[0]-pivot < delta/2:
            marginal.append(r_point)
        else: break
    return sort_y_coords(marginal)

def main():
    tests = (
        [(0, 0), (1, 20), (5, 2), (1, 1), (20, 20)],
        [(0, 0), (1, 20), (5, 2)],
        [(-10, 10), (1, 5), (4, 3)],
        [(1, 1), (2, 20), (20, 2), (3, 3)],
        [(0,0),(7,6),(2,20),(12,5),(16,16),(5,8),(19,7),(14,22),(8,19),(7,29),(10,11),(1,13)],
        [(1,1), (3,3), (3,5), (4,5), (4,7), (9,9)]
    )
    for testset in tests:
        print "The closest points of", testset, " is ", nearest(testset)
    
if __name__ == "__main__":
    main()

