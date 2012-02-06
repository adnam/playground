"""
Find closest pairs of a list of coordinates
"""

from pairs import squaredist

def sort_x_coords(coordinates):
    """ For a list of coordinates, sort according to their
    placement on the X-axis """
    return sorted(coordinates, key=lambda k: (k[0]))

def sort_y_coords(coordinates):
    """ For a list of coordinates, sort according to their
    placement on the Y-axis """
    return sorted(coordinates, key=lambda k: (k[1]))

def closest_sorted(coordinates, sort_method):
    sorted = sort_method(coordinates)
    closest = closest_dist = None
    for i in xrange(len(coordinates)-1):
        dist = squaredist(sorted[i], sorted[i+1])
        if closest_dist is None or dist < closest_dist:
            closest_dist = dist
            closest = (sorted[i], sorted[i+1])
    return (closest, closest_dist)
    
def closest_x(coordinates):
    return closest_sorted(coordinates, sort_x_coords)

def closest_y(coordinates):
    return closest_sorted(coordinates, sort_y_coords)

def closest(coordinates):
    cx, dx = closest_x(coordinates)
    cy, dy = closest_y(coordinates)
    if dy < dx:
        return cy
    return cx

def main():
    tests = (
        [(0, 0), (1, 20), (5, 2), (1, 1), (20, 20)],
        [(0, 0), (1, 20), (5, 2)],
        [(-10, 10), (1, 5), (4, 3)]
    )
    for testset in tests:
        #closest_x(testset)
        #closest_y(testset)
        print "The closest points of ", testset, " is ", closest(testset)
    
if __name__ == "__main__":
    main()

