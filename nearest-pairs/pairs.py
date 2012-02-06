"""
Find closest pairs of a list of coordinates
"""

from itertools import combinations
from math import sqrt

def squaredist(P1, P2):
    return pow(P1[0]-P2[0], 2) + pow(P1[1]-P2[1], 2)

def dist(P1, P2):
    return sqrt(squaredist(P1, P2))

def closest(coordinates):
    (closest, shortest_dist) = (None, 0)
    for (P1, P2) in combinations(coordinates, 2):
        sqdist = squaredist(P1, P2)
        if closest is None or sqdist < shortest_dist:
            shortest_dist = sqdist
            closest = (P1, P2)
    return closest

def main():
    tests = (
        [(0, 0), (1, 20), (5, 2)],
        [(-10, 10), (1, 5), (4, 3)]
    )
    for testset in tests:
        print "The closest points of ", testset, " is ", closest(testset)
    
if __name__ == "__main__":
    main()

