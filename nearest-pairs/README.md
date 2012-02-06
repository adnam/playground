Nearest Pairs Challenge
=======================

Build a function that given a list of Integers pairs in the
range [-65000,65000], that represents {X, Y} Coordinates in 
a Plane. The function returns the two closest pairs.

Example
-------

- [{0,0}, {1,20}, {5, 2}] returns [{0,0}, {5, 2}]
- [{-10,10}, {1,5}, {4, 3}] returns [{1,5}, {4, 3}]

Notes
-----

*pairs.py* is the naïve solution which iterates all 2-tuple subsets
of a given list and selects the pair with the shortest distance.

*sorted_pairs.py* uses sorting to reach a quicker solution. First the
list of coordinates is sorted according to x-axis coordinates and the
distance between consecutive pairs is calculated; the shortest being 
found. The same procedure is performed again, but this time the coordinates 
are normalized to the y-axis. The solution is the shorter of the two
resulting coodinate-pairs.

In the second solution the coordinate set is sorted and compared in three
separate steps - two sorts and one final iteration to select the shortest
distance. But these steps can also be combined to further increase speed.
The running time would therefore be equal to that of the sorting algorithm,
eg O(NlogN).

