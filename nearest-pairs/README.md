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
list of coordinates is sorted according to x-axis and then split into
two sets: "left" and "right" along a central pivot line.

The solution now lies in one of three posibilities:

- The closest-coordinates are both contained within the left-set;
- The closest-coordinates are both contained within the right-set;
- The closest pair has one point in the left-set and the other point in 
  the right set.

So we solve for each scenario separately, and pick the closest of the 
three. The first two possibilies can be solved recursively. If a subset 
contains three-or-less points, we can trivially solve if using the 
brute-force method described in *pairs.py*. The closest of the two
we assign the mutual distance to a variable 'delta'.

To find the closest pair which has a coordinate on both sides, we need 
only consider points that lie within 'delta/2' of the central pivot.
The function *marginal_points()* does this and returns the points
sorted by Y-axis coordinate.

Now we compare the points within this marginal set. Fortunately we need
not compare all combinations of points, only those that lie within
'delta' of each other, so we can traverse the marginal set in linear
time. While traversing, we can improve our value for delta if a closer
pair of points arises.

At the end we return the closest pair found in the left-set, 
right-set or the marginal-set.

