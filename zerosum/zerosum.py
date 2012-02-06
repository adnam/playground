"""
Find subsets of a list that sum to zero
"""
from itertools import combinations

def subsets(numbers):
    """ Return a generator for all the subsets of a given list of numbers"""
    for i in xrange(1, len(numbers)):
        for combo in combinations(numbers, i):
            yield combo

def sumzero(numbers):
    """ Returns True if a subset sums to zero, False otherwise """
    for subset in subsets(numbers):
        if sum(subset) == 0:
            return True
    return False

def main():
    tests = (
        [0, 1, 2, -3],
        [1, 2, 3, -8],
        [1, 4, 5, 2, -3]
    )
    for testset in tests:
        if sumzero(testset):
            print "The set ", testset, " has a subset that sums to zero"
        else:
            print "The set ", testset, " does NOT have a subset that sums to zero"
            
if __name__ == "__main__":
    main()

