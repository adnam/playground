"""
Horowitz and Sahni solution to subset-sum problem
"""

from itertools import combinations

def subsets(numbers):
    """ Return a generator for all the subsets of a given list of numbers"""
    for i in xrange(1, len(numbers)):
        for combo in combinations(numbers, i):
            yield combo

def sumsubsets(numbers):
    """ Return a generator of the sums for all subsets of a list"""
    for subset in subsets(numbers):
        yield sum(subset)

def subset_sum_zero(numbers):
    """ Returns True if a subset sums to zero, False otherwise """
    return subset_sum(numbers, 0)

def subset_sum(numbers, N):
    """ Returns True if a subset sums to N, False otherwise """
    pivot = abs(len(numbers)/2)
    left = numbers[0:pivot]
    right = numbers[pivot:]
    sumleft = list(sumsubsets(left))
    sumleft.sort()
    sumright = list(sumsubsets(right))
    sumright.sort()
    for el1 in sumleft[-1:0:-1]:
        sumtry = el1
        for el2 in sumright:
            sumtry += el2
            if sumtry == N:
                return True
            elif sumtry > N:
                continue
    return False

def main():
    tests = (
        [0, 1, 2, -3],
        [1, 2, 3, -8],
        [1, 4, 5, 2, -3]
    )
    for testset in tests:
        if subset_sum_zero(testset):
            print "The set ", testset, " has a subset that sums to zero"
        else:
            print "The set ", testset, " does NOT have a subset that sums to zero"
            
if __name__ == "__main__":
    main()

