Zerosum Challenge
=================

Challenge
---------

Build a function that given a list of Integers in the range 
[-65000,65000], the function returns true if any subset of 
the list summed is equal to zero. False otherwise.

Example
-------

- [0, 1, 2, -3] returns true. As 1+2+(-3)==0.
- [1, 2, 3, -8] returns false. As no subset summed is equal 0.
- [1, 4, 5, 2, -3] returns true.

Notes
-----

*zerosum.erl* provides the 'trivial' solution to the subset sum problem in 
Erlang. The function "subsets/1" will produce all the combinations 
(subsets) of a list. The function "zerosum/1" then sums the subsets 
one-by-one and returns true if any sum to zero. Given that there are 2^N
subsets, and each subset requires summing, the program runs in O(N2^N) time.

*zerosum.py* is the same technique but implemented in python. It includes
a pythonic optimization of "lazily" generating the list of subsets, making 
use of python generators. To generate the subsets we use pythons 
'itertools.combinations()' function. Note that unlike "zerosum.erl", the 
python solution does not employ recursive functions. This is because python 
is not optimized for tail-call recursion, and very large lists would
undoubtably hit the built-in recursion limit. Like the Erlang solution, the
program runs in O(N2^N) time.

*horowitzi_sahni.py* is a faster solution to the subset-sum problem which
was published in the "Journal of the Association for Computing Machinery"
in 1972. It works like this:
 
 1. The function "subset_sum_zero()" receives a list of length N
 
 2.  Split the input list into two shorter lists of length ~N/2
 
 3.  For each list, get a list of all their subsets. Hence the list
     
        [1, 2, 3, 4, -5, 6]
     
     becomes the two following lists, A & B:
        
     - A: [[1], [2], [3], [1, 2], [1, 3], [2, 3]] and 
     - B: [[4], [-5], [6], [4, -5], [4, 6], [-5, 6]].
 
 4. For lists A & B, produce a list summing each sub list. In the above example:
    
    - Asum: [1, 2, 3, 3, 4, 5]
    - Bsum: [4, -5, 6, -1, 10, 1]
 
 5. Sort list "A" by order ascending, and "B" by order descending.
    
    - Asumsort: [1, 2, 3, 3, 4, 5]
    - Bsumsort: [-5, -1, 1, 4, 6, 10]
    
    If the result is larger than zero, skip to the next element in list A.
 
 6. For each element in list A, progressively sum each element in list B.
    If the result is zero, return true. Otherwise, return false.

This algorithm can potentially run in O(N2^(N/2)) by performing the expansion, 
summation and sorting steps together. However as implemented the steps are 
calculated separately so will unboubtably be slower.

Faster solutions
----------------

There exists a Pseudo-polynomial time solution which uses dynamic programming,
described in Wikipedia (see http://en.wikipedia.org/wiki/Subset_sum_problem).

Interestingly, the Horowitzi-Sahni solution has an interesting property that
could possibly yield even further spped improvements - parallelizability.

The generation of the two lists "Asumsort" and "Bsumsort" can be done 
indepentently and therefore in parallel. Additionally, each outer iteration 
in step "6" above (i.e. iterating list "A") can also be computed in parallel.

