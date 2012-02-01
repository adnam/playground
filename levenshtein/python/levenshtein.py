"""
Function to recursively compute the Levenshtein distance between two strings
"""

def rLevenshtein(S1, S2):
    L1 = len(S1)
    L2 = len(S2)
    if L1==0 or L2==0:
        return max([L1, L2])
   
    if L1 > 1 and L2 > 1 and S1[-1] != S2[-1]:
        substitutionCost = 1
    else:
        substitutionCost = 0
    try: H1 = S1[0:-1]
    except IndexError: H1 = ""
    try: H2 = S2[0:-1]
    except IndexError: H2 = ""
    return min([
        rLevenshtein(H1, S2)+1,
        rLevenshtein(S1, H2)+1,
        rLevenshtein(H1, H2)+substitutionCost])

