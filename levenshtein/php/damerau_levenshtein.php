<?php

/**
 * Naïve implementation of Damerau-Levenshtein distance
 * - does not work when there are neighbouring transpositions.
 */
function damerau_levenshtein($S1, $S2)
{
	$L1 = strlen($S1);
	$L2 = strlen($S2);
	if ($L1==0 || $L2==0) {
		// Trivial case: one string is 0-length
		return max($L1, $L2);
	}
	else {
		// The cost of substituting the last character
		$substitutionCost = ($S1[$L1-1] != $S2[$L2-1])? 1 : 0;
		// {H1,H2} are {L1,L2} with the last character chopped off
		$H1 = substr($S1, 0, $L1-1);
		$H2 = substr($S2, 0, $L2-1);
		if ($L1>1 && $L2>1 && $S1[$L1-1]==$S2[$L2-2] && $S1[$L1-2]==$S2[$L2-1]) {
			return min (
				DamerauLevenshtein($H1, $S2) + 1,
				DamerauLevenshtein($S1, $H2) + 1,
				DamerauLevenshtein($H1, $H2) + $substitutionCost,
				DamerauLevenshtein(substr($S1, 0, $L1-2), substr($S2, 0, $L2-2)) + 1
			);
		}
		return min (
			DamerauLevenshtein($H1, $S2) + 1,
			DamerauLevenshtein($S1, $H2) + 1,
			DamerauLevenshtein($H1, $H2) + $substitutionCost
		);
	}
}

