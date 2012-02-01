<?php

require_once __DIR__ . '/levenshtein.php';
require_once __DIR__ . '/damerau_levenshtein.php';


$tests = array();
// format: [string 1, string 2, expected result, comment]
$tests[] = array("",		"",			0,	'Empty strings');
$tests[] = array("abc",		"abc",		0,	'Identicle strings');
$tests[] = array("abc",		"abcd",		1,	'Insert 1 character at end of string');
$tests[] = array("abc",		"",			3,	'Adding 3 characters to empty string');
$tests[] = array("acbd",	"abcd",		1,	'Swap 2 characters in middle of string');
$tests[] = array("abcd",	"abdc",		1,	'Swap 2 characters at end of string');
$tests[] = array("abcde",	"acde",		1,	'Insert 1 character in middle of string');
$tests[] = array("abcdef",	"acebdf",	3,  'Adjacent transpositions');

?>
<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN'
	'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>
<html lang="en">
<head>
	<title>Levenshtein distance</title>
	<link rel="stylesheet" href="http://www.w3.org/StyleSheets/Core/Steely" type="text/css" />
	<style type="text/css">
		table.results {
			border-collapse: collapse;
			width: 600px;
		}
		table.results td, table.results th {
			border: 1px solid #aaaaaa;
			padding: 3px;
		}
	</style>
</head>

<body>
	<h2>Levenshtein distance algorithm</h2>
	<table class="results" border=1>
		<tr>
			<th>S1</th>
			<th>S2</th>
			<th>levenshtein()<br/><small>PHP function</small></th>
			<th>rLevenshtein()</th>
			<th>damerau_levenshtein()</th>
			<th>Expected</th>
			<th>Comment</th>
		</tr>
		<?php foreach ($tests as $test): ?>
		<tr>
			<td><?=$test[0];?></td>
			<td><?=$test[1];?></td>
			<td><?=levenshtein($test[0], $test[1]); ?></td>
			<td><?=rLevenshtein($test[0], $test[1]); ?></td>
			<td><?=damerau_levenshtein($test[0], $test[1]); ?></td>
			<td><?=$test[2];?></td>
			<td style="font-size: 0.85em"><?=$test[3];?></td>
		</tr>
		<?php endforeach; ?>
	</table>

</body>
</html>

