<?php

// Ensure ./lib is on include_path
set_include_path(implode(PATH_SEPARATOR, array(
    __DIR__ . '/lib',
    get_include_path(),
)));
require_once 'Zend/Loader/Autoloader.php';
Zend_Loader_Autoloader::getInstance()
    ->registerNamespace("Softonic_");

$toEncrypt = "abcdwxyzABCDWXYZ";
$numericEncrypter = Softonic_Encrypter::getInterface('numeric');
$kakunaEncrypter = Softonic_Encrypter::getInterface('kakuna');

echo "Input string: " . $toEncrypt . "\n";
echo "Kakuna encryption: " . $kakunaEncrypter->encrypt($toEncrypt) . "\n";
echo "Numeric encryption: " . $numericEncrypter->encrypt($toEncrypt) . "\n";

