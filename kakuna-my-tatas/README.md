Encrypter
=========

We would like to encrypt the password for our users, so every class that needs
to encrypt or decrypt a password in our application, uses an “Encrypter” 
object that given an input string returns an encrypted / decrypted string. We
use this object by calling 2 methods: encrypt and decrypt.

We have currently 2 different strategies to encrypt passwords in our 
application. First one uses and old fashion method called ‘Kakuna’ that 
transforms every character to the character situated 3 positions after.
7For example an ‘a’ would be transformed to a ‘d’. A ‘b’ to a ‘e’. A ‘z’
to a ‘c’ and so on. That means that this system is circular and every letter 
has a unique translation.

The numeric strategy transforms every letter to the number given by the 
letter’s position in the alphabet. And it separates the numbers with dashes.
So ‘abc’ would be ’1-2-3’.

We haven’t decided which system we are going to use yet. So we need a 
structure that allows us to switch from a system to another very easily,
like changing one line of code in the class that was using the 
‘Encrypter’ object.

It’s possible that next month we come up with a new strategy to encrypt our 
passwords, so the structure has to allow us to easily implement a new strategy 
and start using it.

We ask you to write classes for the Kakuna strategy, and the Numeric strategy.
They must contain at least the methods encrypt and decrypt. Write all the 
classes that you think are needed to build the system described above. Think 
what the best way to structure these classes is. We don’t ask you to create 
the classes that would use the ‘Encrypter’ object, just the ‘Encrypter’ system 
itself. You can assume that all texts will be lowercase.

Places where the “Encrypter” object is used look more or less like this:

    $user = $this->getPostParam( ‘user’ );
    $password = $this->getPostParam( ‘password’ );

    $encrypter = Encrypter::getInstance( 'kakuna' );
    $encryptedPassword = $encrypter->encrypt( $password );

    $found = $this->findUserInDB( $user, $encryptedPassword );

    If ($found)
    echo “Success!”;

Instructions
============

 *  We will evaluate if your code is easy to read / maintain, so try to pay 
    attention to this, including some documentation in your code.You have 
    to attach a zip file with all your classes
 *  It's not mandatory, but you can write some unit testing for your classes.
    You can even solve this exercise using TDD if you want.These things will 
    increase your marks.
 *  You have 1h 45m to solve this exercise, and you need to do it in PHP. 
    That means that you need a LAMP /WAMP /MAMP  environment running in your 
    computer before you start this test. Once you start the test, you can’t 
    pause it

Solution
========

We implement the adapter pattern with an "Encrypter" class that instantiates
a specified encrypter driver. Encrypter::getInterface() is a factory that 
returns the driver, with an optional namespace parameter.

Files
=====

    ├── example.php
    ├── lib
    │   └── Softonic
    │       ├── Encrypter
    │       │   ├── Driver
    │       │   │   ├── Interface.php
    │       │   │   ├── Kakuna.php
    │       │   │   └── Numeric.php
    │       │   └── Interface.php
    │       └── Encrypter.php
    └── README.md

    4 directories, 7 files

The file `example.php` assumes you have the Zend Framework in your `include_path`.


