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


