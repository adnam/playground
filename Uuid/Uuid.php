<?php
/**
 * A class for fast parsing and genertion of V4 UUIDs.
 * 
 * Examples:
 * <code>
 *  
 *  $uuid = new Uuid();
 *  
 *  print $uuid;
 *  // Gives, for example, 6b162bcc-0b12-49b0-9eac-fa7d946e368c
 *  
 *  print $uuid->hex();
 *  // Gives, for example, 6b162bcc0b1249b09eacfa7d946e368c
 *  
 *  // Create a UUID from a random hash
 *  $u2 = new Uuid(md5(mt_rand()));
 *
 *  // Use raw UUIDs in MySQL
 *  $sql = "INSERT INTO stuff (uuid) VALUES ('"
 *       . mysql_real_escape_string(new Uuid()->bin()) . "');";
 * </code>
 *
 * @see         http://en.wikipedia.org/wiki/UUID
 * @author      Adam Hayward <adam at happy dot cat>
 * @copyright   Copyright (c) 2010 Adam Hayward (http://happy.cat)
 * @license     New BSD License, http://svn.happy.cat/public/LICENSE.txt
**/

class Uuid
{
    
    protected $_bin;
    protected $_hex;
    
    /**
     * Create a new uuid
     * @param string    uuid string, in binary or hexadecimal representation
     */
    function __construct($uuid = null)
    {
        if (null === $uuid){
            $this->_bin = Uuid::gen();
            return;
        }
        $len = strlen($uuid);
        if ($len === 16) {
            $this->_bin = $uuid;
        }
        elseif ($len === 32) {
            $this->_hex = $uuid;
        }
        elseif ($len === 36) {
            $uuid = str_replace('-', '', $uuid);
            $this->_hex = $uuid;
        }
        else {
            throw new Exception('Invalid uuid');
        }
    }

    /**
     * Generate a v4 binary uuid
     */
    public static function gen()
    {
        return pack('n*', 
            mt_rand(0, 0xffff), mt_rand(0, 0xffff), mt_rand(0, 0xffff),
            mt_rand(0, 0x0fff) | 0x4000, mt_rand(0, 0x3fff) | 0x8000,
            mt_rand(0, 0xffff), mt_rand(0, 0xffff), mt_rand(0, 0xffff)
        );
    }

    /**
     * Return a hexadecimal representation (without dashes)
     * 
     * @return string   32-character string represetation of uuid
     */
    public function hex()
    {
        if (null === $this->_hex) {
            $hex = unpack('H*', $this->_bin);
            $this->_hex = $hex[1];
        }
        return $this->_hex;
    }

    /**
     * Return a binary representation
     * 
     * @return string   16-byte binary representation of uuid
     */
    public function bin()
    {
        if (null === $this->_bin) {
            $this->_bin = pack('H*', $this->_hex);
        }
        return $this->_bin;
    }

    /**
     * Return uuid as a string
     * 
     * @return string   36-character string representation of uuid
     */
    public function __toString()
    {
        return implode('-', sscanf($this->hex(), '%08s%04s%04s%04s%12s'));
    }
}
