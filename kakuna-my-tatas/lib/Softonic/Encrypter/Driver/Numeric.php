<?php

class Softonic_Encrypter_Driver_Numeric
    implements Softonic_Encrypter_Driver_Interface
{

    protected $_trans = null;

    public function __construct($config = null)
    {
        $this->init();
    }
    
    /**
     * Initialise the translation table
     *
     */
    public function init()
    {
        for ($i = 65; $i <= 90; $i++) {
            $this->_trans[chr($i)] = $i - 64;
        }
        for ($i = 97; $i <= 122; $i++) {
            $this->_trans[chr($i)] = $i - 96;
        }
    }
        
    /**
     * Encypt a string according to the "Numeric" cipher
     * 
     * Does not support multibyte strings
     */
    public function encrypt($string)
    {
        list ($output, $len) = array(array(), strlen($string));
        for ($i = 0; $i < $len; $i++) {
            $chr = $string[$i];
            if (!array_key_exists($chr, $this->_trans)) {
                throw new Exception("Numeric encrypter can only handle"
                    . " characters in the range [a-zA-Z]");
            }
            $output[] = $this->_trans[$chr];
        }
        return implode('-', $output);
    }
    
}
