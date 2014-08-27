<?php

class Softonic_Encrypter_Driver_Kakuna
    implements Softonic_Encrypter_Driver_Interface
{
    

    /**
     * Encypt a string according to the "Kahuna" cipher
     * 
     * NB. Does not support multibyte strings
     */
    public function encrypt($string)
    {
        list ($output, $len) = array("", strlen($string));
        for ($i = 0; $i < $len; $i++) {
            $output .= $this->transform($string[$i]);
        }
        return $output;
    }

    /**
     * Apply the Caeser cipher transformation to a character
     */
    public function transform($char)
    {
        $ascii = ord($char);
        if ($ascii < 65 || $ascii > 122 || ($ascii > 90 && $ascii < 97)) {
            throw new Exception("Kakuna encrypter can only handle"
                . " characters in the range [a-zA-Z]");
        }
        $ord = $ascii + 3;
        if ($ord > 122 || ($ord > 90 && $ord < 97)) {
            $ord = $ord - 26;
        }
        return chr($ord);
    }

}
