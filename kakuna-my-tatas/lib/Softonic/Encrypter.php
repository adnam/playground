<?php


class Softonic_Encrypter
{
    
    const DRIVER_NS = 'Softonic_Encrypter_Driver_';

    public static function getInterface($driver, $config = array())
    {
        $className = self::getDriverClassName($driver);
        if (!class_exists($className)) {
            throw new Exception("Encrypter driver $className does not exist.");
        }
        $instance = new $className($config);
        if (!$instance instanceof Softonic_Encrypter_Driver_Interface) {
            throw new Exception("Encrypter driver $className must implement "
                . "Softonic_Encrypter_Driver_Interface.");
        }
        return $instance;
    }
    
    public static function getDriverClassName($driver, $namespace = Softonic_Encrypter::DRIVER_NS)
    {
        $className = ucfirst($driver);
        if ($namespace) {
            $className = $namespace . $className;
        }
        return $className;
    }

}
