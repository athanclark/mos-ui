{-# LANGUAGE
    OverloadedStrings
  #-}

module Daemon.Constants where

import DBus (BusName, ObjectPath, InterfaceName, MemberName)



monerodoBus :: BusName
monerodoBus = "com.moneroworld.Monerodo"


monerodoObject :: ObjectPath
monerodoObject = "/"


monerodoControl :: InterfaceName
monerodoControl = "com.moneroworld.Control"


monerodoControlMethod :: MemberName
monerodoControlMethod = "Control"
