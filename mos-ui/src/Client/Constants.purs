module Client.Constants where

import DBus (BusName (..), ObjectPath (..), InterfaceName (..), MemberName (..))


monerodoBus :: BusName
monerodoBus = BusName "com.moneroworld.Monerodo"

monerodoObject :: ObjectPath
monerodoObject = ObjectPath "/"

monerodoControl :: InterfaceName
monerodoControl = InterfaceName "com.moneroworld.Control"

monerodoControlMethod :: MemberName
monerodoControlMethod = MemberName "Control"



-- Electron:

controlInput :: String
controlInput = "ControlInput"

controlOutput :: String
controlOutput = "ControlOutput"
