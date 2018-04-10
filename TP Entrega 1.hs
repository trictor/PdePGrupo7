{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Usuario= UnUsuario {
        nombre :: String,
        billetera::Dinero
      } deriving (Show, Eq)

type Dinero = Float

--EJEMPLOS--
pepe = UnUsuario "Jose" 10
lucho = UnUsuario "Luciano" 2

-- 1) EVENTOS 

deposito :: Dinero -> Usuario -> Usuario

deposito dineroADepositar usuario = usuario{ billetera = dineroADepositar + (billetera usuario)}

extracción :: Dinero -> Usuario -> Usuario

extracción dineroAExtraer usuario =
        usuario{ billetera = (resultadoFinal ((billetera usuario) - dineroAExtraer)) }

resultadoFinal :: Dinero -> Dinero

resultadoFinal dineroRestado | dineroRestado  > 0 = dineroRestado
                             | otherwise          = 0

upgrade :: Usuario -> Usuario

upgrade usuario = usuario{ billetera = upgradeBilletera (billetera usuario)}

upgradeBilletera :: Dinero -> Dinero

upgradeBilletera monto | monto * 0.2 < 10 = monto * 1.2
                       | otherwise        = monto + 10


cierreDeCuenta :: Usuario -> Usuario

cierreDeCuenta unUsuario = unUsuario {billetera = 0}

quedaIgual :: Usuario -> Usuario 
quedaIgual = id


--Consulta: todas las funciónes tiene que devovler al usuario completo ?
