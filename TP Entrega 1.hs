{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Usuario= UnUsuario {
        nombre :: String,
        billetera::Float
      } deriving (Show, Eq)

--EJEMPLOS--
pepe = UnUsuario "Jose" 10
lucho = UnUsuario "Luciano" 2

-- 1) EVENTOS

deposito dineroADepositar usuario = usuario{ billetera = dineroADepositar + (billetera usuario)}

extracción dineroAExtraer usuario =
        usuario{ billetera = (resultadoFinal ((billetera usuario) - dineroAExtraer)) }

resultadoFinal dineroRestado | dineroRestado  > 0 = dineroRestado
                             | otherwise          = 0

upgrade usuario = usuario{ billetera = upgradeBilletera (billetera usuario)}

upgradeBilletera monto | monto * 0.2 < 10 = monto * 1.2
                       | otherwise        = monto + 10

cierreDeCuenta unUsuario = unUsuario {billetera = 0}


quedaIgual = id


--Consulta: todas las funciónes tiene que devovler al usuario completo ?
