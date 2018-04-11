{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

--TESTS--
testear = hspec $ do
	test1
	test2
	test3
	test4
	test5
	test6
	test7
	test8
	test9
	test10

test1 = it "La billetera deberia quedar en 20, luego de depositar 10" ( ( billetera.(deposito 10) $ pepe )`shouldBe` 20)

test2 = it "La billetera deberia quedar en 7, luego de extraer 3" ( ( billetera.(extracción 3) $ pepe )`shouldBe` 7)

test3 = it "La billetera deberia quedar en 0, luego de extraer 15" ( ( billetera.(extracción 15) $ pepe )`shouldBe` 0)

test4 = it "La billetera deberia quedar en 12, luego de un upgrade" ( ( billetera.upgrade $ pepe )`shouldBe` 12)

test5 = it "La billetera deberia quedar en 0, luego de cerrar cuenta" ( ( billetera.cierreDeCuenta $ pepe )`shouldBe` 0)

test6 = it "La billetera deberia quedar en 10, si no se hace nada" ( ( billetera.quedaIgual $ pepe )`shouldBe` 10)

test7 = it "La billetera deberia quedar en 1020, luego de depositar 1000 y hacer un upgrade" ( ( billetera.upgrade.(deposito 1000) $ pepe )`shouldBe` 1020)

test8 = it "La billetera de pepe esta en 10" (billetera pepe `shouldBe` 10)

test9 = it "la Billetera de Pepe queda en 0, luego de un cierre de cuenta" (( billetera.cierreDeCuenta $ pepe ) `shouldBe` 0)

test10 = it "La billetera deberia quedar en 27.6, luego de depositar 15, extraer 2 y hacer un upgrade" ( ( billetera.upgrade.(extracción 2).(deposito 15) $ pepe )`shouldBe` 27.6)

{-
	testN = it "QueTestea" (EstoEjecuta `shouldBe` Resultado)
-}

data Usuario= UnUsuario {
        nombre :: String,
        billetera::Dinero
      } deriving (Show, Eq)

type Dinero = Float

type Transaccion = Usuario -> Usuario

type ValidacionUsuario = Usuario -> Bool

--EJEMPLOS--
pepe = UnUsuario "Jose" 10
lucho = UnUsuario "Luciano" 2

-- 1) EVENTOS

deposito :: Dinero -> Transaccion

deposito dineroADepositar usuario = usuario{ billetera = dineroADepositar + (billetera usuario)}

extracción :: Dinero -> Transaccion

extracción dineroAExtraer usuario =
        usuario{ billetera = (resultadoFinal ((billetera usuario) - dineroAExtraer)) }

resultadoFinal :: Dinero -> Dinero

resultadoFinal dineroRestado | dineroRestado  > 0 = dineroRestado
                             | otherwise          = 0

upgrade :: Transaccion

upgrade usuario = usuario{ billetera = upgradeBilletera (billetera usuario)}

upgradeBilletera :: Dinero -> Dinero

upgradeBilletera monto | monto * 0.2 < 10 = monto * 1.2
                       | otherwise        = monto + 10


cierreDeCuenta :: Transaccion

cierreDeCuenta unUsuario = unUsuario {billetera = 0}

quedaIgual :: Transaccion
quedaIgual = id

tocoYMeVoy :: Transaccion

tocoYMeVoy  = cierreDeCuenta.upgrade.(deposito 15)

ahorranteErrante :: Transaccion

ahorranteErrante  = (deposito 10).upgrade.(deposito 8).(extracción 1).(deposito 2).(deposito 1)

transaccion :: Int -> Transaccion

transaccion numeroDeTransaccion usuario = (obtenerOperacion numeroDeTransaccion usuario) usuario

obtenerOperacion :: Int -> Usuario -> Transaccion

obtenerOperacion n usuario | n == 1 && esLucho usuario = cierreDeCuenta
						   | n == 2 && esPepe usuario = deposito 5
						   | n == 3 && esLucho usuario = tocoYMeVoy
						   | n == 4 && esLucho usuario = ahorranteErrante
						   | n == 5 && esPepe usuario = extracción 7
						   | n == 5 && esLucho usuario = deposito 7
 						   | otherwise = quedaIgual

esLucho :: ValidacionUsuario

esLucho usuario = nombre usuario == "Luciano"

esPepe :: ValidacionUsuario

esPepe usuario =  nombre usuario == "Jose"

