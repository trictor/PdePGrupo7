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
	test11
	test12
	test13
	test14
	test15
	test16
	test17

test1 = it "La billetera deberia quedar en 20, luego de depositar 10" $ ( billetera.deposito 10 ) pepe `shouldBe` 20 -- REVISAR PARENTESIS

test2 = it "La billetera deberia quedar en 7, luego de extraer 3" ( ( billetera.(extracción 3) $ pepe )`shouldBe` 7)

test3 = it "La billetera deberia quedar en 0, luego de extraer 15" ( ( billetera.(extracción 15) $ pepe )`shouldBe` 0)

test4 = it "La billetera deberia quedar en 12, luego de un upgrade" ( ( billetera.upgrade $ pepe )`shouldBe` 12)

test5 = it "La billetera deberia quedar en 0, luego de cerrar cuenta" ( ( billetera.cierreDeCuenta $ pepe )`shouldBe` 0)

test6 = it "La billetera deberia quedar en 10, si no se hace nada" ( ( billetera.quedaIgual $ pepe )`shouldBe` 10)

test7 = it "La billetera deberia quedar en 1020, luego de depositar 1000 y hacer un upgrade" ( ( billetera.upgrade.(deposito 1000) $ pepe )`shouldBe` 1020)

test8 = it "La billetera de pepe esta en 10" (billetera pepe `shouldBe` 10)

test9 = it "la Billetera de Pepe queda en 0, luego de un cierre de cuenta" (( billetera.cierreDeCuenta $ pepe ) `shouldBe` 0)

test10 = it "La billetera deberia quedar en 27.6, luego de depositar 15, extraer 2 y hacer un upgrade" ( ( billetera.upgrade.(extracción 2).(deposito 15) $ pepe )`shouldBe` 27.6)

test11 = it "La billetera deberia quedar con el monto que se creo al usuario pepe" (((transaccion 1) $ pepe) `shouldBe` pepe)

test12 = it "La billetera de Pepe queda en 15, luego de depositar 5 monedas" (((transaccion 2) $ pepe) `shouldBe` nuevoValorBilletera 15 pepe)

test13 = it "La billetera de Pepe2 queda en 55, luego de depositar 5 monedas y depositar 5 a la nueva billetera que tenia 50 monedas" (((deposito 5.nuevoValorBilletera 50.deposito 5) $ pepe2) `shouldBe` nuevoValorBilletera 55 pepe2)

test14 = it "La billetera de Lucho queda en 0, luego de depositar 15, hacer un upgrade y cerrar la cuenta" (((transaccion 3) $ lucho) `shouldBe` nuevoValorBilletera 0 lucho)

test15 = it "La billetera de Lucho2 queda en 34, luego de depositar 1 moneda, luego depositar 2, extraer 1, depositar 8, tener un upgrade, y depositar 10." (((transaccion 4) $ lucho2) `shouldBe` nuevoValorBilletera 34 lucho2)

test16 = it "La billetera de Pepe queda en 3, luego de extraer 7 monedas" (((transaccion 5) $ pepe) `shouldBe` nuevoValorBilletera 3 pepe)

test17 = it "La billetera de Lucho2 queda en 17, luego de depositar 7 monedas" (((transaccion 5) $ lucho2) `shouldBe` nuevoValorBilletera 17 lucho2)

{-
	testN = it "QueTestea" (EstoEjecuta `shouldBe` Resultado)
-}

data Usuario = UnUsuario {
        nombre :: String,
        billetera::Dinero
      } deriving (Show, Eq)

type Dinero = Float

type Transaccion = Usuario -> Usuario

type ValidacionUsuario = Usuario -> Bool

--EJEMPLOS--
pepe = UnUsuario "Jose" 10
pepe2 = UnUsuario "Jose" 20
lucho = UnUsuario "Luciano" 2
lucho2 = UnUsuario "Luciano" 10

-- 1) EVENTOS

deposito :: Dinero -> Transaccion

deposito dineroADepositar usuario = usuario{ billetera = dineroADepositar + (billetera usuario)} -- ABSTRAER A NUEVA BILLETERA

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

tocoYMeVoy  = cierreDeCuenta.upgrade.(deposito 15) -- REVISAR 

ahorranteErrante :: Transaccion

ahorranteErrante  = (deposito 10).upgrade.(deposito 8).(extracción 1).(deposito 2).(deposito 1) -- REVISAR 

transaccion :: Int -> Transaccion

transaccion numeroDeTransaccion usuario = (obtenerOperacion numeroDeTransaccion usuario) usuario

obtenerOperacion :: Int -> Usuario -> Transaccion     -- GENERALIZAR FUNCION DE VALUDACION Y FUNCION TRANSACCION GENERICA 

obtenerOperacion n usuario 	| n == 1 && esLucho usuario = cierreDeCuenta
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

--Funcion para hacer tests

nuevoValorBilletera nuevoValor unUsuario= unUsuario {billetera = nuevoValor}
