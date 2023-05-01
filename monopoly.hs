import Text.Show.Functions ()

--ESTRUCTURA DE UN JUGADOR
data Jugador = UnJugador { 
    nombre      :: String, 
    dinero      :: Int, 
    tactica     :: String, 
    propiedades :: [Propiedad], 
    acciones    :: [Accion] 
} deriving Show

--ESTRUCTURA DE UNA PROPIEDAD
data Propiedad = UnaPropiedad {
    titulo  :: String,
    precio  :: Int
} deriving Show

--MODELADO DE JUGADORES CAROLINA Y MANUEL
carolina :: Jugador
carolina = UnJugador "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Jugador
manuel = UnJugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

--ACCIONES
type Accion = Jugador -> Jugador

mapDinero :: (Int -> Int) -> Jugador -> Jugador
mapDinero unaFuncion unJugador = unJugador { dinero = (unaFuncion . dinero) unJugador }

agregarPlata :: Int -> Jugador -> Jugador           
agregarPlata unaCantidad unJugador = mapDinero (+ unaCantidad) unJugador

sacarPlata :: Int -> Jugador -> Jugador
sacarPlata unaCantidad unJugador = mapDinero (flip (-) unaCantidad) unJugador

agregarAccion :: Accion -> Jugador -> Jugador      
agregarAccion unaAccion unJugador = unJugador {acciones = unaAccion : acciones unJugador}

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = (cambiarTacticaACompradorCompulsivo . agregarPlata 40) unJugador

gritar :: Accion
gritar unJugador = unJugador {nombre = "AHHHH" ++ nombre unJugador}

enojarse :: Accion
enojarse unJugador = (agregarAccion gritar . agregarPlata 50) unJugador

cambiarTacticaACompradorCompulsivo :: Jugador -> Jugador
cambiarTacticaACompradorCompulsivo unJugador = unJugador {tactica = "Comprador compulsivo"}

esAccionista :: Jugador -> Bool
esAccionista unJugador = tactica unJugador == "Accionista"

esOferente :: Jugador -> Bool
esOferente unJugador = tactica unJugador == "Oferente singular"

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | esAccionista unJugador = agregarPlata 200 unJugador
    | otherwise = sacarPlata 100 unJugador

restarValorDePropiedad :: Propiedad -> Jugador -> Jugador
restarValorDePropiedad unaPropiedad unJugador = unJugador {dinero = dinero unJugador - precio unaPropiedad}

agregarPropiedad :: Propiedad -> Jugador -> Jugador
agregarPropiedad unaPropiedad unJugador = unJugador {propiedades = unaPropiedad : propiedades unJugador}

ganarSubasta :: Propiedad -> Jugador -> Jugador
ganarSubasta unaPropiedad unJugador = (agregarPropiedad unaPropiedad . restarValorDePropiedad unaPropiedad) unJugador

puedeSubastar :: Jugador -> Bool
puedeSubastar unJugador = esOferente unJugador || esAccionista unJugador

subastar :: Propiedad -> Jugador -> Jugador
subastar unaPropiedad unJugador
    | puedeSubastar unJugador = ganarSubasta unaPropiedad unJugador
    | otherwise = unJugador

cantidadDePropiedades :: Jugador -> Int
cantidadDePropiedades unJugador = length (propiedades unJugador)

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = precio unaPropiedad < 150

cantidadDePropiedadesBaratas :: Jugador -> Int
cantidadDePropiedadesBaratas unJugador = (length . filter (==True) . map esPropiedadBarata) (propiedades unJugador)

gananciaDeAlquileres :: Jugador -> Int
gananciaDeAlquileres unJugador = (cantidadDePropiedadesBaratas unJugador * 10) + (cantidadDePropiedades unJugador - cantidadDePropiedadesBaratas unJugador)*20

cobrarAlquileres :: Accion
cobrarAlquileres unJugador = unJugador {dinero = dinero unJugador + gananciaDeAlquileres unJugador}