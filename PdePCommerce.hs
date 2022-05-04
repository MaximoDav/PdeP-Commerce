type Producto = (String, Float)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal unProducto cantidad descuento costoDeEnvio = aplicarCostoDeEnvio (cantidad * (aplicarDescuento unProducto descuento)) costoDeEnvio

productoDeElite :: Producto -> Bool
productoDeElite unProducto = (productoDeLujo unProducto) && (productoCodiciado unProducto) && (not.productoCorriente $ unProducto)

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento (_, precioDelProducto) descuento = precioDelProducto - descuento

entregaSencilla :: String -> Bool
entregaSencilla fechaDeEntrega = even.length $ fechaDeEntrega

descodiciarProducto :: String -> String
descodiciarProducto nombreDelProducto = take 10 nombreDelProducto

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreDelProducto, _) = elem 'x' nombreDelProducto || elem 'z' nombreDelProducto

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio unPrecio costoDeEnvio = unPrecio + costoDeEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreDelProducto, _) = (>10).length $ nombreDelProducto

productoCorriente :: Producto -> Bool
productoCorriente (nombreDelProducto, _) = elem (head nombreDelProducto) "aeiouAEIOU"

productoXL :: Producto -> Producto
productoXL (nombreDelProducto, precioDelProducto) = (nombreDelProducto ++ "XL", precioDelProducto)

versionBarata :: String -> String
versionBarata nombreDelProducto = reverse.descodiciarProducto $ nombreDelProducto