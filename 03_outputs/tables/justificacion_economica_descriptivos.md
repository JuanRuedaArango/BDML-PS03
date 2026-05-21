# Justificación económica de las estadísticas descriptivas

Las estadísticas descriptivas permiten revisar si las variables construidas tienen sentido económico y si existen diferencias relevantes entre la base de entrenamiento y la base de predicción. Esto es especialmente importante porque el modelo se entrena con información de propiedades de Bogotá, pero debe predecir precios en Chapinero. Por tanto, comparar Train y Test ayuda a identificar posibles problemas de extrapolación espacial.

Las variables extraídas del texto del anuncio capturan atributos de calidad del inmueble que no siempre aparecen en las columnas estructuradas. Características como parqueadero, terraza, balcón, depósito, gimnasio, piscina, seguridad, cocina integral, remodelación, lujo, penthouse o dúplex pueden aumentar la disposición a pagar porque representan comodidad, exclusividad, mejor dotación o mayor calidad percibida de la vivienda.

Las variables estructurales de la base, como número de baños, habitaciones, cuartos y superficie, representan los atributos físicos centrales del inmueble. Desde un enfoque de precios hedónicos, estas características deberían explicar una parte importante del precio porque reflejan tamaño, capacidad, funcionalidad y condiciones habitacionales básicas.

Las variables OSM incorporan información del entorno urbano. Las distancias a parques, hospitales, CAI, aeropuerto, estaciones de transporte y centro, junto con conteos de supermercados, colegios, estaciones y tiendas en un radio cercano, aproximan accesibilidad, disponibilidad de servicios y calidad de localización. En economía urbana, estos atributos pueden capitalizarse en el precio de la vivienda porque afectan tiempos de desplazamiento, calidad de vida y conveniencia del entorno.

Las variables de Datos Abiertos de Bogotá permiten capturar condiciones institucionales y territoriales. Variables como valor de referencia, número de predios, grupo de uso económico, estrato y seguridad aproximan características del mercado local, intensidad de uso del suelo, entorno socioeconómico y riesgo percibido. Estas dimensiones son relevantes para explicar diferencias de precios entre zonas de la ciudad.

En conjunto, estas descriptivas no solo resumen los datos, sino que permiten evaluar si las variables incluidas en los modelos son consistentes con la teoría económica de precios hedónicos y si el modelo enfrenta un cambio de distribución importante entre Train y Test.
