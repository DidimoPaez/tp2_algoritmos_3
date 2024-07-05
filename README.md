### Materia: Paradigmas de Programación, curso Corsi-Essaya.
### Integrantes:
Ricardo Contreras: 107239,
Dídimo Páez: 98910


# TP2: Sistemas-L

## Consigna:
Implementar un programa en Clojure que permita generar imágenes fractales, mediante un algoritmo basado en sistemas-L,
una simulación de gráficos tortuga y el formato de imágenes estándar SVG.

## El programa:
El proyecto está programado en Clojure utilizando Leiningen.

Para ejecutar el programa se debe recibir tres parámetros por la línea de comandos (que se recibirán como parámetros
en la función -main), de la forma:

```sh
$ lein run arbol.sl 3 arbol.svg
```
De esta forma el primer parámetro corresponderá al archivo .sl de lectura, el segundo parámetro será la cantidad de 
iteraciones a realizar por la tortuga y el tercer parámetro el nombre de archivo .svg que se desea para escribir el
resultado (*Los archivos de lectura de prueba se encuentran en la carpeta **doc** del proyecto*).

## Entrega:
El proyecto se entregará en Git Hub, para lo cual se realizará el respectivo pull request, efectuándolo desde la rama
**etapa_1**. 
