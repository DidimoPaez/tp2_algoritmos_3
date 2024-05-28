(ns tp-2-algo-3.core
  (:gen-class))

(def caracteres-permitidos ["F" "f" "G" "g" "+" "-" "[" "]" "|"])

;Recibe una lista done esten las reglas. ejemplo:
;("F FF", "G FG")
;Y devuelve un diccionario con clave:valor. Ejemplo:
;{F:"FF", G:"FG"}
(defn generar_reglas [v]
  (reduce (fn [acc x]
            (let [regla_k (first x)
                  regla_v (subs x 2)]
              (assoc acc (str regla_k) regla_v)))
          {} v))

(defn parser [s]
  (reduce (fn [acc x]
            (if (not= x \newline)
              (str acc x)
              (reduced acc)))
          "" s))

;; Tener presente que "drop" devuelve una secuencia, y al final del let lo que se está concatenando
;;es un string, por eso hay que utilizar "apply str"
(defn split [s]
  (if (<= (count s) 0)
    '()
    (let [linea (parser s)
          resto (drop (inc (count linea)) s)]
      (vec (cons linea (split (apply str resto)))))))

(defn leer_archivo [archivo]
  (slurp archivo))

;devuelve una sublista de a inclusive hasta b no inclussive
(defn sublista [lista a b]
  (let [lista-a-vector (apply vector lista)]
    (apply list (subvec lista-a-vector a b))))

;Devuelve un vector donde: 
;pos 0: el angulo
;pos 1: el axioma 
;pos 2: un diccionario co las reglas correspondientes
;pos 3: la catidad de iteraciones
;pos 4: el archivo de escritura
(defn obtener-informacion [args]
  (let [contenido-arch-read (leer_archivo (first args))
        obtener-lista-arch-read (split contenido-arch-read)
        obtener-directorio-reglas (generar_reglas (subvec obtener-lista-arch-read 2 (+ 1 (count obtener-lista-arch-read))));Obtiene una lista con un elemento que sera el directorio con las reglas
        obtener-lista-grado-axioma (subvec obtener-lista-arch-read 0 2)
        obtener-lista-iteracion-arch-write (vector (sublista args 2 4))];;VER DE QUE TIPO ES arg, SI ES UN VECTOR ENTONCES REEMPLAZAR POR: nth args 2 4.
    (concat obtener-lista-grado-axioma obtener-directorio-reglas obtener-lista-iteracion-arch-write)))

;Devuelve la clave de la pos del directorio
(defn vector-claves [reglas];NO USADOO!!
  (vec (map name (keys reglas))))

;Recibe solo una regla
;Devuelve UN CHAR que es la clave de la regla (el caracter)
(defn obtener-caracter-clave [regla];NO USADOO!!
  (nth (str (first regla)) 1))

;Recibe solo una regla
;Devuelve el valor de la regla recivida
(defn obtener-valor-regla [regla];NO USADOO!!
  (str (nth regla 1)))

;TRADUCIR:
;Devuelve un string con todas las reglas aplicadas
;Explicacion de losparametros:
;Axioma: sera una cadena de caracteres donde se le va a aplicar las reglas. Ejm un axioma puede ser F+G+FG
;Reglas: es un diccionario  donde en las claves estaran los caracteres que se van a traucir en el axioma,
;        Ejm las reglas pueden ser {:F "FF", :G "GF", :X "G-G+F"}
;Explicacion Codigo:
;A cada caracter de la cadena de caracteres de "axioma" se le va a aplicar una funcion anonima, donde dicha
;funcion anonima va a obtener del diccionario "reglas" el valor de la clave que recibe por parametro
;(en este caso (keyword (str %) me devuelve la clave [(ejm devuelve -> :F o :G o :X, etc)] del str recibido ([ejm "F" o "G" o "X", etc]) )
;Y en tal caso de que la clave que no exista va a devolver el caracter recibio casteado a str:  #(get reglas (keyword (str %)) (str %))
;                                                                                                                                  ^
(defn traducir [reglas axioma]
  (apply str (map #(get reglas (keyword (str %)) (str %)) axioma)))

;TORTUGA
;Devuelve un vector donde en cada posicion del vector representa la salida de cada iteracion
;Explicacion de los parametros:
;Axioma: sera una cadena de caracteres donde se le va a aplicar las reglas. Ejm un axioma puede ser F+G+FG
;Reglas: es un diccionario  donde en las claves estaran los caracteres que se van a traucir en el axioma,
;        Ejm las reglas pueden ser {:F "FF", :G "GF", :X "G-G+F"}
;Iteraciones: La cantidad de iteraciones que se va a traducir el Axioma
;Explicacion del coigo:
;Si las iteraciones llegaron a 0, quiere decir que ta se va a dejar de traducir el axioma
;En cambio si es distinto de 0, eso quiere decir que se tiene que seguir traduciendo el axioma
;Y ademas en cada iteracion se va a modificar el axioma ya que se traduce en cada iteracion (Se
;va reemplazando los valores) y al final junto todas las salidas de cada iteracion en un vector
(defn tortuga [iteraciones axioma reglas]
  (if (= iteraciones 0)
    '[]
    (let [nuevo-axioma (traducir reglas axioma)]
      (conj (tortuga (dec iteraciones) nuevo-axioma reglas) axioma))))

;Aca se escribe en el archivo svg, pero creo que antes hat que hacer una serie de cosas
(defn escribir-archivo [lista-iteraciones]
  ())

;Comenzar todo el proceso del dibujo
(defn comenzar [vector]
  (let [dibujo-tortuga (tortuga (nth vector 3) (nth vector 1) (nth vector 2))
        estritura (escribir-archivo dibujo-tortuga)]))

;;cant_iteraciones archivo_svg
(defn -main [& args]
  (if (not= (count args) 1)                               ;;3
    (println "Debes ingresar 1 argumentos para ejecutar el programa (ruta archivo lectura, cantidad de veces
    a implementar el fractal, ruta archivo escritura\n")
    (comenzar (obtener-informacion args))))

