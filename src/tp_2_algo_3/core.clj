(ns tp-2-algo-3.core
  (:gen-class))

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

(defn sublista [lista a b] 
  (let [lista-a-vector (apply vector lista)] 
    (apply list(subvec lista-a-vector a b))))

;Devuelve una lista donde: 
;sus primeros 2 elementos el angulo y la axioma respectivamente. 
;3er elemento es un diccionario co las reglas correspondientes
;4to elemento es la catidad de iteraciones
;5to elemento el archivo de escritura
(defn obtener-informacion [args]
  (let [contenido-arch-read (leer_archivo (first args))
        obtener-lista-arch-read (split contenido-arch-read)
        obtener-directorio-reglas (list (generar_reglas (sublista obtener-lista-arch-read 2 (+ 1 (count obtener-lista-arch-read)))));Obtiene una lista con un elemento que sera el directorio con las reglas
        obtener-lista-grado-axioma (sublista obtener-lista-arch-read 0 2)
        obtener-lista-iteracion-arch-write (sublista args 2 4)]
    (concat obtener-lista-grado-axioma obtener-directorio-reglas obtener-lista-iteracion-arch-write)))

(defn comenzar [lista] 
  ())

;;cant_iteraciones archivo_svg
(defn -main [& args]
  (if (not= (count args) 1)                               ;;3
    (println "Debes ingresar 1 argumentos para ejecutar el programa (ruta archivo lectura, cantidad de veces
    a implementar el fractal, ruta archivo escritura\n")
    (comenzar (obtener-informacion args))))

