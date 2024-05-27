(ns tp-2-algo-3.core
  (:gen-class))

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

;;cant_iteraciones archivo_svg
(defn -main [& args]
  (if (not= (count args) 1)                               ;;3
    (println "Debes ingresar 1 argumentos para ejecutar el programa (ruta archivo lectura, cantidad de veces
    a implementar el fractal, ruta archivo escritura\n")
    (let [contenido (leer_archivo (first args))]
      (split contenido))))

