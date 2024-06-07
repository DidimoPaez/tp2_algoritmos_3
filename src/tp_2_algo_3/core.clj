(ns tp-2-algo-3.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def avance-tortuga 1)
(def color-negro "black")
(def angulo-acumulado-inicial 0)
(def grosor-pluma "1")
(def ancho-pluma 1)
(def pos-inicial-x 0)
(def pos-inicial-y 0)
(def pos-movimientos 2)
(def pos-limites 3)

(def limites-coordenadas-inicial [0 0 0 0])
(def recuadro-movimiento 10)
;(def limites-display-svg [-15 -15 50 50]) ;ESTO HAY QUE BORRARLO, ESO SOLO DE PRUEBA///////////////////
(def inicio-primera-linea "<svg viewBox=\"")
(def final-primera-linea  "\" xmlns=\"http://www.w3.org/2000/svg\">")
(def ultima-linea "</svg>")

(defn crear-tortuga [angulo pos-x pos-y]
  {:angulo angulo
   :angulo-acumulado angulo-acumulado-inicial
   :pos-x pos-x
   :pos-y pos-y
   :pluma true
   :color "black"
   :ancho-pluma ancho-pluma})

(defn getter-angulo [tortuga]
  (tortuga :angulo))
(defn getter-angulo-acumulado [tortuga]
  (tortuga :angulo-acumulado))
(defn getter-pos-x [tortuga]
  (tortuga :pos-x))
(defn getter-pos-y [tortuga]
  (tortuga :pos-y))
(defn getter-pluma [tortuga]
  (tortuga :pluma))

(defn setter-angulo [tortuga nueva-angulo]
  (assoc tortuga :angulo nueva-angulo))
(defn setter-angulo-acumulado [tortuga nuevo-angulo]
  (assoc tortuga :angulo-acumulado nuevo-angulo))
(defn setter-pos-x [tortuga nueva-x]
  (assoc tortuga :pos-x nueva-x))
(defn setter-pos-y [tortuga nueva-y]
  (assoc tortuga :pos-y nueva-y))
(defn setter-pluma [tortuga estado-pluma]
  (assoc tortuga :pluma estado-pluma))


;VER SI EL FORMATO ANDA POR EL TEMA DE LAS COMILLAS!!!
;Para el "v2" se puede hacer un vector que sea constante y declararlo con "def"
(defn join-vectors [x1 y1 x2 y2 stroke-width stroke]
  (let [v1 [(str x1) (str y1) (str x2) (str y2) (str stroke-width) (str stroke)]
        v2 ["<line x1=\""  "\" y1=\"" "\" x2=\"" "\" y2=\"" "\" stroke-width=\"" "\" stroke=\""]]
    (str (apply str(map str v2 v1))  "\" />")))


;VERRRR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;SI HACE FALTA CONTEMPLAR EL DERECHA O IZQUIERDA CON EL SIGNO NEGATIVO PARA LA "y" POR
;EL TEMA DEL EJECARTESIANO
(defn cambio-angulo [tortuga sentido]
  (let [ang (getter-angulo tortuga)
        ang-acumulado (getter-angulo-acumulado tortuga)]
    (case sentido
      \+ (setter-angulo-acumulado tortuga (- ang-acumulado ang))
      \- (setter-angulo-acumulado tortuga (+ ang-acumulado ang))
      \| (setter-angulo-acumulado tortuga (+ 180 ang-acumulado))
      :else tortuga)))          ;DEVUELVE UNA COPIA DE TORTUGA

;VERRRRRR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;SI EL MENOS DE LA COORDENADA "y" ESTÁ ES CORRECTA, DEBIDO A QUE EL EJE Y ESTÁ INVERTIDO
(defn mover-tortuga [tortuga]
  (let [a (java.lang.Math/toRadians(getter-angulo-acumulado tortuga))
        avance-en-x (* avance-tortuga (Math/cos a))
        avance-en-y (* avance-tortuga (Math/sin a))
        pos-inicial-x (getter-pos-x tortuga)
        pos-inicial-y (getter-pos-y tortuga)
        pos-final-x (+ pos-inicial-x avance-en-x)
        pos-final-y (- pos-inicial-y avance-en-y)]
    (-> tortuga
        (setter-pos-x pos-final-x)          ;SE ENVIARÀ UNA COPIA DE TORTUGA
        (setter-pos-y pos-final-y))))       ;SE ENVIARÀ UNA COPIA DE TORTUGA

(defn pluma-arriba [tortuga]
  (setter-pluma tortuga false))             ;SE ENVIARÀ UNA COPIA DE TORTUGA

(defn pluma-abajo [tortuga]
  (setter-pluma tortuga true))

;recibirá la copia de la tortuga anterior y a partir de los datos de ella se apilará "la nueva tortuga"
(defn apilar-tortuga [v-pila tortuga]
  (-> v-pila
      (assoc (dec (count v-pila)) tortuga)
      (conj tortuga)))

(defn desapilar-tortuga [v-pila movimientos coord-limits]
  (let [pila-actualizada (pop v-pila)
        tortuga-a-emplear (peek pila-actualizada)]
    [pila-actualizada tortuga-a-emplear movimientos coord-limits]))       ;VERIFICAR QUE EL PUSH/POP SE HAGA SOBRE EL PRIMER ELEMENTO


;en el vector "movimientos" se apilará el movimiento nuevo que se le pasará al SVG
;esta funcion devuelve un vector, pues movs es de tipo "vector" y conj devuelve el mismo tipo
(defn pre-svg [movimientos t-estado-1 t-estado-2]
  (let [movs movimientos
        t1 t-estado-1
        t2 t-estado-2]
    (conj movs (join-vectors (getter-pos-x t1)(getter-pos-y t1)(getter-pos-x t2)(getter-pos-y t2) grosor-pluma color-negro))))



(defn pos-maxima [pos-1 pos-2 pos-3]
  (min pos-1 pos-2 pos-3))
(defn pos-minima [pos-1 pos-2 pos-3]
  (min pos-1 pos-2 pos-3))


(defn actualizar-coordenadas [coord-limits t-estado-1 t-estado-2]
  (let [t-pos-x1 (getter-pos-x t-estado-1)
        t-pos-x2 (getter-pos-x t-estado-2)
        min-x (pos-minima t-pos-x1 t-pos-x2 (coord-limits 0))
        max-x (pos-maxima t-pos-x1 t-pos-x2 (coord-limits 2))
        t-pos-y1 (getter-pos-y t-estado-1)
        t-pos-y2 (getter-pos-y t-estado-2)
        min-y (pos-minima t-pos-y1 t-pos-y2 (coord-limits 1))
        max-y (pos-maxima t-pos-y1 t-pos-y2 (coord-limits 3))]
    [min-x min-y max-x max-y]))

;PARA QUE UN VECTOR ACTÚE COMO UNA LISTA SE DEBEN USAR LOS COMANDOS: "(conj v 5)" y "(pop v)", siendo "v" el vector
(defn enviar-a-mover [tortugas tortuga movimientos coord-limits]
  (let [t-estado-1 tortuga
        t-estado-2 (mover-tortuga tortuga)
        movs movimientos
        cl coord-limits]
    (if (= true (getter-pluma t-estado-1))
      [tortugas t-estado-2 (pre-svg movs t-estado-1 t-estado-2) (actualizar-coordenadas cl t-estado-1 t-estado-2)]
      [tortugas (pluma-abajo t-estado-2) movs cl]))) ;VUELVE A DEJAR LA PLUMA ABAJO (true)

(defn movimiento-tortuga [tortuga guia]
  (let [tortugas-inicial (conj [] tortuga)
        movimientos-inicial []
        init-coord-limits limites-coordenadas-inicial]
    (reduce (fn [[tortugas tortuga movimientos coord-limits] %]          ;ELIMINAR LA FUNCION: "tortuga-actual"
              (cond
                (or (= \F %) (= \G %))
                (enviar-a-mover tortugas tortuga movimientos coord-limits) ;RECIBE UN VECTOR CON TRES ELEMENTOS

                (or (= \f %) (= \g %))
                (enviar-a-mover tortugas (pluma-arriba tortuga) movimientos coord-limits)

                (or (= \+ %) (= \- %) (= \| %))
                [tortugas (cambio-angulo tortuga %) movimientos coord-limits]

                (= \[ %)
                [(apilar-tortuga tortugas tortuga) tortuga movimientos coord-limits]

                (= \] %)
                (desapilar-tortuga tortugas movimientos coord-limits)

                :else [tortugas tortuga movimientos coord-limits]))
            [tortugas-inicial tortuga movimientos-inicial init-coord-limits] guia)))



;Recibe un vector donde esten las reglas. ejemplo:
;(generar-reglas ["F XF+F" "G XX-F"])
;Y devuelve un diccionario con clave:valor. Ejemplo:
;{:F "XF+F", :G "XX-F"} , (OJO!! que la clave es una "keyword")
(defn generar-reglas [v]
  (reduce (fn [acc x]
            (let [regla-key (first x)
                  regla-value (subs x 2)]
              (assoc acc (keyword (str regla-key)) regla-value)))
          {} v))

(defn parser-line [s]
  (reduce (fn [acc x]
            (if (not= x \newline)
              (str acc x)
              (reduced acc)))
          "" s))

;; Tener presente que "drop" devuelve una secuencia, y al final del let lo que se está concatenando
;;es un string, por eso hay que utilizar "apply str"
;devuelve un vector de strings. (Ej: recibiendo: "(parser-file "25\nX\nF XF+F\nG XX-F\n")"
;devolverá: "["25" "X" "F XF+F" "G XX-F"]"
(defn parser-file [s]
  (if (<= (count s) 0)
    '()
    (let [linea (parser-line s)
          resto (drop (inc (count linea)) s)]
      (vec (cons linea (parser-file (apply str resto)))))))

(defn leer-archivo [archivo]
  (slurp archivo))

(defn etapa-lectura [archivo]
  (-> archivo
      (leer-archivo)
      (parser-file)))

(defn traducir [reglas axioma]
  (apply str (map #(get reglas (keyword (str %)) (str %)) axioma)))

(defn obtencion-limites-display [coord-limit]
  (let [x-min (- (coord-limit 0) recuadro-movimiento )
        y-min (- (coord-limit 1) recuadro-movimiento)
        x-max (+ (coord-limit 2) recuadro-movimiento)
        y-max (+ (coord-limit 3) recuadro-movimiento)]
    [x-min y-min x-max y-max]))

;;funcion que en el TP se llama "tortuga"
(defn guia-para-tortuga [iteraciones axioma reglas]
  (if (= iteraciones 0)
    axioma
    (let [nuevo-axioma (traducir reglas axioma)]
      (guia-para-tortuga (dec iteraciones) nuevo-axioma reglas))))


(defn obtencion-angulo-guia [archivo-lectura iteraciones]
  (let [vector-base (etapa-lectura archivo-lectura)          ;vector base:(pos 0: angulo; pos 1: axioma; > pos 1: reglas)
        angulo (first vector-base)
        axioma (second vector-base)
        reglas (generar-reglas (vec (drop 2 vector-base))) ;devuelve un diccionario con la reglas
        guia (guia-para-tortuga iteraciones axioma reglas)]
    [angulo guia]))

(defn obtencion-vector-para-svg [tortuga guia]
  (-> tortuga
      (movimiento-tortuga guia)))

;Coloca los limites en la primera linea del svg
(defn inicializar-primera-linea [comienzo-primera-linea limites-display-svg ultima-parte-primera-linea]
  (let [limites (str/join " " limites-display-svg)]
    (str comienzo-primera-linea limites ultima-parte-primera-linea)))

;Obtiene un vector con la primera y la ultima fila del arch svg
(defn obtencion-vector-svg [linea vec-movimientos]
  (vec (cons linea (conj vec-movimientos ultima-linea))))

;Escribe en el archivo svg
(defn escritura-svg [archivo-escritura movimientos limites-display-svg]
  (let [primera-linea (inicializar-primera-linea inicio-primera-linea limites-display-svg final-primera-linea)
        vec-lineas-svg-totales (obtencion-vector-svg primera-linea movimientos)]
    (spit archivo-escritura (str/join "\n" vec-lineas-svg-totales))))

(defn -main [& args]
  (if (not= (count args) 3)                               ;;3
    (println "Debes ingresar 3 argumentos para ejecutar el programa (ruta archivo lectura, cantidad de veces
    a implementar el fractal, ruta archivo escritura\n")
    (let [vector-angulo-guia (obtencion-angulo-guia (first args) (Integer/parseInt (second args)))
          angulo (Double/parseDouble(first vector-angulo-guia))   ;VER MANEJO DE ERRORES (VALIDAR NUMERO)
          guia (second vector-angulo-guia)
          tortuga (crear-tortuga angulo pos-inicial-x pos-inicial-y)
          vector-para-svg (obtencion-vector-para-svg tortuga guia)
          movimientos-svg (nth vector-para-svg pos-movimientos)
          limites-display-svg (obtencion-limites-display (nth vector-para-svg pos-limites))] ;[x-min y-min x-max y-max]]
      (escritura-svg (nth args 2) movimientos-svg limites-display-svg))))




