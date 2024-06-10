(ns tp-2-algo-3.core
  (:gen-class)
  (:require [clojure.string :as str]))

;Constante de la tortuga
(def avance-tortuga 1)
(def angulo-acumulado-inicial 90)
(def ancho-pluma 1)
(def pos-inicial-x 0)
(def pos-inicial-y 0)
(def pos-movimientos 2)
(def pos-limites 3)
(def limites-coordenadas-inicial [0 0 0 0])

;Constante del SVG
(def grosor-pluma "0.1")
(def color-negro "black")
(def recuadro-movimiento 10)
(def inicio-primera-linea "<svg viewBox=\"") 
(def final-primera-linea  "\" xmlns=\"http://www.w3.org/2000/svg\">")
(def ultima-linea "</svg>")
(def formato-linea ["  <line x1=\""  "\" y1=\"" "\" x2=\"" "\" y2=\"" "\" stroke-width=\"" "\" stroke=\""])
(def formato-fin-de-linea "\" />")

;Inicializa una tortuga
(defn crear-tortuga [angulo pos-x pos-y]
  {:angulo angulo
   :angulo-acumulado angulo-acumulado-inicial
   :pos-x pos-x
   :pos-y pos-y
   :pluma true
   :color "black"
   :ancho-pluma ancho-pluma})

;Obtener el angulo de una tortuga
(defn getter-angulo [tortuga]
  (tortuga :angulo))
;Obtener el angulo acomulado de una tortuga
(defn getter-angulo-acumulado [tortuga]
  (tortuga :angulo-acumulado))
;Obtener la posicion X de una tortuga
(defn getter-pos-x [tortuga]
  (tortuga :pos-x))
;Obtener la posicion Y de una tortuga
(defn getter-pos-y [tortuga]
  (tortuga :pos-y))
;Obtener si la pluma esta arriba o abajo
(defn getter-pluma [tortuga]
  (tortuga :pluma))

;Modifica el angulo acomulado de la tortuga
(defn setter-angulo-acumulado [tortuga nuevo-angulo]
  (assoc tortuga :angulo-acumulado nuevo-angulo))
;Modifica la pos X de la tortuga
(defn setter-pos-x [tortuga nueva-x]
  (assoc tortuga :pos-x nueva-x))
;Modifica la pos Y de la tortuga
(defn setter-pos-y [tortuga nueva-y]
  (assoc tortuga :pos-y nueva-y))
;Modifica la el estado de la pluma (coloca la pluma arriba o pluma abajo)
(defn setter-pluma [tortuga estado-pluma]
  (assoc tortuga :pluma estado-pluma))

;Recibe la posicion de X e Y de partida y de llegada.
;Devolvera un string con una nueva linea de SVG que corresponde a un movimiento de la tortuga. 
(defn join-vectors [x1 y1 x2 y2 stroke-width stroke]
  (let [v1 [(str x1) (str y1) (str x2) (str y2) (str stroke-width) (str stroke)]
        v2 formato-linea]
    (str (apply str (map str v2 v1))  formato-fin-de-linea)))

;Devuelve una copia de la tortuga con el valor del angulo modificado dependiendo del sentido que reciba
(defn cambio-angulo [tortuga sentido]
  (let [ang (getter-angulo tortuga)
        ang-acumulado (getter-angulo-acumulado tortuga)]
    (case sentido
      \+ (setter-angulo-acumulado tortuga (- ang-acumulado ang))
      \- (setter-angulo-acumulado tortuga (+ ang-acumulado ang))
      \| (setter-angulo-acumulado tortuga (+ 180 ang-acumulado))
      :else tortuga)))

;Mueve la tortuga.
;Devuelve una tortuga con las posiciones correspondientes al movimiento realizado.
(defn mover-tortuga [tortuga]
  (let [a (java.lang.Math/toRadians (getter-angulo-acumulado tortuga))
        avance-en-x (* avance-tortuga (Math/cos a))
        avance-en-y (* avance-tortuga (Math/sin a))
        pos-inicial-x (getter-pos-x tortuga)
        pos-inicial-y (getter-pos-y tortuga)
        pos-final-x (+ pos-inicial-x avance-en-x)
        pos-final-y (- pos-inicial-y avance-en-y)]
    (-> tortuga
        (setter-pos-x pos-final-x)          
        (setter-pos-y pos-final-y))))       

;Coloca la pluma  arriba
(defn pluma-arriba [tortuga]
  (setter-pluma tortuga false))             

;Coloca la pluma  abajo
(defn pluma-abajo [tortuga]
  (setter-pluma tortuga true))

;Recibirá la copia de la tortuga anterior y a partir de los datos de ella se apilará "la nueva tortuga"
(defn apilar-tortuga [v-pila tortuga]
  (-> v-pila
      (assoc (dec (count v-pila)) tortuga)
      (conj tortuga)))

;Desapila una tortuga de la pila de tortuga
;Va a actualizar la pila e tortugas y actualizara la nueva tortuga a mover.
(defn desapilar-tortuga [v-pila movimientos coord-limits]
  (let [pila-actualizada (pop v-pila)
        tortuga-a-emplear (peek pila-actualizada)]
    [pila-actualizada tortuga-a-emplear movimientos coord-limits])) 

;Recibe el vector de movimientos de la tortuga en el formato svg, el estado de partida  
;y el estado de llegada de la tortuga actual del movimiento a realizar.
;Colocara en el vector de movimientos una nueva linea en formato svg que indica el movimieno a indicar de la tortuga
(defn pre-svg [movimientos t-estado-1 t-estado-2]
  (let [movs movimientos
        t1 t-estado-1
        t2 t-estado-2]
    (conj movs (join-vectors (getter-pos-x t1) (getter-pos-y t1) (getter-pos-x t2) (getter-pos-y t2) grosor-pluma color-negro))))

;Devuelve la posicion maxima de las posiiones recibidas
(defn pos-maxima [pos-1 pos-2 pos-3]
  (max pos-1 pos-2 pos-3))

;Devuelve la posicion minima de las posiiones recibidas
(defn pos-minima [pos-1 pos-2 pos-3]
  (min pos-1 pos-2 pos-3))

;Actualiza las coordenadas minimas y maximas en X e Y.
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

;Realizara el movimiento de la tortuga en una unidad hacia alante. 
;Se verificara si la pluma esta arriba o abajo para realizar el movimiento correspondiente.
;Si esta abajo la pluma se va a añadir la linea del dibujo svg en el vector de movimientos, se modificaran las posiciones de la tortuga
;actual y se actualizar las coordenadas maximas y minimas de todas las tortugas si es necesario
;Si esta la pluma arriba solo se actualizara la posicion de la tortuga actual y se colocara el la pluma arriba, pero no se va a 
;guardar la linea en el vector la linea del svg (ya que la pluma esta arriba).
(defn enviar-a-mover [tortugas tortuga movimientos coord-limits]
  (let [t-estado-1 tortuga
        t-estado-2 (mover-tortuga tortuga)
        movs movimientos
        cl coord-limits]
    (if (= true (getter-pluma t-estado-1))
      [tortugas t-estado-2 (pre-svg movs t-estado-1 t-estado-2) (actualizar-coordenadas cl t-estado-1 t-estado-2)]
      [tortugas (pluma-abajo t-estado-2) movs cl])))

;Devuelve un vector donde
;En la posicion 0: Estara un vector de las tortugas que estan en la pila en su estado final.
;En la posicion 1: LA tortuga actual que se esta moviendo.
;En la posicion 2: Contiene el vector de la movimientos que va haciendo las diferentes tortugas 
;                  en el formato de una linea en SVG.
;En la posicion 3: Contendra un vector con las coordenadas maximas y minimas de X e Y [x-min y-min x-max y-max].
(defn movimiento-tortuga [tortuga guia]
  (let [tortugas-inicial (conj [] tortuga)
        movimientos-inicial []
        init-coord-limits limites-coordenadas-inicial]
    (reduce (fn [[tortugas tortuga movimientos coord-limits] %]
              (cond
                (or (= \F %) (= \G %))
                (enviar-a-mover tortugas tortuga movimientos coord-limits)

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

;Parsea el string leido
;Devuelve un string hasa que se topa con un \newline
(defn parser-line [s]
  (reduce (fn [acc x]
            (if (not= x \newline)
              (str acc x)
              (reduced acc)))
          "" s))

;Devuelve un vector de strings. (Ej: recibiendo: "(parser-file "25\nX\nF XF+F\nG XX-F\n")"
;Devolverá: "["25" "X" "F XF+F" "G XX-F"]"
(defn parser-file [s]
  (if (<= (count s) 0)
    '()
    (let [linea (parser-line s)
          resto (drop (inc (count linea)) s)]
      (vec (cons linea (parser-file (apply str resto)))))))

;Lee el archivo
(defn leer-archivo [archivo]
  (slurp archivo))

;Lee el archivo y lo parsea
(defn etapa-lectura [archivo]
  (-> archivo
      (leer-archivo)
      (parser-file)))

;Recibe el diccionario con las reglas y el axioma a traducir.
;Recorrera todo el axioma y se fijara si el caracter recibido en la funcion anonima es una clave de una
;regla, si es asi reemplazarla por su valor, sino colocar el mismo caracter leido. 
(defn traducir [reglas axioma]
  (apply str (map #(get reglas (keyword (str %)) (str %)) axioma)))

;Se va a obener el alto o ancho (dependiendo si se recibe las posiciones en X o en Y)
;que va a tener el dibujo en SVG.
(defn obtener-ancho-alto [min max]
  (cond
    (and (<= min 0) (<= max 0))
    (+ (abs min) max)

    (and (<= min 0) (>= max 0))
    (+ (abs min) max)

    (and (>= min 0) (>= max 0))
    (- max min)
    :else nil))

;Devuelve un vector con la porsicion maxima y minima en X e Y de todo el svg. 
(defn obtencion-limites-display [coord-limit]
  (let [x-min (- (coord-limit 0) recuadro-movimiento)
        y-min (- (coord-limit 1) recuadro-movimiento)
        x-max (+ (coord-limit 2) recuadro-movimiento)
        y-max (+ (coord-limit 3) recuadro-movimiento)
        ancho (obtener-ancho-alto x-min x-max)
        alto (obtener-ancho-alto y-min y-max)]
    [x-min y-min ancho alto]))

;Va a traducir el axioma recibido iteradas veces por ende va a actualizar el axioma en cada iteracion.
(defn guia-para-tortuga [iteraciones axioma reglas]
  (if (= iteraciones 0)
    axioma
    (let [nuevo-axioma (traducir reglas axioma)]
      (guia-para-tortuga (dec iteraciones) nuevo-axioma reglas))))

;Recibe el archivo para leer y las iteraciones. 
;Devuelve un vector con la informacion del angulo y el axioma final resultante de las iteraciones (la guia)
(defn obtencion-angulo-guia [archivo-lectura iteraciones]
  (let [vector-base (etapa-lectura archivo-lectura)        
        angulo (first vector-base)
        axioma (second vector-base)
        reglas (generar-reglas (vec (drop 2 vector-base)))  
        guia (guia-para-tortuga iteraciones axioma reglas)]
    [angulo guia]))

;Devuelve el vector de l informaion de las orugas con sus movimientos y posiciones minimas y maximas
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

;Funcion main
(defn -main [& args]
  (if (not= (count args) 3)
    '()
    (let [vector-angulo-guia (obtencion-angulo-guia (first args) (Integer/parseInt (second args)))
          angulo (Double/parseDouble (first vector-angulo-guia))   
          guia (second vector-angulo-guia)
          tortuga (crear-tortuga angulo pos-inicial-x pos-inicial-y)
          vector-para-svg (obtencion-vector-para-svg tortuga guia)
          movimientos-svg (nth vector-para-svg pos-movimientos)
          limites-display-svg (obtencion-limites-display (nth vector-para-svg pos-limites))] 
      (escritura-svg (nth args 2) movimientos-svg limites-display-svg))))


