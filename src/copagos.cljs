(ns copagos
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]
            [promesa.core :as p]
            [clojure.string :as string]))

(def datos (r/atom (hash-map)))

(defn limpiar-checkboxes
  []
  (->> ["consulta-comun" "consulta-especialista" "consulta-nutricion"]
       (mapv #(js/document.getElementById %))
       (mapv #(set! (.-checked %) false))))

(defn limpiar-fecha
  []
  (set! (.-value (js/document.getElementById "fecha")) ""))

(defn limpiar 
  [] 
  (reset! datos (hash-map))
  (limpiar-fecha)
  (limpiar-checkboxes))

(defn parsear-planes
  [obra planes-str]
  (let [planes (mapv #(string/trim %) (string/split planes-str #","))]
    (mapv #(str obra "-" %) planes)))

(defn validar-datos
  [data]
  (and
   (every? data [:obra :plan :especialidad :copago :vigencia])
   (seq (:especialidad data))))
 
(defn procesar 
  []
  (let [especialidades (->> (:especialidad @datos) (map name) (string/join ", "))
        mensaje-confirmacion (str "Se van a guardar los siguientes datos: \n"
                                  "Obra: " (:obra @datos) "\n"
                                  "Plan: " (:plan @datos) "\n"
                                  "Especialidad: " especialidades "\n"
                                  "Copago: " (:copago @datos) "\n"
                                  "Vigencia: " (:vigencia @datos) "\n"
                                  "\n ¿Está seguro?")]
    (when (.confirm js/window mensaje-confirmacion)
      (swap! datos assoc :codplan (parsear-planes (:obra @datos) (:plan @datos)))
      (-> (js/fetch "/guardar" (clj->js {:headers {"Content-Type" "application/json"}
                                         :method "POST"
                                         :body (->> @datos
                                                    clj->js
                                                    (.stringify js/JSON))}))
          (p/then (fn [resp]
                    (if (.-ok resp)
                      (do
                        (limpiar)
                        (js/alert "¡Éxito! ¡Registro guardado!"))
                      (js/alert "¡Lo sentimos! ¡Hubo un error!"))))))))

(defn enviar
  []
  (if (validar-datos @datos)
    (procesar)
    (js/alert "¡No se pudieron validar los datos! Por favor, complete todos los campos")))

(defn registrar-seleccion-especialidad
  [coll valor_nuevo]
  (cond 
    (nil? coll) #{valor_nuevo} 
    (contains? coll valor_nuevo) (disj coll valor_nuevo)
    :else (conj coll valor_nuevo)))

(defn formulario
  []
  [:form {:hidden false
          :on-submit #(.preventDefault %)}
   [:div#grilla
    [:div.renglon
     [:label [:b "Obra social"]]
     [:input {:type "text"
              :required true
              :value (:obra @datos)
              :on-change #(swap! datos assoc :obra (-> % .-target .-value))}]]
    [:div.renglon
     [:label [:b "Plan"]]
     [:input {:type "text"
              :required true
              :value (:plan @datos)
              :on-change #(swap! datos assoc :plan (-> % .-target .-value))}]]
    [:div.renglon
     [:label [:b "Especialidad"]]
     [:div.checkbox
      [:input {:type "checkbox"
               :id "consulta-comun"
               :required true
               :value (-> @datos :especialidad :consulta-comun) 
               :on-change #(swap! datos update :especialidad registrar-seleccion-especialidad :consulta-comun)}]
      [:label {:for "consulta-comun"} [:i "Consulta común"]]]
     [:div.checkbox 
      [:input {:type "checkbox"
               :id "consulta-especialista"
               :required true
               :value (-> @datos :especialidad :consulta-especialista) 
               :on-change #(swap! datos update :especialidad registrar-seleccion-especialidad :consulta-especialista)}]
      [:label {:for "consulta-especialista"} [:i "Consulta especialista"]]]
     [:div.checkbox
      [:input {:type "checkbox"
               :id "consulta-nutricion"
               :required true
               :value (-> @datos :especialidad :consulta-nutricion) 
               :on-change #(swap! datos update :especialidad registrar-seleccion-especialidad :consulta-nutricion)}]
      [:label {:for "consulta-nutricion"} [:i "Consulta nutricion"]]]]
    [:div.renglon
     [:label [:b "Vigente desde"]]
     [:input {:type "date"
              :id "fecha"
              :required true
              :on-change #(swap! datos assoc :vigencia (-> % .-target .-value))}]]
    [:div.renglon
     [:label [:b "Monto copago"]]
     [:input {:type "number"
              :required true
              :value (:copago @datos)
              :on-change #(swap! datos assoc :copago (->> % .-target .-value (.parseFloat js/Number)))}]]
    [:div.botones
     [:button {:on-click enviar} "Enviar" ]
     [:button {:on-click limpiar} "Cancelar" ]]]])

(defn historia
  []
  [:div#historia {:hidden true}
   [:h3 "Registro histórico"]])

(defn visual-registros
  []
  [:div#visual-registros {:hidden true}
   [:h3 "Copagos por obra y especialidad"]])

(defn pagina
  []
  [:<> 
   [formulario]
   [historia]
   [visual-registros]])

(rdom/render [pagina] (js/document.getElementById "main"))


(comment
  
  (limpiar-fecha)
  (limpiar-checkboxes)
   
  (.stringify js/JSON (clj->js {:a 33 :b 334}))

  (-> (js/fetch "/guardar" {:method "POST" :body (.stringify js/JSON
                                                             (clj->js {:codplan "1820-U"
                                                                       :especialidad 12323
                                                                       :copago 1245}))})
      (p/then #(js/console.log %))
      (p/catch #(js/console.error "Upps!!")))

  (p/-> (js/fetch "/guardar" (clj->js {:headers {"Content-Type" "application/json"}
                                     :method "POST"
                                     :body (->> {:codplan "108-A"
                                                 :especialidad 931
                                                 :copago 1500}
                                                clj->js
                                                (.stringify js/JSON))}))
        ((juxt #(.-ok %) #(.json %))) 
        ((fn [[ok prm]]
           (if ok
             (js/alert "Todo bien!!")
              prm)))
        (some-> 
         .-error 
         (string/includes? "duplicate key")
         (if 
            (js/alert "Ya existe un registro con esos datos")
            (js/alert "Hubo un error"))))
      
   
  (.confirm js/window "¿Está seguro?") (.parseFloat js/Number "23.55")

  (-> {:a 1 :z #{:a :b}} :z)
  
  (let [p "p,f,e,r,t"
        pl "K, L, Ñ, E"
        pla " D ,A ,e , a"]
    (parsear-planes 1820 pla)) 
           
  @datos   
  (every? @datos [:obra :plan :especialidad :copago :vigencia])

  (validar-datos @datos)

  )  
 
 
