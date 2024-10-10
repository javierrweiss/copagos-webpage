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

(defn limpiar 
  [] 
  (reset! datos (hash-map))
  (limpiar-checkboxes))

(defn enviar
  []
  (let [especialidades (->> (:especialidad @datos) (map name) (string/join ", "))
        mensaje-confirmacion (str "Se van a guardar los siguientes datos: \n"
                                  "Obra: " (:obra @datos) " Plan: " (:plan @datos) "\n"
                                  "Especialidad: " especialidades " Copago: " (:copago @datos) "\n"
                                  "\n ¿Está seguro?")]
    (when (.confirm js/window mensaje-confirmacion)
      (swap! datos assoc :codplan (str (:obra @datos) "-" (:plan @datos))) 
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

(defn registrar-seleccion-especialidad
  [coll valor_nuevo]
  (cond 
    (nil? coll) #{valor_nuevo} 
    (contains? coll valor_nuevo) (disj coll valor_nuevo)
    :else (conj coll valor_nuevo)))

(defn formulario
  []
  [:form {:on-submit #(.preventDefault %)}
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
     [:label [:b "Monto copago"]]
     [:input {:type "number"
              :required true
              :value (:copago @datos)
              :on-change #(swap! datos assoc :copago (->> % .-target .-value (.parseFloat js/Number)))}]]
    [:div.botones
     [:button {:on-click enviar} "Enviar" ]
     [:button {:on-click limpiar} "Cancelar" ]]]])

(rdom/render [formulario] (js/document.getElementById "main"))


(comment
  
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
  
  
  @datos
  
  ) 
 

