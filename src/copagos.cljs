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

(defn buscar
  [atomo-coleccion atomo-param ruta]
  (-> (js/fetch (str ruta @atomo-param))
      (p/then (fn [response]
                (if (.-ok response)
                  (-> (.json response)
                      (p/then (fn [cuerpo]
                                (prn (js->clj cuerpo))
                                (reset! atomo-coleccion (js->clj cuerpo :keywordize-keys true)))))
                  (js/alert "No se encontraron registros"))))
      (p/catch (fn [error]
                 (prn error)
                 (js/alert "¡Lo sentimos, hubo un problema!")))))

(defn buscar-planes-actuales
  [atom-coll atom-obra]
  (buscar atom-coll atom-obra "/planes?obra="))

(defn buscar-planes-historico
  [atom-coll atom-obra]
  (buscar atom-coll atom-obra "/historico?obra="))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMPONENTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dropdownmenu
  [seleccion menu-activo]
  [:div#opciones-dropdownmenu
   [:table#dropdownmenu-table
    [:tr
     {:on-click #(do 
                   (reset! seleccion :visual-planes-actuales)
                   (swap! menu-activo not))}
     "Ver registros por obra"]
    [:tr
     {:on-click #(do 
                   (reset! seleccion :visual-historica)
                   (swap! menu-activo not))}
     "Ver copago histórico por obra"]
    [:tr
     {:on-click  #(do 
                    (reset! seleccion nil)
                    (swap! menu-activo not))}
     "Ingreso copagos"]]])

(defn header
  [seleccion]
  (let [esta-abierto? (r/atom false)]
    (fn []
      [:header
       [:div#logo
        [:div#imagen
         [:img {:src "img/Logo Sanatorio Colegiales - Horizontal-689x300 2.png"
                :alt "Logo"}]]
        [:div#titulo [:h1 "Ingreso copagos telemedicina"]]
        [:div#menu 
         (if @esta-abierto?
           [dropdownmenu seleccion esta-abierto?]
           [:button#menu-button {:on-click #(swap! esta-abierto? not)}])]]])))

(defn formulario
  []
  [:form#formulario-principal
   {:on-submit #(.preventDefault %)}
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
     [:button {:on-click enviar} "Enviar"]
     [:button {:on-click limpiar} "Cancelar"]]]])

(defn excel
  [dataset]
  (let [_ dataset]
    (fn []
      [:div
       [:h3 "Lo sentimos, esta opción aún no está disponible."]])))

(defn tabla-planes-actuales
  [resultado]
  [:div.tabla-container
   [:table.tabla
    [:tr
     [:th "Obra/plan"]
     [:th "Especialidad"]
     [:th "Categoría"]
     [:th "Copago"]]
    (for [elem @resultado]
      [:tr
       [:td (:tbl_planes_obras_sociales/codplan elem)]
       [:td (:tbl_planes_obras_sociales/especialidad elem)]
       [:td (:tbl_planes_obras_sociales/categoria elem)]
       [:td (str (:tbl_planes_obras_sociales/copago elem) " $")]])]])

(defn visual-registros
  []
  (let [resultado (r/atom [])
        obra (r/atom 0)
        resultado-json (r/atom nil)
        opcion-excel (r/atom nil)]
    (fn []
      [:div#visual-registros
       [:h2 "Copagos por obra y especialidad"]
       [:div#visual-registros-grid
        [:div
         [:input {:type "number"
                  :required true
                  :value @obra
                  :placeholder "Ingrese el código de obra social y presione Enter"
                  :on-change #(reset! obra (->> % .-target .-value (.parseFloat js/Number)))}]]
        [:div.botones
         [:button {:on-click (fn [_]
                               (-> (buscar-planes-actuales resultado obra)
                                   (p/then #(reset! resultado-json (.stringify js/JSON (clj->js @resultado))))
                                   (p/then #(prn @resultado-json))))}
          "Buscar"]]]
       [:div.botones-iconos
        [:button#excel {:title "Descargue en excel"
                        :on-click (fn [_] (swap! opcion-excel #(if (nil? %) :excel nil)))}]]
       (if @opcion-excel
         [excel resultado-json]
         [tabla-planes-actuales resultado])])))
 
(defn tabla-historico
  [resultado]
  [:div.tabla-container
   [:table.tabla
    [:tr
     [:th "Obra/plan"]
     [:th "Categoría"]
     [:th "Vigente desde"]
     [:th "Copago"]]
    (for [elem @resultado]
      [:tr
       [:td (:tbl_copago_historico/codplan elem)]
       [:td (:tbl_copago_historico/categoria elem)]
       [:td (:tbl_copago_historico/vigente_desde elem)]
       [:td (str (:tbl_copago_historico/monto elem) " $")]])]])

(defn grafico
  [dataset]
  (let [especificacion (clj->js {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
                                 :data {:values @dataset}
                                 :mark "line"
                                 :encoding {:x {:field "vigente_desde"
                                                :type "ordinal"
                                                :timeUnit "month"}
                                            :y {:field "monto"
                                                :type "quantitative"}
                                            :color {:field "categoria"
                                                    :type "nominal"}}})]
    (fn []
      (prn (str "DataSet" @dataset))
      (if-not @dataset
        [:h3 "Debe realizar primero una búsqueda"]
        (do (js/vegaEmbed "#visual-grafico" especificacion)
            [:div#visual-grafico])))))

  (defn historia
    []
    (let [resultado (r/atom [])
          obra (r/atom 0)
          resultado-json (r/atom nil)
          toggle-option (r/atom nil)]
      (fn []
        [:div#historia
         [:h2 "Registro histórico"]
         [:div#visual-historia-grid
          [:div
           [:input {:type "number"
                    :required true
                    :value @obra
                    :placeholder "Ingrese el código de obra social y presione Enter"
                    :on-change #(reset! obra (->> % .-target .-value (.parseFloat js/Number)))}]]
          [:div.botones
           [:button {:on-click (fn [_]
                                 (-> (buscar-planes-historico resultado obra)
                                     (p/then #(reset! resultado-json (.stringify js/JSON (clj->js @resultado))))
                                     #_(p/then #(prn @resultado-json))))}
            "Buscar"]]]
         [:div.botones-iconos
          [:button#excel {:title "Descargue en excel"
                          :on-click (fn [_] (swap! toggle-option #(if (nil? %) :excel nil)))}]
          [:button#grafico {:title "Visualice en un gráfico"
                            :on-click (fn [_] (swap! toggle-option #(if (nil? %) :grafico nil)))}]]
         (cond 
           (= @toggle-option :excel) [excel resultado-json]
           (= @toggle-option :grafico) [grafico resultado-json]
           :else [tabla-historico resultado])])))

(defn pagina
  []
  (let [seleccion-vista (r/atom nil)]
    (fn []
      [:<>
       [header seleccion-vista]
       [:main 
        (cond 
          (= @seleccion-vista :visual-historica) [historia]
          (= @seleccion-vista :visual-planes-actuales) [visual-registros]
          :else [formulario])]])))

(rdom/render [pagina] (js/document.getElementById "main"))
 

(comment

  (limpiar-fecha)
  (limpiar-checkboxes)

  (let [ds [{:a 'C', :b 2},
            {:a 'C', :b 7},
            {:a 'C', :b 4},
            {:a 'D', :b 1},
            {:a 'D', :b 2},
            {:a 'D', :b 6},
            {:a 'E', :b 8},
            {:a 'E', :b 4},
            {:a 'E', :b 7}]
        especificacion (clj->js {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
                                 :data {:values ds}
                                 :mark "bar"
                                 :encoding {:x {:field "a"
                                                :type "nominal"}
                                            :y {:field "b"
                                                :type "quantitative"}}})]
  #_(js/vegaEmbed "#main" especificacion)
    (rdom/render [grafico ds] (js/document.getElementById "main")))
  
  (let [data [{"tbl_copago_historico/codplan" "1820-A",
               "tbl_copago_historico/categoria" "CONSULTA COMUN", 
               "tbl_copago_historico/vigente_desde" "2024-10-19T03:00:00Z", 
               "tbl_copago_historico/monto" 8000} 
              {"tbl_copago_historico/codplan" "1820-A", 
               "tbl_copago_historico/categoria" "CONSULTA NUTRICION", 
               "tbl_copago_historico/vigente_desde" "2024-10-19T03:00:00Z", 
               "tbl_copago_historico/monto" 8000}]]
    (group-by ))
  

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
  
  (reset! obra 1900)
 
  @obra
  
   
  (js/React.useEffect)

  (let [historia (js/document.getElementById "historia")
        visual-registros (js/document.getElementById "visual-registros")
        formulario-principal (js/document.getElementById "formulario-principal")]
    (set! (.-hidden formulario-principal) false)
    (set! (.-hidden visual-registros) true)
    (set! (.-hidden historia) true))
  
(let [vect [{"tbl_planes_obras_sociales/codplan" "1900-B", 
             "tbl_planes_obras_sociales/especialidad" 354, 
             "tbl_planes_obras_sociales/categoria" "CONSULTA COMUN", 
             "tbl_planes_obras_sociales/copago" 3500} 
            {"tbl_planes_obras_sociales/codplan" "1900-B", 
             "tbl_planes_obras_sociales/especialidad" 386, 
             "tbl_planes_obras_sociales/categoria" "CONSULTA COMUN", 
             "tbl_planes_obras_sociales/copago" 3500} 
            {"tbl_planes_obras_sociales/codplan" "1900-B", 
             "tbl_planes_obras_sociales/especialidad" 358, 
             "tbl_planes_obras_sociales/categoria" "CONSULTA COMUN", 
             "tbl_planes_obras_sociales/copago" 3500}]]
   (.stringify js/JSON (clj->js vect)))

  )  
  
 
