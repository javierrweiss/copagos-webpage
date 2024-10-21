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
  (let [_ (when dataset 
            (let [worksheet (.json_to_sheet js/XLSX.utils dataset)
                  workbook (.book_new js/XLSX.utils)
                  _ (.book_append_sheet js/XLSX.utils workbook worksheet "Copagos")
                  fecha (js/Date)]
              (js/XLSX.writeFile workbook (str "copagos-" fecha ".xlsx") (clj->js {:compression true}))))]
    (fn []
      (if-not dataset 
        [:h3 "Lo sentimos, debe realizar primero una búsqueda."]
        [:div 
         [:h3 "Se está realizando su descarga"]]))))

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
         [:button {:on-click #(buscar-planes-actuales resultado obra)}
          "Buscar"]]]
       [:div.botones-iconos
        [:button#excel {:title "Descargue en excel"
                        :on-click (fn [_] (swap! opcion-excel #(if (nil? %) :excel nil)))}]]
       (if @opcion-excel
         [excel (clj->js @resultado)]
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
  (let [obra-plan (group-by :tbl_copago_historico/codplan @dataset)
        especificaciones (mapv (fn [llave]
                                 {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
                                  :title llave
                                  :data {:values (get obra-plan llave)}
                                  :mark "line"
                                  :encoding {:x {:field "vigente_desde"
                                                 :type "ordinal"
                                                 :timeUnit "month"}
                                             :y {:field "monto"
                                                 :type "quantitative"}
                                             :color {:field "categoria"
                                                     :type "nominal"}}})
                               (keys obra-plan))
        cantidad-especificaciones (range (count especificaciones))
        ids (for [elem cantidad-especificaciones] (str "visual-grafico-" elem))]
    (fn []
      (prn (str "DataSet procesado: " obra-plan))
      (if-not @dataset
        [:h3 "Debe realizar primero una búsqueda"]
        [:div {:class "visual-grafico"}
         (do (mapv #(js/vegaEmbed (str "#" %1) (clj->js %2)) ids especificaciones)
             (for [id ids] [:div {:id id}]))]))))

(defn historia
  []
  (let [resultado (r/atom [])
        obra (r/atom 0)
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
         [:button {:on-click #(buscar-planes-historico resultado obra)}
          "Buscar"]]]
       [:div.botones-iconos
        [:button#excel {:title "Descargue en excel"
                        :on-click (fn [_] (swap! toggle-option #(if (nil? %) :excel nil)))}]
        [:button#grafico {:title "Visualice en un gráfico"
                          :on-click (fn [_] (swap! toggle-option #(if (nil? %) :grafico nil)))}]]
       (cond
         (= @toggle-option :excel) [excel (clj->js @resultado)]
         (= @toggle-option :grafico) [grafico resultado]
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

  @datos

  (validar-datos @datos)

  (js/React.useEffect)

  (let [historia (js/document.getElementById "historia")
        visual-registros (js/document.getElementById "visual-registros")
        formulario-principal (js/document.getElementById "formulario-principal")]
    (set! (.-hidden formulario-principal) false)
    (set! (.-hidden visual-registros) true)
    (set! (.-hidden historia) true))

  (let [vect [#:tbl_copago_historico{:codplan "1900-A",
                                     :categoria "CONSULTA NUTRICION",
                                     :vigente_desde "2024-10-18T03:00:00Z",
                                     :monto 1800}
              #:tbl_copago_historico{:codplan "1900-A",
                                     :categoria "CONSULTA ESPECIALISTA",
                                     :vigente_desde "2024-10-18T03:00:00Z",
                                     :monto 1800}
              #:tbl_copago_historico{:codplan "1900-A",
                                     :categoria "CONSULTA COMUN",
                                     :vigente_desde "2024-10-18T03:00:00Z",
                                     :monto 1800}
              #:tbl_copago_historico{:codplan "1900-B",
                                     :categoria "CONSULTA NUTRICION",
                                     :vigente_desde "2024-10-18T03:00:00Z",
                                     :monto 1800}
              #:tbl_copago_historico{:codplan "1900-B",
                                     :categoria "CONSULTA ESPECIALISTA",
                                     :vigente_desde "2024-10-18T03:00:00Z",
                                     :monto 1800}
              #:tbl_copago_historico{:codplan "1900-B",
                                     :categoria "CONSULTA COMUN",
                                     :vigente_desde "2024-10-17T03:00:00Z",
                                     :monto 12000}
              #:tbl_copago_historico{:codplan "1900-B",
                                     :categoria "CONSULTA COMUN",
                                     :vigente_desde "2024-10-17T03:00:00Z",
                                     :monto 3500}]
        obra-plan (group-by :tbl_copago_historico/codplan vect)]
    #_(into [:div] (map (fn [llave]
                          [:div [grafico (get obra-plan llave) llave]])
                        (keys obra-plan)))
    #_(for [llave (keys obra-plan)]
        [grafico (get obra-plan llave) llave])
    (map (fn [llave]
           (get obra-plan llave))
         (keys obra-plan)))


  (let [json "[{\"codplan\":\"1900-B\",\"especialidad\":354,\"categoria\":\"CONSULTA COMUN\",\"copago\":3500},
              {\"codplan\":\"1900-B\",\"especialidad\":386,\"categoria\":\"CONSULTA COMUN\",\"copago\":3500},
              {\"codplan\":\"1900-B\",\"especialidad\":358,\"categoria\":\"CONSULTA COMUN\",\"copago\":3500}]"
        js-obj (clj->js [{"tbl_copago_historico/codplan" "1900-A",
                          "tbl_copago_historico/categoria" "CONSULTA NUTRICION",
                          "tbl_copago_historico/vigente_desde" "2024-10-18T03:00:00Z",
                          "tbl_copago_historico/monto" 1800}
                         {"tbl_copago_historico/codplan" "1900-A",
                          "tbl_copago_historico/categoria" "CONSULTA ESPECIALISTA",
                          "tbl_copago_historico/vigente_desde" "2024-10-18T03:00:00Z",
                          "tbl_copago_historico/monto" 1800}
                         {"tbl_copago_historico/codplan" "1900-A",
                          "tbl_copago_historico/categoria" "CONSULTA COMUN",
                          "tbl_copago_historico/vigente_desde" "2024-10-18T03:00:00Z",
                          "tbl_copago_historico/monto" 1800}
                         {"tbl_copago_historico/codplan" "1900-B",
                          "tbl_copago_historico/categoria" "CONSULTA NUTRICION",
                          "tbl_copago_historico/vigente_desde" "2024-10-18T03:00:00Z",
                          "tbl_copago_historico/monto" 1800}
                         {"tbl_copago_historico/codplan" "1900-B",
                          "tbl_copago_historico/categoria" "CONSULTA ESPECIALISTA",
                          "tbl_copago_historico/vigente_desde" "2024-10-18T03:00:00Z",
                          "tbl_copago_historico/monto" 1800}
                         {"tbl_copago_historico/codplan" "1900-B",
                          "tbl_copago_historico/categoria" "CONSULTA COMUN",
                          "tbl_copago_historico/vigente_desde" "2024-10-17T03:00:00Z",
                          "tbl_copago_historico/monto" 12000}
                         {"tbl_copago_historico/codplan" "1900-B",
                          "tbl_copago_historico/categoria" "CONSULTA COMUN",
                          "tbl_copago_historico/vigente_desde" "2024-10-17T03:00:00Z",
                          "tbl_copago_historico/monto" 3500}])
        worksheet (.json_to_sheet js/XLSX.utils js-obj)
        workbook (.book_new js/XLSX.utils) 
        _ (.book_append_sheet js/XLSX.utils workbook  worksheet "Copagos")
        fecha (js/Date)]
    #_workbook
    #_(.json_to_sheet js/XLSX.utils js-obj)
    (js/XLSX.writeFile workbook (str "copagos-" fecha ".xlsx") (clj->js {:compression true})))


  :rcf)  
  
 
