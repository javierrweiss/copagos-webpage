(ns server
  (:require [org.httpkit.server :refer [run-server]]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [babashka.pods :as pods]
            [honey.sql :as sql]
            [cheshire.core :as json]
            [taoensso.timbre :as timbre]
            [clojure.string :as string]
            [overtone.at-at :as at]
            [hiccup2.core :refer [html]])
  (:import java.io.IOException
           java.time.LocalDate))

(pods/load-pod 'org.babashka/postgresql "0.1.2")

(require '[pod.babashka.postgresql :as pg])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISCELANEO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dia-en-ms (* 60000 60 24))

(def especialidad->codigo (-> (io/file "public/especialidades.edn") slurp edn/read-string))

(def categorias
  {:consulta-comun [:inline "CONSULTA COMUN"]
   :consulta-especialista [:inline "CONSULTA ESPECIALISTA"]
   :consulta-nutricion [:inline "CONSULTA NUTRICION"]})

(defn categoria->keyword
  [categoria]
  (if-not (string? categoria)
    (throw (java.lang.IllegalArgumentException. "El input no es un String"))
    (-> categoria
        (string/replace #"\s" "-")
        string/lower-case
        keyword)))

(defn es-hoy-o-antes?
  "Recibe un LocalDate o un string y devuelve true si la fecha es igual o menor a la fecha actual"
  [fecha]
  (when-let [f (if (instance? java.time.LocalDate fecha) fecha (LocalDate/parse fecha))]
    (or (.isBefore f (LocalDate/now))
        (.isEqual (LocalDate/now) f))))

(def pool (at/mk-pool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SQL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def db {:dbtype "postgresql"
         :host (System/getenv "TELECONSULTA_SANCOL_HOST")
         :dbname "scweb"
         :user (System/getenv "TELECONSULTA_SANCOL_DBUSUARIO")
         :password (System/getenv "TELECONSULTA_SANCOL_CONTRASENA")
         :port 5432})

(defn insertar-planes
  "Recibe un vector con valores para codplan, especialidad, categoria y copago (mismo orden)"
  [valores]
  (sql/format {:insert-into :tbl_planes_obras_sociales
               :columns [:codplan :especialidad :categoria :copago]
               :values valores}))

(defn insertar-historico
  "Recibe un vector con valores para codplan, categoria, monto y vigente_desde (mismo orden)"
  [valores]
  (sql/format {:insert-into :tbl_copago_historico
               :columns [:codplan :categoria :monto :vigente_desde]
               :values valores}))

(defn actualizar
  [monto codplan especialidad]
  (sql/format {:update :tbl_planes_obras_sociales
               :set  {:copago monto}
               :where [:and [:= :codplan codplan] [:= :especialidad especialidad]]}))

(defn buscar
  [codplan especialidad]
  (sql/format {:select 1
               :from :tbl_planes_obras_sociales
               :where [:and [:= :codplan codplan] [:= :especialidad especialidad]]}))

(defn buscar-copagos-por-entrar-en-vigencia
  []
  (sql/format {:select [:codplan :categoria :monto]
               :from :tbl_copago_historico
               :where [:= :vigente_desde :current_date]}))

(defn existe-registro?
  [conn codplan especialidad]
  (== 1 (or (-> (pg/execute! conn (buscar codplan especialidad))
                first
                :?column?)
            0)))

(defn guardar-si-esta-vigente
  [conn]
  (timbre/info "Busca copagos vigentes...")
  (let [vigentes (pg/execute! conn (buscar-copagos-por-entrar-en-vigencia))]
    (when (seq vigentes)
      (letfn [(inserta-o-actualiza [{:tbl_copago_historico/keys [codplan categoria monto]}]
                (let [especialidades ((categoria->keyword categoria) especialidad->codigo)]
                  (doseq [especialidad especialidades]
                    (if (existe-registro? conn codplan especialidad)
                      (pg/execute! conn (actualizar monto codplan especialidad))
                      (pg/execute! conn (insertar-planes [[codplan especialidad [:inline categoria] monto]]))))))]
        (doseq [vigente vigentes] (inserta-o-actualiza vigente))))))

(defn buscar-planes-por-obra
  [conn obra]
  (let [sql (sql/format {:select [:codplan :especialidad :categoria :copago]
                         :from :tbl_planes_obras_sociales
                         :where [:like :codplan [:inline (str obra "%")]]})]
    (pg/execute! conn sql)))

(defn buscar-historico-por-obra
  [conn obra]
  (let [sql (sql/format {:select [:codplan :categoria :vigente_desde :monto]
                         :from :tbl_copago_historico
                         :where [:like :codplan [:inline (str obra "%")]]})]
    (pg/execute! conn sql)))

(defn preparar-registros-planes
  "Recibe el cuerpo del request con las llaves requeridas y el mapeo de especialidad->código y devuelve un lazy-seq de vectores 
   con los valores listos para la actualización o inserción del registro correspondiente"
  [{:keys [codplan especialidad copago] :as registro} esp->cod]
  (when (or (nil? registro) (nil? esp->cod) (not (map? esp->cod)) (not (map? registro)))
    (throw (IllegalArgumentException. "El input es nulo o no es un mapa")))
  (->> (seq especialidad)
       (map keyword)
       (mapcat #(for [plan codplan esp (% esp->cod)]
                  [plan esp (% categorias) copago]))))

(defn preparar-registros-historico
  "Recibe el cuerpo del request con las llaves requeridas y devuelve un vector de vectores 
   con los valores listos para la inserción del registro correspondiente"
  [{:keys [codplan especialidad copago vigencia] :as registro}]
  (when (or (nil? registro) (not (map? registro)))
    (throw (IllegalArgumentException. "El input es nulo o no es un mapa")))
  (into [] (for [plan codplan esp (seq especialidad)]
             [plan ((keyword esp) categorias) copago [:inline vigencia]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn guardar
  "En el body del request recibe un JSON con las siguientes llaves: codplan, especialidad, vigencia y copago
   De codplan recibe un vector, de especialidad un conjunto y de copago un número. 
   Crea registros de la forma [codplan, especialidad, categoria, copago] y [codplan, categoria, vigencia, copago] 
   y los inserta en la tabla de copagos (si la fecha de vigencia es igual o menor a la actual) y en la tabla de historico"
  [{:keys [body] :as req}]
  (timbre/info "Request recibida: " req)
  (try
    (let [conn (pg/get-connection db)]
      (try
        (let [datos (-> body
                        io/reader
                        slurp
                        (json/decode keyword))
              fecha-vigencia (LocalDate/parse (:vigencia datos))
              registros-historicos (preparar-registros-historico datos)
              registros-planes (preparar-registros-planes datos especialidad->codigo)
              inserta-planes (fn []
                               (doseq [[codplan especialidad _ copago :as reg] registros-planes]
                                 (if (existe-registro? conn codplan especialidad)
                                   (pg/execute! conn (actualizar copago codplan especialidad))
                                   (pg/execute! conn (insertar-planes [reg])))))
              inserta-historico (fn []
                                  (pg/execute! conn (insertar-historico registros-historicos)))]
          (if (es-hoy-o-antes? fecha-vigencia)
            (do (inserta-planes)
                (inserta-historico))
            (inserta-historico))
          {:status 201})
        (catch IOException e {:status 500
                              :body (json/encode {:error (ex-message e)})})
        (catch Exception e {:status 500
                            :body (json/encode {:error (ex-message e)})})
        (finally (pg/close-connection conn))))
    (catch IOException e {:status 500
                          :body (json/encode {:error (ex-message e)})})))

(defn obtiene-datos-por-obra
  [query-string query-fn] 
  (try
    (let [conn (pg/get-connection db)
          obra (if-let [s (re-seq #"\d+" query-string)]
                 (first s)
                 {:status 400
                  :body (-> [:h1 "Debe ingresar el código de la obra social"] html str)})]
      (try
        (when-let [resultado (seq (query-fn conn obra))]
          {:status 200
           :body (-> resultado
                     vec
                     json/encode)})
        (catch IOException e {:status 500
                              :body (json/encode {:error (ex-message e)})})
        (catch Exception e {:status 500
                            :body (json/encode {:error (ex-message e)})})
        (finally (pg/close-connection conn))))
    (catch IOException e {:status 500
                          :body (json/encode {:error (ex-message e)})})))

(defn obtiene-copagos-guardados
  [{:keys [query-string]}] 
  (timbre/info "Query string: " query-string)
  (obtiene-datos-por-obra query-string buscar-planes-por-obra))

(defn obtiene-copagos-historico
  [{:keys [query-string]}]
  (timbre/info "Query string: " query-string)
  (obtiene-datos-por-obra query-string buscar-historico-por-obra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BACKGROUND-JOB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ejecutar-background-job
  []
  (timbre/info "Ejecutando trabajo en el trasfondo...")
  (at/every
   dia-en-ms
   (fn []
     (try
       (let [conn (pg/get-connection db)]
         (try
           (guardar-si-esta-vigente conn)
           (catch IOException e (timbre/error (ex-message e)))
           (finally (pg/close-connection conn))))
       (catch IOException e (timbre/error (ex-message e)))))
   pool
   :initial-delay 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SERVICIO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn app
  [req]
  (timbre/info (str "Request: " req))
  (match [(:uri req)]
    ["/"] {:status 200
           :headers {"Content-Type" "text/html"}
           :body (slurp "public/index.html")}
    ["/style.css"] {:status 200
                    :headers {"Content-Type" "text/css"}
                    :body (slurp "public/style.css")}
    ["/copagos.cljs"] {:status 200
                       :body (slurp "src/copagos.cljs")}
    ["/img/Logo%20Sanatorio%20Colegiales%20-%20Horizontal-689x300%202.png"] {:status 200
                                                                             :headers {"Content-Type" "img/png"}
                                                                             :body (io/file "public/img/Logo Sanatorio Colegiales - Horizontal-689x300 2.png")}
    ["/img/SanatorioColegialesEntrada.jpg"] {:status 200
                                             :headers {"Content-Type" "img/jpg"}
                                             :body (io/file "public/img/SanatorioColegialesEntrada.jpg")}
    ["/img/menu-icon.png"] {:status 200
                            :headers {"Content-Type" "img/jpg"}
                            :body (io/file "public/img/menu-icon.png")}
    ["/guardar"] (guardar req)
    [#"\/planes\?obra=\d+|\/planes"] (obtiene-copagos-guardados req)
    [#"\/historico\?obra=\d+|\/historico"] (obtiene-copagos-historico req)
    :else {:status 404
           :headers {"Content-Type" "text/html"}
           :body (str (html [:h1 "¡Lo sentimos! No encontramos lo que anda buscando"]))}))
 
(defonce server (atom nil))

(defn start []
  (timbre/info "Iniciando servicio...")
  (when-let [srv (run-server #'app {:port 1341})]
    (reset! server srv)))

(defn stop
  []
  (timbre/info "Deteniendo servicio...")
  (when @server
    (@server :timeout 100)
    (reset! server nil)))

(defn restart
  []
  (timbre/info "Reiniciando servicio...")
  (stop)
  (start))

(defn main
  []
  (timbre/info "Iniciando servicio...")
  (start)
  (ejecutar-background-job))


(comment
  (let [especialidades {:consulta-comun [354 386 358]
                        :consulta-especialista  [355 356 360 385 381 359 409 410 411 412 413 414 415 416 417 418 419 420 421
                                                 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440
                                                 441 442 443 444 445 446 447 448 449 450 451 452 453]
                        :consulta-nutricion [357]}
        req {:especialidad #{:consulta-comun :consulta-nutricion},
             :obra "1820",
             :vigencia "2024-10-14",
             :codplan ["1820-A" "1820-F" "1820-K" "1820-L" "1820-P"],
             :copago 5000,
             :plan "A,F,K,L,P"}
        ejecucion (preparar-registros-planes req especialidades)
        historico (preparar-registros-historico req)
        request  {:body (json/encode req)}]
    #_(== (* 51 5) (count (for [obr (:codplan req) esp (:consulta-especialista especialidades)] [obr esp])))
    #_(flatten ((juxt :consulta-comun :consulta-especialista) especialidades))
    #_(seleccionar-especialidades especialidades :consulta-comun :consulta-nutricion :consulta-especialista)
    #_(seq (:especialidad req))
    #_ejecucion
    #_historico
    (-> (:body request)
        io/reader
        slurp
        (json/decode keyword))
    #_(guardar request)))


(comment
 
  (match ["/planes?obra=122" #_#_"a" 122]
    ["a"] "Es A"
    [122] "Es numero"
    [#"si"] "Ajá"
    [#"\s"] "Espacio"
    [#"\w+"] "yes!!"
    [#"\/planes\?obra=\d+"] "Bien!!!" 
    :else :not-found)

  (.isEqual (LocalDate/now) (LocalDate/parse "2024-10-15"))
  (.isBefore (LocalDate/now) (LocalDate/parse "2024-10-12"))

  (es-hoy-o-antes? (LocalDate/now))
  (es-hoy-o-antes? "2024-10-15")
  (es-hoy-o-antes? "2024-10-12")
  (es-hoy-o-antes? "2024-10-21")
  (es-hoy-o-antes? "2025-10-21")
  (es-hoy-o-antes? "2024-09-21")
  (es-hoy-o-antes? (LocalDate/of 2021 10 1))

  (start)
  (restart)
  (stop)

  (babashka.fs/exists? (io/file "public/img/Logo Sanatorio Colegiales - Horizontal-689x300 2.png"))

  (let [valores [["a" "b" "c"] ["d" "gg" "dsd"]]]
    (sql/format {:insert-into :tbl_planes_obras_sociales
                 :values valores}))

  (pg/execute! db ["SELECT * FROM tbl_planes_obras_sociales"])

  (pg/execute! db (insertar [["1800-A" 18 1800] ["1800-B" 58 2563]]))

  (let [{{:keys [codplan especialidad copago]} :body :as m} {:body {:codplan 2
                                                                    :especialidad 23
                                                                    :copago 343}}]
    #_[codplan especialidad copago]
    m)

  (require '[org.httpkit.client :as client])

  @(client/post "http://127.0.0.1:1341/guardar" {:headers {"Content-Type" "application/json"}
                                                 :accept :json
                                                 :body (json/encode {:codplan "1800-Y"
                                                                     :copago 225
                                                                     :especialidad 23})})

  (let [req @(client/get "http://127.0.0.1:1341/planes" {:query-params {:obra 1900}})]
    (-> req :body io/reader slurp json/decode))
   
  (let [req @(client/get "http://127.0.0.1:1341/planes?obra=1900")]
    (-> req :body io/reader slurp json/decode))
  
  @(client/get "http://127.0.0.1:1341/style.css")

  (def r (pg/execute! db (buscar "101-A" 931)))

  (pg/execute! db (actualizar 18000 "101-A" 931))

  (pg/execute! db (insertar-historico [["1240-B" [:inline "CONSULTA COMUN"] 2000 [:inline "2024-10-15"]]]))

  (def body-example {:especialidad #{:consulta-comun :consulta-nutricion},
                     :obra "1820",
                     :vigencia "2024-10-14",
                     :codplan ["1820-A" "1820-F" "1820-K" "1820-L" "1820-P"],
                     :copago 5000,
                     :plan "A,F,K,L,P"})

  (def t (pg/execute! db (buscar-copagos-por-entrar-en-vigencia)))

  (categoria->keyword "CONSULTA COMUN")

  (let [conn (pg/get-connection db)]
    (try
      #_(guardar-si-esta-vigente conn)
      (buscar-planes-por-obra conn 1900)
      (finally (pg/close-connection conn)))) 
 
  :rcf)    