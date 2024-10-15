(ns server
  (:require [org.httpkit.server :refer [run-server]]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [babashka.pods :as pods]
            [honey.sql :as sql]
            [cheshire.core :as json]
            [taoensso.timbre :as timbre]
            [clojure.string :as string])
  (:import java.io.IOException
           java.time.LocalDate))

(pods/load-pod 'org.babashka/postgresql "0.1.2")

(require '[pod.babashka.postgresql :as pg])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def especialidad->codigo (-> (io/file "public/especialidades.edn") slurp edn/read-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SQL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def db {:dbtype "postgresql"
         :host (System/getenv "TELECONSULTA_SANCOL_HOST")
         :dbname "scweb"
         :user (System/getenv "TELECONSULTA_SANCOL_DBUSUARIO")
         :password (System/getenv "TELECONSULTA_SANCOL_CONTRASENA")
         :port 5432})

(defn es-hoy-o-antes?
  "Recibe un LocalDate o un string y devuelve true si la fecha es igual o menor a la fecha actual"
  [fecha]
  (when-let [f (if (instance? java.time.LocalDate fecha) fecha (LocalDate/parse fecha))]
    (or (.isBefore f (LocalDate/now))
        (.isEqual (LocalDate/now) f))))

(defn insertar-planes
  [valores]
  (sql/format {:insert-into :tbl_planes_obras_sociales
               :columns [:codplan :especialidad :categoria :copago]
               :values valores}))

(defn insertar-historico
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

(defn existe-registro?
  [conn codplan especialidad]
  (== 1 (or (-> (pg/execute! conn (buscar codplan especialidad))
                first
                :?column?)
            0)))

(def categorias
  {:consulta-comun [:inline "CONSULTA COMUN"]
   :consulta-especialista [:inline "CONSULTA ESPECIALISTA"]
   :consulta-nutricion [:inline "CONSULTA NUTRICION"]})

(defn preparar-registros-planes
  "Recibe el cuerpo del request con las llaves requeridas y el mapeo de especialidad->código y devuelve un lazy-seq de vectores 
   con los valores listos para la actualización o inserción del registro correspondiente"
  [{:keys [codplan especialidad copago]} esp->cod]
  (->> (seq especialidad)
       (map keyword)
       (mapcat #(for [plan codplan esp (% esp->cod)]
                  [plan esp (% categorias) copago]))))

(defn preparar-registros-historico
  "Recibe el cuerpo del request con las llaves requeridas y devuelve un vector de vectores 
   con los valores listos para la inserción del registro correspondiente"
  [{:keys [codplan especialidad copago vigencia]}]
  (into [] (for [plan codplan esp (seq especialidad)]
             [plan ((keyword esp) categorias) copago [:inline vigencia]])))

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
                               (->> registros-planes
                                    (mapv (fn [[codplan especialidad _ copago :as reg]]
                                            (if (existe-registro? conn codplan especialidad)
                                              (pg/execute! conn (actualizar copago codplan especialidad))
                                              (pg/execute! conn (insertar-planes [reg])))))))
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

(defn app
  [req]
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
    ["/guardar"] (guardar req)))

(defonce server (atom nil))

(defn start []
  (when-let [srv (run-server #'app {:port 1341})]
    (reset! server srv)))

(defn stop
  []
  (when @server
    (@server :timeout 100)
    (reset! server nil)))

(defn restart
  []
  (stop)
  (start))

(defn main
  []
  (start)
  @(promise))

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

  (.isEqual (LocalDate/now) (LocalDate/parse "2024-10-15"))
  (.isBefore (LocalDate/now) (LocalDate/parse "2024-10-12"))

  (es-hoy-o-antes? (LocalDate/now))
  (es-hoy-o-antes? "2024-10-15")
  (es-hoy-o-antes? "2024-10-12")
  (es-hoy-o-antes? "2024-10-21")
  (es-hoy-o-antes? "2025-10-21")
  (es-hoy-o-antes? "2024-09-21")
  (es-hoy-o-antes? (LocalDate/of 2021 10 1))


  (hyperfiddle.rcf/enable!)

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
  

  (def r (pg/execute! db (buscar "101-A" 931)))

  (pg/execute! db (actualizar 18000 "101-A" 931))
 
  (pg/execute! db (insertar-historico [["1240-B" [:inline "CONSULTA COMUN"] 2000 [:inline "2024-10-15"]]]))
 
  (def body-example {:especialidad #{:consulta-comun :consulta-nutricion},
                     :obra "1820",
                     :vigencia "2024-10-14",
                     :codplan ["1820-A" "1820-F" "1820-K" "1820-L" "1820-P"],
                     :copago 5000,
                     :plan "A,F,K,L,P"})


  :rcf)   