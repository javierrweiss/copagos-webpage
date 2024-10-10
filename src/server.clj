(ns server
  (:require [org.httpkit.server :refer [run-server]]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [babashka.pods :as pods]
            [honey.sql :as sql]
            [cheshire.core :as json]
            [taoensso.timbre :as timbre]))

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
 
(defn insertar
  [valores]
  (sql/format {:insert-into :tbl_planes_obras_sociales
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
  [codplan especialidad]
  (== 1 (or (-> (pg/execute! db (buscar codplan especialidad))
                first
                :?column?)
            0)))
 
(defn guardar
  [{:keys [body] :as req}]
  #_(def request req)
  (timbre/info "Request recibida: " req)
  (let [{:strs [codplan especialidad copago]} (-> body
                                                  io/reader
                                                  slurp
                                                  json/decode)
        ejecucion (try (-> (if (existe-registro? codplan especialidad)
                             (pg/execute! db (actualizar copago codplan especialidad))
                             (pg/execute! db (insertar [[codplan especialidad copago]])))
                           first
                           :next.jdbc/update-count)
                       (catch Exception e (ex-message e)))]
    (if (string? ejecucion)
      {:status 500
       :body (json/encode {:error ejecucion})}
      {:status 201})))

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

  (start)
  (restart)

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
 (existe-registro? "101-A" 931)
  (existe-registro? "102-A" 931)
  
  (def r (pg/execute! db (buscar "101-A" 931)))
  
  (pg/execute! db (actualizar 18000 "101-A" 931))
   
  :rcf)   