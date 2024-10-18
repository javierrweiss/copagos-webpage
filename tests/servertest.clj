(ns servertest
  (:require 
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop] 
   [clojure.test :refer :all]
   [server :refer :all])
  (:import java.time.LocalDate))

(def gen-date
  (gen/let [ano (gen/choose 2020 2040)
            mes (gen/choose 1 12)
            dia (gen/choose 1 (.lengthOfMonth (LocalDate/of ano mes 1)))]
    (LocalDate/of ano mes dia)))

(def gen-date-str
  (gen/let [ano (gen/choose 2020 2040)
            mes (gen/choose 1 12)
            dia (gen/choose 1 (.lengthOfMonth (LocalDate/of ano mes 1)))]
    (.toString (LocalDate/of ano mes dia))))

(defn generar-fecha-aleatorias
  "Genera fechas aleatorias como LocalDate y como String"
  []
  (let [ld (gen/sample gen-date)
        st (->> ld (take (- (count ld) 2)) (mapv #(.toString %)))]
    (concat ld st)))

(deftest categoria->keyword-test
  (testing "Siempre lanza excepcion cuando recibe otro input"
    (let [in (rand-nth (gen/sample (gen/such-that #(not (string? %)) gen/any)))]
      (is (thrown? java.lang.IllegalArgumentException (categoria->keyword in)))))
  (testing "Siempre devuelve keyword cuando recibe string"
    (let [propiedad (prop/for-all [st gen/string]
                                  (keyword? (categoria->keyword st)))
          test (tc/quick-check 100 propiedad)]
      (prn test)
      (is (true? (:pass? test))))))

(deftest es-hoy-o-antes?-test
  (testing "Devuelve siempre booleano"
    (is (every? boolean? (map es-hoy-o-antes? (generar-fecha-aleatorias))))
    (is (true? (:pass? (tc/quick-check 100 (prop/for-all [fec (gen/one-of [gen-date gen-date-str])]
                                                         (boolean? (es-hoy-o-antes? fec))))))))
  (testing "Cuando la fecha está en el futuro devuelve false"
    (is (false? (es-hoy-o-antes? (-> (LocalDate/now) (.plusMonths 1)))))
    (is (false? (es-hoy-o-antes? (-> (LocalDate/now) (.plusDays 10)))))
    (is (false? (es-hoy-o-antes? (-> (LocalDate/now) (.plusYears 1))))))
  (testing "Cuando la fecha es de hoy o anterior, devuelve true"
    (is (true? (es-hoy-o-antes? (LocalDate/now))))
    (is (true? (es-hoy-o-antes? (-> (LocalDate/now) (.minusMonths 1)))))
    (is (true? (es-hoy-o-antes? (-> (LocalDate/now) (.minusDays 15)))))))

(deftest preparar-registros-planes-test
  (let [especialidades {:consulta-comun [354 386 358]
                        :consulta-especialista  [355 356 360 385 381 359 409 410 411 412 413 414 415 416 417 418 419 420 421
                                                 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440
                                                 441 442 443 444 445 446 447 448 449 450 451 452 453]
                        :consulta-nutricion [357]}
        propiedad (gen/hash-map :codplan (gen/not-empty (gen/vector gen/string))
                                :especialidad (gen/not-empty (gen/set (gen/elements ["consulta-comun" "consulta-especialista" "consulta-nutricion"])))
                                :copago gen/nat)
        mapa (rand-nth (gen/sample propiedad))
        mapa-determinista {:codplan "1820-A" :especialidad #{"consulta-comun" "consulta-especialista"} :copago 10000}]
    (testing "Devuelve colección secuencial de vectores" 
      (is (coll? (preparar-registros-planes mapa especialidades)))
      (is (every? vector? (preparar-registros-planes mapa especialidades)))
      (is (true? (:pass? (tc/quick-check 100 (prop/for-all [input propiedad]
                                                           (preparar-registros-planes input especialidades)))))))
    (testing "Vector interno tiene 4 elementos" 
      (is (== 4 (count (first (preparar-registros-planes mapa-determinista especialidades))))))
    (testing "Lanza excepción cuando ambos inputs son nil"
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-planes nil nil))))
    (testing "Lanza excepción cuando alguno de los inputs es nil"
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-planes nil especialidad->codigo)))
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-planes mapa-determinista nil))))
    (testing "Lanza excepción si alguno de los inputs no es un mapa"
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-planes [] especialidad->codigo)))
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-planes mapa-determinista "hola"))))))
 
(deftest preparar-registros-historico-test
  (let [propiedad  (gen/such-that #(not (nil? %)) (gen/hash-map :codplan (gen/not-empty (gen/vector gen/string))
                                                                :especialidad (gen/not-empty (gen/set (gen/elements ["consulta-comun" "consulta-especialista" "consulta-nutricion"])))
                                                                :copago gen/nat
                                                                :vigencia gen-date-str))
        mapa (rand-nth (gen/sample propiedad))]
    (testing "Devuelve colección secuencial de vectores" 
      (is (coll? (preparar-registros-historico mapa)))
      (is (every? vector? (preparar-registros-historico mapa)))
      (is (true? (:pass? (tc/quick-check 100 (prop/for-all [p propiedad]
                                                           (let [res (preparar-registros-historico p)] 
                                                             (and (coll? res) (every? vector? res)))))))))
    (testing "Lanza excepción cuando input es nil"
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-historico nil))))
    (testing "Lanza excepción si el input no es un mapa"
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-historico [])))
      (is (thrown? java.lang.IllegalArgumentException (preparar-registros-historico #{}))))))

(deftest handler-test
  (testing "Handler siempre devuelve un mapa"
    (let [req (gen/hash-map :headers (gen/hash-map "Content-Type" (gen/elements ["application/json" "text/html" "image/jpeg" "application/ld+json" "text/javascript"]))
                            :accept (gen/return :json)
                            :body gen/string
                            :method (gen/elements [:post :get :put :delete])
                            :uri (gen/elements ["" 
                                                "/cualquier_cosa" 
                                                "/guardar" 
                                                "/style.css" 
                                                "/" 
                                                "/img/SanatorioColegialesEntrada.jpg" 
                                                "img"
                                                "/copagos.cljs"
                                                "/planes"
                                                "/la-chingada"
                                                "x.cljs"]))] 
      (with-redefs-fn {#'server/guardar (fn [_] {:status 200
                                                 :body "Guardado!"})
                       #'server/obtiene-copagos-guardados (fn [_]
                                                            {:status 200
                                                             :body "Ok"})}
        #(is (true? (:pass? (tc/quick-check 100 (prop/for-all [r req]
                                                              (map? (app r)))))))))))
 
(comment 

  (run-test categoria->keyword-test)  
  (run-test es-hoy-o-antes?-test) 
  (run-test preparar-registros-planes-test) 
  (run-test preparar-registros-historico-test)
  (run-test handler-test)
  (run-all-tests)

  (tc/quick-check 100 (prop/for-all [st gen/string]
                                    (string? (categoria->keyword st))))

  (gen/sample (gen/such-that #(not (string? %)) gen/any))

  (gen/sample gen-date)

  (gen/sample (gen/such-that #(not (nil? %)) (gen/hash-map :codplan (gen/not-empty (gen/vector gen/string))
                                                           :especialidad (gen/set (gen/elements ["consulta-comun" "consulta-especialista" "consulta-nutricion"]))
                                                           :copago gen/nat
                                                           :vigencia gen-date-str)))
  
  (gen/sample (gen/hash-map :headers (gen/hash-map "Content-Type" (gen/elements ["application/json" "text/html" "image/jpeg" "application/ld+json" "text/javascript"]))
                            :accept (gen/return :json)
                            :body  gen/string
                            :method (gen/elements [:post :get :put :delete])
                            :uri (gen/elements ["" "/cualquier_cosa" "/guardar" "/style.css" "/" "/img/SanatorioColegialesEntrada.jpg" "img"]))) 
  
  (tc/quick-check 100 (gen/such-that #(not (nil? %)) (gen/hash-map :codplan (gen/not-empty (gen/vector gen/string))
                                                                   :especialidad (gen/not-empty (gen/set (gen/elements ["consulta-comun" "consulta-especialista" "consulta-nutricion"])))
                                                                   :copago gen/nat
                                                                   :vigencia gen-date-str)))

  (generar-fecha-aleatorias)

  (let [especialidades {:consulta-comun [354 386 358]
                        :consulta-especialista  [355 356 360 385 381 359 409 410 411 412 413 414 415 416 417 418 419 420 421
                                                 422 423 424 425 426 427 428 429 430 431 432 433 434 435 436 437 438 439 440
                                                 441 442 443 444 445 446 447 448 449 450 451 452 453]
                        :consulta-nutricion [357]}
        mapa (rand-nth (gen/sample (gen/hash-map :codplan gen/string
                                                 :especialidad (gen/set (gen/elements ["consulta-comun" "consulta-especialista" "consulta-nutricion"]))
                                                 :copago gen/nat)))] 
    (preparar-registros-planes mapa especialidades))
   gen-date-str
  )