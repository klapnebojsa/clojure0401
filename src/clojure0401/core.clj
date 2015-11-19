(ns clojure0401.core)

#_(def d (delay (println "Running...")
                :done!))
;(deref d)

(defn get-document
  [id]
  ; ... do some work to retrieve the identified document's metadata ...
  {:url "http://www.mozilla.org/about/manifesto.en.html"
   :title "The Mozilla Manifesto"
   :mime "text/html"
   :content (delay (slurp "http://www.google.com"))})
(def d (get-document "some-id"))
d
;{:url "http://www.mozilla.org/about/manifesto.en.html", :title "The Mozilla Manifesto", :mime "text/html", :content #<Delay@7dbf8d34: :pending>}

(realized? (:content d))
;false
;@(:content d)
;"<!doctype html><h......
(realized? (:content d))
;true

(def long-calculation (future (apply + (range 1e4))))
;@long-calculation
;49995000

;@(future (Thread/sleep 5000) :done!)      ;odlaze izvrsenje za et sekundi

#_(deref (future (Thread/sleep 5000) :done!)   ;obezbedjuje tajmaut.
        1000                                       ;kada vrednost u gornjoj liniji dostigne 1000 izvrsava se linija ispod 
        :impatient!)                                ;ako je precizirano vreme isto kao navedeno u gornjoj liniji onda ce se izvrsiti linija :done


(defn get-document1
  [id]
  ; ... do some work to retrieve the identified document's metadata ...
  {:url "http://www.mozilla.org/about/manifesto.en.html"
   :title "The Mozilla Manifesto"
   :mime "text/html"
   :content (future (slurp "http://www.google.com"))})
(def d1 (get-document1 "some-id1"))
d1
;{:url "http://www.mozilla.org/about/manifesto.en.html", :title "The Mozilla Manifesto", :mime "text/html", :content #<core$future_call$reify__6320@7853fcaf: :pending>}

(def p (promise))
(realized? p)
;= false
(deliver p 42)
;= #<core$promise$reify__1707@3f0ba812: 42>
(realized? p)
;= true
@p
;= 42

(def a (promise))
(def b (promise))
(def c (promise))
#_(future
   (deliver c (+ @a @b))                          ;kada se stvore uslovi izracunaj c
   (println "Delivery complete! OOOOOOOOOO" @c))  ;i stampaj vrednost i poruku      
(deliver a 15) 
(deliver b 16)    ;kada su uslovi ispunjeni izvrsava se komanda
;Delivery complete! OOOOOOOOOO 31


(def a (promise))
(def b (promise))
(future (deliver a @b))
(future (deliver b @a))
(realized? a)
;= false
(realized? b)
;= false
(deliver a 42)
@a
;42
@b
;42

(defn call-service     ;funkcija koja koja uzima drugu funkciju kao callback-fn
  [arg1 arg2 callback-fn]
  (println arg1 arg2 callback-fn)
  ; ...perform service call, eventually invoking callback-fn with results...
  (future (callback-fn (+ arg1 arg2) (- arg1 arg2))))
;(call-service 42 5 +)
;#<core$future_call$reify__6320@59d730f2: 84>

(defn sync-fn
  [async-fn]   ;async-fn je po pozivu call-service
  (fn [& args]   ;vrednosti args su (8 7)
    (let [result (promise)]                                        ;definise se result kao promise   
      (apply async-fn (conj (vec args) #(deliver result %&)))     ;apply poziva funkciju async-fn=call-service sa vrednostima vektor args tj. 8 7
                                                                  ;conj spaja vrednosti vektora args tj [8 7] i #(deliver result %&)
                                                                  ;deliver result %& - je postavljanje pocetne vrednosti resulat ALI NA STA?????? STA JE %&
                                                                  ;trebalo bi da je to funkcija sync_fn
     @result)))

((sync-fn call-service) 8 7)  ;poziva sync-fn gde je async-fn=call-service i args=8 7 u okviru fn funkcije
;(15 1)               -resenje prvo je zbir unetih brojeva (15), drugo je razlika unetih brojeva (1)









