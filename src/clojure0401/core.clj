(ns clojure0401.core
  (:use clojure.pprint)   ;da bi se koristila lepa stampa pprint
  (:use [clojure.repl]))  ;da bi se koristio (doc)

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

;@(future (Thread/sleep 5000) :done!)      ;odlaze izvrsenje za pet sekundi

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
  ;(println arg1 arg2 callback-fn)
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

(defn phone-numbers
  [string]
  (re-seq #"(\d{3})[\.-]?(\d{3})[\.-]?(\d{4})" string))
(phone-numbers " Sunil: 617.555.2937, Betty: 508.555.2218")
;(["617.555.2937" "617" "555" "2937"] ["508.555.2218" "508" "555" "2218"])

(def files (repeat 100
                   (apply str 
                          (concat (repeat 1000000 \space)
                                  "Sunil: 617.555.2937, Betty: 508.555.2218"))))
;(time (dorun (map phone-numbers files))) ;dorun se koristi zato sto ne zelimo da stampamo sve rezultate u REPL
;"Elapsed time: 1596.122235 msecs"
;(time (dorun (pmap phone-numbers files)))
;"Elapsed time: 385.455303 msecs"

(def files1 (repeat 100000 
                    (apply str 
                           (concat (repeat 1000 \space)
                                   "Sunil: 617.555.2937, Betty: 508.555.2218"))))
;(time (dorun (map phone-numbers files1)))
;"Elapsed time: 1768.136658 msecs"
;(time (dorun (pmap phone-numbers files1)))
;"Elapsed time: 471.532979 msecs"

(def sarah {:name "Sarah" :age 25 :wears-glasses? false})

@(atom 12)
;= 12
@(agent {:c 42})
;= {:c 42}
(map deref [(agent {:c 42}) (atom 12) (ref "http://clojure.org") (var +)])
;({:c 42} 12 "http://clojure.org" #<core$_PLUS_ clojure.core$_PLUS_@14a5296f>)

(def sarah (atom {:name "Sarah" :age 25 :wears-glasses? false}))
(swap! sarah update-in [:age] + 3)
;{:age 28, :wears-glasses? false, :name "Sarah"}

(swap! sarah (comp #(update-in % [:age] inc)
                   #(assoc % :wears-glasses? true)))
;{:age 29, :name "Sarah", :wears-glasses? true}

(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))

(defmacro wait-futures
  [& args]
  ;(println "args" args)
  `(doseq [f# (futures ~@args)]
     @f#))

(def xs (atom #{1 2 3}))
#_(wait-futures 1 (swap! xs (fn [v]
                               (Thread/sleep 250)
                               (println "trying 4")
                               (conj v 4)))
                 (swap! xs (fn [v]
                             (Thread/sleep 500)
                             (println "trying 5")
                             (conj v 5))))
#_(trying 4
trying 5
trying 5
nil)
;@xs
;#{1 4 3 2 5}

(compare-and-set! xs :wrong "new value")
;= false
(compare-and-set! xs @xs "new value")
;= true
;@xs
;"new value"

(def xs (atom #{1 2}))
;= #'user/xs
(compare-and-set! xs #{1 2} "new value")
;= false

(reset! xs :y)
;:y
@xs
;:y

(defn echo-watch
  [key identity old new]
  (println key old "=>" new))
(def sarah (atom {:name "Sarah" :age 25}))
(add-watch sarah :echo echo-watch)
;(swap! sarah update-in [:age] inc)
;:echo {:name Sarah, :age 25} => {:name Sarah, :age 26}
(add-watch sarah :echo2 echo-watch)
;(swap! sarah update-in [:age] inc)
;:echo {:age 25, :name Sarah} => {:age 26, :name Sarah}
;:echo {:age 26, :name Sarah} => {:age 27, :name Sarah}
;:echo2 {:age 26, :name Sarah} => {:age 27, :name Sarah}
;{:age 27, :name "Sarah"}

(remove-watch sarah :echo2)
;(swap! sarah update-in [:age] inc)
;:echo {:age 27, :name Sarah} => {:age 28, :name Sarah}
;{:age 28, :name "Sarah"}

;(reset! sarah @sarah)
;:echo {:age 28, :name Sarah} => {:age 28, :name Sarah}
;{:age 28, :name "Sarah"}

(def history (atom ()))
(defn log->list
  [dest-atom key source old new]
  (when (not= old new)
    (swap! dest-atom conj new)))
(def sarah (atom {:name "Sarah", :age 25}))
(add-watch sarah :record (partial log->list history))
(swap! sarah update-in [:age] inc)
;{:age 26, :name "Sarah"}
(swap! sarah update-in [:age] inc)
;{:age 27, :name "Sarah"}
(swap! sarah identity)
;{:age 27, :name "Sarah"}
(swap! sarah assoc :wears-glasses? true)
;{:age 27, :name "Sarah", :wears-glasses? true}
(swap! sarah update-in [:age] inc)
;{:age 28, :name "Sarah", :wears-glasses? true}
;(pprint @history)
#_({:age 28, :name "Sarah", :wears-glasses? true}
 {:age 27, :name "Sarah", :wears-glasses? true}
 {:age 27, :name "Sarah"}
 {:age 26, :name "Sarah"})

(def n (atom 1 :validator pos?))
(swap! n + 500)
;501
;(swap! n - 1000)
;CompilerException java.lang.IllegalStateException: Invalid reference state, compiling:(clojure0401\core.clj:234:16)

(set-validator! sarah #(or (:age %)
                           (throw (IllegalStateException. "People must have `:age`s!"))))
;(swap! sarah dissoc :age)
;CompilerException java.lang.IllegalStateException: People must have `:age`s!, compiling:(clojure0401\core.clj:240:90)

(defn character
  [name & {:as opts}]
  (ref (merge {:name name :items #{} :health 500}
              opts)))
(def smaug   (character "Smaug"   :health 500 :strength 400 :items (set (range 50))))
;smaug
;#<Ref@5fa9b254: {:strength 400, :name "Smaug", :items #{0 7 20 27 1 24 39 46 4 15 48 21 31 32 40 33 13 22 36 41 43 29 44 6 28 25 34 17 3 12 2 23 47 35 19 11 9 5 14 45 26 16 38 30 10 18 42 37 8 49}, :health 500}>
(def bilbo   (character "Bilbo"   :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75  :mana 750))

(defn loot 
  [from to]
  (dosync                    ;startuje transakciju ako vec nije startovana u toj niti
    (when-let [item (first (:items @from))]   ;ukoliko se ostvari funkcija tj. ako postoji bar jedan items u liku from (od koga), prvi se smesta u promenjivu item i izvrsavaju se sledece dve linije koda
     ;(println "from to" (:name @from) (:name @to))  ;bez ovoga nece da promeni nit i onda obojica preuzimaju sve items od gubitnika
      ;alter - sluzi za transakcijsku obradu podataka. ispunjava se redom i vrsi se cekanje da se prethodna transakcija izvrsi
        (alter to update-in [:items] conj item)           ;u promenjivu to se dodaje vrednost oduzeta od promenjive to 
                                                            ;alter-mora se izvrsiti u okviru transakcije (dosync)
                                                            ;menja vrednost ref tipa podataka  okviru transakcije
        (alter from update-in [:items] disj item))))      ;od promejnive to se oduzima item 

(wait-futures 1                               ;wait-futures je ranije definisan MACRO - kada se dodje do makroa videti sa on radi
              (while (loot smaug bilbo))
              (while (loot smaug gandalf)))

@smaug
;{:strength 400, :name "Smaug", :items #{}, :health 500}
@bilbo
;{:strength 100, :name Bilbo, :items #{33 13 22 36 41 43 29 44 28 23 35 11 5 45 16 30 18 37 49}, :health 100}>
@gandalf
;{:mana 750, :name Gandalf, :items #{0 7 20 27 1 24 39 46 4 15 48 21 31 32 40 6 25 34 17 3 12 2 47 19 9 14 26 38 10 42 8}, :health 75}>

(map (comp count :items deref) [bilbo gandalf])
;(19 31)
(filter (:items @bilbo) (:items @gandalf))
;()

(= (/ (/ 120 3) 4) (/ (/ 120 4) 3))
(= ((comp #(/ % 3) #(/ % 4)) 120)    ;comp - kompozicija 
   ((comp #(/ % 4) #(/ % 3)) 120))     ;#(/ % 3) - funkcija koja deli zadati broj sa 3
                                       ;#(/ % 4) - funkcija koja deli broj (koji je dobijen u prethodnom koraku) sa 4
                                       ;120 je zadati broj
(map
  (comp - (partial + 3) (partial * 5))
  [1 2 3 4])
;(-8 -13 -18 -23)

(def x (ref 0))
#_(time (wait-futures 5
                        (dotimes [_ 1000]
                          (dosync (alter x + (apply + (range 1000)))))
                        (dotimes [_ 1000]
                          (dosync (alter x - (apply + (range 1000)))))))
;"Elapsed time: 8390.735556 msecs"


#_(time (wait-futures 5
                      (dotimes [_ 1000]
                        (dosync (commute x + (apply + (range 1000)))))          ;commute - setuje vrednost ref tipa promenjive u transakciji
                                                                                     ; optimizovani alter. nije bitno kojim se redom obavljaju vrednosti brojaca
                                                                                     ;transakcija ce se vratiti da izvrsi sve operacije koje nisu izvrsene
                                                                                     ;ne ceka se izvrsenje svih transakcija do kraja nego idu u uparaleli
                      (dotimes [_ 1000]
                        (dosync (commute x - (apply + (range 1000)))))))
;"Elapsed time: 110.156591 msecs"

(def counter (ref 0))                                                 ;ALTER vs COMMUTE
(defn commute-inc! [counter]
         (dosync (Thread/sleep 100) (commute counter inc)))
(defn alter-inc! [counter]
         (dosync (Thread/sleep 100) (alter counter inc)))
(defn bombard-counter! [n f counter]
         (apply pcalls (repeat n #(f counter))))
(dosync (ref-set counter 0))
;(time (doall (bombard-counter! 20 alter-inc! counter)))
;"Elapsed time: 2072.324499 msecs"
;(3 1 2 4 7 10 5 8 6 9 13 14 15 12 11 16 17 20 18 19)
; note that it took about 2000 ms = (20 workers * 100 ms / update)
(dosync (ref-set counter 0))
;(time (doall (bombard-counter! 20 commute-inc! counter)))
;"Elapsed time: 203.543491 msecs"
;(1 2 3 4 5 9 10 6 7 8 11 15 13 12 14 16 19 17 18 20)
; notice that we got actual concurrency this time.

;NEISPRAVNO 
(defn flawed-loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
      (commute to update-in [:items] conj item)
      (commute from update-in [:items] disj item))))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(wait-futures 1
              (while (flawed-loot smaug bilbo))
              (while (flawed-loot smaug gandalf)))

(map (comp count :items deref) [bilbo gandalf])
;(18 34)   ;bilbo ima 5 items, gandalf ima 34  - PROBLEM nije zbir 50
(filter (:items @bilbo) (:items @gandalf))
;(0 7 20 27 24)   ;ovi items se ponavljaju kod jednog i drugog

(defn fixed-loot
  [from to]
  (dosync
    (when-let [item (first (:items @from))]
    ;(println "from to" (:name @from) (:name @to))  ;bez ovoga nece da promeni nit i onda obojica preuzimaju sve items od gubitnika
      (commute to update-in [:items] conj item)
      (alter from update-in [:items] disj item))))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(wait-futures 1
              (while (fixed-loot smaug bilbo))
              (while (fixed-loot smaug gandalf)))

(map (comp count :items deref) [bilbo gandalf])
;(20 30)
(filter (:items @bilbo) (:items @gandalf))
;()

(defn attack
  [aggressor target]
  (dosync
    (let [damage (* (rand 0.1) (:strength @aggressor))]
      (commute target update-in [:health] #(max 0 (- % damage))))))
(defn heal
  [healer target]
  (dosync
    (let [aid (* (rand 0.1) (:mana @healer))]
      (when (pos? aid)
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (commute target update-in [:health] + aid)))))

(def alive? (comp pos? :health))
(defn play
  [character action other]
  (while (and (alive? @character)
              (alive? @other)
              (action character other))
    (Thread/sleep (rand-int 50))))

(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo))
(map (comp :health deref) [smaug bilbo])
;(487.3434192243626 0)

(dosync
  (alter smaug assoc :health 500)
  (alter bilbo assoc :health 100))
#_(wait-futures 1
                 (play bilbo attack smaug)
                 (play smaug attack bilbo)
                 (play gandalf heal bilbo))
(map (comp #(select-keys % [:name :health :mana]) deref) [smaug bilbo gandalf])
;({:health 0, :name "Smaug"} {:health 281.1463912431616, :name "Bilbo"} {:mana -2.807984377856087, :health 75, :name "Gandalf"})

(dosync (ref-set bilbo {:name "Bilbo"}))
;= {:name "Bilbo"}

(dosync (alter bilbo (constantly {:name "Bilbo"})))
; {:name "Bilbo"}

#_(defn- enforce-max-health
   [{:keys [name health]}]
   (println name health)
   (fn [character-data]
     (or (<= (:health character-data) health)
         (throw (IllegalStateException. (str name " is already at max health!"))))))

(defn- enforce-max-health
  [name health]
  (println name health)
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already at max health!"))))))

(defn character
  [name & {:as opts}]
  (let [cdata (merge {:name name :items #{} :health 500}
                     opts)
        cdata (assoc cdata :max-health (:health cdata))
        validators (list* (enforce-max-health name (:health cdata))
                          (:validators cdata))]
    (ref (dissoc cdata :validators)
         :validator #(every? (fn [v] (v %)) validators))))

;(def bilbo (character "Bilbo" :health 100 :strength 100))
;(heal gandalf bilbo)
;Bilbo 100
;CompilerException java.lang.IllegalStateException: Bilbo is already at max health!, compiling:(clojure0401\core.clj:416:58) 

(dosync (alter bilbo assoc-in [:health] 95))
;{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 95}
;(heal gandalf bilbo)
;Bilbo 100
;CompilerException java.lang.IllegalStateException: Bilbo is already at max health!, compiling:(clojure0401\core.clj:421:45) 

(defn heal
      [healer target]
      (dosync
        (let [aid (min (* (rand 0.1) (:mana @healer))
                       (- (:max-health @target) (:health @target)))]
          (when (pos? aid)
            (commute healer update-in [:mana] - (max 5 (/ aid 5)))
            (alter target update-in [:health] + aid)))))
(dosync (alter bilbo assoc-in [:health] 95))
;(heal gandalf bilbo)
;Bilbo 100
;{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100}
;(heal gandalf bilbo)
;Bilbo 100
;nil

(defn unsafe
  []
  (io! (println "writing to database...")))

;(dosync (unsafe))
;CompilerException java.lang.IllegalStateException: I/O in transaction, compiling:(clojure0401\core.clj:445:44) 

(def x (ref (java.util.ArrayList.)))
#_(wait-futures 2                        ;ranije definisan makro
               (dosync    ;startuje transakciju ako vec nije stsrovana u toj niti
                 (dotimes [v 5]             ;u promenjivu v stavlja vredosti 0 1 2 3 4 i vrti po svakoj od vrednosti
                   (Thread/sleep (rand-int 50))   ;(rand-int 50) - bira slucajan broj od 0 to 49. Thread/sleep - nastavlja za slucajan broj odabranih sekundi
                   (alter x            ;alter - mora biti starovana u transakciji. Setuje vrednost ref (tip promenjive) u okviru transakcije
                          #(doto % (.add v))))))   ;#=macro. doto u promrnjivu % dodaj vrednost trenutnog v
                                                   ;bez poziva makro-a wait-futures 2 resenje je x = [0 1 2 3 4]
;@x
;[0 0 1 0 2 3 4 0 1 2 3 4]

;(def x (ref 0))
#_(dosync
    @(future (dosync (ref-set x 0)))   ;future-odlaze izvrsenje komade dok se ne ispune uslovi
    (ref-set x 1))
;@x
;CompilerException java.lang.RuntimeException: Transaction failed after reaching retry limit, compiling:(clojure0401\core.clj:485:16)

(ref-max-history (ref "abc" :min-history 3 :max-history 30))
;30

(def a (ref 0))
;(future (dotimes [_ 500] (dosync (Thread/sleep 200) (alter a inc))))
;@(future (dosync (Thread/sleep 1000) @a))
;28
(ref-history-count a)
;5

(def a (ref 0))
;(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
;@(future (dosync (Thread/sleep 100) @a))
;= 500
(ref-history-count a)
;= 10

(def a (ref 0 :max-history 100))
;(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
;@(future (dosync (Thread/sleep 1000) @a))
;= 500
(ref-history-count a)
;= 10

(def a (ref 0 :min-history 50 :max-history 100))
;(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
;@(future (dosync (Thread/sleep 1000) @a))
;= 0
(ref-history-count a)
;49

(def daylight (ref 1))
(defn attack
  [aggressor target]
  (dosync
    (let [damage (* (rand 0.1) (:strength @aggressor) @daylight)]
      (commute target update-in [:health] #(max 0 (- % damage))))))

(def daylight (ref 1))
(defn attack
  [aggressor target]
  (dosync
    (ensure daylight)  ;ensure se koristi da ne dodje do write skew-a. Ako je u toku transakcija nad ulaznom vrednoscu ceka se zavrsetak transakcije pa se onda vrsi obrada
    (let [damage (* (rand 0.1) (:strength @aggressor) @daylight)]  
      (commute target update-in [:health] #(max 0 (- % damage))))))

                                          ; VARS
(def ^:private everything 42)

(def a
  "A sample value."
  5)
;= #'user/a
(defn b
  "A simple calculation using `a`."
  [c]
  (+ a c))
;(doc a)
; -------------------------
; clojure0401.core/a
; A sample value.
;(doc b)
; -------------------------
; clojure0401.core/b
; ([c])
; A simple calculation using `a`.

(meta #'a)
;{:ns #<Namespace clojure0401.core>, :name a, :file "clojure0401\\core.clj", :column 1, :line 542, :doc "A sample value."}

(def ^{:doc "A sample value."} a 5)
;= #'user/a
;(doc a)
;-------------------------
;clojure0401.core/a
;  A sample value.
;(alter-meta! #'a assoc :doc "A dummy value.")
;-------------------------
;clojure0401.core/a
;  A sample value.
;-------------------------
;clojure0401.core/b
;([c])
;  A simple calculation using `a`.
;{:ns #<Namespace clojure0401.core>, :name a, :file "clojure0401\\core.clj", :column 1, :line 563, :doc "A dummy value."}
;(doc a)
;-------------------------
;clojure0401.core/a
;  A dummy value.

(def ^:const everything 42)

(def max-value 255)
(defn valid-value?
  [v]
  (<= v max-value))

(valid-value? 218)
;true
(valid-value? 299)
;false
(def max-value 500)
(valid-value? 299)
;= true

(def ^:const max-value 255)
;= #'user/max-value
(defn valid-value?
[v]
(<= v max-value))
;= #'user/valid-value?
(def max-value 500)
;= #'user/max-value
(valid-value? 299)
;= false

#_(let [a 1
        b 2]
    (println (+ a b))
    (let [b 3
          + -]
      (println (+ a b))))
#_(3
-2)

(def ^:dynamic *max-value* 255)     ;definise *max-value* kao 255
(defn valid-value?
  [v]
  ;(println "vvv" v *max-value*)
  (<= v *max-value*))
#_(binding [*max-value* 500]
      (valid-value? 299))
;true

#_(binding [*max-value* 500]         ;redefinise *max-value* kao 500
   (println (valid-value? 299))     ;proverava da li je 299 manje od 500 i stampa rezultat
   (doto (Thread. #(println "in other thread:" (valid-value? 299)))   ;startuje novu nit i samimi tim je *max-value* je postavljen na 255 
                                                                      ;i proverava da li je 299 manje od 500
     .start
     .join))

  (def ^:dynamic *var* :root)
  (defn get-*var* [] *var*)
  (binding [*var* :a]
    (binding [*var* :b]
      (binding [*var* :c]
        (get-*var*))))
  ; :c

(defn http-get
  [url-string]
  (let [conn (-> url-string java.net.URL. .openConnection)
        response-code (.getResponseCode conn)]
    (if (== 404 response-code)
      [response-code]
      [response-code (-> conn .getInputStream slurp)])))

(http-get "http://google.com/bad-url")
;[404]
(http-get "http://google.com/")
;[200 "<!doctype html><html itemscope=\"\" itemtype=\"http://sc ...

(def ^:dynamic *response-code* nil)
(defn http-get
  [url-string]
  (let [conn (-> url-string java.net.URL. .openConnection)
        response-code (.getResponseCode conn)]
    (when (thread-bound? #'*response-code*)
      (set! *response-code* response-code))
    (when (not= 404 response-code) (-> conn .getInputStream slurp))))
(http-get "http://google.com")
;"<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org/WebPage\" lang=\"sr\"><head><m ...
*response-code*
;nil
(binding [*response-code* nil]
  (let [content (http-get "http://google.com/bad-url")]
    (println "Response code was:" *response-code*)
    ; ... do something with `content` if it is not nil ...
    ))
;Response code was: 404
;nil



























