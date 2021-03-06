(ns clojure0401.core
  ;(:use clojure.pprint)   ;da bi se koristila lepa stampa pprint
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
#_(defn commute-inc! [counter]
         (dosync (Thread/sleep 100) (commute counter inc)))
#_(defn alter-inc! [counter]
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
#_(binding [*response-code* nil]
      (let [content (http-get "http://google.com/bad-url")]
        (println "Response code was:" *response-code*)
        ; ... do something with `content` if it is not nil ...
        ))
;Response code was: 404
;nil

#_(binding [*max-value* 500]        ;u tekucoj niti postavljamo vrednost *max-value* na 500
   (println (valid-value? 299))
   @(future (valid-value? 299)))
;true
;true

#_(binding [*max-value* 500]     ;?????????????????????????????????????????
   (map valid-value? [299]))     ;map (funkcija vrednosti) ovde je funkcija valid-value? a broj koji se kontrolise je 299
                                    ;ako se ne varam ali tako da je formirano zbog map funkcije u novoj niti pa je vrednost *max-value* po deafolt-u 255
;(false)

#_(map #(binding [*max-value* 500]   ;map (funkcija vrednosti)  prvo funkcijom #(binding [*max-value* 500] dodeljujemo vrednost promenjivoj *max-value* 500
          (valid-value? %))              ;pa kontrolisemo da li je vrednost 299 manja ili jednaka novoj vrednosti 500
       [299])
;(true)

(def x 80)
(defn never-do-this []
  (def x 123)
  (def y 456)
  (def x (+ x y))
  x)
(never-do-this)
;579

(def x 0)
(alter-var-root #'x inc)
;1

(def j)
j
;#<Unbound Unbound: #'clojure0401.core/j>

#_(declare complex-helper-fn other-helper-fn)
#_(defn public-api-function
    [arg1 arg2]
    ...
    (other-helper-fn arg1 arg2 (complex-helper-fn arg1 arg2)))
#_(defn- complex-helper-fn
      [arg1 arg2]
      ...)
#_(defn- other-helper-fn
     [arg1 arg2 arg3]
     ...)

(def a (agent 500))
(send a range 1000)
@a
;(500 501 502 503 504 ... 999)

(def a (agent 0))
(send a inc)
;= #<Agent@65f7bb1f: 1>

(def a (agent 5000))
(def b (agent 10000))
;(send-off a #(Thread/sleep %))
;#<Agent@3468e8d2: 5000>
;(send-off b #(Thread/sleep %))
;#<Agent@7e2dad1: 10000>
@a                     ;1
;5000
@b                     
;10000
;(await a b)            ;2
;nil
@a                     ;3
;nil

(def a (agent nil))
(send a (fn [_] (throw (Exception. "something is wrong"))))
;= #<Agent@3cf71b00: nil>
a
;= #<Agent@3cf71b00 FAILED: nil>
;(send a identity)
;= #<Exception java.lang.Exception: something is wrong>

(restart-agent a 42)
;42
(send a inc)                                                          ;1
;#<Agent@1f23c0d6: 43>
(reduce send a (for [x (range 3)]                                     ;2
                 (fn [_] (throw (Exception. (str "error #" x))))))
;#<Agent@60f57515: 43>
(agent-error a)                                                       
;#<Exception java.lang.Exception: error #0>
(restart-agent a 42)
;42
(agent-error a)                                                        ;3
;#<Exception java.lang.Exception: error #1>
(restart-agent a 42 :clear-actions true)                               ;4
;42
(agent-error a)
;nil

(def a (agent nil :error-mode :continue))
(send a (fn [_] (throw (Exception. "something is wrong"))))
;= #<Agent@44a5b703: nil>
(send a identity)
;= #<Agent@44a5b703: nil>

(def a (agent nil
              :error-mode :continue
              :error-handler (fn [the-agent exception]
                               (.println System/out (.getMessage exception)))))
;(send a (fn [_] (throw (Exception. "something is wrong"))))
;#<Agent@5f4b678a: nil>
;something is wrong
(send a identity)
;#<Agent@22b8f2c4: nil>

(set-error-handler! a (fn [the-agent exception]
                        (when (= "FATAL" (.getMessage exception))
                          (set-error-mode! the-agent :fail))))
;(send a (fn [_] (throw (Exception. "FATAL"))))
;= #<Agent@6fe546fd: nil>
;(send a identity)
;= #<Exception java.lang.Exception: FATAL>

(require '[clojure.java.io :as io])
(def console (agent *out*))
(def character-log (agent (io/writer "character-states.log" :append true)))

(defn write
  [^java.io.Writer w & content]
  (doseq [x (interpose " " content)]
    (.write w (str x)))
  (doto w
    (.write "\n")
    .flush))

(defn log-reference
  [reference & writer-agents]
  (add-watch reference :log
             (fn [_ reference old new]
               (doseq [writer-agent writer-agents]
                 (send-off writer-agent write new)))))

(def smaug (character "Smaug" :health 500 :strength 400))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 1000))

(log-reference bilbo console character-log)
;Smaug 500
;Bilbo 100
;Gandalf 75
;#<Ref@32ba2e25: {:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100}>
(log-reference smaug console character-log)
;Smaug 500
;Bilbo 100
;Gandalf 75
;#<Ref@7d9b32ba: {:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 500}>
#_(wait-futures 1
               (play bilbo attack smaug)
               (play smaug attack bilbo)
               (play gandalf heal bilbo))

#_(
Smaug 500
Bilbo 100
Gandalf 75
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 492.0715066027129}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 97.77010476339552}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100.0}                   ;1
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 487.94404620456567}
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 486.13460591108714}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 73.22875328590104}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 72.35125295755785}
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 479.752100507298}
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 476.9490083907357}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 64.5169878515156}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 35.98593084126644}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 7.172606176131783}
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 476.36442157180613}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 0}
nil
)

(defn attack
  [aggressor target]
  (dosync
    (let [damage (* (rand 0.1) (:strength @aggressor) (ensure daylight))]
      (send-off console write
                (:name @aggressor) "hits" (:name @target) "for" damage)
      (commute target update-in [:health] #(max 0 (- % damage))))))
(defn heal
  [healer target]
  (dosync
    (let [aid (min (* (rand 0.1) (:mana @healer))
                   (- (:max-health @target) (:health @target)))]
      (when (pos? aid)
        (send-off console write
                  (:name @healer) "heals" (:name @target) "for" aid)
        (commute healer update-in [:mana] - (max 5 (/ aid 5)))
        (alter target update-in [:health] + aid)))))
#_(dosync
   (alter smaug assoc :health 500)
   (alter bilbo assoc :health 100))

;{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 500}
;{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100}

#_(wait-futures 1
                 (play bilbo attack smaug)
                 (play smaug attack bilbo)
                 (play gandalf heal bilbo))

#_(
Smaug 500
Bilbo 100
Gandalf 75
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100}
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 500}
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 83.79621908754453}
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 498.84352364561397}
Bilbo hits Smaug for 1.156476354386038
Smaug hits Bilbo for 16.203780912455475
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100.0}
Gandalf heals Bilbo for 16.20378091245547
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 73.62519853943135}
Smaug hits Bilbo for 26.374801460568648
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100.0}
Gandalf heals Bilbo for 26.374801460568648
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 494.0377267159964}
Bilbo hits Smaug for 4.805796929617563
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 85.97230230127796}
Smaug hits Bilbo for 14.027697698722038
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 485.77527726273837}
Bilbo hits Smaug for 8.26244945325806
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100.0}
Gandalf heals Bilbo for 14.027697698722037
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 90.25258369737399}
Smaug hits Bilbo for 9.747416302626002
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100.0}
Gandalf heals Bilbo for 9.747416302626007
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 480.6176381595708}
Bilbo hits Smaug for 5.157639103167584
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 79.2768574586542}
Smaug hits Bilbo for 20.723142541345794
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 84.16883029266126}
Gandalf heals Bilbo for 4.891972834007056
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 478.60265153359995}
Bilbo hits Smaug for 2.014986625970823
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 469.4108713003282}
Bilbo hits Smaug for 9.191780233271723
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 70.44864412761707}
Smaug hits Bilbo for 13.720186165044188
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 461.8574103034488}
Bilbo hits Smaug for 7.553460996879433
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100.0}
Gandalf heals Bilbo for 29.551355872382928
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 461.4571536079265}
Bilbo hits Smaug for 0.40025669552225973
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 90.66342871616342}
Smaug hits Bilbo for 9.336571283836573
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 100.0}
Gandalf heals Bilbo for 9.336571283836577
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 459.5429426482949}
Bilbo hits Smaug for 1.9142109596316015
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 77.16454857545826}
Smaug hits Bilbo for 22.835451424541745
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 454.30122924629745}
Bilbo hits Smaug for 5.241713401997472
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 452.2505139308546}
Bilbo hits Smaug for 2.050715315442882
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 66.61148043359417}
Smaug hits Bilbo for 10.553068141864093
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 451.5781326654401}
Bilbo hits Smaug for 0.6723812654144978
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 451.2218192315935}
Bilbo hits Smaug for 0.3563134338465968
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 31.965401392700343}
Smaug hits Bilbo for 34.646079040893824
{:max-health 500, :strength 400, :name "Smaug", :items #{}, :health 449.1519052184677}
Bilbo hits Smaug for 2.0699140131258287
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 9.84608834805245}
Smaug hits Bilbo for 22.119313044647892
{:max-health 100, :strength 100, :name "Bilbo", :items #{}, :health 0}
Smaug hits Bilbo for 31.193353444823053
nil    
)

(require '[net.cgrand.enlive-html :as enlive])
(use '[clojure.string :only (lower-case)])
(import '(java.net URL MalformedURLException))

(defn- links-from
  [base-url html]
  (remove nil? (for [link (enlive/select html [:a])]
                 (when-let [href (-> link :attrs :href)]
                   (try
                     (URL. base-url href)
                     ; ignore bad URLs
                     (catch MalformedURLException e))))))
(defn- words-from
  [html]
  (let [chunks (-> html
                 (enlive/at [:script] nil)
                 (enlive/select [:body enlive/text-node]))]
    (->> chunks
      (mapcat (partial re-seq #"\w+"))
      (remove (partial re-matches #"\d+"))
      (map lower-case))))
(def url-queue (java.util.concurrent.LinkedBlockingQueue.))
(def crawled-urls (atom #{}))
(def word-freqs (atom {}))

(declare get-url)
(def agents (set (repeatedly 25 #(agent {::t #'get-url :queue url-queue}))))
(declare run process handle-results)            

(use '[clojure.java.io :only (as-url)])
(defn ^::blocking get-url
  [{:keys [^java.util.concurrent.BlockingQueue queue] :as state}]
  (let [url (as-url (.take queue))]
    (try
      (if (@crawled-urls url)
        state
        {:url url
         :content (slurp url)
         ::t #'process})
      (catch Exception e
        ;; skip any URL we failed to load
        state)
      (finally (run *agent*)))))

(defn process
  [{:keys [url content]}]
  (try
    (let [html (enlive/html-resource (java.io.StringReader. content))]
      {::t #'handle-results
       :url url
       :links (links-from url html)
       :words (reduce (fn [m word]
                        (update-in m [word] (fnil inc 0)))
                      {}
                      (words-from html))})
    (finally (run *agent*))))

(defn ^::blocking handle-results
  [{:keys [url links words]}]
  (try
    (swap! crawled-urls conj url)
    (doseq [url links]
      (.put url-queue url))
    (swap! word-freqs (partial merge-with +) words)
    {::t #'get-url :queue url-queue}
    (finally (run *agent*))))

(defn paused? [agent] (::paused (meta agent)))
(defn run
  ([] (doseq [a agents] (run a)))
  ([a]
    (when (agents a)
      (send a (fn [{transition ::t :as state}]
                (when-not (paused? *agent*)
                  (let [dispatch-fn (if (-> transition meta ::blocking)
                                      send-off
                                      send)]
                    (dispatch-fn *agent* transition)))
                state)))))

(defn pause
  ([] (doseq [a agents] (pause a)))
  ([a] (alter-meta! a assoc ::paused true)))
(defn restart
  ([] (doseq [a agents] (restart a)))
  ([a]
    (alter-meta! a dissoc ::paused)
    (run a)))

(defn test-crawler
  "Resets all state associated with the crawler, adds the given URL to the
   url-queue, and runs the crawler for 60 seconds, returning a vector
   containing the number of URLs crawled, and the number of URLs
   accumulated through crawling that have yet to be visited."
  [agent-count starting-url]
  (def agents (set (repeatedly agent-count
                               #(agent {::t #'get-url :queue url-queue}))))
  (.clear url-queue)
  (swap! crawled-urls empty)
  (swap! word-freqs empty)
  (.add url-queue starting-url)
  (run)
  (Thread/sleep 6000)
  (pause)
  [(count @crawled-urls) (count url-queue)])
  
;(test-crawler 1 "http://www.bbc.co.uk/news/")
;[9 1728]

;(test-crawler 25 "http://www.bbc.co.uk/news/")
;[97 24018]

#_(->> (sort-by val @word-freqs)
reverse
(take 10))
;(["the" 3256] ["to" 1319] ["bbc" 1161] ["full" 1124] ["from" 1116] ["article" 1081] ["december" 986] ["section" 941] ["video" 833] ["of" 831])
#_(->> (sort-by val @word-freqs)
  (take 10))
;(["sovereign" 1] ["vying" 1] ["mac" 1] ["gingerbread" 1] ["premises" 1] ["prompting" 1] ["tune" 1] ["bogglingly" 1] ["fydd" 1] ["credible" 1])

;(alter-meta! #'process assoc ::blocking true)
;{:ns #<Namespace clojure0401.core>, :name process, :file "clojure0401\\core.clj", :column 1, :line 1001, :clojure0401.core/blocking true, :arglists ([{:keys [url content]}])}

;(test-crawler 1 "http://www.bbc.co.uk/news/")
;[68 14130]

(.start (Thread. #(println "Running...")))
;Running...

#_(defn relay [x i]
  (when (:next x)
    (send (:next x) relay i))
  (when (and (zero? i) (:report-queue x))
    (.put (:report-queue x) i))
  x)
 
#_(defn run [m n]
  (let [q (new java.util.concurrent.SynchronousQueue)
        hd (reduce (fn [next _] (agent {:next next}))
                   (agent {:report-queue q}) (range (dec m)))]
    (doseq [i (reverse (range n))]
      (send hd relay i))
    (.take q)))

;(time (run 1000 1000))
;"Elapsed time: 710.897294 msecs"





