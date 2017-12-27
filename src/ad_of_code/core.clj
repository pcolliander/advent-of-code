(ns ad-of-code.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            
            ))

; testing spec a bit.

;; (s/def ::id pos-int?)
;; (s/def ::name string?)
;;
;; (s/def ::seq (s/keys :req [::id ::name])) 
;;
;; (s/exercise ::seq)
;;
;; (defn add-one [x]
;;   (inc x))
;;
;; (defn subtract-one [x]
;;   (dec x))
;;
;; (defn add-me [x]
;;   (if x
;;     (inc x)))
;;
;; (s/fdef add-one
;;   :args (s/cat :x number?)
;;   :ret number?
;;   :fn #(+ :args 1))
;;
;;
;; (s/fdef subtract-one
;;   :args (s/cat :x number?)
;;   :ret int?
;;   :fn #(- :args 1))
;;
;; (s/fdef add-me
;;   :args (s/cat :x int?)
;;   :ret nat-int?
;;   :fn #(+ :args 1))
;;
;; (s/exercise-fn `add-one)
;; (s/exercise-fn `subtract-one)
;;
;; (stest/instrument `add-one)
;; (stest/check `add-me)

;; (add-one "1.5") ; => does not conform to spec. "Instrumentation validates that the :args spec is being invoked on instrumented functions and thus provides validation for external uses of a function."


(defmulti websocket-handler (fn [message channel] (:message-type message)))

(defmethod websocket-handler :new-message 
  [message channel]
  (println "message " (:payload message) channel))

(defmethod websocket-handler :user-is-typing 
  [message channel]
  (println "message " (:payload message) (:message-type message)))

(websocket-handler {:message-type :new-message
                     :payload "this is the payload"}

                    "channeL"
                    
                    )

(websocket-handler {:message-type :user-is-typing
                     :payload "this is the payload."}
                    
                    "channel"
                    )

(defn f []
  (let [coll [ {:id 1 :username "bob"}
               {:id 2 :username "bobb"}
               {:id 3 :username "boba"}
               {:id 4 :username "bobi"} ]]

    (->> coll 
      (map #(if (= (:id %) 3) (assoc % :current-user true) %))  
      (doall))))

(f)







