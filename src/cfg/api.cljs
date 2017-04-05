(ns cfg.api
  (:require [ajax.core :refer [GET POST PUT]]
            [clojure.string :as string]
            [cljs.core.async :refer [put! chan <! close!]]
            [cfg.common :as cmn]
            [cfg.config :as cfg.config])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(defn- api-url
  [url & rest]
  (str cfg.config/api-base-url url (string/join "" rest)))


(defn- http
  ([method url]
   (http method url {}))

  ([method url body]
   (let [ch (chan)]
     (method url (merge {:format          :json
                         :response-format :json
                         :keywords?       true
                         :handler         #(put! ch {:status :success :response %})
                         :error-handler   #(put! ch {:status :error :response %})}
                        body))
     ch)))


(defn- api-get [url s e]
  (go
    (let [resp (<! (http GET (api-url url)))]
      (if (= :success (:status resp))
        (s (:response resp))
        (e (:response resp))))))


(defn get-disassembly
  ([offset count s e]
   ;; seek: s 0x401000
   ;; disassemble: aoj 10 (opcode analysis, json-mode)
   (api-get (str "/s " offset "; aoj " count) s e))
  ([offset s e] (get-disassembly offset 1 s e)))


(defn get-functions
  [s e]
  ;; list all functions: afl
  (api-get "/aflj" s e))


(defn analyze-all
  [s e]
  ;; analyze all+experimental: aaaa
  (api-get "/aaaa" s e))



(defn analyze-all2
  []
  ;; analyze all+experimental: aaaa
  ;; analyze all: aaaa
  (http GET (api-url "/aaa")))


(defn get-functions2
  []
  ;; list all functions: afl
  (http GET (api-url "/aflj")))


(defn get-basic-blocks2
  [fva]
  ;; list all functions: afl
  (http GET (api-url (str "/s " fva "; afbj"))))


