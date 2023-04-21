(ns app.core
  (:require [clojure.data.json :as json]
            [clojure.pprint :as pp]
            [clojure.string :refer [join split trim]]
            [clojure.walk :refer [keywordize-keys]]
            [aero.core :refer (read-config)]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :refer [body-params]]
            [io.pedestal.interceptor :as intc]
            [io.pedestal.log :refer [debug info error]]
            [ring.util.response :refer [response]]
            [hiccup.page :refer [html5]]
            [java-time.api :as jt]
            [babashka.fs :as fs]
            [alandipert.enduro :as e]
            [clj-meme.core :refer [generate-image!]])
  (:import (java.net Socket)
           (java.io PrintWriter InputStreamReader BufferedReader)
           (java.util UUID)))

;; Config and constants
;; =============================================================================

(def cfg (read-config "config.edn" {}))
(def irc-servers (:irc-servers cfg))
(def chan (:channel cfg))
(def start-inst (jt/instant))
(declare conn-handler)
(def data-path "resources/public/data")
(def images-path "resources/public/data/images")
(def state (e/file-atom {:signals-count 0} "riker-state.clj"))

;; Utility functions
;; =============================================================================

(defn- pretty-spit
  [f-name xs]
  (spit (java.io.File. f-name) (with-out-str (pp/write xs :dispatch pp/code-dispatch))))

(defn- uptime-by-unit [unit] (jt/as (jt/duration start-inst (jt/instant)) unit))

(defn- meme-templates []
  (let [templates (map fs/file-name (fs/glob (str images-path "/meme/templates") "**{.*}"))]
    (join " " templates)))

(defn- make-meme [msg]
  (let [words (split msg #" ")
        template (nth words 5)
        label-all (second (split msg (re-pattern template)))
        labels (split label-all #"\^")
        meme (str images-path "/meme/memes/" (UUID/randomUUID) ".png")]
    (generate-image! (str images-path "/meme/templates/" template)
                     (trim (first labels))
                     (trim (last labels))
                     meme)
    (str (:site-root cfg) (last (split meme #"resources/public/")))))

;; IRC integration
;; =============================================================================

(defn- connect [server]
  (let [socket (Socket. (:uri server) (:port server))
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (PrintWriter. (.getOutputStream socket))
        conn (ref {:in in :out out})]
    (doto (Thread. #(conn-handler conn)) (.start))
    conn))

(defn- write [conn msg]
  (doto (:out @conn)
    (.println (str msg "\r"))
    (.flush)))

(defn- irc [conn msg] (write conn (str "PRIVMSG " chan " " msg)))

(defn- conn-handler [conn]
  (while (nil? (:exit @conn))
    (let [msg (.readLine (:in @conn))]
      (info :rikerbot/conn-handler msg)
      (cond 
       (re-find #"^ERROR :Closing Link:" msg) (dosync (alter conn merge {:exit true}))
       (re-find #"^PING" msg)                 (write conn (str "PONG "  (re-find #":.*" msg)))
       (re-find #"help" msg)                  (irc conn ":hi rikerbot, uptime days, uptime minutes, meme templates, make meme")
       (re-find #"hi rikerbot" msg)           (write conn "PRIVMSG #rossmcd ohai")
       (re-find #"uptime days" msg)           (irc conn (uptime-by-unit :days))
       (re-find #"uptime minutes" msg)        (irc conn (uptime-by-unit :minutes))
       (re-find #"!test-irc-signal" msg)      (do (e/swap! state update :signals-count inc) (irc conn ":signal conveyed to riker bot app"))
       (re-find #"meme templates" msg)        (irc conn (str ":" (meme-templates)))
       (re-find #"make meme" msg)             (let [meme (make-meme msg)] (irc conn (str ":" meme)))))))

(defn- login [conn user]
  (write conn (str "NICK " (:nick user)))
  (write conn (str "USER " (:nick user) " 0 * :" (:name user))))

;; Service, webapp, API
;; =============================================================================

(def api-tors [(body-params)])
(def htm-tors [(body-params) http/html-body])

(defn inject-irc-con
  "A simple interceptor to inject IRC connection 'conn' into the context."
  [conn]
  {:name  ::inject-irc-con
   :enter (fn [ctx] (assoc-in ctx [:request :conn] conn))})

(defn- github [req]
  (let [headers (keywordize-keys (:headers req))
        params (:params req)
        payload (keywordize-keys (json/read-str (:payload params)))
        repo (get-in payload [:repository :name])
        org (or (get-in payload [:organization :login]) "N/A")
        git-evt (cond
          (:issue payload) "issue"
          (:hook payload) "webhook"
          :else "unknown")
        action (or (get-in payload [:action]) "unknown action")
        provider (or (get-in payload [:sender :html_url]) "unknown")
        msg (str ":" provider " " action " " git-evt " at organisation : " org ", repository : " repo "\n")]
    (irc (:conn req) msg)
    (e/swap! state update :signals-count inc)
    {:status 200 :body "ok"}))

(defn head []
  [:html [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
   [:link {:rel "stylesheet" :type "text/css" :href "/css/bootstrap.min.css"}]
   [:link {:rel "stylesheet" :type "text/css" :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.5.0/font/bootstrap-icons.css"}]
   [:link {:rel "stylesheet" :type "text/css" :href "/css/style.css"}]
   [:title "Riker Bot Dashboard"]]])

(defn navbar [user]
  [:nav {:class "navbar navbar-expand-md navbar-light fixed-top bg-light"}
   [:div {:class "container-fluid"}
    [:a {:class "navbar-brand" :href "/"} "Riker Bot Dashboard"]
    [:button {:class "navbar-toggler" :type "button" :data-toggle "collapse" :data-target "#navbarCollapse" :aria-controls "navbarCollapse" :aria-expanded "false" :aria-label "Toggle navigation"}
     [:span {:class "navbar-toggler-icon"}]]
    [:div {:class "collapse navbar-collapse" :id "navbarCollapse"}
     ; placeholder for nav
      ]]])

(defn body
  [user & content]
  [:body
    (navbar user)
    [:div {:class "container-fluid"}
    [:div {:class "row"}
      content]]
    [:div {:class "container-fluid"}
    [:footer
      [:p
      [:small "Riker Bot v" (get-in cfg [:version :riker]) ", uptime " (uptime-by-unit :days) " days, signals handled " (:signals-count @state)]]]]
    [:script {:src "//code.jquery.com/jquery.js"}]
    [:script {:src "/js/bootstrap.min.js"}]])

(defn home [req]
(response
 (html5
  (head)
  (body nil
   [:div {:class "col-lg-9" :role "main"}
    [:div {:class "card"}
     [:div {:class "card-header"} [:h2 "Riker Bot Dashboard"]]
     [:div {:class "card-body"}
      [:h5 {:class "card-title"} "About Riker Bot"]
      [:p "Details on the version, uptime, commands and events handled by Riker Bot."]
      [:p "Commands available:"]
      [:ul
       [:li "hi rikerbot"]
       [:li "uptime days"]
       [:li "uptime minutes"]]]]
    [:div {:class "card"}
     [:div {:class "card-header"} [:h2 "Latest creations"]]
     [:div {:class "card-body"} "There are times where various interfaces (e.g. IRC) are not capable of displaying the content created with them (e.g. images) - this captures the most recent artefacts."]]])))) 

(def routes
  #{["/"       :get  (conj htm-tors `home)]
    ["/github" :post (conj api-tors `github)]})

;; Server and entry point
;; =============================================================================

(def service-map {
    ::http/secure-headers    {:content-security-policy-settings {:object-src "none"}}
    ::http/routes            routes
    ::http/type              :jetty
    ::http/resource-path     "public"
    ::http/host              "0.0.0.0"
    ::http/port              (Integer. (or (:port cfg) 5001))
    ::http/container-options {:h2c? true :h2?  false :ssl? false}})

(defn create-server [conn]
  (-> service-map
      (http/default-interceptors)
      (update ::http/interceptors conj (intc/interceptor (inject-irc-con conn)))
      http/create-server))

(defn -main [_]
  (info :rikerbot/main (str "starting rikerbot v" (get-in cfg [:version :riker]) "...") )
  (when (nil? (:irc-servers cfg)) (do (error :rikerbot/main "Please check config - missing item") (System/exit 0)))
  (info :rikerbot/main "starting IRC connection...")
  (let [conn (connect (:libera irc-servers))]
    (info :rikerbot/main "started IRC connection")
    (login conn (:libera irc-servers))
    (write conn "JOIN #rossmcd")
    (http/start (create-server conn))))
