(ns app.core
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :refer [join replace split trim]]
            [clojure.walk :refer [keywordize-keys]]
            [clojure.zip :as z]
            [aero.core :refer (read-config)]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :refer [body-params]]
            [io.pedestal.http.ring-middlewares :as mw]
            [io.pedestal.interceptor :as intc]
            [io.pedestal.log :refer [debug info error]]
            [ring.middleware.session.cookie :as cookie]
            [ring.util.response :refer [response]]
            [org.httpkit.client :as client]
            [org.httpkit.sni-client :as sni-client]
            ;[hiccup.page :refer [html5]]
            [ten-d-c.hiccup-server-components.core :refer [->html]]
            [java-time.api :as jt]
            [babashka.fs :as fs]
            [alandipert.enduro :as e]
            [clj-meme.core :refer [generate-image!]]
            [chime.core :as chime]
            [ui.layout :refer [page ses-tors]]
            [ui.components])
  (:import (java.net Socket)
           (java.io File PrintWriter InputStreamReader BufferedReader)
           (java.util UUID)
           (java.time Duration Instant LocalTime ZonedDateTime ZoneId Period)))

;;;; Config and constants.
;;;; ===========================================================================

(alter-var-root #'org.httpkit.client/*default-client* (fn [_] sni-client/default-client))
(def cfg (read-config "config.edn" {}))

(def start-inst (jt/instant))

(def data-path "resources/public/data")
(def images-path "resources/public/data/images")
(def state (e/file-atom {:events-count 0} "riker-state.clj"))

;;;; Utility functions.
;;;; ===========================================================================

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

;;;; Service, webapp, API.
;;;; ===========================================================================

(defn head []
  [:html [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
          [:link {:rel "microsub" :href (:microsub-uri cfg)}]
          [:link {:rel "stylesheet" :type "text/css" :href "/css/bootstrap.min.css"}]
          [:link {:rel "stylesheet" :type "text/css" :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.5.0/font/bootstrap-icons.css"}]
          [:link {:rel "stylesheet" :type "text/css" :href "/css/style.css"}]
          [:title "Riker Bot Dashboard"]]])

(defn body [session & content]
  [:body
    [:ui.l/navbar-simple {} "Riker Bot Dashboard"]
    [:div.container-fluid [:div.row [:div.col-lg-9 content]]]
    [:div.container-fluid
    [:footer
      [:p
      [:small "Riker Bot v" (get-in cfg [:version :riker]) ", uptime " (uptime-by-unit :days) " days, events handled " (:events-count @state)]]]]
    [:script {:src "//code.jquery.com/jquery.js"}]
    [:script {:src "/js/bootstrap.min.js"}]])

(defn- meme-list-items []
  (let [memes (map str (fs/glob (str images-path "/meme/memes") "**{.*}"))]
    (for [img memes]
      [:li {:class "h-event thread list-group-item"}
       [:img {:src (last (split img #"resources/public/"))}]])))

;;;; UI Views.
;;;; ===========================================================================

(defn home [{:keys [session]}]
  (page session head body
        [:ui.l/card {} "Riker Bot Dashboard"
         [:p "Details on the version, uptime, commands and events handled by Riker Bot."]
         [:p "Commands available:"]
         [:ul
          [:li "hi rikerbot"]
          [:li "uptime days"]
          [:li "uptime minutes"]]]

        [:ui.l/card {} "Latest Creations"
         [:div "There are times where various interfaces (e.g. IRC) are not capable of displaying the content created with them (e.g. images) - this captures the most recent artefacts."
          [:ul.list-group (meme-list-items)]]]))

;;;; Routes, service, Server and app entry point.
;;;; ===========================================================================

(def routes #{["/" :get (conj ses-tors `home)]})

(def service-map {
    ::http/secure-headers    {:content-security-policy-settings {:object-src "none"}}
    ::http/routes            routes
    ::http/type              :jetty
    ::http/resource-path     "public"
    ::http/host              "0.0.0.0"
    ::http/port              (Integer. (or (:port cfg) 5001))
    ::http/container-options {:h2c? true :h2?  false :ssl? false}})

(defn -main [_]
  (info :rikerbot/main (str "starting rikerbot v" (get-in cfg [:version :riker]) "...") )
  (-> service-map http/create-server http/start))
