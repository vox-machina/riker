(ns riker.app.core
  (:require [clojure.pprint :as pp]
            [clojure.string :refer [join split trim]]
            [aero.core :refer (read-config)]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :refer [body-params]]
            [io.pedestal.http.ring-middlewares :as mw]
            [io.pedestal.interceptor :as intc]
            [io.pedestal.log :refer [debug info error]]
            [ring.util.response :refer [redirect]]
            [org.httpkit.client :as client]
            [org.httpkit.sni-client :as sni-client]
            [ten-d-c.hiccup-server-components.core :refer [->html]]
            [java-time.api :as jt]
            [babashka.fs :as fs]
            [alandipert.enduro :as e]
            [clj-meme.core :refer [generate-image!]]
            [omnom.generators.file :refer [pedestal-log->events]]
            [ui.layout :refer [page ses-tors]]
            [ui.components])
  (:import (java.util UUID)))

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

(defn picard-events []
  (let [evts-filter #{:bookmark/log :discovery/log :personal/log :professional/log}
        events (pedestal-log->events "logs/my.log" "riker.app.core" "riker.app.core - ")] 
    (filter #(some #{(first (keys (:log %)))} evts-filter) events)))

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

;;;; UI Components.
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
    [:ui.l/navbar-simple {} "Riker"]
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

(defn form [form-name input-name input-placeholder input-label]
  [:form {:action "/log" :method "post" :name form-name}
   [:div.row
    [:div.col [:label {:for input-name} input-label]]
    [:div.col [:input {:name input-name :id input-name :placeholder input-placeholder}]] 
    [:div.col [:input {:name "tags" :placeholder "tag1,tag2"}]] 
    [:div.col [:button {:type "submit"} "post"]]]])

;;;; UI Views.
;;;; ===========================================================================

(defn home [{:keys [session]}]
  (page session head body
        [:ui.l/card {} "Dashboard"
         [:p "Details on the version, uptime, commands and events handled by Riker Bot."]
         (form "logPersonal" "personal" "something to remember" "Personal log")
         (form "logProfessional" "professional" "something to remember" "Professional log")
         (form "logBookmark" "bookmark" "bookmark uri" "Bookmark log")
         (form "logDiscoveries" "discovery" "something I discovered" "Discovery log")
         (form "logDigitalPurchase" "digital-purchase" "a digital purchase e.g. ebook" "Digital purchase log")
         (form "logPhysicalPurchase" "physical-purchase" "a physical purchase e.g. device" "Physical purchase log")
         (form "logSubscriptionPurchase" "subscription-purchase" "a subscription purchase e.g. emagazine or software licence" "Subscription purchase log")
         (form "logFilmWatch" "film-watch" "a film I watched" "Film watch log")
         (form "logSeriesWatch" "series-watch" "a series episode I watched" "Series watch log")
         [:p "Commands available:"]
         [:ul
          [:li "hi rikerbot"]
          [:li "uptime days"]
          [:li "uptime minutes"]]]

        [:ui.l/card {} "Latest Picard log entries"
         [:table.table.table-striped
          [:thead
           [:tr
            [:td {:scope "col"} "Event"]
            [:td {:scope "col"} "Tags"]
            [:td {:scope "col"} "Date-time"]]]
          [:tbody 
           (for [x (picard-events)]
             [:tr [:td (first (vals (:data (first (vals (:log x))))))] [:td "tags"] [:td (:instant x)]])]]]

        [:ui.l/card {} "Latest Creations"
         [:div "There are times where various interfaces (e.g. IRC) are not capable of displaying the content created with them (e.g. images) - this captures the most recent artefacts."
          [:ul.list-group (meme-list-items)]]]))

;;;; API.
;;;; ===========================================================================

(defn log [{:keys [form-params] :as req}]
  (let [entry (dissoc form-params :tags)
        post-type (name (first (keys entry)))
        log-type (keyword (str post-type "/log"))
        log-tag (keyword (str post-type "-log"))
        tag-str (:tags form-params)
        tags-split (.split tag-str ",")
        payload (assoc entry :tags (conj (into #{} (map #(keyword %) tags-split)) log-tag))]
    (info log-type {:data payload})
    (-> (redirect "/"))))

;;;; Routes, service, Server and app entry point.
;;;; ===========================================================================

(def routes #{["/"    :get  (conj ses-tors `home)]
              ["/log" :post (conj ses-tors `log)]})

(def service-map {
    ::http/secure-headers    {:content-security-policy-settings {:object-src "none"}}
    ::http/routes            routes
    ::http/type              :jetty
    ::http/resource-path     "public"
    ::http/host              "0.0.0.0"
    ::http/port              (Integer. (or (:port cfg) 5001))
    ::http/container-options {:h2c? true :h2?  false :ssl? false}})

(defn -main [_]
  (info :main/start (str "starting riker bot v" (get-in cfg [:version :riker]) "..."))
  (-> service-map http/create-server http/start))
