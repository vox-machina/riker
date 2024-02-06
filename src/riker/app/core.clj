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
            [omnom.generators.file :refer [pedestal-log->events]]
            [ui.layout :refer [page ses-tor]]
            [ui.components])
  (:import (java.util UUID)))

;;;; Config and constants.
;;;; ===========================================================================

(alter-var-root #'org.httpkit.client/*default-client* (fn [_] sni-client/default-client))
(def config (read-config "config.edn" {}))

(def start-inst (jt/instant))

(def data-path "resources/public/data")
(def images-path "resources/public/data/images")
(def state (e/file-atom {:events-count 0} "riker-state.clj"))

;;;; Utility functions.
;;;; ===========================================================================
(defn- uptime-by-unit [unit] (jt/as (jt/duration start-inst (jt/instant)) unit))

(defn picard-events []
  (let [today (jt/local-date)
        yesterday (jt/minus today (jt/days 1))
        t-log (pedestal-log->events (str "logs/riker-" today ".0.log") "riker.app.core" "riker.app.core - ")
        y-log (pedestal-log->events (str "logs/riker-" yesterday ".0.log") "riker.app.core" "riker.app.core - ")
        evts-filter #{:bookmark/log :discovery/log :personal/log :professional/log :digital-purchase/log :physical-purchase/log :subscription-purchase/log :film-watch/log :series-watch/log :watchlist-watch/log :audio-list/log :book-read/log}
        events (concat y-log t-log)] 
    (filter #(some #{(first (keys (:log %)))} evts-filter) events)))

;;;; Service interceptors
;;;; ===========================================================================
(def cfg-tor {:name :cfg-tor :enter (fn [context] (assoc-in context [:request :cfg] config))})

;(def common-tors [ses-tors cfg-tor])

;;;; UI Components.
;;;; ===========================================================================

(defn head [{:keys [cfg] :as req}]
  [:html [:head
          [:meta {:charset "utf-8"}]
          [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
          [:link {:rel "microsub" :href (:microsub-uri cfg)}]
          [:link {:rel "stylesheet" :type "text/css" :href "/css/bootstrap.min.css"}]
          [:link {:rel "stylesheet" :type "text/css" :href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.5.0/font/bootstrap-icons.css"}]
          [:link {:rel "stylesheet" :type "text/css" :href "/css/style.css"}]
          [:title "Riker Bot Dashboard"]]])

(defn body [{:keys [cfg session] :as req} & content]
  [:body
   [:ui.l/navbar-simple {} "Riker Dashboard"]
    [:div.container-fluid [:div.row [:div.col-lg-9 content]]]
    [:div.container-fluid
    [:footer
      [:p
      [:small "Riker Bot v" (get-in cfg [:version :riker]) ", uptime " (uptime-by-unit :days) " days, events handled " (:events-count @state)]]]]
    [:script {:src "//code.jquery.com/jquery.js"}]
    [:script {:src "/js/bootstrap.min.js"}]])

(defn form [form-name input-name input-placeholder input-label]
  [:form {:action "/log" :method "post" :name form-name}
   [:label {:for input-name} input-label]
   [:textarea {:name input-name :id input-name :placeholder input-placeholder :rows "3" :cols "80"}] 
   [:input {:name "tags" :placeholder "tag1,tag2"}] 
   [:button {:type "submit"} "post"]])

(defn btn
  "Create a button link which points to a Picard log form configured for the correct event.
   We re-use link text as the input label in the Picard form."
  [uri text input-name input-sample]
  [:ui.c/btn-link {:uri (str "/picard?input-name=" input-name "&input-label=" text "&input-sample=" input-sample) :text text}])

(defn events-table [{{:keys [:events-limit]} :cfg}]
  [:ui.l/card {} "Latest Picard log entries"
   [:table.table.table-striped
    [:thead
     [:tr
      [:td {:scope "col"} "Event"]
      [:td {:scope "col"} "Tags"]
      [:td {:scope "col"} "Date-time"]]]
    [:tbody 
     (for [{:keys [instant ns log]} (take events-limit (reverse (picard-events))) :let [log-m (first (vals log))]]
       [:tr
        [:td (first (vals (:data log-m)))]
        [:td [:ul (for [y (get-in log-m [:data :tags])] [:li y])]]
        [:td instant]])]]])

;;;; UI Views.
;;;; ===========================================================================

(defn home [{:keys [cfg session] :as req}]
  (page req head body
        [:ui.l/card {} "Picard log"
         [:p "The Picard log allows you to log a number of events in your life conveniently."]
         [:div.container
          [:div.row
           [:div.col (btn "/picard" "Personal" "personal" "something to remember")]
           [:div.col (btn "/picard" "Professional" "professional" "something to remember")]]
          [:div.row
           [:div.col (btn "/picard" "Bookmark" "bookmark" "bookmark uri")]
           [:div.col (btn "/picard" "Discovery" "discovery" "something I discovered")]]
          [:div.row
           [:div.col (btn "/picard" "Digital purchase" "digital-purchase" "a digital purchase e.g. ebook")]
           [:div.col (btn "/picard" "Physical purchase" "physical-purchase" "a physical purchase e.g. device")]]
          [:div.row
           [:div.col (btn "/picard" "Subscription purchase" "subscription-purchase" "a subscription purchase e.g. emagazine or software licence")]
           [:div.col (btn "/picard" "Film watch" "film-watch" "a film I watched")]]
          [:div.row
           [:div.col (btn "/picard" "Series watch" "series-watch" "a show I watched")]
           [:div.col (btn "/picard" "Watchlist watch" "watchlist-watch" "a video, youtube or otherwise")]]
          [:div.row
           [:div.col (btn "/picard" "Audiolist listen" "audiolist-listen" "a podcast or audio file")]
           [:div.col (btn "/picard" "Book read" "book-read" "a book or ebook")]]]]
        
        (events-table req)))

(defn picard [{:keys [cfg query-params session] :as req}]
  (page req head body
        [:ui.l/card {} "Picard log"
         (form "log" (:input-name query-params) (:input-sample query-params) (:input-label query-params))]))

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

(def routes #{["/"       :get  (conj ses-tor `cfg-tor `home)]
              ["/picard" :get (conj ses-tor `cfg-tor `picard)]
              ["/log"    :post (conj ses-tor `log)]})

(defn service-map [{:keys [port]}]
  {::http/secure-headers    {:content-security-policy-settings {:object-src "none"}}
   ::http/routes            routes
   ::http/type              :jetty
   ::http/resource-path     "public"
   ::http/host              "0.0.0.0"
   ::http/port              (Integer. (or port 5001))
   ::http/container-options {:h2c? true :h2?  false :ssl? false}})

(defn -main [_]
  (info :main/start (str "starting riker bot v" (get-in config [:version :riker]) "..."))
  (-> (service-map config) http/create-server http/start))
