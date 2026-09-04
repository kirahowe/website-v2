(ns site.views.entry
  "The single-entry page: a full reading article, then a footer of tag
  chips, a dense related grid, and a short bio. Quotes render as a large
  blockquote; outbound types (links, releases, tools) title-link to source."
  (:require [clojure.string :as str]
            [site.markdown :as markdown]
            [site.util :as util]
            [site.views.components :as c]
            [site.views.layout :as layout]))

(def ^:private default-bio
  "Kira Howe is a software engineer writing about building with care in the age of AI.")

(defn- first-name [config]
  (first (str/split (:site-title config) #"\s+")))

(defn- article-head [entry]
  (list
   (when-let [title (:title entry)]
     [:h1 (if (:link-url entry)
            [:a {:href (:link-url entry)} title]
            title)
      (c/title-credits entry)])
   [:div.article-meta
    (util/format-date (:date entry))
    (cond
      (:link-url entry)
      (list [:span.sep "/"] (util/host (:link-url entry)))
      (= :post (:type entry))
      (list [:span.sep "/"] (str (markdown/read-time (:body entry)) " min read")))
    (when-let [note (c/canonical-note entry)]
      (list [:span.sep "/"] note))]))

(defn- article-body [entry]
  (if (= :quote (:type entry))
    (list
     (c/quote-blockquote entry)
     (c/quote-source entry))
    (rest (markdown/render (:body entry) (:wikilinks entry) {:anchors? true}))))

(defn- post-footer [config index entry]
  (let [rel (c/related (:entries index) entry 3)]
    [:div.post-footer
     (when (seq (:tags entry))
       [:section.section
        [:div.tag-chips (c/tag-links entry)]])
     (when (seq rel)
       [:section.section
        [:h2 "Related"]
        (c/entry-list rel)])
     [:section.section
      [:p.bio (or (:bio config) default-bio)]
      [:a.more {:href "/about"} (str "More about " (first-name config) " →")]]
     (c/follow-widget config
                      {:variant :entry
                       :form-id "follow-post"})]))

(defn entry-page [config index entry]
  (layout/page config {:title (or (:title entry) (util/format-date (:date entry)))
                       :path (:path entry)
                       :canonical (:canonical-url entry)}
               [:article.article.prose
                (article-head entry)
                (article-body entry)]
               (post-footer config index entry)))

(defn draft-page [config entry]
  (layout/page config {:title (str "Draft: " (or (:title entry) (:draft-name entry)))}
               [:div.draft-banner "Draft — dev preview, not published."]
               [:article.article.prose
                (when (:title entry) [:h1 (:title entry)])
                (when (seq (:tags entry))
                  [:div.tag-chips (c/tag-links entry)])
                (article-body entry)]))
