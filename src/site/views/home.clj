(ns site.views.home
  "The home feed: recent entries grouped by day, whole days only, at
  least :home-entries items. It is a front page, not page one of the
  stream — the month, type and tag listings are the stream — so when
  more exists it ends with a single link into the archive index rather
  than continuing. No infinite scroll, no page numbers."
  (:require [site.util :as util]
            [site.views.components :as c]
            [site.views.layout :as layout]))

(def default-home-entries 10)

(defn- take-whole-days
  "The newest entries, accumulated a day at a time until at least n are
  included — a day is never split."
  [entries n]
  (loop [groups (partition-by util/day-key entries)
         shown []]
    (if (or (empty? groups) (>= (count shown) n))
      shown
      (recur (rest groups) (into shown (first groups))))))

;; Deliberately the archive index, not the month the feed stopped in: that
;; month's first page opens with the very entries the reader just scrolled
;; past, because the feed cuts on whole days and month pages cut every
;; :page-entries, and the two only coincide at a month boundary.
(def ^:private archive-link
  [:a.feed-more {:href "/archive"} "More in the archive →"])

(defn home [config index]
  (let [n (or (:home-entries config) default-home-entries)
        entries (:entries index)
        shown (take-whole-days entries n)]
    (layout/page config {:path "/"}
                 (c/cols (list (c/feed shown)
                               (when (< (count shown) (count entries)) archive-link))
                         (c/sidebar config {:follow :inline}
                                    (c/recent-links entries 10)
                                    (c/follow-widget config)
                                    (c/top-tags (:tag-counts index) 5))))))
