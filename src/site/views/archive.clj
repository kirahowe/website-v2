(ns site.views.archive
  "Archive listings: year (a month index), month (a day-grouped feed with a
  calendar), day, plus type and tag listings and the tag index. The feed
  pages reuse the home-page shell; the year page is its own dense index."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]
            [site.util :as util]
            [site.views.components :as c]
            [site.views.layout :as layout])
  (:import [java.time LocalDate YearMonth]))

;; --- year archive: a month index with per-type counts --------------------

(defn- month-index [year entries]
  [:div.month-index
   (for [[month es] (->> entries
                         (group-by #(-> % :date :month))
                         (sort-by key #(compare %2 %1)))
         :let [slug (util/month-slug month)
               es (sort-by util/day-key #(compare %2 %1) es)]]
     [:section.month-block
      [:div.month-head
       [:a.month-name {:href (str "/" year "/" slug)} (util/month-name month)]
       [:span.month-count (c/type-summary es false)]]
      (for [e es]
        (c/index-row {:href (:path e)
                      :date (util/ordinal (-> e :date :day))
                      :type (:type e)
                      :title (c/entry-label e)}))])])

(defn- year-nav
  "The Years side list — one link per year in `years` (newest first), the
  `current` year marked. Callers scope `years` and supply `href` (year →
  URL): the archive lists every year and links to the whole year, a type
  listing lists only the years holding that type and stays scoped to it."
  [years current href]
  (c/side-section "Years"
                  [:div.year-nav
                   (for [y (sort > years)]
                     [:a {:class (when (= y current) "current") :href (href y)} y])]))

(defn- facet-tags
  "The Tags side facet for a listing. `entries` are the listing's already
  filtered rows; `href` maps a tag to a URL that adds it to the current
  view, so a tag click refines in place rather than leaving for its global
  page. `active` (the tag applied now, if any) is dropped from the cloud —
  it's cleared from the header instead. Counts always reflect what's on
  screen."
  [entries active href]
  (let [counts (->> entries (mapcat :tags) (remove #{active}) frequencies
                    (sort-by (fn [[t n]] [(- n) (name t)])))]
    (when (seq counts)
      (c/side-section "Tags" (c/tag-cloud (take 8 counts) href)))))

(defn- filter-chip
  "An applied facet in a listing header — a year or a #tag — shown small and
  muted next to the title, with an × that clears just that facet back to
  `clear-href` (leaving any other facet in place)."
  [label clear-href]
  [:span.filter label
   [:a.filter-x {:href clear-href :title "Clear filter"
                 :aria-label (str "Clear " label " filter")} "×"]])

(defn- header-parts
  "The <h1> parts of a listing header: the base `label` then each facet chip
  in `chips`, every chip led by a slash in the chip's own muted formatting.
  The count and its slash are appended by `page-header`, so the header reads
  as one slashed row — `Links / #tag × / 8 entries` — like the metadata
  layer, each slash matching the item that follows it."
  [label chips]
  (cons label (mapcat (fn [chip] [[:span.sep.filter "/"] chip]) (remove nil? chips))))

(defn year-page [config index year tag entries]
  (let [heading (header-parts (str year)
                              [(when tag (filter-chip (str "#" (name tag)) (str "/" year)))])
        ;; With a tag applied, scope the Years list and the month index to
        ;; the years/rows that actually hold it — no dead links, no rows
        ;; that don't match — and carry the tag as we jump between years.
        years (if tag
                (distinct (map #(-> % :date :year) (get (:by-tag index) tag)))
                (keys (:by-year index)))
        year-href (fn [y] (str "/" y (when tag (str "?tag=" (name tag)))))
        tag-href (fn [t] (str "/" year "?tag=" (name t)))]
    (layout/page config {:title (cond-> (str year) tag (str " / #" (name tag)))
                         :path (str "/" year)}
                 (c/page-header heading (c/count-label (count entries)))
                 (c/cols (month-index year entries)
                         (c/sidebar config
                                    (year-nav years year year-href)
                                    (facet-tags entries tag tag-href))))))

;; --- month page: the feed + a mini calendar ------------------------------

(defn- calendar
  "A Sunday-first month grid; days that have entries link to their archive,
  the rest are dimmed."
  [index year month]
  (let [ndays (.lengthOfMonth (YearMonth/of year month))
        start (mod (.getValue (.getDayOfWeek (LocalDate/of year month 1))) 7) ; Sun=0
        active (into #{}
                     (comp (filter (fn [[y m _]] (and (= y year) (= m month))))
                           (map (fn [[_ _ d]] d)))
                     (keys (:by-day index)))
        slug (util/month-slug month)
        total (+ start ndays)
        cells (concat (repeat start nil)
                      (range 1 (inc ndays))
                      (repeat (mod (- 7 (mod total 7)) 7) nil))]
    [:section.side
     [:div.calendar-head
      [:a {:href (str "/" year) :aria-label (str "All of " year)} year]
      [:span {:aria-hidden "true"} "»"]
      [:span (util/month-name month)]]
     [:table.calendar
      [:thead [:tr (for [d ["S" "M" "T" "W" "T" "F" "S"]] [:th d])]]
      [:tbody
       (for [week (partition 7 cells)]
         [:tr
          (for [d week]
            (cond
              (nil? d) [:td]
              (active d) [:td.on [:a {:href (str "/" year "/" slug "/" d)
                                      :aria-label (str (util/month-name month) " " d ", " year)} d]]
              :else [:td d]))])]]]))

(defn- nearby [newer older]
  (when (or newer older)
    (c/side-section "Nearby"
                    [:div.year-nav
                     (when newer [:a {:href (util/month-url newer)} (util/month-label newer)])
                     (when older [:a {:href (util/month-url older)} (util/month-label older)])])))

(defn- month-path [year month page]
  (str (util/month-url [year month]) (when (< 1 page) (str "/page/" page))))

(defn month-page [config index year month all-entries {:keys [entries page pages total]}]
  (let [months (:months index)                       ; newest first
        i (.indexOf months [year month])
        newer (when (pos? i) (nth months (dec i)))
        older (when (< (inc i) (count months)) (nth months (inc i)))
        label (str (util/month-name month) " " year)]
    (layout/page config {:title (cond-> label (< 1 page) (str " / Page " page))
                         :path (month-path year month page)}
                 (c/page-header label (c/count-label total))
                 [:div.type-summary (c/type-summary all-entries true)]
                 (c/cols (list (c/feed entries)
                               (c/pagination page pages #(month-path year month %)))
                         (c/sidebar config
                                    (calendar index year month)
                                    (nearby newer older))))))

(defn- day-path [year month day page]
  (str (util/day-url {:year year :month month :day day}) (when (< 1 page) (str "/page/" page))))

(defn day-page [config year month day {:keys [entries page pages]}]
  (let [label (util/format-date {:year year :month month :day day})]
    (layout/page config {:title (cond-> label (< 1 page) (str " / Page " page))
                         :path (day-path year month day page)}
                 (c/feed entries)
                 (c/pagination page pages #(day-path year month day %)))))

;; --- type and tag listings ----------------------------------------------

(defn type-page [config index type year tag all-entries {:keys [entries page pages total]}]
  (let [slug (str (name type) "s")
        ;; The listing's canonical URL for any (year, tag) selection — the
        ;; one place the path shape lives, so chips, the × clears, the Years
        ;; nav and the tag facet all stay consistent.
        path (fn [y t p]
               (str (if y (str "/" y "/" slug) (str "/" slug))
                    (when (< 1 p) (str "/page/" p))
                    (when t (str "?tag=" (name t)))))
        label (str/capitalize slug)
        ;; Year and tag each render as their own muted chip; each × clears
        ;; only itself, leaving the other facet applied.
        heading (header-parts label
                              [(when year (filter-chip (str year) (path nil tag 1)))
                               (when tag (filter-chip (str "#" (name tag)) (path year nil 1)))])
        ;; Facet the Years list to this type — every year that holds one,
        ;; not the global year set — regardless of the year filter in view,
        ;; and narrowed further to the applied tag so no year link dead-ends.
        scope (cond->> (get (:by-type index) type)
                tag (filter #(contains? (set (:tags %)) tag)))
        years (distinct (map #(-> % :date :year) scope))
        feed {:kind :type :value type}]
    (layout/page config {:title (cond-> label
                                 year (str " / " year)
                                 tag (str " / #" (name tag))
                                 (< 1 page) (str " / Page " page))
                         ;; Every numbered page has its own canonical URL;
                         ;; the query-free form remains canonical when a tag
                         ;; facet is applied.
                         :path (path year nil page)
                         :feed feed}
                 (c/page-header heading (c/count-label total))
                 (c/cols (list (c/feed entries)
                               (c/pagination page pages #(path year tag %)))
                         (c/sidebar config {:feed feed}
                                    (year-nav years year #(path % tag 1))
                                    (facet-tags all-entries tag #(path year % 1)))))))

(defn- related-tags [entries tag]
  (let [counts (->> entries (mapcat :tags) (remove #{tag}) frequencies
                    (sort-by (fn [[t n]] [(- n) (name t)])))]
    (when (seq counts)
      (c/side-section "Related tags" (c/tag-cloud (take 6 counts))))))

(defn- tag-path [tag year page]
  (str (util/tag-url tag) (when year (str "/" year)) (when (< 1 page) (str "/page/" page))))

(defn tag-page [config tag year all-entries {:keys [entries page pages total]}]
  (let [heading (header-parts (list [:span.hash "#"] (name tag))
                              [(when year (filter-chip (str year) (tag-path tag nil 1)))])
        feed {:kind :tag :value tag}]
    (layout/page config {:title (cond-> (str "#" (name tag) (when year (str " / " year)))
                                 (< 1 page) (str " / Page " page))
                         :path (tag-path tag year page)
                         :feed feed}
                 (c/page-header heading (c/count-label total))
                 (c/cols (list (c/feed entries)
                               (c/pagination page pages #(tag-path tag year %)))
                         (c/sidebar config {:feed feed}
                                    (related-tags all-entries tag))))))

;; Live filtering for the tag index: hides tags whose name doesn't contain
;; the query. Matching is against the link's own text node ("#clojure"), so
;; a digit in the query can't match the counts. The bar and the × ship
;; hidden and the script un-hides them — without JS they'd be dead
;; controls, and everything else on the page works fine without it.
(def ^:private tag-filter-script
  (str "(function(){var bar=document.querySelector('.tag-filter');"
       "var input=bar.querySelector('input');"
       "var clear=bar.querySelector('.field-clear');"
       "var tags=document.querySelectorAll('.tag-index .tag');"
       "var empty=document.querySelector('.tag-filter-empty');"
       "function apply(){var q=input.value.trim().toLowerCase();var shown=0;"
       "tags.forEach(function(a){"
       "var hide=q!==''&&a.firstChild.textContent.toLowerCase().indexOf(q)<0;"
       "a.hidden=hide;if(!hide)shown++;});"
       "clear.hidden=q==='';empty.hidden=shown>0;}"
       "input.addEventListener('input',apply);"
       "clear.addEventListener('click',function(){input.value='';apply();input.focus();});"
       "bar.hidden=false;})();"))

(defn- tag-filter
  "The tag index's filter bar — the shared field+button pair, as a div: it
  filters live and never leaves the page, so it submits nothing and shows no
  button. The clear × is a button the JS resets, not a link."
  []
  (c/field-form
   {:tag :div
    :attrs {:class "tag-filter" :hidden true}
    :field {:type "search" :placeholder "Filter tags…"
            :autocomplete "off" :aria-label "Filter tags"}
    :aside [:button.field-clear {:type "button" :hidden true :aria-label "Clear filter"} "×"]}))

(defn tags-page [config index]
  (let [tags (:tag-counts index)]
    (layout/page config {:title "Tags" :path "/tags"}
                 (c/page-header "Tags" (str (count tags) " tags"))
                 (if (seq tags)
                   (list (tag-filter)
                         [:div.tag-index (c/tag-cloud tags)]
                         [:p.empty.tag-filter-empty {:hidden true} "No tags match."]
                         [:script (h/raw tag-filter-script)])
                   [:p.empty "No tags yet."]))))
