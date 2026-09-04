(ns site.content
  "Loads the content repo (a folder of markdown files) into an in-memory
  index.

  Files are the source of truth; everything built here is derived and
  rebuildable at any time.

  Layout of the content repo:
    drafts/<name>.md          — unpublished; served only via dev preview
    pages/<slug>.md           — static pages (about, ...)
    attachments/              — pasted images, served at /attachments/
    YYYY/MM/DD/<name>.md      — published entries; date comes from the path

  Frontmatter dialects, detected by the first line:
    ---   YAML — the Obsidian Properties-panel shape. Natural
          property names map onto the entry model: link → :link-url,
          via → :link-via, author → :source, source → :source-url
          (a quote's cite URL; a tool's source code),
          canonical → :canonical-url, previously → :previous-urls.
    ;;;   EDN — the original format, still accepted.
    none  a bare entry publishes as a :post.

  The filename is the human title (a `title` property overrides it); the
  slug is slugified from it unless a slug property pins something else
  (e.g. to preserve an old URL)."
  (:require [clj-yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [site.util :as util])
  (:import [java.time LocalDate ZoneOffset]))

(def edn-delimiter ";;;")
(def yaml-delimiter "---")

(defn- split-frontmatter
  "raw → [meta-str body] when raw starts with the delimiter line, else nil.
  Throws on an opening delimiter that never closes."
  [raw delimiter file]
  (let [lines (str/split-lines raw)]
    (when (= delimiter (str/trim (or (first lines) "")))
      (let [end (->> (map-indexed vector (rest lines))
                     (filter #(= delimiter (str/trim (second %))))
                     ffirst)]
        (when-not end
          (throw (ex-info (str "Unterminated frontmatter (no closing " delimiter "): " file)
                          {:file file})))
        [(str/join "\n" (take end (rest lines)))
         (str/trim (str/join "\n" (drop (+ end 2) lines)))]))))

(defn- read-edn-meta [s file]
  (let [meta (try (edn/read-string s)
                  (catch Exception e
                    (throw (ex-info (str "Invalid EDN frontmatter in " file ": " (ex-message e))
                                    {:file file} e))))]
    (when-not (map? meta)
      (throw (ex-info (str "Frontmatter must be an EDN map: " file)
                      {:file file :meta meta})))
    meta))

(defn- normalize-yaml-meta
  "Obsidian properties → entry keys. Blank/empty properties are treated
  as absent; date and publish are workflow properties, not entry data
  (the path is the date; see workflow-properties)."
  [meta file]
  (when-not (map? meta)
    (throw (ex-info (str "Frontmatter must be a YAML map: " file)
                    {:file file :meta meta})))
  (let [present (into {}
                      (remove (fn [[_ v]]
                                (or (nil? v) (and (string? v) (str/blank? v)))))
                      meta)]
    (-> present
        (set/rename-keys {:link :link-url
                          :via :link-via
                          :author :source
                          :source :source-url
                          :canonical :canonical-url
                          :previously :previous-urls})
        (dissoc :date :publish)
        (cond->
         (:type present) (update :type keyword)
         (:tags present) (update :tags #(if (coll? %) % [%]))
         (:previously present) (update :previous-urls #(if (coll? %) (vec %) [%]))))))

(defn- read-yaml-meta [s file]
  (normalize-yaml-meta
   (try (yaml/parse-string s)
        (catch Exception e
          (throw (ex-info (str "Invalid YAML frontmatter in " file ": " (ex-message e))
                          {:file file} e))))
   file))

(defn parse-frontmatter
  "Splits raw file content into {:meta <map> :body <markdown string>}.
  Detects the frontmatter dialect by the first line; a file with no
  frontmatter at all is an entry with empty metadata."
  [raw file]
  (if-let [[meta-str body] (split-frontmatter raw edn-delimiter file)]
    {:meta (read-edn-meta meta-str file) :body body}
    (if-let [[meta-str body] (split-frontmatter raw yaml-delimiter file)]
      {:meta (read-yaml-meta meta-str file) :body body}
      {:meta {} :body (str/trim raw)})))

(defn- parse-workflow-date
  "Normalizes a raw :date value into a LocalDate. clj-yaml parses an
  unquoted `date: 2026-07-15` into a java.util.Date at UTC midnight — it
  must be read back out in UTC, or the system zone shifts it to the
  previous day. A quoted date, or an Obsidian \"Date & time\" property
  (e.g. \"2026-07-15T09:30\"), arrives as a string instead."
  [v file]
  (cond
    (nil? v) nil

    (instance? java.util.Date v)
    (.. ^java.util.Date v toInstant (atZone ZoneOffset/UTC) toLocalDate)

    (string? v)
    (let [s (str/trim v)]
      (when-not (str/blank? s)
        (let [candidate (if (re-find #"^\d{4}-\d{2}-\d{2}" s) (subs s 0 10) s)]
          (try
            (LocalDate/parse candidate)
            (catch Exception e
              (throw (ex-info (str "Unparseable date property in " file ": " (pr-str v))
                              {:file file :value v} e)))))))

    :else
    (throw (ex-info (str "Unparseable date property in " file ": " (pr-str v))
                    {:file file :value v}))))

(defn workflow-properties
  "Authoring-workflow data ({:date LocalDate-or-nil :publish boolean}) from
  raw YAML frontmatter — the two properties normalize-yaml-meta deliberately
  drops (the path is the date). Throws on an unparseable date so a typo'd
  date fails a publish loudly rather than silently publishing under today."
  [raw file]
  (if-let [[meta-str] (split-frontmatter raw yaml-delimiter file)]
    (let [meta (try (yaml/parse-string meta-str)
                    (catch Exception e
                      (throw (ex-info (str "Invalid YAML frontmatter in " file ": " (ex-message e))
                                      {:file file} e))))]
      {:date (parse-workflow-date (:date meta) file)
       :publish (true? (:publish meta))})
    {:date nil :publish false}))

(defn queued-flag
  "Just the `publish` boolean from raw YAML frontmatter, read leniently —
  false on anything unparseable. Unlike workflow-properties it never
  throws, so a draft with a garbled *date* still reports whether it was
  queued (the date and the publish toggle fail independently)."
  [raw file]
  (boolean
   (try
     (when-let [[meta-str] (split-frontmatter raw yaml-delimiter file)]
       (true? (:publish (yaml/parse-string meta-str))))
     (catch Exception _ false))))

(defn check-type!
  "Resolves :type, defaulting to :post (a bare entry is a post), and
  validates it against the configured :entry-types. Fails loudly so a
  typo can't silently coin a new type."
  [config meta file]
  (let [allowed (set (:entry-types config))
        t (or (:type meta) :post)]
    (when-not (keyword? t)
      (throw (ex-info (str "Non-keyword :type in " file)
                      {:file file :type t})))
    (when-not (allowed t)
      (throw (ex-info (str "Unknown entry type " t " in " file
                           " — allowed: " (pr-str (:entry-types config)))
                      {:file file :type t :allowed allowed})))
    t))

(def entry-path-re
  "Published entries live at YYYY/MM/DD/<name>.md relative to the content root."
  #"(\d{4})/(\d{2})/(\d{2})/([^/]+)\.md")

(defn- rel-path [root file]
  (-> (.toPath (io/file root))
      (.relativize (.toPath (io/file (str file))))
      str
      (str/replace "\\" "/")))

(defn- base-name [file]
  (str/replace (.getName (io/file (str file))) #"\.md$" ""))

(defn- normalize-tags
  "Tags as keywords, each passed through the slug rule: a tag is a URL
  segment (/tags/<tag>), and the admin app slugifies what it writes, so
  reading through the same function keeps a hand-edited file from
  minting a tag URL nothing else on the site would produce. One that
  slugs to nothing is dropped rather than kept as an empty keyword."
  [tags]
  (into #{} (comp (map util/slugify) (remove str/blank?) (map keyword)) tags))

(defn- title-for
  "The title is the filename, for every type; a `title` property overrides
  it (e.g. to title a quote by its point rather than its filename)."
  [meta fname]
  (or (:title meta) fname))

(defn load-entry
  "file → entry map. The date comes from the file's path; the title from
  the filename; the slug is slugified from the filename unless :slug
  overrides it."
  [config root file]
  (let [rp (rel-path root file)
        [_ y m d fname] (re-matches entry-path-re rp)]
    (when-not y
      (throw (ex-info (str "Entry file not under YYYY/MM/DD/: " rp) {:file rp})))
    (let [{:keys [meta body]} (parse-frontmatter (slurp (io/file (str file))) rp)
          type (check-type! config meta rp)
          date {:year (parse-long y) :month (parse-long m) :day (parse-long d)}]
      (when-not (and (<= 1 (:month date) 12) (<= 1 (:day date) 31))
        (throw (ex-info (str "Invalid date in path: " rp) {:file rp :date date})))
      (let [entry (merge (dissoc meta :slug)
                         {:type type
                          :title (title-for meta fname)
                          :slug (or (:slug meta) (util/slugify fname))
                          :file-title fname
                          :date date
                          :tags (normalize-tags (:tags meta))
                          :body body
                          :file rp})]
        (assoc entry :path (util/entry-url entry))))))

(defn load-draft
  "Drafts have no date (that's decided at publish time); they are addressed
  by filename and only served through the dev preview route."
  [config root file]
  (let [rp (rel-path root file)
        name (base-name file)
        {:keys [meta body]} (parse-frontmatter (slurp (io/file (str file))) rp)
        type (check-type! config meta rp)]
    (merge (dissoc meta :slug)
           {:type type
            :title (title-for meta name)
            :slug (or (:slug meta) (util/slugify name))
            :file-title name
            :draft-name name
            :draft? true
            :tags (normalize-tags (:tags meta))
            :body body
            :file rp
            :path (str "/drafts/" name)})))

(defn load-page
  "Static pages: frontmatter is optional; the slug is the filename."
  [root file]
  (let [rp (rel-path root file)
        slug (base-name file)
        {:keys [meta body]} (parse-frontmatter (slurp (io/file (str file))) rp)]
    {:slug slug
     :title (or (:title meta) (str/capitalize slug))
     :body body
     :path (str "/" slug)}))

(defn- md-files [dir]
  (let [d (io/file dir)]
    (when (.isDirectory d)
      (->> (file-seq d)
           (filter #(and (.isFile %) (str/ends-with? (.getName %) ".md")))))))

(defn load-content
  "Walks the content repo → {:entries [...] :drafts {name entry} :pages {slug page}}.
  Markdown files that are neither dated entries, drafts, nor pages are ignored
  (so stray files can't break the site)."
  [config]
  (let [root (:content-path config)
        classified (group-by (fn [f]
                               (let [rp (rel-path root f)]
                                 (cond
                                   (re-matches entry-path-re rp) :entry
                                   (str/starts-with? rp "drafts/") :draft
                                   (str/starts-with? rp "pages/") :page
                                   :else :ignored)))
                             (md-files root))]
    {:entries (mapv #(load-entry config root %) (:entry classified))
     :drafts (into {} (map (fn [f]
                             (let [d (load-draft config root f)]
                               [(:draft-name d) d])))
                   (:draft classified))
     :pages (into {} (map (fn [f]
                            (let [p (load-page root f)]
                              [(:slug p) p])))
                  (:page classified))}))

(defn- date-key [{:keys [date slug]}]
  [(:year date) (:month date) (:day date) slug])

(defn- link-neighbors
  "Adds :newer/:older pointers (path + title + type + date) to each entry.
  Entries are already sorted newest-first."
  [entries]
  (let [brief #(select-keys % [:path :title :type :date])]
    (vec (map-indexed
          (fn [i e]
            (cond-> e
              (pos? i) (assoc :newer (brief (entries (dec i))))
              (< i (dec (count entries))) (assoc :older (brief (entries (inc i))))))
          entries))))

(defn wikilink-targets
  "{lowercased filename → entry path} for every published entry — what
  [[wikilinks]] resolve against. Attached to each entry so rendering can
  resolve links wherever the entry travels."
  [entries]
  (into {} (map (fn [e] [(str/lower-case (:file-title e)) (:path e)])) entries))

(def empty-index
  "What the server serves when content is unavailable at boot — the sync
  loop replaces it as soon as a pull succeeds."
  {:entries [] :by-path {} :by-type {} :nav-types [] :by-year {} :by-month {}
   :by-day {} :by-tag {} :tag-counts [] :months [] :drafts {} :pages {}
   :redirects {}})

(defn local-previous-path
  "A previous URL reduced to a path on this site: a bare path passes
  through, an absolute URL on the site's own host loses its scheme and
  host, and a URL on any other host is nil — that redirect must be
  served at the old host (see `bb redirects`). Trailing slashes drop so
  redirect lookups can be exact."
  [base-url url]
  (let [path (cond
               (str/starts-with? url "/") url
               (and base-url (= (util/host base-url) (util/host url)))
               (str/replace url #"^\w+://[^/]+" "")
               :else nil)]
    (some-> path (str/replace #"/+$" ""))))

(defn- build-redirects
  "The entries' :previous-urls as {old site-local path → live entry path}
  — what the server 301s when a request would otherwise 404. Foreign-host
  previous URLs are left out (they redirect at their own edge). Fails
  loudly on a previous URL that is blank, is already a live entry URL,
  or is claimed by two different entries."
  [config entries]
  (let [live (into #{} (map :path) entries)]
    (reduce
     (fn [acc {:keys [path previous-urls file]}]
       (reduce
        (fn [acc url]
          (let [src (local-previous-path (:base-url config) url)]
            (cond
              (nil? src) acc
              (str/blank? src)
              (throw (ex-info (str "Previous URL has no path in " file ": " (pr-str url))
                              {:file file :url url}))
              (live src)
              (throw (ex-info (str "Previous URL " src " in " file " is a live entry URL")
                              {:file file :url src}))
              (and (contains? acc src) (not= (get acc src) path))
              (throw (ex-info (str "Previous URL " src " claimed by both "
                                   (get acc src) " and " path)
                              {:url src :paths [(get acc src) path]}))
              :else (assoc acc src path))))
        acc
        previous-urls))
     {}
     entries)))

(defn build-index
  "Content repo → the in-memory index every request reads from."
  [config]
  (let [{:keys [entries drafts pages]} (load-content config)
        entries (vec (sort-by date-key #(compare %2 %1) entries))
        dupes (->> entries (map :path) frequencies (filter #(> (val %) 1)) (map key))
        _ (when (seq dupes)
            (throw (ex-info (str "Duplicate entry paths: " (str/join ", " dupes))
                            {:paths (vec dupes)})))
        wikilinks (wikilink-targets entries)
        entries (mapv #(assoc % :wikilinks wikilinks) (link-neighbors entries))
        drafts (update-vals drafts #(assoc % :wikilinks wikilinks))
        by-type (group-by :type entries)]
    {:entries entries
     :by-path (into {} (map (juxt :path identity)) entries)
     :by-type by-type
     ;; entry types with at least one published entry, in configured
     ;; order — what the nav links (an empty type's listing is a 404)
     :nav-types (filterv by-type (:entry-types config))
     :by-year (group-by #(-> % :date :year) entries)
     :by-month (group-by (fn [e] [(-> e :date :year) (-> e :date :month)]) entries)
     ;; months that have content, newest first — drives month-to-month nav
     :months (vec (sort #(compare %2 %1)
                        (distinct (map (fn [e] [(-> e :date :year) (-> e :date :month)])
                                       entries))))
     :by-day (group-by (fn [e] [(-> e :date :year) (-> e :date :month) (-> e :date :day)]) entries)
     :by-tag (reduce (fn [acc e]
                       (reduce (fn [acc t] (update acc t (fnil conj []) e))
                               acc
                               (:tags e)))
                     {}
                     entries)
     :tag-counts (->> entries
                      (mapcat :tags)
                      frequencies
                      (sort-by (fn [[t n]] [(- n) (name t)]))
                      vec)
     ;; old site-local URLs (the entries' :previous-urls) → live paths;
     ;; served as 301s where a request would otherwise 404
     :redirects (build-redirects config entries)
     :drafts drafts
     :pages pages}))
