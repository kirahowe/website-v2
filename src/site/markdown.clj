(ns site.markdown
  "The only namespace that touches the markdown library, so rendering
  concerns stay in one place. Obsidian's dialect is handled here:
  [[wikilinks]] resolve against the entry index at the AST level (so
  code blocks stay literal), and ![[image embeds]] become /attachments/
  images. Raw HTML passes through verbatim (per CommonMark), and $ is
  never a formula delimiter — the site has no math rendering, so $
  always means money."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]
            [nextjournal.markdown :as md]
            [nextjournal.markdown.utils :as md.utils]
            [site.util :as util]))

(def ^:private parse-ctx
  (-> md.utils/empty-doc
      (assoc :disable-inline-formulas true)
      (update :text-tokenizers conj md.utils/internal-link-tokenizer)))

(def ^:private image-extensions
  #{"png" "jpg" "jpeg" "gif" "webp" "svg"})

(defn- attachment-url [file-name]
  (str "/attachments/" (str/replace (str/trim file-name) " " "%20")))

(defn- embeds->images
  "Obsidian image embeds → standard markdown images served from
  /attachments/: ![[img.png]] and ![[img.png|alt text]]."
  [line]
  (str/replace line #"!\[\[([^\]|]+?)(?:\|([^\]]*))?\]\]"
               (fn [[whole target alt]]
                 (let [ext (str/lower-case (or (peek (str/split target #"\.")) ""))]
                   (if (image-extensions ext)
                     (str "![" (or alt "") "](" (attachment-url target) ")")
                     whole)))))

(defn- absolutize-attachments
  "Relative attachments/ refs (Obsidian's markdown-style links) → the
  served path, which works from any page depth."
  [line]
  (str/replace line "](attachments/" "](/attachments/"))

(defn- fence-line? [line]
  (boolean (re-matches #"\s*(```|~~~).*" line)))

(defn- preprocess
  "Line-level Obsidian-isms that must become standard markdown before
  parsing; fenced code blocks pass through untouched."
  [s]
  (->> (reduce (fn [[out in-fence?] line]
                 (cond
                   (fence-line? line) [(conj out line) (not in-fence?)]
                   in-fence? [(conj out line) true]
                   :else [(conj out (-> line embeds->images absolutize-attachments))
                          false]))
               [[] false]
               (str/split-lines (str s)))
       first
       (str/join "\n")))

(defn- wikilink-renderer
  "[[Name]] / [[Name|label]] → an anchor when the name resolves to an
  entry, a plain (classed) span when it doesn't — an unpublished target
  must never leak a dead link."
  [resolve-target]
  (fn [_ctx {:keys [text]}]
    (let [[target label] (str/split (str text) #"\|" 2)
          label (str/trim (or label target))]
      (if-let [url (resolve-target target)]
        [:a.internal {:href url} label]
        [:span.unresolved-link label]))))

(defn- raw-html
  "Raw HTML in the source passes through unescaped; the author's own
  content is the only input."
  [_ctx node]
  (h/raw (md/node->text node)))

(defn- strip-tags [s]
  (str/replace (str s) #"<[^>]*>" ""))

(defn- heading-renderer
  "Headings get an id — the site's slug of their text, the rule entry
  URLs follow, so a section's fragment is as clean as its page's path —
  made unique within one render (setup, setup-2, …). The library's own
  ids kept punctuation (`why?`, quotes) and leaked raw HTML tags. With
  `anchors?` each heading also ends in a link to itself, the handle a
  reader copies to point at a section; views rendering a body on its
  own page ask for it, feed rows and the Atom feed don't, since their
  HTML lands in other documents where a fragment link is noise."
  [anchors?]
  (let [seen (atom {})]
    (fn [ctx node]
      (let [slug (util/slugify (strip-tags (md/node->text node)))
            base (if (str/blank? slug) "section" slug)
            n (get (swap! seen update base (fnil inc 0)) base)
            id (if (= 1 n) base (str base "-" n))
            heading ((:heading md/default-hiccup-renderers) ctx (assoc-in node [:attrs :id] id))]
        (cond-> heading
          anchors? (conj [:a.anchor {:href (str "#" id) :aria-label "Link to this section"} "#"]))))))

(defn render
  "markdown string → hiccup. `wikilinks` is {lowercased filename → url}
  (built by the content index and carried on each entry); without it,
  [[links]] render as plain text. Options:
    :anchors?  end each heading in a link to itself (a body on its own page)"
  ([s] (render s nil))
  ([s wikilinks] (render s wikilinks nil))
  ([s wikilinks {:keys [anchors?]}]
   (let [resolve-target (fn [t]
                          (let [t (-> (str t)
                                      (str/split #"#") first        ; drop heading anchors
                                      (str/split #"/") peek         ; path-qualified links
                                      str/trim)]
                            (get wikilinks (str/lower-case t))))
         renderers (assoc md/default-hiccup-renderers
                          :internal-link (wikilink-renderer resolve-target)
                          :html-inline raw-html
                          :html-block raw-html
                          :heading (heading-renderer anchors?))]
     (md/->hiccup renderers (md/parse parse-ctx (preprocess s))))))

;; --- the lede: what a feed row previews ----------------------------------

(defn- lede
  "The first paragraph of a markdown string."
  [s]
  (first (str/split (str s) #"\n\s*\n" 2)))

(defn lede-image
  "When a body opens on an image — its first block is a lone markdown
  image or Obsidian embed — {:src ... :alt ...}, else nil. Feed rows use
  it to carry a small version of the image into a post's preview."
  [s]
  (let [block (str/trim (or (lede s) ""))]
    (or (when-let [[_ alt src] (re-matches #"!\[([^\]]*)\]\(([^)\s]+)\)" block)]
          {:src src :alt alt})
        (when-let [[_ target alt] (re-matches #"!\[\[([^\]|]+?)(?:\|([^\]]*))?\]\]" block)]
          (when (image-extensions (str/lower-case (or (peek (str/split target #"\.")) "")))
            {:src (attachment-url target) :alt (or alt "")})))))

(defn word-count
  "Rough word count of a markdown string (whitespace-separated tokens)."
  [s]
  (count (re-seq #"\S+" (str s))))

(defn read-time
  "Reading-time estimate in whole minutes (~200 wpm), at least 1."
  [s]
  (max 1 (Math/round (/ (word-count s) 200.0))))

(defn plain
  "A markdown string as plain text: images and embeds drop, links keep
  their labels, syntax marks strip, whitespace collapses. Search snippets
  run this over whole bodies; `excerpt` over the lede."
  [s]
  (-> (str s)
      (str/replace #"!\[\[[^\]]*\]\]" "")               ; ![[embeds]]
      (str/replace #"!\[[^\]]*\]\([^)]*\)" "")           ; ![alt](img)
      (str/replace #"\[\[[^\]|]+\|([^\]]+)\]\]" "$1")    ; [[t|label]] → label
      (str/replace #"\[\[([^\]]+)\]\]" "$1")             ; [[t]] → t
      (str/replace #"\[([^\]]+)\]\([^)]*\)" "$1")        ; [text](url) → text
      (str/replace #"[*_`>#]" "")                         ; emphasis / code / heading marks
      (str/replace #"\s+" " ")
      str/trim))

(defn excerpt
  "The first paragraph that has any text, as plain text — a leading
  image-only paragraph is skipped, so a post that opens on an image still
  previews as prose."
  [s]
  (->> (str/split (str s) #"\n\s*\n")
       (map plain)
       (remove str/blank?)
       first))
