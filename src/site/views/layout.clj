(ns site.views.layout
  "Site chrome. `page` wraps hiccup content and renders to an HTML string;
  views are pure functions of data."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [site.markdown :as markdown]
            [site.util :as util]))

;; The wordmark logotype, inlined once so it inherits the theme colour via
;; currentColor and inverts on hover. Read once, not per request.
(def ^:private wordmark
  (delay (slurp (io/resource "public/images/wordmark.svg"))))

;; Cache-busting asset URLs: every asset a page links carries its content
;; hash (?v=), so a changed file arrives at a brand-new URL and the old
;; one can sit in browser and CDN caches forever without ever going
;; stale. Hashed once per process in prod (assets ship inside the deploy
;; image, so a change always means a fresh process); per render under
;; `bb dev`, so an edited stylesheet busts through on the next reload.
(defn- asset-hash [path]
  (let [bytes (with-open [in (io/input-stream (io/resource (str "public" path)))]
                (.readAllBytes in))]
    (subs (format "%032x" (BigInteger. 1 (.digest (java.security.MessageDigest/getInstance "MD5") bytes)))
          0 8)))

(def ^:private asset-hash-cached (memoize asset-hash))

(defn- asset-url [config path]
  (str path "?v=" ((if (:dev? config) asset-hash asset-hash-cached) path)))

;; Injected only under `bb dev`: opens an SSE stream to site.livereload and
;; refreshes the page when a watched file changes. Reconnecting after the
;; server itself restarts also reloads, so code edits (which need a `bb dev`
;; restart) still land in the browser without a manual refresh.
(def ^:private livereload-script
  (str "(function(){var opened=false;"
       "var es=new EventSource('/__livereload');"
       "es.addEventListener('reload',function(){location.reload()});"
       "es.addEventListener('open',function(){if(opened)location.reload();opened=true});"
       "})();"))

;; The site's only JavaScript besides theme-toggle-script. Inlined and
;; blocking, right after the color-scheme <meta> in <head> — anywhere later
;; and a stored "light" choice would flash the dark canvas before this runs.
;; Wrapped in try/catch because localStorage throws in some privacy modes;
;; failing silently just leaves the OS preference in charge, which is the
;; correct fallback anyway.
(def ^:private theme-init-script
  (str "(function(){try{var t=localStorage.getItem('theme');"
       "if(t==='light'||t==='dark')document.documentElement.dataset.theme=t"
       "}catch(e){}})();"))

;; Privacy-friendly, self-hosted analytics (Umami). Emitted in the head of
;; every page in production only — counting local `bb dev` views would skew
;; the stats, so it is gated the same way livereload is (but inverted).
(def ^:private analytics-script
  [:script {:defer true
            :src "https://kirasumami.pikapod.net/script.js"
            :data-website-id "e54cf840-1328-45a5-a617-7f15ef917005"
            :data-do-not-track true}])

;; The button's accessible name, one phrasing shared by the server-rendered
;; attribute and by the click handler below (which is built from this same
;; map), so the two can't drift. Each names the current mode AND what
;; pressing does: a control labelled only with its state leaves a
;; screen-reader user to guess where the next press lands.
(def ^:private theme-labels
  {"system" "Theme: system (follows your device). Click to switch to light."
   "light"  "Theme: light. Click to switch to dark."
   "dark"   "Theme: dark. Click to switch to system."})

;; The footer theme toggle. Ships `hidden` — theme-toggle-script un-hides
;; it, so a visitor with JS off never sees a control that can't work. All
;; three icons live in the DOM at once; style.css shows exactly one, keyed
;; off the button's own data-mode.
(def ^:private theme-toggle
  [:button.theme-toggle {:type "button" :id "theme-toggle" :hidden true
                         :aria-label (theme-labels "system")
                         :title (theme-labels "system")}
   ;; Remix Icon (Apache-2.0): sun-line, moon-line, contrast-2-line. One
   ;; filled path each — line art thins out at 15px, and these read as a
   ;; set because they share a drawing.
   [:svg.i-light {:viewBox "0 0 24 24" :fill "currentColor"
                  :aria-hidden "true" :focusable "false"}
    [:path {:d (str "M12 18a6 6 0 1 1 0-12 6 6 0 0 1 0 12zm0-2a4 4 0 1 0 0-8 4 4 0 0 0 0 8z"
                    "M11 1h2v3h-2V1zm0 19h2v3h-2v-3zM3.515 4.929l1.414-1.414L7.05 5.636 5.636 "
                    "7.05 3.515 4.93zM16.95 18.364l1.414-1.414 2.121 2.121-1.414 1.414-2.121"
                    "-2.121zm2.121-14.85l1.414 1.415-2.121 2.121-1.414-1.414 2.121-2.121zM5.636 "
                    "16.95l1.414 1.414-2.121 2.121-1.414-1.414 2.121-2.121zM23 11v2h-3v-2h3zM4 "
                    "11v2H1v-2h3z")}]]
   [:svg.i-dark {:viewBox "0 0 24 24" :fill "currentColor"
                 :aria-hidden "true" :focusable "false"}
    [:path {:d (str "M10 7a7 7 0 0 0 12 4.9v.1c0 5.523-4.477 10-10 10S2 17.523 2 12 6.477 2 12 "
                    "2h.1A6.979 6.979 0 0 0 10 7zm-6 5a8 8 0 0 0 15.062 3.762A9 9 0 0 1 8.238 "
                    "4.938 7.999 7.999 0 0 0 4 12z")}]]
   [:svg.i-system {:viewBox "0 0 24 24" :fill "currentColor"
                   :aria-hidden "true" :focusable "false"}
    [:path {:d (str "M12 22C6.477 22 2 17.523 2 12S6.477 2 12 2s10 4.477 10 10-4.477 10-10 "
                    "10zm0-2V4a8 8 0 1 0 0 16z")}]]])

;; Reveals the toggle and wires the cycle light -> dark -> system -> light.
;; "system" is the absence of both the override attribute and the storage
;; key, so restoring it means removing rather than writing either. Runs at
;; the end of <body>, in every environment (not dev-gated, unlike
;; livereload-script) — the control is permanent site chrome, not a dev aid.
(def ^:private theme-toggle-script
  (str "(function(){var btn=document.getElementById('theme-toggle');"
       "var next={system:'light',light:'dark',dark:'system'};"
       ;; Rendered from theme-labels, so the name the handler writes is the
       ;; same string the markup shipped with. Safe to single-quote: the
       ;; phrasings above are literals here, and none contains an apostrophe.
       "var labels={"
       (str/join "," (for [[mode label] theme-labels]
                       (str mode ":'" label "'")))
       "};"
       "function apply(m){"
       "if(m==='system'){delete document.documentElement.dataset.theme}"
       "else{document.documentElement.dataset.theme=m}"
       "try{if(m==='system')localStorage.removeItem('theme');else localStorage.setItem('theme',m)}catch(e){}"
       "btn.dataset.mode=m;btn.setAttribute('aria-label',labels[m]);btn.title=labels[m];}"
       "var stored;try{stored=localStorage.getItem('theme')}catch(e){}"
       "apply(stored==='light'||stored==='dark'?stored:'system');"
       "btn.hidden=false;"
       "btn.addEventListener('click',function(){apply(next[btn.dataset.mode])});"
       "})();"))

(defn- header [config home?]
  [:header.site-header
   [:a {:class (if home? "brand" "brand brand-sm") :href "/" :aria-label "Kira Howe — home"} (h/raw @wordmark)]
   [:nav.site-nav
    ;; :nav-types, not :entry-types — only types with at least one
    ;; published entry get a link, so an unused type is never a
    ;; permanent dead link to a 404.
    (for [t (:nav-types config)]
      [:a {:class (str "type " (name t)) :href (str "/" (name t) "s")}
       (str/capitalize (str (name t) "s"))])
    [:span.nav-sep {:aria-hidden "true"}]
    [:a {:href "/tags"} "Tags"]
    [:a {:href "/archive"} "Archive"]
    [:a {:href "/search"} "Search"]
    [:a {:href "/about"} "About"]
    [:a {:href "/follow"} "Follow"]]])

(defn- footer [config]
  (let [year (.getYear (java.time.LocalDate/now))]
    [:footer.site-footer
     [:div.footer-start
      [:span "© " year " " (:site-title config)
       " / made with ♥ in Yarmouth, NS / "
       [:a {:href "/privacy"} "Privacy"]]]
     ;; Grouped so the toggle lands beside .social at the right edge rather
     ;; than becoming a third space-between child (see .footer-end).
     [:div.footer-end
      [:span.social
       [:a {:href "/feed.xml"} "Atom"]
       (for [[label url] (:social config)]
         [:a {:href url} label])]
      theme-toggle]]))

(defn page
  "config, opts, hiccup content → full HTML string. opts:
    :title      the page title; nil for the homepage
    :path       the page's own path (\"/2026/jul/4/x\") — absolutized into
                its rel=canonical and og:url. Facetted views pass their
                clean path, so ?tag= variants canonicalize to the plain
                listing; pages with no stable URL (drafts, 404) pass none.
    :canonical  an absolute URL that replaces the self rel=canonical — an
                entry whose canonical home is elsewhere points there.
    :feed       {:kind :value} descriptor (see util/feed-scope) of an Atom
                feed scoped to this page (a type index's, a tag's),
                advertised for discovery ahead of the site feed."
  [config {:keys [title path canonical feed]} & content]
  (let [full-title (if title
                     (str title " — " (:site-title config))
                     (:site-title config))
        self-url   (when path (str (:base-url config) path))
        og-image   (str (:base-url config) (asset-url config "/images/og.png"))
        feed       (util/feed-scope feed)]
    (str
     (h/html {:mode :html}
             (h/raw "<!DOCTYPE html>\n")
             [:html {:lang "en"}
              [:head
               [:meta {:charset "utf-8"}]
               [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
               ;; Read during HTML parse, before any stylesheet loads, so the
               ;; browser paints a dark canvas on the first frame of every
               ;; navigation — no white flash while style.css/fonts arrive.
               ;; "dark light" = dark by default, light for OS-light users,
               ;; matching the light-dark() default in style.css.
               [:meta {:name "color-scheme" :content "dark light"}]
               ;; Applies a stored light/dark override before either of the
               ;; above can paint — see theme-init-script.
               [:script (h/raw theme-init-script)]
               [:title full-title]
               [:meta {:name "description" :content (:site-description config)}]
               ;; The page's one true URL: an external :canonical wins (the
               ;; content's home is elsewhere), else the page's own address.
               (when-let [href (or canonical self-url)]
                 [:link {:rel "canonical" :href href}])
               ;; Open Graph — how the site renders when shared (link previews).
               ;; og:url stays this page's own URL even when rel=canonical
               ;; points elsewhere: a share of this page is about this page.
               [:meta {:property "og:type" :content "website"}]
               [:meta {:property "og:site_name" :content (:site-title config)}]
               [:meta {:property "og:title" :content full-title}]
               [:meta {:property "og:description" :content (:site-description config)}]
               [:meta {:property "og:url" :content (or self-url (:base-url config))}]
               [:meta {:property "og:image" :content og-image}]
               [:meta {:property "og:image:type" :content "image/png"}]
               [:meta {:property "og:image:width" :content "2400"}]
               [:meta {:property "og:image:height" :content "1260"}]
               [:meta {:property "og:image:alt" :content (:site-title config)}]
               ;; Twitter/X uses its own namespace; large summary card
               [:meta {:name "twitter:card" :content "summary_large_image"}]
               [:meta {:name "twitter:title" :content full-title}]
               [:meta {:name "twitter:description" :content (:site-description config)}]
               [:meta {:name "twitter:image" :content og-image}]
               [:link {:rel "stylesheet" :href (asset-url config "/css/style.css")}]
               ;; SVG first: a browser that understands image/svg+xml takes it
               ;; and never fetches the .ico, which exists only for older
               ;; clients and for the bare /favicon.ico request that agents
               ;; (feed readers, bookmarking services) make without ever
               ;; reading this HTML. apple-touch-icon is the iOS home-screen
               ;; icon — a flattened opaque PNG, since iOS composites it onto
               ;; a solid tile rather than a transparent canvas like the SVG.
               [:link {:rel "icon" :type "image/svg+xml" :href (asset-url config "/favicon.svg")}]
               [:link {:rel "icon" :sizes "32x32" :href (asset-url config "/favicon.ico")}]
               [:link {:rel "apple-touch-icon" :href (asset-url config "/apple-touch-icon.png")}]
               ;; Feed autodiscovery. A page with a scoped feed (a tag's)
               ;; advertises it FIRST: a reader handed the page URL takes
               ;; the first alternate it finds, so listing the site feed
               ;; ahead of it is what quietly turns "subscribe to #clojure"
               ;; into "subscribe to everything".
               (when feed
                 [:link {:rel "alternate" :type "application/atom+xml"
                         :title (:title feed) :href (:href feed)}])
               [:link {:rel "alternate" :type "application/atom+xml"
                       :title (:site-title config) :href "/feed.xml"}]
               (when-not (:dev? config) analytics-script)]
              [:body
               (header config (nil? title))
               [:main content]
               (footer config)
               [:script (h/raw theme-toggle-script)]
               (when (:dev? config) [:script (h/raw livereload-script)])]]))))

(defn static-page [config {:keys [title body path]}]
  (page config {:title title :path path}
        [:article.article.prose
         [:h1 title]
         (rest (markdown/render body nil {:anchors? true}))]))

(defn not-found [config]
  (page config {:title "Not found"}
        [:article.article.prose
         [:h1 "404"]
         [:p "Nothing lives at this address. Try "
          [:a {:href "/"} "the homepage"] "."]]))
