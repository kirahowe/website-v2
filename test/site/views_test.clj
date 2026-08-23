(ns site.views-test
  "End-to-end: exercises the whole Ring app as a plain function against
  example-content — routing, content loading, markdown rendering, views."
  (:require [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [site.app :as app]
            [site.content :as content]))

(def config
  {:site-title "Test Site"
   :site-description "Testing"
   :base-url "https://example.com"
   :content-path "example-content"
   :entry-types [:post :note :link :quote :release :tool]})

(def handler
  (app/make-app config (atom (content/build-index config))))

(def dev-handler
  (app/make-app (assoc config :dev? true)
                (atom (content/build-index config))))

(defn- GET
  ([uri] (GET uri nil))
  ([uri query] (handler {:request-method :get :uri uri :query-string query})))

(deftest pages-render
  (testing "home is a day-grouped feed; posts preview, short types are full"
    (let [{:keys [status body headers]} (GET "/")]
      (is (= 200 status))
      (is (= "public, max-age=0, must-revalidate" (get headers "Cache-Control")))
      (is (= "public, max-age=86400"
             (get headers "Cloudflare-CDN-Cache-Control")))
      (is (str/includes? body "Hello, world"))
      (is (str/includes? body "nextjournal/markdown"))
      (is (str/includes? body "Rich Hickey"))
      ;; all 8 example entries fit within :home-entries, so the oldest shows
      (is (str/includes? body "Babashka"))
      ;; feed previews are the first paragraph only, as plain-text excerpts
      (is (not (str/includes? body "where code sleeps")))
      ;; ...but quotes are short-form: the whole body publishes to the feed
      (is (str/includes? body "prerequisite for reliability"))
      ;; ...as do links (and releases/tools): a pull-quote in a link body
      ;; survives into the feed as a blockquote, not dropped by the excerpt
      (is (str/includes? body "a lousy data structure"))
      (is (str/includes? body "min read"))                     ; reading-time hint
      (is (str/includes? body "Untitled entries are fine"))    ; untitled post excerpt shows
      ;; day headings link to the day archives
      (is (str/includes? body "July 4, 2026"))
      (is (str/includes? body "\"/2026/jul/4\""))))

  (testing "single entry page renders markdown"
    (let [{:keys [status body headers]} (GET "/2026/jul/4/hello-world")]
      (is (= 200 status))
      (is (= "public, max-age=300, stale-while-revalidate=86400"
             (get headers "Cache-Control")))
      (is (nil? (get headers "Cloudflare-CDN-Cache-Control")))
      (is (str/includes? body "How it works</h2>"))  ; ## heading
      (is (str/includes? body "<code"))              ; inline code
      (is (str/includes? body "entry-url")))
    (let [{:keys [body]} (GET "/2025/nov/12/repl-driven")]
      (is (str/includes? body "where code sleeps"))       ; full body lives on the entry page
      (is (not (str/includes? body "words]")))))          ; no preview link there

  (testing "non-canonical entry URL redirects to canonical"
    (let [{:keys [status headers]} (GET "/2026/07/04/hello-world")]
      (is (= 301 status))
      (is (= "/2026/jul/4/hello-world" (get headers "Location")))))

  (testing "archives"
    (is (= 200 (:status (GET "/2026"))))
    (is (= 200 (:status (GET "/2026/jul"))))
    (is (= 200 (:status (GET "/2026/jul/4"))))
    (is (str/includes? (:body (GET "/2026/jul/4")) "Hello, world"))
    (is (= 404 (:status (GET "/2024")))))

  (testing "month calendars are Sunday-first"
    ;; March 1, 2026 is a Sunday, so the first row starts with day 1.
    (let [body (:body (GET "/2026/mar"))]
      (is (str/includes? body
                         "<thead><tr><th>S</th><th>M</th><th>T</th><th>W</th><th>T</th><th>F</th><th>S</th></tr></thead>"))
      (is (str/includes? body "</thead><tbody><tr><td>1</td>"))))

  (testing "month pages link to the neighboring months that have content"
    (let [{:keys [body]} (GET "/2026/jun")]
      (is (str/includes? body "\"/2026/jul\""))     ; newer neighbor
      (is (str/includes? body "\"/2026/may\"")))    ; older neighbor
    (let [{:keys [body]} (GET "/2026/may")]
      (is (str/includes? body "\"/2026/apr\"")))    ; older neighbor (new release entry)
    (let [{:keys [body]} (GET "/2026/mar")]
      (is (str/includes? body "\"/2025/nov\""))))   ; skips empty months, crosses years

  (testing "type and tag listings"
    (is (str/includes? (:body (GET "/posts")) "REPL-driven"))
    (is (str/includes? (:body (GET "/quotes")) "Simplicity"))
    (is (str/includes? (:body (GET "/tags/clojure")) "Babashka"))
    (is (= 200 (:status (GET "/2026/posts"))))
    (is (= 404 (:status (GET "/2024/posts"))))
    (is (str/includes? (:body (GET "/releases")) "website v1.0"))
    (is (str/includes? (:body (GET "/tools")) "vault-publish")))

  (testing "the per-type count summary singularizes a lone entry"
    ;; June has exactly one entry (a quote), so the month summary must
    ;; read "1 quote", not "1 quotes".
    (let [{:keys [body]} (GET "/2026/jun")]
      (is (str/includes? body ">quote</a>"))
      (is (not (str/includes? body ">quotes</a>")))))

  (testing "quote renders with attribution, full body, and one via credit"
    (let [{:keys [body]} (GET "/2026/jun/21/rich-hickey-on-simplicity")]
      ;; a quote-type entry carries the .quote class (its hanging serif marks);
      ;; other blockquotes get the plain left-border style
      (is (str/includes? body "<blockquote class=\"quote\">"))
      (is (str/includes? body "Rich Hickey"))
      (is (str/includes? body "prerequisite for reliability"))
      ;; the via credit sits on the cite line — once, even on a titled quote
      (is (str/includes? body "news.ycombinator.com"))
      (is (= 1 (count (re-seq #"class=\"via\"" body))))
      ;; the closing mark is real markup; its opening partner is a CSS
      ;; ::before on .quote, which style.css supplies on every page — so
      ;; the entry page carries the closing span
      (is (str/includes? body "class=\"quote-close\""))))

  (testing "a quote's via credit reaches the feed too"
    (let [{:keys [body]} (GET "/quotes")]
      (is (str/includes? body "news.ycombinator.com"))))

  (testing "a note publishes whole: live markup, no continuation link"
    (let [{:keys [body]} (GET "/notes")]
      ;; the body renders, so its links and emphasis survive the feed —
      ;; an excerpt would have flattened both to plain text
      (is (str/includes? body "href=\"https://example.org/short-form\""))
      (is (str/includes? body "<em>is</em>"))
      ;; nothing more to read: no reading-time link, and the title already
      ;; leads home, so the foot carries no second permalink
      (is (not (str/includes? body "min read]")))
      (is (not (str/includes? body "class=\"permalink\"")))
      ;; its own page still exists, reached from that title
      (is (str/includes? body "href=\"/2026/jul/4/a-note-is-one-thought\""))))

  (testing "a note's entry page carries no reading-time either"
    (let [{:keys [status body]} (GET "/2026/jul/4/a-note-is-one-thought")]
      (is (= 200 status))
      (is (str/includes? body "published whole"))
      (is (not (str/includes? body "min read")))))

  (testing "under search a note drops to a highlighted excerpt, like a link"
    (let [{:keys [body]} (GET "/search" "q=thought")]
      (is (str/includes? body "<mark class=\"hit\">thought</mark>"))
      (is (str/includes? body "class=\"entry-excerpt\""))))

  (testing "link entry title points at the external URL"
    (let [{:keys [body]} (GET "/2025/aug/30/babashka")]
      (is (str/includes? body "https://babashka.org"))
      (is (str/includes? body "via"))))

  (testing "release and tool titles link out too"
    (is (str/includes? (:body (GET "/2026/apr/18/website-v1")) "releases/tag/v1.0"))
    (is (str/includes? (:body (GET "/2026/mar/7/vault-publish")) "tools.example.com/vault-publish")))

  (testing "a tool credits its source code after the title, styled like a via credit"
    (let [{:keys [body]} (GET "/2026/mar/7/vault-publish")]
      (is (str/includes? body "href=\"https://github.com/kirahowe/vault-publish\">source</a>"))
      (is (= 1 (count (re-seq #"class=\"via\"" body)))))
    ;; ...and the credit reaches the feed rows too
    (is (str/includes? (:body (GET "/tools")) ">source</a>")))

  (testing "static page"
    (let [{:keys [status body]} (GET "/about")]
      (is (= 200 status))
      (is (str/includes? body "personal weblog")))
    (let [{:keys [status body]} (GET "/privacy")]
      (is (= 200 status))
      (is (str/includes? body "Email subscriptions"))))

  (testing "search: relevance order, highlights, facet rails"
    (let [{:keys [status body headers]} (GET "/search" "q=babashka")]
      (is (= 200 status))
      (is (= "no-store" (get headers "Cache-Control")))
      ;; the query bar is the shared field/button pair
      (is (str/includes? body "field-form search"))
      (is (str/includes? body ">Search</button>"))
      (is (str/includes? body "sorted by relevance"))
      ;; matched terms are marked, in titles and snippets
      (is (str/includes? body "<mark class=\"hit\">Babashka</mark>"))
      ;; results are the same cards the feed uses, one day group per hit
      (is (str/includes? body "class=\"day-group\""))
      (is (str/includes? body "min read]"))                ; post continuation link
      ;; the title match ranks above the body-only matches
      (is (< (str/index-of body "babashka.org")
             (str/index-of body "/2026/jul/4/hello-world")))
      ;; both rails render, with filter links carrying the query
      (is (str/includes? body "Filter by type"))
      (is (str/includes? body "Refine"))
      (is (str/includes? body "/search?q=babashka&amp;type=link"))
      (is (str/includes? body "/search?q=babashka&amp;tag=clojure"))))

  (testing "search: a deep body match shows an ellipsised snippet"
    (let [{:keys [body]} (GET "/search" "q=pipeline")]
      (is (str/includes? body "<mark class=\"hit\">pipeline</mark>"))
      (is (str/includes? body "… "))))

  (testing "search: type and tag filters narrow the results"
    (let [{:keys [body]} (GET "/search" "q=babashka&type=link")]
      (is (str/includes? body "babashka.org"))                       ; the link stays
      (is (not (str/includes? body "/2026/jul/4/hello-world")))      ; the post is filtered out
      (is (str/includes? body "Found <b>2 links</b>")))
    (let [{:keys [body]} (GET "/search" "q=babashka&tag=libraries")]
      (is (str/includes? body "nextjournal/markdown"))
      (is (not (str/includes? body "babashka.org"))))
    ;; an unknown type is ignored, not an error
    (is (= 200 (:status (GET "/search" "q=babashka&type=zebra")))))

  (testing "search: blank prompt and empty result states"
    (is (str/includes? (:body (GET "/search")) "Searches titles, tags"))
    (is (str/includes? (:body (GET "/search" "q=xyzzy")) "Nothing found")))

  (testing "feed"
    (let [{:keys [status body headers]} (GET "/feed.xml")]
      (is (= 200 status))
      (is (str/includes? (get headers "Content-Type") "application/atom+xml"))
      (is (str/includes? body "<feed xmlns=\"http://www.w3.org/2005/Atom\""))
      (is (= "feed" (name (:tag (xml/parse-str body)))))
      (is (str/includes? body "Hello, world"))
      (is (str/includes? body "<id>https://example.com/feed.xml</id>"))
      (is (str/includes? body "<updated>2026-07-04T12:00:00Z</updated>"))
      (is (str/includes? body "<content type=\"html\">"))
      ;; the channel names its own address, so a reader never has to
      ;; re-derive it from the channel link
      (is (str/includes? body "href=\"https://example.com/feed.xml\" rel=\"self\""))
      ;; root-relative refs inside entry content are absolutized: a feed
      ;; reader renders this HTML against its own host, not ours
      (is (str/includes? body "https://example.com/attachments/caching-diagram.png"))
      (is (not (str/includes? body "src=&quot;/")))
      (is (not (str/includes? body "href=&quot;/")))))

  (testing "tag feed: the site channel scoped to one tag"
    (let [{:keys [status body headers]} (GET "/tags/clojure/feed.xml")]
      (is (= 200 status))
      (is (str/includes? (get headers "Content-Type") "application/atom+xml"))
      (is (= "feed" (name (:tag (xml/parse-str body)))))
      (is (str/includes? body "Test Site / #clojure"))
      (is (str/includes? body "https://example.com/tags/clojure"))
      (is (str/includes? body "Hello, world"))
      ;; only tagged entries make it in
      (is (not (str/includes? body "vault-publish")))
      ;; rel=self is the tag's own feed, not the site feed — a reader that
      ;; re-resolves this feed must land back on the scoped one
      (is (str/includes? body "href=\"https://example.com/tags/clojure/feed.xml\" rel=\"self\"")))
    ;; a tag with no entries has no feed, like its listing
    (is (= 404 (:status (GET "/tags/zebra/feed.xml"))))
    ;; the tag page points at the scoped feed: sidebar link + head alternate
    (let [{:keys [body]} (GET "/tags/clojure")]
      (is (str/includes? body "href=\"/tags/clojure/feed.xml\""))
      (is (str/includes? body "rel=\"alternate\" title=\"Atom for #clojure\""))
      ;; ...and it comes FIRST, ahead of the site-wide feed: autodiscovery
      ;; takes the first alternate, so the order is the subscription
      (is (< (str/index-of body "href=\"/tags/clojure/feed.xml\" rel=\"alternate\"")
             (str/index-of body "href=\"/feed.xml\" rel=\"alternate\"")))))

  (testing "type feed: the site feed scoped to one entry type"
    (doseq [plural ["posts" "notes" "links" "quotes" "releases" "tools"]]
      (let [{:keys [status body]} (GET (str "/" plural "/feed.xml"))]
        (is (= 200 status) plural)
        (is (= "feed" (name (:tag (xml/parse-str body)))) plural)))
    (let [{:keys [status body headers]} (GET "/posts/feed.xml")]
      (is (= 200 status))
      (is (str/includes? (get headers "Content-Type") "application/atom+xml"))
      (is (= "feed" (name (:tag (xml/parse-str body)))))
      (is (str/includes? body "Test Site / Posts"))
      (is (str/includes? body "<id>https://example.com/posts/feed.xml</id>"))
      (is (str/includes? body "href=\"https://example.com/posts/feed.xml\" rel=\"self\""))
      (is (str/includes? body "Hello, world"))
      (is (not (str/includes? body "A note is one thought"))))
    ;; A type listing advertises its scoped feed before the site-wide feed.
    (let [body (:body (GET "/posts"))]
      (is (str/includes? body "rel=\"alternate\" title=\"Atom for Posts\""))
      (is (< (str/index-of body "href=\"/posts/feed.xml\" rel=\"alternate\"")
             (str/index-of body "href=\"/feed.xml\" rel=\"alternate\"")))))

  (testing "tags index carries the client-side filter, hidden until its JS runs"
    (let [{:keys [body]} (GET "/tags")]
      (is (str/includes? body "field-form tag-filter"))
      (is (str/includes? body "aria-label=\"Filter tags\""))
      (is (str/includes? body "tag-filter-empty"))))

  (testing "404"
    (is (= 404 (:status (GET "/nope"))))
    (is (= 404 (:status (GET "/2026/jul/4/nope"))))))

(deftest follow
  ;; example-content/pages/follow.md is six blocks — p, h2, p, p, h2, p — so
  ;; the split shows up in the counts: three above the form, three below.
  (testing "the signup form splits the /follow page after its third block"
    (let [{:keys [status body]} (GET "/follow")
          article (subs body (str/index-of body "<article") (str/index-of body "</article>"))
          [above below] (str/split article #"<form[^>]*follow-form" 2)
          blocks #(count (re-seq #"<(?:p|h2)[ >]" %))]
      (is (= 200 status))
      (is (= 3 (blocks above)))
      (is (= 3 (blocks below)))))

  (testing "an entry's page footer carries the follow section"
    (let [{:keys [body]} (GET "/2026/jul/4/hello-world")]
      (is (str/includes? body "field-form follow-form"))
      (is (str/includes? body "name=\"email\""))
      ;; the marker Buttondown's embed endpoint expects
      (is (str/includes? body "name=\"embed\""))
      (is (str/includes? body "Follow"))
      (is (str/includes? body
                         "href=\"/feed.xml\">Atom feed</a>"))))

  (testing "the site footer no longer carries a follow band"
    (is (not (str/includes? (:body (GET "/")) "follow-band"))))

  (testing "the site nav links to the /follow page"
    (let [body (:body (GET "/"))]
      (is (str/includes? body "<a href=\"/follow\">Follow</a>"))
      ;; it lives in the header nav, so it precedes the footer entirely
      (is (< (str/index-of body "<a href=\"/follow\">Follow</a>")
             (str/index-of body "class=\"site-footer\"")))))

  (testing "the left side of the site footer links to the privacy page"
    (let [body (:body (GET "/"))]
      (is (str/includes? body "class=\"footer-start\""))
      (is (str/includes? body "Yarmouth, NS / <a href=\"/privacy\">Privacy</a>"))))

  (testing "every archive/index sidebar carries the Follow widget"
    (doseq [uri ["/" "/2026" "/2026/jul" "/tags/clojure" "/posts"]]
      (is (str/includes? (:body (GET uri)) ">Follow</h2>")
          (str uri " sidebar should carry the Follow widget"))))

  (testing "Follow uses the most specific available Atom feed"
    (let [tag-bodies (map (comp :body GET) ["/tags/clojure" "/tags/clojure/2026"])
          type-bodies (map (comp :body GET) ["/posts" "/2026/posts"])
          archive-body (:body (GET "/2026"))]
      (doseq [body tag-bodies]
        (is (str/includes? body
                           "href=\"/tags/clojure/feed.xml\">just #clojure</a>"))
        (is (str/includes? body "href=\"/feed.xml\">everything</a>")))
      (doseq [body type-bodies]
        (is (str/includes? body
                           "href=\"/posts/feed.xml\">just posts</a>"))
        (is (str/includes? body "href=\"/feed.xml\">everything</a>")))
      (is (str/includes? archive-body
                         "href=\"/feed.xml\">Atom feed</a>"))
      (is (str/includes? archive-body
                         "Get a weekly digest of new posts in your inbox"))
      (is (not (str/includes? (first tag-bodies) ">Feeds</h2>")))
      (is (= 1 (count (re-seq #">Follow</h2>" (first tag-bodies)))))
      ;; the whole point of the single sentence: with only one feed on offer,
      ;; "Atom feed" is itself the anchor — no second link naming it again
      (is (not (str/includes? archive-body ">everything</a>")))))

  (testing "on the home page the Follow widget sits above Top tags"
    (let [body (:body (GET "/"))]
      (is (< (str/index-of body ">Follow</h2>")
             (str/index-of body ">Top tags</h2>")))))

  (testing "elsewhere the Follow widget stays last in the sidebar"
    (let [body (:body (GET "/tags/clojure"))]
      (is (< (str/index-of body ">Related tags</h2>")
             (str/index-of body ">Follow</h2>"))))))

(deftest image-lede-posts
  (testing "a post that opens on an image previews with a small linked image plus prose"
    (let [{:keys [body]} (GET "/")]
      (is (str/includes? body "class=\"entry-thumb\""))
      (is (str/includes? body "src=\"/attachments/caching-diagram.png\""))
      ;; the excerpt skips the image paragraph and previews the first prose
      (is (str/includes? body "Untitled entries are fine"))))

  (testing "the thumb links to the post, not the image"
    ;; class and href adjacent, rather than the whole opening tag: the anchor
    ;; also carries an aria-label, and hiccup sorts that ahead of both.
    (is (str/includes? (:body (GET "/2026/may"))
                       "class=\"entry-thumb\" href=\"/2026/may/2/caching-thought\"")))

  (testing "the thumb is named — its image may carry no alt, and an unnamed link is a dead end"
    (is (re-find #"<a aria-label=\"[^\"]+\" class=\"entry-thumb\""
                 (:body (GET "/2026/may")))))

  (testing "search results stay compact prose — no thumbs"
    (let [{:keys [body]} (GET "/search" "q=cacheable")]
      (is (str/includes? body "Untitled entries are fine"))
      (is (not (str/includes? body "entry-thumb")))))

  (testing "the attachment itself serves"
    (is (= 200 (:status (GET "/attachments/caching-diagram.png"))))))

(deftest canonical-urls
  (testing "every page declares its own absolute URL as canonical, and og:url matches"
    (let [{:keys [body]} (GET "/2026/jul/4/hello-world")]
      (is (str/includes? body "<link href=\"https://example.com/2026/jul/4/hello-world\" rel=\"canonical\">"))
      (is (str/includes? body "content=\"https://example.com/2026/jul/4/hello-world\" property=\"og:url\"")))
    (is (str/includes? (:body (GET "/"))
                       "<link href=\"https://example.com/\" rel=\"canonical\">"))
    (is (str/includes? (:body (GET "/2026/jul"))
                       "<link href=\"https://example.com/2026/jul\" rel=\"canonical\">"))
    (is (str/includes? (:body (GET "/tags/clojure"))
                       "<link href=\"https://example.com/tags/clojure\" rel=\"canonical\">"))
    (is (str/includes? (:body (GET "/about"))
                       "<link href=\"https://example.com/about\" rel=\"canonical\">")))

  (testing "a ?tag= facet variant canonicalizes to the clean listing"
    (is (str/includes? (:body (GET "/posts" "tag=clojure"))
                       "<link href=\"https://example.com/posts\" rel=\"canonical\">"))
    (is (str/includes? (:body (GET "/2026" "tag=clojure"))
                       "<link href=\"https://example.com/2026\" rel=\"canonical\">")))

  (testing "a cross-post's canonical points at its home elsewhere and credits it visibly"
    (let [{:keys [body]} (GET "/2026/may/2/caching-thought")]
      (is (str/includes? body "<link href=\"https://oldblog.example.org/2026/caching\" rel=\"canonical\">"))
      (is (str/includes? body "originally published at"))
      (is (str/includes? body ">oldblog.example.org</a>"))
      ;; og:url stays this page's own URL: a share of this page is about this page
      (is (str/includes? body "content=\"https://example.com/2026/may/2/caching-thought\" property=\"og:url\"")))))

(deftest previous-urls-redirect
  (testing "an old same-site URL 301s to the entry, cacheably"
    (let [{:keys [status headers]} (GET "/blog/repl-driven")]
      (is (= 301 status))
      (is (= "/2025/nov/12/repl-driven" (get headers "Location")))
      (is (str/includes? (get headers "Cache-Control") "public")))
    (testing "with a trailing slash too"
      (is (= 301 (:status (GET "/blog/repl-driven/")))))
    (testing "an own-host absolute previous URL redirects by its path"
      (let [{:keys [status headers]} (GET "/notes/repl.html")]
        (is (= 301 status))
        (is (= "/2025/nov/12/repl-driven" (get headers "Location"))))))

  (testing "a foreign-host previous URL never registers a redirect here"
    (is (= 404 (:status (GET "/2025/repl-driven"))))))

(deftest drafts-are-dev-only
  (testing "production server: drafts don't exist"
    (is (= 404 (:status (GET "/drafts/an-idea-brewing"))))
    (is (= 404 (:status (GET "/drafts/an-idea-brewing" "preview=anything")))))

  (testing "dev mode: drafts render, uncached"
    (let [{:keys [status body headers]}
          (dev-handler {:request-method :get :uri "/drafts/an-idea-brewing"})]
      (is (= 200 status))
      (is (= "no-store" (get headers "Cache-Control")))
      (is (str/includes? body "Draft"))))

  (testing "drafts never leak into public listings, search, or the feed"
    (is (not (str/includes? (:body (GET "/")) "isn't ready")))
    (is (not (str/includes? (:body (GET "/feed.xml")) "isn't ready")))
    (is (not (str/includes? (:body (GET "/search" "q=brewing")) "isn't ready")))))

(deftest nav-hides-empty-types
  ;; :essay is a configured type with no published entries — the nav
  ;; must not link it (the link would be a permanent 404), while types
  ;; with entries keep their links.
  (let [cfg (update config :entry-types conj :essay)
        h (app/make-app cfg (atom (content/build-index cfg)))
        body (:body (h {:request-method :get :uri "/"}))]
    (is (not (str/includes? body "\"/essays\"")))
    (is (str/includes? body "\"/posts\""))
    (is (str/includes? body "\"/tools\"")))
  (testing "the 404 page's nav hides them too"
    (let [{:keys [status body]} (GET "/nope")]
      (is (= 404 status))
      (is (str/includes? body "\"/posts\"")))))

(deftest home-limits-to-whole-days
  (let [h (app/make-app (assoc config :home-entries 2)
                        (atom (content/build-index config)))
        body (:body (h {:request-method :get :uri "/"}))]
    (testing "both same-day entries show (days are never split)"
      (is (str/includes? body "Hello, world"))
      (is (str/includes? body "nextjournal/markdown")))
    (testing "the feed stops at the whole-day cut (later days aren't in the feed)"
      (is (not (str/includes? body "June 21, 2026"))))
    (testing "the feed continues into the month archive of the next entry"
      (is (str/includes? body "Older"))
      (is (str/includes? body "\"/2026/jun\"")))))

(deftest feed-count-is-configurable
  (testing ":feed-entries caps how many entries the Atom feed carries"
    (let [h (app/make-app (assoc config :feed-entries 1)
                          (atom (content/build-index config)))
          body (:body (h {:request-method :get :uri "/feed.xml"}))]
      (is (= 1 (count (re-seq #"<entry>" body))))))
  (testing "without :feed-entries it falls back to the default"
    (let [body (:body (GET "/feed.xml"))]
      ;; example-content has more than one entry, so the default (20)
      ;; lets several entries through
      (is (< 1 (count (re-seq #"<entry>" body)))))))

;; --- feed entry metadata ---------------------------------------------------
;; site.entry-meta is the one description of an entry's credits and tags;
;; site.feed renders every fact it describes into each <entry>, structured
;; (<link>/<category>, for a reader's software) as well as visibly (inside
;; <content>, for the reader themselves — most feed apps show only that).
;; These check the parsed XML tree rather than the raw string, because the
;; whole point is that the *facts* arrived, not any particular rendering
;; of them.

(defn- xml-children
  "The immediate children of `el` whose local (namespace-stripped) tag
  name is `tag` — `xml/parse-str` qualifies every tag with the feed's
  default Atom namespace, so a bare `:link` never matches without this."
  [el tag]
  (filter #(= (name tag) (name (:tag %))) (:content el)))

(defn- feed-entries-by-title
  "A feed body's <entry> elements keyed by their <title> text. Every
  fixture entry used below has a distinct title, so a test can grab the
  one it cares about without depending on feed order or the fixture's
  full cast."
  [body]
  (into {}
        (map (fn [e] [(first (:content (first (xml-children e :title)))) e]))
        (xml-children (xml/parse-str body) :entry)))

(defn- entry-links [entry-el] (map :attrs (xml-children entry-el :link)))
(defn- entry-categories [entry-el] (map :attrs (xml-children entry-el :category)))

(defn- entry-content
  "An <entry>'s <content> text, already entity-decoded by the XML parser
  back into ordinary HTML — the same markup `entry-body` would have
  rendered, not the doubly-escaped form living in the raw feed string."
  [entry-el]
  (first (:content (first (xml-children entry-el :content)))))

(def ^:private tag-scheme (str (:base-url config) "/tags/"))
(def ^:private type-scheme (str (:base-url config) "/types/"))

(deftest feed-entry-metadata
  (testing "a link entry's outbound target and via credit are structured links, its tags structured categories"
    (let [entry (get (feed-entries-by-title (:body (GET "/feed.xml"))) "Babashka")]
      (is (some #(= {:rel "via" :href "https://clojure.org/community/resources" :title "via"} %)
                (entry-links entry)))
      (is (some #(= {:rel "related" :href "https://babashka.org" :title "link"} %)
                (entry-links entry)))
      ;; tag categories carry the tag's own scheme, in display order — the
      ;; same order the chips and the feed rows use
      (is (= [{:scheme tag-scheme :term "clojure" :label "#clojure"}
              {:scheme tag-scheme :term "tools" :label "#tools"}]
             (filter #(= tag-scheme (:scheme %)) (entry-categories entry))))
      ;; the type category sits under its own, non-address scheme, so a
      ;; reader can tell it apart from a tag category of the same term
      (is (some #(= {:scheme type-scheme :term "link"} %) (entry-categories entry)))
      (let [content (entry-content entry)]
        (is (str/includes? content "clojure.org/community/resources"))
        (is (str/includes? content "babashka.org"))
        (is (str/includes? content "#clojure")))))

  (testing "a quote's source and via reach the feed as links, and appear once each in content — the cite line, not a second copy in the meta line"
    (let [entry (get (feed-entries-by-title (:body (GET "/feed.xml"))) "Simplicity is a choice")
          content (entry-content entry)]
      (is (str/includes? content "<blockquote class=\"quote\""))
      (is (str/includes? content "Rich Hickey"))
      (is (str/includes? content "news.ycombinator.com"))
      ;; entry-meta's via/source are :head-only, and a quote's both sit on
      ;; the cite line (:cite) instead — so the meta line contributes none
      (is (= 1 (count (re-seq #"class=\"via\"" content))))
      ;; the closing mark's opening partner is a CSS ::before on .quote —
      ;; style.css never travels with a feed item, so a reader would see a
      ;; quotation close that never visibly opened; the feed drops it
      (is (not (str/includes? content "quote-close")))
      (is (some #(= {:rel "related"
                     :href "https://www.infoq.com/presentations/Simple-Made-Easy/"
                     :title "source"} %)
                (entry-links entry)))))

  (testing "a tool's link and source credits are distinguished by title, and source is visible in content"
    (let [entry (get (feed-entries-by-title (:body (GET "/feed.xml"))) "vault-publish")]
      (is (some #(= {:rel "related" :href "https://tools.example.com/vault-publish" :title "link"} %)
                (entry-links entry)))
      (is (some #(= {:rel "related" :href "https://github.com/kirahowe/vault-publish" :title "source"} %)
                (entry-links entry)))
      (is (str/includes? (entry-content entry) ">source<"))))

  (testing "an untitled cross-post's canonical home is a structured link and a visible credit"
    (let [entry (get (feed-entries-by-title (:body (GET "/feed.xml"))) "caching-thought")]
      (is (some #(= {:rel "canonical" :href "https://oldblog.example.org/2026/caching" :title "canonical"} %)
                (entry-links entry)))
      (is (str/includes? (entry-content entry) "originally published at"))))

  (testing "an entry with no credits still renders, with no stray link/category/via markup"
    (let [entry (get (feed-entries-by-title (:body (GET "/feed.xml"))) "Hello, world")]
      ;; the only <link> is the entry's own permalink — no credit earned one
      (is (= [{:rel "alternate" :href "https://example.com/2026/jul/4/hello-world" :type "text/html"}]
             (entry-links entry)))
      (is (not (str/includes? (entry-content entry) "class=\"via\"")))))

  (testing "the same metadata reaches every feed — tag and type feeds share entry-element with the site feed"
    (let [tag-entry (get (feed-entries-by-title (:body (GET "/tags/clojure/feed.xml"))) "Babashka")]
      (is (some #(= "via" (:rel %)) (entry-links tag-entry)))
      (is (= [{:scheme tag-scheme :term "clojure" :label "#clojure"}
              {:scheme tag-scheme :term "tools" :label "#tools"}]
             (filter #(= tag-scheme (:scheme %)) (entry-categories tag-entry)))))
    (let [type-entry (get (feed-entries-by-title (:body (GET "/posts/feed.xml"))) "Hello, world")]
      (is (= [{:scheme tag-scheme :term "clojure" :label "#clojure"}
              {:scheme tag-scheme :term "meta" :label "#meta"}]
             (filter #(= tag-scheme (:scheme %)) (entry-categories type-entry)))))))

(deftest type-listings-are-paginated
  (let [h (app/make-app (assoc config :page-entries 1)
                        (atom (content/build-index config)))
        get-page (fn
                   ([uri] (h {:request-method :get :uri uri}))
                   ([uri query] (h {:request-method :get :uri uri
                                    :query-string query})))
        first-page (:body (get-page "/posts"))
        second-page (:body (get-page "/posts/page/2"))]
    (testing "the first page shows only its slice and the total count"
      (is (str/includes? first-page "Hello, world"))
      (is (not (str/includes? first-page "caching-thought")))
      (is (str/includes? first-page "3 entries"))
      (is (str/includes? first-page "href=\"/posts/page/2\""))
      (is (str/includes? first-page "Older →")))
    (testing "middle pages link in both directions and have their own canonical URL"
      (is (str/includes? second-page "caching-thought"))
      (is (not (str/includes? second-page "Hello, world")))
      (is (str/includes? second-page "href=\"/posts\" rel=\"prev\""))
      (is (str/includes? second-page "href=\"/posts/page/3\" rel=\"next\""))
      (is (str/includes? second-page "Page 2 of 3"))
      (is (str/includes? second-page "<title>Posts / Page 2 — Test Site</title>"))
      (is (str/includes? second-page
                         "href=\"https://example.com/posts/page/2\" rel=\"canonical\"")))
    (testing "pagination preserves an active tag facet"
      (let [body (:body (get-page "/posts" "tag=clojure"))]
        (is (str/includes? body "href=\"/posts/page/2?tag=clojure\""))))
    (testing "pages past the end are a 404"
      (is (= 404 (:status (get-page "/posts/page/4")))))))

(deftest tag-listings-are-paginated
  ;; :clojure tags 5 example entries, newest first: nextjournal-markdown,
  ;; hello-world, rich-hickey-on-simplicity, repl-driven, babashka.
  (let [h (app/make-app (assoc config :page-entries 1)
                        (atom (content/build-index config)))
        get-page (fn [uri] (h {:request-method :get :uri uri}))
        first-page (:body (get-page "/tags/clojure"))
        last-page (:body (get-page "/tags/clojure/page/5"))]
    (testing "the first page shows only its slice and the true total"
      (is (str/includes? first-page "nextjournal/markdown"))
      (is (not (str/includes? first-page "Babashka")))
      (is (str/includes? first-page "5 entries"))
      (is (str/includes? first-page "href=\"/tags/clojure/page/2\"")))
    (testing "the last page links back and declares its own canonical URL"
      (is (str/includes? last-page "Babashka"))
      (is (not (str/includes? last-page "nextjournal/markdown")))
      (is (str/includes? last-page "href=\"/tags/clojure/page/4\" rel=\"prev\""))
      (is (str/includes? last-page "Page 5 of 5"))
      (is (str/includes? last-page "<title>#clojure / Page 5 — Test Site</title>"))
      (is (str/includes? last-page
                         "href=\"https://example.com/tags/clojure/page/5\" rel=\"canonical\"")))
    (testing "related tags describe the whole tag, not the current page's slice"
      (is (str/includes? first-page "#workflow"))
      (is (str/includes? last-page "#workflow")))
    (testing "pages past the end are a 404"
      (is (= 404 (:status (get-page "/tags/clojure/page/6")))))))

(deftest month-listings-are-paginated
  ;; July 2026 has 3 example entries, all on the 4th: nextjournal-markdown,
  ;; hello-world, then the note, newest first.
  (let [h (app/make-app (assoc config :page-entries 1)
                        (atom (content/build-index config)))
        get-page (fn [uri] (h {:request-method :get :uri uri}))
        first-page (:body (get-page "/2026/jul"))
        last-page (:body (get-page "/2026/jul/page/3"))]
    (testing "the first page shows only its slice and the true total"
      (is (str/includes? first-page "nextjournal/markdown"))
      (is (not (str/includes? first-page "one thought")))
      (is (str/includes? first-page "3 entries"))
      (is (str/includes? first-page "href=\"/2026/jul/page/2\"")))
    (testing "the type summary describes the whole month, not the slice"
      (is (str/includes? first-page ">post</a>"))
      (is (str/includes? first-page ">note</a>"))
      (is (str/includes? first-page ">link</a>"))
      (is (str/includes? last-page ">post</a>"))
      (is (str/includes? last-page ">note</a>"))
      (is (str/includes? last-page ">link</a>")))
    (testing "the last page links back and declares its own canonical URL"
      (is (str/includes? last-page "href=\"/2026/jul/page/2\" rel=\"prev\""))
      (is (str/includes? last-page "Page 3 of 3"))
      (is (str/includes? last-page "<title>July 2026 / Page 3 — Test Site</title>"))
      (is (str/includes? last-page
                         "href=\"https://example.com/2026/jul/page/3\" rel=\"canonical\"")))
    (testing "pages past the end are a 404"
      (is (= 404 (:status (get-page "/2026/jul/page/4")))))))

(deftest day-listings-are-paginated
  ;; July 4, 2026 has the same 3 entries as the month (it's the only day
  ;; with content that month): nextjournal-markdown, hello-world, the note.
  (let [h (app/make-app (assoc config :page-entries 1)
                        (atom (content/build-index config)))
        get-page (fn [uri] (h {:request-method :get :uri uri}))
        first-page (:body (get-page "/2026/jul/4"))
        last-page (:body (get-page "/2026/jul/4/page/3"))]
    (testing "the first page shows only its slice"
      (is (str/includes? first-page "nextjournal/markdown"))
      (is (not (str/includes? first-page "one thought")))
      (is (str/includes? first-page "href=\"/2026/jul/4/page/2\"")))
    (testing "the last page links back and declares its own canonical URL"
      (is (str/includes? last-page "one thought"))
      (is (not (str/includes? last-page "nextjournal/markdown")))
      (is (str/includes? last-page "href=\"/2026/jul/4/page/2\" rel=\"prev\""))
      (is (str/includes? last-page "Page 3 of 3"))
      (is (str/includes? last-page "<title>July 4, 2026 / Page 3 — Test Site</title>"))
      (is (str/includes? last-page
                         "href=\"https://example.com/2026/jul/4/page/3\" rel=\"canonical\"")))
    (testing "pages past the end are a 404"
      (is (= 404 (:status (get-page "/2026/jul/4/page/4")))))))

(deftest static-assets
  (let [{:keys [status headers]} (GET "/css/style.css")]
    (is (= 200 status))
    (is (= "text/css" (get headers "Content-Type"))))
  (testing "the favicon trio resolves at the bare root, not just under /images"
    (let [{:keys [status headers body]} (GET "/favicon.svg")]
      (is (= 200 status))
      (is (= "image/svg+xml" (get headers "Content-Type")))
      ;; Served as XML, parsed strictly by browsers — malformed markup
      ;; (say, a "--" inside a comment) is a blank tab icon, not a nit.
      ;; parse-str is lazy; walking the whole tree is what makes bad
      ;; markup actually throw here.
      (is (pos? (count (xml-seq (xml/parse-str (slurp body)))))))
    (let [{:keys [status headers]} (GET "/favicon.ico")]
      (is (= 200 status))
      (is (= "image/x-icon" (get headers "Content-Type"))))
    (let [{:keys [status headers]} (GET "/apple-touch-icon.png")]
      (is (= 200 status))
      (is (= "image/png" (get headers "Content-Type")))))
  (testing "pages link assets by content-hashed URLs (cache busting)"
    (let [body (:body (GET "/"))]
      (is (re-find #"/css/style\.css\?v=[0-9a-f]{8}" body))
      (is (re-find #"/images/og\.png\?v=[0-9a-f]{8}" body))
      (is (re-find #"/favicon\.svg\?v=[0-9a-f]{8}" body))))
  (testing "a ?v= request is cacheable forever; a bare one keeps a TTL"
    (is (str/includes? (get-in (GET "/css/style.css" "v=abc12345") [:headers "Cache-Control"])
                       "immutable"))
    (is (not (str/includes? (get-in (GET "/css/style.css") [:headers "Cache-Control"])
                            "immutable"))))
  (testing "no path traversal"
    (is (not= 200 (:status (GET "/css/../../config/config.edn")))))
  (testing "the root carve-out serves three named files, not the root itself"
    ;; re-matches anchors, so a near-miss on either side of a carved-out
    ;; name is not a path the pattern can reach.
    (is (not= 200 (:status (GET "/xfavicon.ico"))))
    (is (not= 200 (:status (GET "/favicon.ico.bak"))))
    (is (not= 200 (:status (GET "/config.edn"))))))

(deftest theme-toggle
  (testing "every page's head runs the pre-paint script before first paint"
    (is (str/includes? (:body (GET "/")) "localStorage.getItem('theme')"))
    (is (str/includes? (:body (GET "/2026/jul/4/hello-world")) "localStorage.getItem('theme')")))

  (testing "the footer carries the toggle, hidden until its own script un-hides it"
    (let [{:keys [body]} (GET "/")]
      (is (str/includes? body "id=\"theme-toggle\""))
      (is (str/includes? body "class=\"theme-toggle\""))
      (is (re-find #"<button[^>]*class=\"theme-toggle\"[^>]*hidden" body))))

  (testing "all three icons ship in the DOM"
    (let [{:keys [body]} (GET "/")]
      (is (str/includes? body "class=\"i-light\""))
      (is (str/includes? body "class=\"i-dark\""))
      (is (str/includes? body "class=\"i-system\""))
      ;; decorative: the button's aria-label is the accessible name, so an
      ;; icon announcing itself as well would read the control out twice
      (doseq [icon ["i-light" "i-dark" "i-system"]]
        (is (str/includes? body (str "aria-hidden=\"true\" class=\"" icon "\""))))))

  (testing "the accessible name states the mode AND what pressing does"
    (let [{:keys [body]} (GET "/")]
      (is (str/includes? body "aria-label=\"Theme: system (follows your device). Click to switch to light.\""))
      ;; the handler relabels from the same map the markup rendered from,
      ;; so every mode it can reach has a name shipped with the page
      (doseq [label ["Click to switch to light" "Click to switch to dark"
                     "Click to switch to system"]]
        (is (str/includes? body label)))))

  (testing "an inner page carries the toggle too, not just the homepage"
    (let [{:keys [body]} (GET "/2026/jul/4/hello-world")]
      (is (str/includes? body "id=\"theme-toggle\""))
      (is (str/includes? body "class=\"i-light\"")))))

(deftest no-admin-http-surface
  (testing "there is no reindex endpoint — the server has no admin routes"
    (is (= 404 (:status (handler {:request-method :post
                                  :uri "/admin/reindex"
                                  :query-string "token=anything"}))))))
