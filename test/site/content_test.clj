(ns site.content-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [site.content :as content])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def config
  {:entry-types [:post :note :link :quote :release :tool]
   :content-path "example-content"})

(deftest frontmatter-parsing
  (testing "valid frontmatter"
    (let [{:keys [meta body]} (content/parse-frontmatter
                               ";;;\n{:type :link\n :tags [:a]}\n;;;\n\nHello *world*\n"
                               "test.md")]
      (is (= :link (:type meta)))
      (is (= [:a] (:tags meta)))
      (is (= "Hello *world*" body))))

  (testing "body may contain the delimiter"
    (let [{:keys [body]} (content/parse-frontmatter
                          ";;;\n{:type :link}\n;;;\nbefore\n;;;\nafter"
                          "test.md")]
      (is (= "before\n;;;\nafter" body))))

  (testing "no frontmatter at all is a bare entry: empty meta, body intact"
    (let [{:keys [meta body]} (content/parse-frontmatter "just text" "test.md")]
      (is (= {} meta))
      (is (= "just text" body))))

  (testing "unterminated frontmatter fails loudly"
    (is (thrown-with-msg? Exception #"Unterminated"
                          (content/parse-frontmatter ";;;\n{:type :link}\nno end" "test.md"))))

  (testing "invalid EDN fails loudly"
    (is (thrown-with-msg? Exception #"Invalid EDN"
                          (content/parse-frontmatter ";;;\n{:type\n;;;\nbody" "test.md"))))

  (testing "non-map frontmatter fails loudly"
    (is (thrown-with-msg? Exception #"must be an EDN map"
                          (content/parse-frontmatter ";;;\n:just-a-keyword\n;;;\nbody" "test.md")))))

(deftest yaml-frontmatter
  (testing "Obsidian property names map onto the entry model"
    (let [{:keys [meta body]} (content/parse-frontmatter
                               (str "---\n"
                                    "type: link\n"
                                    "link: https://example.com\n"
                                    "via: https://news.example.com\n"
                                    "tags:\n  - clojure\n  - web\n"
                                    "date: 2025-01-09\n"
                                    "---\n\nBody here\n")
                               "test.md")]
      (is (= :link (:type meta)))
      (is (= "https://example.com" (:link-url meta)))
      (is (= "https://news.example.com" (:link-via meta)))
      (is (= ["clojure" "web"] (vec (:tags meta))))
      (is (nil? (:date meta)) "the path is the date; a date property is workflow noise")
      (is (= "Body here" body))))

  (testing "quotes: author → :source, source → :source-url"
    (let [{:keys [meta]} (content/parse-frontmatter
                          "---\ntype: quote\nauthor: Ada Lovelace\nsource: https://example.com/notes\n---\nQ"
                          "test.md")]
      (is (= "Ada Lovelace" (:source meta)))
      (is (= "https://example.com/notes" (:source-url meta)))))

  (testing "blank properties are treated as absent"
    (let [{:keys [meta]} (content/parse-frontmatter
                          "---\ntype: link\nlink: https://example.com\nvia: \ntags:\n---\nB"
                          "test.md")]
      (is (nil? (:link-via meta)))
      (is (nil? (:tags meta)))))

  (testing "a single-string tag still becomes a collection"
    (let [{:keys [meta]} (content/parse-frontmatter
                          "---\ntags: solo\n---\nB"
                          "test.md")]
      (is (= ["solo"] (vec (:tags meta))))))

  (testing "canonical → :canonical-url, previously → :previous-urls"
    (let [{:keys [meta]} (content/parse-frontmatter
                          (str "---\n"
                               "canonical: https://elsewhere.example/post\n"
                               "previously:\n  - /old/path\n  - https://old.example.org/post\n"
                               "---\nB")
                          "test.md")]
      (is (= "https://elsewhere.example/post" (:canonical-url meta)))
      (is (= ["/old/path" "https://old.example.org/post"] (:previous-urls meta)))))

  (testing "a single-string previously still becomes a collection"
    (let [{:keys [meta]} (content/parse-frontmatter
                          "---\npreviously: /old/path\n---\nB"
                          "test.md")]
      (is (= ["/old/path"] (:previous-urls meta))))))

(deftest type-validation
  (testing "typo'd type cannot silently coin a new type"
    (is (thrown-with-msg? Exception #"Unknown entry type"
                          (content/check-type! config {:type :postt} "x.md"))))
  (testing "missing type defaults to :post — a bare entry is a post"
    (is (= :post (content/check-type! config {} "x.md"))))
  (testing "valid type passes"
    (is (= :quote (content/check-type! config {:type :quote} "x.md")))))

(deftest index-building
  (let [index (content/build-index config)]
    (testing "all example entries load, newest first"
      (is (= 9 (count (:entries index))))
      (is (= "/2026/jul/4/nextjournal-markdown" (:path (first (:entries index)))))
      (is (apply >= (map #(-> % :date :year) (:entries index)))))

    (testing "date comes from the path"
      (let [e (get (:by-path index) "/2026/jul/4/hello-world")]
        (is (= {:year 2026 :month 7 :day 4} (:date e)))
        (is (= :post (:type e)))))

    (testing "slug override via frontmatter"
      (is (some? (get (:by-path index) "/2025/nov/12/repl-driven")))
      (is (nil? (get (:by-path index) "/2025/nov/12/repl-driven-development"))))

    (testing "same-day entries all load"
      (is (= 3 (count (get (:by-day index) [2026 7 4])))))

    (testing "type and tag groupings"
      (is (= 3 (count (get (:by-type index) :post))))
      (is (= 1 (count (get (:by-type index) :note))))
      (is (= 2 (count (get (:by-type index) :link))))
      (is (= 1 (count (get (:by-type index) :quote))))
      (is (= 1 (count (get (:by-type index) :release))))
      (is (= 1 (count (get (:by-type index) :tool))))
      (is (= 5 (count (get (:by-tag index) :clojure)))))

    (testing "tags are normalized to keywords"
      (is (every? keyword? (mapcat :tags (:entries index)))))

    (testing "neighbors are linked newest-first"
      (let [newest (first (:entries index))
            oldest (peek (:entries index))]
        (is (nil? (:newer newest)))
        (is (some? (:older newest)))
        (is (nil? (:older oldest)))
        (is (some? (:newer oldest)))))

    (testing "drafts load separately and never join entries"
      (is (= 1 (count (:drafts index))))
      (is (:draft? (get (:drafts index) "an-idea-brewing")))
      (is (not-any? :draft? (:entries index))))

    (testing "pages load"
      (is (= "About" (:title (get (:pages index) "about"))))
      (is (= "Privacy" (:title (get (:pages index) "privacy")))))))

(deftest obsidian-native-entries
  (let [dir (str (Files/createTempDirectory "content-test" (into-array FileAttribute [])))
        day (io/file dir "2026" "07" "10")]
    (.mkdirs day)
    (spit (io/file day "My Great Idea.md") "---\ntags:\n  - clojure\n  - Data Science\n  - '???'\n---\nSee [[Everything fails]] for more.")
    (spit (io/file day "Everything fails.md") "---\ntype: quote\nauthor: W. Vogels\n---\nEverything fails, all the time.")
    (let [index (content/build-index (assoc config :content-path dir))
          post (get (:by-path index) "/2026/jul/10/my-great-idea")
          quote (get (:by-path index) "/2026/jul/10/everything-fails")]
      (testing "filename is the title; slug is slugified from it"
        (is (= "My Great Idea" (:title post)))
        (is (= :post (:type post)) "no type property means post"))
      (testing "a hand-written tag reads through the slug rule; one that slugs to nothing is dropped"
        (is (= #{:clojure :data-science} (:tags post))))
      (testing "a quote is titled by its filename, like any other type"
        (is (= "Everything fails" (:title quote)))
        (is (= "W. Vogels" (:source quote))))
      (testing "entries carry the wikilink target map"
        (is (= "/2026/jul/10/everything-fails"
               (get (:wikilinks post) "everything fails")))))))

(deftest previous-url-redirects
  (let [dir (str (Files/createTempDirectory "content-test" (into-array FileAttribute [])))
        day (io/file dir "2026" "07" "10")]
    (.mkdirs day)
    (spit (io/file day "moved.md")
          (str "---\n"
               "previously:\n"
               "  - /blog/moved\n"
               "  - https://example.com/notes/moved/\n"
               "  - https://elsewhere.example.org/moved\n"
               "---\nBody"))
    (let [index (content/build-index (assoc config
                                            :content-path dir
                                            :base-url "https://example.com"))]
      (testing "bare paths and own-host absolutes redirect (trailing slash dropped)"
        (is (= {"/blog/moved" "/2026/jul/10/moved"
                "/notes/moved" "/2026/jul/10/moved"}
               (:redirects index))))
      (testing "a foreign-host previous URL is the old domain's redirect, not ours"
        (is (not-any? #(str/includes? % "elsewhere") (keys (:redirects index))))))))

(deftest previous-url-collisions
  (let [make (fn [files]
               (let [dir (str (Files/createTempDirectory "content-test" (into-array FileAttribute [])))
                     day (io/file dir "2026" "07" "10")]
                 (.mkdirs day)
                 (doseq [[name content] files]
                   (spit (io/file day name) content))
                 (assoc config :content-path dir :base-url "https://example.com")))]
    (testing "two entries claiming the same previous URL fail indexing"
      (is (thrown-with-msg? Exception #"claimed by both"
                            (content/build-index
                             (make {"a.md" "---\npreviously: /old\n---\nA"
                                    "b.md" "---\npreviously: /old\n---\nB"})))))
    (testing "a previous URL that is a live entry URL fails indexing"
      (is (thrown-with-msg? Exception #"live entry URL"
                            (content/build-index
                             (make {"a.md" "---\npreviously: /2026/jul/10/b\n---\nA"
                                    "b.md" "just a body"})))))
    (testing "a previous URL with no path fails indexing"
      (is (thrown-with-msg? Exception #"no path"
                            (content/build-index
                             (make {"a.md" "---\npreviously: https://example.com\n---\nA"})))))))

(deftest workflow-properties
  (testing "unquoted YAML date parses as the authored calendar date (UTC, not system zone)"
    (let [wp (content/workflow-properties "---\ndate: 2026-07-15\n---\nB" "t.md")]
      (is (= (java.time.LocalDate/of 2026 7 15) (:date wp)))
      (is (false? (:publish wp)))))

  (testing "quoted date string"
    (let [wp (content/workflow-properties "---\ndate: \"2026-07-15\"\n---\nB" "t.md")]
      (is (= (java.time.LocalDate/of 2026 7 15) (:date wp)))))

  (testing "Obsidian \"Date & time\" property"
    (let [wp (content/workflow-properties "---\ndate: 2026-07-15T09:30\n---\nB" "t.md")]
      (is (= (java.time.LocalDate/of 2026 7 15) (:date wp)))))

  (testing "absent frontmatter"
    (is (= {:date nil :publish false}
           (content/workflow-properties "just text" "t.md"))))

  (testing "EDN frontmatter carries no workflow properties"
    (is (= {:date nil :publish false}
           (content/workflow-properties ";;;\n{:type :post :date \"2026-07-15\" :publish true}\n;;;\nbody" "t.md"))))

  (testing "publish: true/false/absent"
    (is (true? (:publish (content/workflow-properties "---\npublish: true\n---\nB" "t.md"))))
    (is (false? (:publish (content/workflow-properties "---\npublish: false\n---\nB" "t.md"))))
    (is (false? (:publish (content/workflow-properties "---\ntags: []\n---\nB" "t.md")))))

  (testing "a garbage date fails loudly instead of silently publishing under today"
    (is (thrown-with-msg? Exception #"Unparseable date"
                          (content/workflow-properties "---\ndate: July 15, 2026\n---\nB" "t.md")))))

(deftest broken-content
  (let [dir (str (Files/createTempDirectory "content-test" (into-array FileAttribute [])))]
    (testing "unknown type in a dated entry fails indexing"
      (let [f (io/file dir "2026" "01" "05")]
        (.mkdirs f)
        (spit (io/file f "bad.md") ";;;\n{:type :essay}\n;;;\nbody")
        (is (thrown-with-msg? Exception #"Unknown entry type"
                              (content/build-index (assoc config :content-path dir))))))))
