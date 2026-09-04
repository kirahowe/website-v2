(ns site.markdown-test
  "Heading ids and section anchors, at the render level: the slug rule,
  uniqueness within one body, and that the self-link is opt-in."
  (:require [clojure.test :refer [deftest is testing]]
            [site.markdown :as markdown]))

(defn- headings [hiccup]
  (filter #(and (vector? %) (#{:h1 :h2 :h3 :h4 :h5 :h6} (first %)))
          (rest hiccup)))

(deftest heading-ids
  (let [rendered (markdown/render
                  (str "## Setup\n\ntext\n\n### Setup\n\n"
                       "## Why *this* & \"that\"? (2026)\n\n"
                       "## <sup>1</sup> Raw html\n\n"
                       "## [[Some Note]] link\n\n"
                       "## ???"))]
    (testing "ids are the site's slug of the heading text, unique in order"
      (is (= ["setup" "setup-2" "why-this-that-2026" "1-raw-html"
              "some-note-link" "section"]
             (map #(-> % second :id) (headings rendered)))))
    (testing "without :anchors? a heading holds only its text"
      (is (not-any? #(and (vector? (peek %)) (= :a.anchor (first (peek %))))
                    (headings rendered))))))

(deftest section-anchors
  (let [rendered (markdown/render "## Setup\n\n## Setup" nil {:anchors? true})]
    (is (= [[:a.anchor {:href "#setup" :aria-label "Link to this section"} "#"]
            [:a.anchor {:href "#setup-2" :aria-label "Link to this section"} "#"]]
           (map peek (headings rendered))))))
