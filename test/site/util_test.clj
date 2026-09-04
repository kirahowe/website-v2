(ns site.util-test
  "The slug rule is the one derivation the site never lets drift: an
  entry's URL is slugify(filename), a section's fragment is slugify(its
  heading), and a suggested tag is slugify(the suggestion). The admin app
  carries byte-for-byte ports (web/src/format.ts, worker/src/enrichment.ts,
  enrich/src/enrich/util.clj) so it can predict the URL a publish will
  land at; this table is the contract those ports must match."
  (:require [clojure.test :refer [deftest is]]
            [site.util :as util]))

(deftest slugify-contract
  (is (= "my-post-title" (util/slugify "My Post Title!")))
  ;; every run of anything but ASCII letters and digits is one hyphen…
  (is (= "gmail-protonmail" (util/slugify "GMail → ProtonMail")))
  (is (= "2-make-an-uberjar" (util/slugify "2. Make an uberjar")))
  (is (= "why-this-that-2026" (util/slugify "Why this & \"that\"? (2026)")))
  ;; …including a curly apostrophe, which splits the word rather than vanishing
  (is (= "it-s-a-great-time" (util/slugify "It’s a Great Time")))
  ;; accented letters are not ASCII, so they drop with the rest
  (is (= "caf-n-code" (util/slugify "Café Ünïcode")))
  ;; nothing leading or trailing survives
  (is (= "leading-trailing" (util/slugify "  Leading & trailing --- ")))
  (is (= "" (util/slugify "???")))
  (is (= "" (util/slugify nil))))
