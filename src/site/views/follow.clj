(ns site.views.follow
  "The /follow page: static prose from pages/follow.md with the email follow
  form dropped in after its opening blocks. The prose is editable in the
  content repo like any other page; the form is code (it POSTs to
  Buttondown)."
  (:require [site.markdown :as markdown]
            [site.views.components :as c]
            [site.views.layout :as layout]))

(defn follow-page
  "config and the pages/follow.md page map (may be nil) → the /follow page.
  The form follows the page's third rendered block — the lead-in, the
  heading over the offer, and the sentence that says to sign up below —
  so the ask is what the reader has just finished when they reach the
  field. Everything after it, the reassurance and the feed section,
  trails the form.

  Three is a position in that page, so reshaping its opening means
  revisiting the number."
  [config page]
  (let [title (or (:title page) "Follow")
        blocks (when (:body page)
                 (rest (markdown/render (:body page) nil {:anchors? true})))
        [intro detail] (split-at 3 blocks)]
    (layout/page config {:title title :path "/follow"}
                 [:article.article.prose
                  [:h1 title]
                  intro
                  (c/follow-form config {:id "follow-page-email"})
                  detail])))
