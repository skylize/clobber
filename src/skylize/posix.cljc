(ns skylize.posix
  (:require [skylize.poj :as poj]
            [skylize.poj.util :as p-util]
            [skylize.poj.macro :refer [defcurry]]
            :reload-all))

(def universal-char-classes ["[:alnum:]" "[:cntrl:]" "[:lower:]" "[:space:]"
                             "[:alpha:]" "[:digit:]" "[:print:]" "[:upper:]"
                             "[:blank:]" "[:graph:]" "[:punct:]" "[:xdigit:]"])


(def bracket-escape-chars ["\\" "]" "-"])

(defcurry btwn-strs
  "Parse any value between `left-str` and `right-str`."
  [left-str right-str score]
  (poj/btwn
   (poj/string left-str)
   (poj/string right-str)
   (poj/not-str right-str)
   score))

(defn char-class [score]
  ((poj/tossup
   (fn [t] (str "failed to find character class at pos " (:pos t)))
   (fn [w] {:type :char-class :val w})
   (btwn-strs "[:" ":]"))
   score))

(defn char-equiv [score]
  ((poj/tossup
    (fn [t] (str "failed to find character class at pos " (:pos t)))
    (fn [w] {:type :char-equiv :val w})
    (btwn-strs "[=" "=]"))
   score))

(defn collating-symb [score]
  ((poj/tossup
    (fn [t] (str "failed to find character class at pos " (:pos t)))
    (fn [w] {:type :collating-symb :val w})
    (btwn-strs "[." ".]"))
   score))

(defcurry escaped-char
  "Parse any character that has been escaped by a backslash. If `strict` is truth, then fail if the character is not found in the list of `meta-chars`"
  [{:keys [meta-chars strict]
    :or {meta-chars [] strict false}} state]
  (let [parse (if (:loss state) state
                  (poj/after (poj/string "\\")
                             (if strict
                               (poj/any-of (map poj/string meta-chars))
                               poj/char-str)))
        format (poj/tossup
                (fn [t] (str "Failed to find valid escaped char at pos "
                             (:pos t)))
                (fn [w] {:type  :escaped
                         :value w}))]
    ((format parse) state)))

(comment
  (escaped-char {:strict false} {:pos 3 :source "bcd\\e"})
  (escaped-char nil {:pos 0 :source "bcd\\e"})
  (escaped-char {:strict true} {:pos 3 :source "bcd\\e"})
  (escaped-char {:strict true :meta-chars ["e"]} {:pos 3 :source "bcd\\e"})
  )

(defn negate-pbracket
  [score]
  (poj/tossup
   (fn [t] (str "failed to find negation at pos " (:pos t)))
   (fn [w] {:type :negation :val w})
   (poj/string "^")
   score))

(def close-brack
  (poj/tossup
   (fn [t] (str "failed to find \"]\" at pos " (:pos t)))
   (fn [w] {:type :close-bracket :val w})
   (poj/string "]")))

(def bracket-char-str
  (poj/tossup))

(defcurry escaped-brack-char
  [{:keys [strict]} score]
  (escaped-char
   {:strict     strict-escape
    :meta-chars bracket-escape-chars}
   score))

(defn bracket-char
  [{:keys [strict-escape]} score]
  (poj/any-of [(escaped-char
                {:strict     strict-escape
                 :meta-chars bracket-escape-chars})
               char-class
               char-equiv
               collating-symb
               poj/char-str]
              score))

(comment
  (bracket-char {:strict-escape false :meta-chars bracket-escape-chars}
                {:pos 3 :source "bcd[:foo:]"})
  (bracket-char {:strict-escape false :meta-chars bracket-escape-chars}
                {:pos 2 :source "bcd[:foo:]"})
  )

;; (defn char-range
;;   [score]
;;   ((poj/sep-by (poj/string "-") bracket-char) score))

;; (def pbracket-body
;;   (let [pbracket-main-body (poj/many
;;                             (poj/any-of [char-range
;;                                        bracket-char]))]
;;     (poj/any-of [(poj/in-seq [close-brack
;;                           pbracket-main-body])
;;                pbracket-main-body])))

;; (defn pattern-bracket
;;   "Parse a posix regular expression bracket expression."
;;   [{:keys [meta-chars
;;            strict-escape]
;;     :or {meta-chars ["[" "]" "\\" "-" "^"]
;;          strict-escape false}}
;;    score]
;;   (btwn-strs "[" "]"
;;              (poj/any-of (comp pbracket-body negate-pbracket)
;;                        pbracket-body)
;;              score))
