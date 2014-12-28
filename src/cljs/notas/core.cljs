(ns notas.core
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent defcomponentk]]
            [om-tools.dom :as dom :include-macros true]))

(defonce app-state (atom {:rows 12 :selected-notes {} :selected-cells {}}))

(def all-notes ["A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#"])
 
(def note-to-pos {"A" 0
                  "A#"1 
                  "B" 2
                  "C" 3
                  "C#" 4
                  "D" 5
                  "D#" 6
                  "E" 7
                  "F" 8
                  "F#" 9 
                  "G" 10 
                  "G#" 11})

(def guitar-string-notes {1 "E"
                          2 "A"
                          3 "D"
                          4 "G"
                          5 "B"
                          6 "E"})

(def guitar-string-to-pos {1 (get note-to-pos "E")
                           2 (get note-to-pos "A")
                           3 (get note-to-pos "D")
                           4 (get note-to-pos "G")
                           5 (get note-to-pos "B")
                           6 (get note-to-pos "E")})

(defn string-num-to-note [c]
  (let [norm-index (rem c 12)
        note (get guitar-string-notes norm-index)]
    note))

(defn string-pos-to-note [c fret]
  (let [string-note-index (get guitar-string-to-pos c)
        norm-index (rem (+ string-note-index fret) 12)
        note (get all-notes norm-index)]
    note))

(defn fret-classes [r]
  (if (contains? #{3 5 7 9 12 15 17 19} r)
    "fret dotted-fret"
    "fret"))

(defn toggle-selected-note [selected-notes note-name]
  (om/transact! selected-notes note-name not))

(defn toggle-selected-cell [selected-cells c r]
  (om/transact! selected-cells [c r] not))

(defn render-guitar-string-notes []
  (dom/tr {:class "fret-notes"}
          (for [c (range 7)
                :let [first-col? (zero? c)]]
            (if first-col?
              (dom/td {:class "row-index"} "")
              (dom/td {:class "note-name"}
                      (dom/span (string-num-to-note c)))))))

(defn render-guitar-note [selected-notes selected-cells c r note-name]
  (let [note-selected? (get selected-notes note-name false)
        cell-selected? (get-in selected-cells [c r] false)
        note-selected-class (if note-selected? "class-selected" "")
        cell-selected-class (if cell-selected? "selected" "")
        classes ["note" note-selected-class cell-selected-class]
        classes-str (clojure.string/join " " classes)]
    (dom/td {:class classes-str}
            (dom/button
              {:on-click (fn [_]
                           (toggle-selected-note selected-notes note-name)
                           (toggle-selected-cell selected-cells c r))}
              note-name))))

(defn render-fret [r selected-notes selected-cells]
  (dom/tr {:class (fret-classes r)}
          (for [c (range 7)
                :let [first-col? (zero? c)
                      note-name (string-pos-to-note c r)]]
            (if first-col?
              (dom/td {:class "row-index"} r)
              (render-guitar-note selected-notes selected-cells c r note-name)))))

(defcomponentk guitar [[:data rows selected-notes selected-cells :as state]]
  (render [_]
          (dom/table {:class "guitar"}
            (for [r (range (inc rows))
                  :let [first-row? (zero? r)]]
              (if first-row?
                (render-guitar-string-notes) 
                (render-fret r selected-notes selected-cells))))))

(defn main []
  (om/root guitar app-state
    {:target (. js/document (getElementById "app"))}))
