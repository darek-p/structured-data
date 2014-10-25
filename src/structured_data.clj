(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
   (Math/pow xx xx))
  )

(defn spiff [v]
  (+ (get v 0 0) (get v 2 0)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
   (let [[x y z] v]
     (+ x z)))

(defn point [x y]
  [x y])

;(defn bottom-left [x y]
;  [x y])

;(defn top-right [x y]
;  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [p1 p2] point]
    (and (<= x1 p1 x2) (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1][x2 y2]] outer [[xx1 yy1][xx2 yy2]] inner]
    (and (<= x1 xx1 xx2 x2) (<= y1 yy1 yy2 y2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [mySecond (fn [x] (get x 1))]
    (map mySecond collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
   (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (== (count (set a-seq)) (count a-seq))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union

         (map (fn [x] (set (:authors x))) books))
)

(defn all-author-names [books]
  (set (map :name (authors books))
  ))

(defn author->string [author]
  (let [n (:name author) by (:birth-year author) dy (:death-year author)]
    (if (= by nil) (str n) (str n " (" by " - " dy ")"))))
  ;(let [n (:name author) by (:birth-year author)
  ;      dy (:death-year author)]
  ;    (if (not (== by nil)) (str n "( " by " - " dy " )") (str n))
  ;  ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond
     (== (count books) 0) (str "No books.")
     (== (count books) 1) (str "1 book. " (book->string (first books)) ".")
     :else (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
   )
  )

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [x] (= (:name x) name)) authors)))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
