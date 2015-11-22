(ns mw.mwm
  (:refer-clojure :exclude [defn])
  (:require
   [clojure.pprint :as pp]
   [clojure.walk :as walk]
   )
  (:gen-class)
  )

;; TODO:
;;
;; TODO: do I handle (map :email entities) ?
;; No, you need to write (map #(:foo %) args))
;;
;; TODO: check how (get-in track [:speaker :first]) works (and update-in, assoc-in)
;;
;; extend to :keys DONE!!!!
;; (let [{:keys [a b]} {:a 10, :b 20, :c 39}] [a b])  =>  [10 20]
;;
;; How? They are expanded into (clojure.core/get map :a) Should I replace them too?
;; but if I only look at (get with last arg keyword, it should work!
;;
;; The problem is that (get map key) is also valid for vectors etc.... Do I want to extend to them?
;; http://conj.io/store/v1/org.clojure/clojure/1.7.0/clj/clojure.core/get
;;
;; (pp/pprint (macroexpand '(let [{:keys [a b]} {:a 10, :b 20, :c 39}] [a b])))
;; (let*
;;  [map__10519
;;   {:a 10, :b 20, :c 39}
;;   map__10519
;;   (if
;;    (clojure.core/seq? map__10519)
;;    (clojure.lang.PersistentHashMap/create
;;     (clojure.core/seq map__10519))
;;    map__10519)
;;   a
;;   (clojure.core/get map__10519 :a)
;;   b
;;   (clojure.core/get map__10519 :b)]
;;  [a b])
;; nil
;; fogbugz-stats.core> 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I hate nil:s, and I do not expect them unless I ask for them
;;; function that returns nil should have name ending with ?
;;; (:foo x) assumes :foo exits unless you write (:foo? x)
;;;
;;; I will fix this by implementing my own defn
;;; (or should is it better to do at "(fn" level?
;;; For each argument whose parameter name not ending with ?, add
;;; {:pre [(some? arg1) (some? arg2) ...]
;;; if name not ending with '?' add  THIS MEANS I CANNOT CHECK IT FOR EMBEDDED '(FN' SINCE NO NAME
;;; {:post [(some? %)]
;;; Only add if no :pre or :post

;;  (def f '(defn foo "comment" [x] {:pre [(some? x)]  :post [(some? %)]} (:foo (:bar x))))
;;  (macroexpand f)
;;   (def foo (clojure.core/fn ([x] {:pre [(some? x)], :post [(some? %)]} (:foo (:bar x)))))

;; (mwm/defn2 bar "no nils" ([x] (:foo (:bar x))) ([y z] (:gegga y)))
;; (mwm/defn2 bar ":gegga might be nil" ([x] (:foo (:bar x))) ([y z] (:gegga? y)))
;; (mwm/defn2 foo? "can return nil" ([x] (:foo (:bar x))) ([y z] (:gegga y)))

;;; true if (:keyword ??)
(clojure.core/defn- is-keyword-get? [x]
  (and (list? x)
       (= 2 (count x))
       (keyword? (first x))))

(clojure.core/defn- is-keyword-get-get? [x]
  (and (list? x)
       (= 3 (count x))
       (= 'clojure.core/get (first x))
       (keyword? (last x))))

(clojure.core/defn- is-let? [x]
  (and (list? x)
       (< 2 (count x))
       (= 'clojure.core/let (first x))))



;;; wrap all (:xxx yyy) calls and make sure result is non-nil
;;; this must be called using postwalk, since (:ccc ???) is in the result, and then I would expand that again
(clojure.core/defn- wrap-get-2 [x]
  (if (is-keyword-get? x)
    (let [keyword1 (first x)
          ;; (str) better than (name) + (name-space), but need to remove ':'
          keyword-as-string-raw (str keyword1)
          ;; todo: do we have a bug here and do not hande ::foo?????
          keyword-as-string (.substring keyword-as-string-raw 1)]   
      (if (not= \? (last keyword-as-string))
        `(clojure.core/let [res# ~x] (assert (not (nil? res#)) ~(str keyword-as-string-raw " is nil")) res#) 
        ;; we have to remove the final ? in :foo? since just an annotation
        (let [name2 (.substring keyword-as-string 0 (- (count keyword-as-string) 1))
              keyword2 (keyword name2)]
          (list keyword2 (second x)))
        ))
    x))

;;; (get map :foo) => (:foo map)
(clojure.core/defn- remove-get-3 [x]
  (if (is-keyword-get-get? x)
    (list (last x) (second x))
    x))



;;; return true if names ends with ?
(clojure.core/defn- q? [name]
  ;; (str) in case we get a keyword
  (= \? (last (str name))))

;;; ignore this argument
(clojure.core/defn- ignore-arg? [name]
  (or (q? name) (keyword? name) (= "&" (str name))))


;;; return true of body contains recur
;;; if recur exists, we will skip the :post
(clojure.core/defn- recur? [fun]
  (some #(and (seq? %)(= 'recur (first %)))
        (tree-seq seq? rest fun)))

;;; add :post unless allowed-to-return-nil
;;; add :pre for each argument whose name not ending with ?
(clojure.core/defn- build-pre-post [args no-post]
  (let [pre (into [] (for [arg (filter (complement ignore-arg?) args)] `(not (nil? ~arg))))
        pre2 (if (> (count pre) 0) {:pre pre} {})
        post (if no-post {} {:post [#(not (nil? %))]})
        ]
    (conj pre2 post)))


;;; add {:pre :post} map unless prepost-map already exists
(clojure.core/defn- add-pre-post [clause no-post]
  (let [args (first clause)
        prepost (map? (second clause))
        rst (nthrest clause 1)]
    ;; make sure map is not the only part of the body, it might be that the function returns a map
    (if (and prepost (> (count rst) 1))
      clause
      (cons args (cons (build-pre-post args no-post) rst)))))
            
;;; just like defn, except that it hates nils in (:xxx ??) lookups and
;;; in function arguments and function return values
;;; argument names ending with ? and function names ending with ? can be nil
;;;
;;; comment disappears when macroexpanding, but (doc XXX) works
(defmacro mw.mwm/defn [& body]
  ;; expand first in order to make all bodies look the same, regardless of one or more clauses
  ;; macroexpand instead of macroexpand-1 since we want to handle :keys
  (let [defun (macroexpand-1 (cons 'clojure.core/defn body))
        [c1 name [c2 & clauses]] defun
        ;; expand away let mostly, in order to handle :keys in let
        clauses1 (for [clause clauses] (walk/postwalk macroexpand clause))
        ;; _ (pp/pprint clauses1)
        clauses2 (for [clause clauses1] (walk/postwalk #(wrap-get-2 (remove-get-3 %)) clause))
        no-post (q? name)
        ;; disallow :post if recur occurs in clause
        ;; or wrap using loop, but :pre then only be run once?
        ;; http://dev.clojure.org/jira/browse/CLJ-1475
        clauses3 (for [clause clauses2] (add-pre-post clause (or no-post (recur? clause))))
        res `(~c1 ~name (~c2 ~@clauses3))]
    res))






;; (def ff1 '(mwm/defn2 find-and-slurp
;;   "Search and slurp for file in this dir, and all parents until found. Throw exception if not found. Max 50 levels"
;;   ([filename] (find-and-slurp filename 50 ""))
;;   ([filename level prefix]
;;    (if (< level 0) 
;;      (throw (Exception. (str "Not found: " filename)))
;;      (if (.exists (clojure.java.io/as-file (str prefix filename)))
;;        (slurp (str prefix filename))
;;        (recur filename (- level 1) (str "../" prefix))))))
;;   )

;; (def ff2 '(mwm/defn2 xml-keep-tag-content
;;   "Keep recursive map and only keep :tag and :content as key and value.
;;    The resulting structure looks like json."
;;   ([pxml]
;;    (if (:tag? pxml)
;;      (xml-keep-tag-content (:tag pxml)(:content pxml))
;;      pxml))
;;   ([tag content]
;;    {tag
;;     (if (seq? content)
;;       (let [res (for [pxml content] (xml-keep-tag-content pxml))]
;;         ;; use vector and remove singletons to make structure clearer
;;         (if (> (count res) 1) (into [] res) (first res)))
;;       content)}))
;; )

;; (def ff3 '(mwm/defn2 -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))
;; )

;; (def ff4 '(mwm/defn2 -main?
;;   "I don't do a whole lot ... yet."
;;   [& args?]
;;   (println "Hello, World!"))
;; )

;; (def ff5 '(mwm/defn2 api-xml [config]
;;   (let [res @(http/get (:url config) {})
;;         {:keys [status error]} res]
;;     (if error (error "Failed, exception is " error res))
;;     (:body res)
;;     )))

;; (def ff6 '(mwm/defn2 api-xml? [config]
;;   (let [res @(http/get (:url config) {})
;;         ;;{:keys [status error]} res
;;         ]
;;     ;; (if error (error "Failed, exception is " error res))
;;       ;; (:body res)
;;     res
;;     )))

;; (def ff7 '(mwm/defn2 keys [config]
;;             (let [{:keys [a b]} config
;;                   foo (:foo config)]
;;               [a b])))


;; (def ff8 '(mwm/defn2 keys [config]
;;             (let [x (:foo config)]
;;               [x x])))

;; (def ff9 '(mwm/defn2 keys [config]
;;             (let [x (:foo config)]
;;               [x x])))

;; (def ff10 '(mwm/defn2 api-xml [config]
;;   (let [res @(http/get (:url config) {})
;;         {:keys [status error?]} res
;;         ]
;;     (if error? (error "Failed, exception is " error? res))
;;     (:body res)
;;     )))

;; ;; note that error? refers to field :error
;; ;; the ? is only there to allow nil
;; (def ff10 '(mwm/defn2 api-xml [config]
;;   (let [res @(http/get (:url config) {})
;;         {:keys [status error?]} res
;;         ]
;;     (if error? (error "Failed, exception is " error? res))
;;     (:body res)
;;     )))

;;(def ff12 '(mwm/defn2 bar [args] (map :foo args)))
