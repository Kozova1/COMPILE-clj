(ns lisp-game-cljs.lang
  (:require [clojure.string :as str]
            [lisp-game-cljs.game :as game]))

(def regs
  (atom {:a 0
         :b 0
         :cond false}))
(def output (atom ""))
(def current-line (atom 0))

(def marks (atom {}))

(def commands
  #{"(" "COPY" "DEBUG" "MOVE" "CLEAR" "TJUMP" "JUMP" "FJUMP" "ISZERO" "MARK" "ADD" "SUB" "NOT" "INC" "DEC"})

(defn set-line     [linum] (reset! current-line linum))
(defn reset-line   []      (set-line 0))
(defn bump-line    []      (swap! current-line inc))
(defn reset-output []      (reset! output ""))

(defn scan-commands [vcode]
  (for [line vcode]
    (let [vline (str/split line #" ")
          vcmd (first vline)]
      (if (contains? commands vcmd)
        line
        (throw (js/Error. "SYNTAX-ERROR"))))))

(defn set-reg
  [reg val]
  (cond
    (not (contains? (set (keys @regs)) reg))
    (throw (js/Error. "INVALID-REGISTER"))
    (and (not= reg :cond) (boolean? val))
    (throw (js/Error. "BOOL-VALUE-WRONG-REG"))
    (and (not (contains? #{:a :b} reg)) (int? val))
    (throw (js/Error. "NUMERIC-VALUE-WRONG-REG"))
    (string? val)
    (throw (js/Error. "STRING-VALUE-WRONG-REG"))
    :else
    (swap! regs #(assoc % reg val))))

(defn get-reg
  [reg]
  (reg @regs))

(defn debug-println [& to-print]
  (reset! output (str "\n" to-print)))

(declare clear-reg)

(defn copy-reg [src dst] (set-reg dst (get-reg src)))
(defn copy-val [val dst] (set-reg dst val))
(defn reset-all-regs []
  (doseq [reg @regs]
    (clear-reg (get reg 0))))
(defn clear-reg [reg] (cond
                        (= reg :a)
                        (set-reg :a 0)
                        (= reg :b)
                        (set-reg :b 0)
                        (= reg :cond)
                        (set-reg :cond false)))

(declare resolve-mark jump exec-line numeric? rec-exec-line reset-marks)

(defn add-mark
  [mark]
  (reset! marks (merge @marks mark)))

(defn reset-marks
  []
  (reset! marks {}))

(defn scan-for-marks
  [vcode]
  (doseq [line vcode]
    (let [vline (str/split (str/trim line) #" ")
          vkey (get vline 1)
          vlinum @current-line]
      (bump-line)
      (if (= (get vline 0) "MARK")
        (add-mark {(keyword (.toLowerCase vkey))
                   vlinum})
        nil)
      nil)))

(defn verify-jumps
  [vcode]
  (doseq [line vcode]
    (let [vline (str/split (str/trim line) #" ")
          vcmd (get vline 0)]
      (if (= "JUMP" vcmd)
        (if-not (contains? (keys @marks) (-> vline
                                             second
                                             .toLowerCase
                                             keyword))
          (throw (js/Error. "JUMP-LABEL-MISSING"))
          nil)
        nil))))

(defn rec-exec-line [input]
  (loop []
    (exec-line (str/trim (get input @current-line)))
    (bump-line)
    (if-not (str/blank? (get input @current-line)) (recur) nil)))

(defn exec-string
  [code]
  (let [vcode (str/split-lines code)]
    (reset-line)
    (reset-all-regs)
    (reset-marks)
    (scan-commands vcode)
    (scan-for-marks vcode)
    (reset-line)
    (verify-jumps vcode)
    (rec-exec-line vcode)))

(defn string-bool [x]
  (case x
    "false"
    false
    "true"
    true
    nil))

(defn exec-line
  [input]
  (let [vinput (str/split input #"\s")
        vcmd (first vinput)]
    (case vcmd
      "("
      nil
      "MARK"
      nil ; Needs to be {:mark-name mark-line}
      "COPY"
      (doall ((constantly nil)
              (let [val (.toLowerCase (second vinput))
                    dst (keyword (.toLowerCase (get vinput 2)))]
                (cond
                  (numeric? val)
                  (copy-val (js/parseInt val) dst)
                  (or (= val "true") (= val "false"))
                  (copy-val (string-bool val) dst)
                  :else
                  (copy-reg (keyword val) dst)))))
      "CLEAR"
      ((constantly nil) (clear-reg
                         (keyword (.toLowerCase (second vinput)))))
      "NOT"
      ((constantly nil) (set-reg :cond (not (get-reg :cond))))
      "TJUMP"
      (if (get-reg :cond) (set-line
                           ((keyword (.toLowerCase (second vinput))) @marks)) nil)
      "FJUMP"
      (if-not (get-reg :cond) (set-line
                               ((keyword (.toLowerCase (second vinput))) @marks)) nil)
      "JUMP"
      (set-line
       ((keyword (.toLowerCase (second vinput))) @marks))
      "DEBUG"
      ((constantly nil) (debug-println
                         (str "VALUE OF REGISTER "
                              (second vinput) " IS "
                              (get-reg (keyword (.toLowerCase (second vinput)))))))
      "ISZERO"
      (let [reg (keyword (.toLowerCase (second vinput)))]
        (if (= (get-reg reg) 0)
          (set-reg :cond true)
          (set-reg :cond false))
        nil)
      "INC"
      ((constantly nil)
       (if (#{"A" "B"} (second vinput))
         (set-reg (keyword (.toLowerCase (second vinput))) (inc (get-reg (keyword (.toLowerCase (second vinput))))))))
      "DEC"
      ((constantly nil)
              (if (#{"A" "B"} (second vinput))
                (set-reg (keyword (.toLowerCase (second vinput))) (dec (get-reg (keyword (.toLowerCase (second vinput))))))))

      "ADD"
      ((constantly nil)
              (cond
                (numeric? (second vinput))
                (let [reg (keyword (.toLowerCase (get vinput 2)))]
                  (copy-val (+ (get-reg reg)
                               (js/parseInt (second vinput))) reg))
                :else
                (let [sub (keyword (.toLowerCase (second vinput)))
                      dst (keyword (.toLowerCase (get vinput 2)))]
                  (copy-val (+ (get-reg dst) (get-reg sub)) dst))))
      "SUB"
      ((constantly nil)
              (cond
                (numeric? (second vinput))
                (let [reg (keyword (.toLowerCase (get vinput 2)))]
                  (copy-val (- (get-reg reg)
                               (js/parseInt (second vinput))) reg))
                :else
                (let [sub (keyword (.toLowerCase (second vinput)))
                      dst (keyword (.toLowerCase (get vinput 2)))]
                  (copy-val (- (get-reg dst) (get-reg sub)) dst))))
      "MOVE"
       (do
         (cond
           (#{"LEFT" "DOWN" "UP" "RIGHT"} (second vinput))
           (if (= false
                  (game/check-move-direction!
                   (-> vinput
                       second
                       .toLowerCase
                       keyword)))
             (throw (js/Error "CANT-GO-THERE"))
             nil))
         nil)
      ""
      nil
      nil
      false
      false)))



(defn numeric? [s]
  (not (js/isNaN s)))