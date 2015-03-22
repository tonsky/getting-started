(ns mzai.core
  (:require
    [clojure.string :as string])
  (:gen-class))

(defrecord Planet [id x y growth owner fleet])

(def ME 1)
(def HIM 2)
(def NATIVES 0)

(def turn (atom 0))

(defn set-var [var val]
  (alter-var-root var (constantly val)))

(defn log [& args]
  (binding [*out* *err*]
    (apply println "[" @turn "]" args)))

(defn ÷ [a b]
  (int (/ a b)))

(defn × [a b]
  (int (* a b)))

(defn upgrade-cost [planet]
  (bit-shift-left 1 (:growth planet)))

(defn- planets-by [world whom]
  (filter #(== whom (:owner %)) (vals (:planets world))))

(defn distance [a b]
  (.intValue
    (Math/ceil
      (Math/hypot (- (:x a) (:x b))
                  (- (:y a) (:y b))))))

(defn ppp [p]
  (str "#" (:id p) " [" (:x p) " " (:y p) "] (" (:fleet p) ")"))

(defn reget [world planet]
  (get-in world [:planets (:id planet)]))

(defn min-by [f coll]
  (reduce
    (fn ([] nil)
        ([a b] (if (<= (f a) (f b)) a b)))
    coll))



(defn move! [turns]
  (doseq [turn turns]
    (println turn))
  (println ".")
  (flush))

(defn read-turn []
  (let [planets (loop [world []]
                  (let [line (string/trim (read-line))]
                    (cond
                      (= "." line) world
                      (re-find #"^P" line)
                      (let [values (->> (rest (string/split line #"\s+"))
                                        (map #(Integer/parseInt %)))
                            planet (apply ->Planet values)]
                        (recur (conj world planet)))
                      (re-find #"^Y" line)
                      (let [[player-id] (->> (rest (string/split line #"\s+"))
                                             (map #(Integer/parseInt %)))]
                        (set-var #'ME player-id)
                        (recur world))
                      :else
                        (throw (Exception. "Unkown line" line)))))]
    (set-var #'HIM
          (-> (into #{} (map :owner) planets) (disj ME NATIVES) first))
    (swap! turn inc)
    planets))

(defn upgrade-turn [planet]
  (str "B " (:id planet)))

(defn upgrade [world planet]
  (log "upgrade     \t" (ppp planet)
       ", growth"  (:growth planet) "->" (inc (:growth planet))
       ", fleet"   (:fleet planet) "->" (- (:fleet planet) (upgrade-cost planet)))
  (-> world
      (update-in [:planets (:id planet) :fleet] - (upgrade-cost planet))
      (update :turns conj (upgrade-turn planet))))

(defn attack-turn [txt from to fleet]
  (log txt "from\t" (ppp from) "->" (ppp to) "with" fleet "ships")
  (str "F " (:id from) " " (:id to) " " fleet))

(defn attack
  ([world from to]
    (attack world from to (min (:fleet from) (:fleet to))))
  ([world from to fleet]
    (-> world
        (update-in [:planets (:id from) :fleet] - fleet)
        (update-in [:planets (:id to) :fleet] - fleet) ;; approx
        (update :turns conj (attack-turn "attack" from to fleet)))))

(defn move [world from to fleet]
  (-> world
      (update-in [:planets (:id from) :fleet] - fleet)
;;       (update-in [:planets (:id to) :fleet] + fleet)
      (update :turns conj (attack-turn "move" from to fleet))))

(defn order-upgrades [world]
  (reduce-kv
    (fn [world id planet]
      (let [defense (× (:growth planet) 5)
            est     (- (:fleet planet) (upgrade-cost planet))]
        (if (and (== (:owner planet) ME)
                 (>= est defense))
          (upgrade world planet)
          world)))
   world
   (:planets world)))

(defn my-forces [world]
  (->> (planets-by world ME)
       (map :fleet)
       (reduce + 0)))

(defn frontier-dist [planet world]
  (->> (vals (:planets world))
       (filter #(== HIM (:owner %)))
       (map #(distance planet %))
       (reduce min)))

(defn closest-friendly [planet world]
  (->> (vals (:planets world))
       (filter #(== ME (:owner %)))
       (remove #(== (:id %) (:id planet)))
       (min-by #(distance planet %))))


(defn move-to-frontier [world]
  (reduce
   (fn [world from]
     (if-let [to (closest-friendly from world)]
       (if (and (< (× 1.5 (frontier-dist to world))
                          (frontier-dist from world))
                (or (< (:fleet to) (× 2 (:fleet from)))
                    (> (:fleet from) 100))
                (> (:fleet from) 10))
         (move world from to (÷ (:fleet from) 2))
         world)
       world))
   world
   (planets-by world ME)))

(defn plan-attack [world target]
  (let [queue (sort-by #(distance target %) (planets-by world ME))]
    (loop [attackers []
           power 0
           queue queue]
      (if-let [from (first queue)]
        (if (> (:fleet from) (× (:growth from) 5))
          (let [power (+ power (× 0.75 (:fleet from)))
                dist  (distance from target)
                his-power (if (== (:owner target) HIM)
                            (+ (:fleet target) (× (:growth target) dist))
                            (:fleet target))]
            (if (> power (× 1.1 his-power))
              (reduce (fn [w p] (attack w (reget w p) (reget w target) (× 0.75 (:fleet p)))) world (conj attackers from))
              (recur (conj attackers from) power (next queue))))
          (recur attackers power (next queue)))
        world))))

(defn planet-value [planet world]
  (× (if (== HIM (:owner planet)) 2 1)
     (- (× 5 (:growth planet))
        (:fleet planet))))

(defn plan-attacks [world]
  (reduce
    (fn [world target]
      (if (pos? (:growth target))
        (plan-attack world target)
        world))
    world
    (->>
      (vals (:planets world))
      (remove #(== ME (:owner %)))
      (sort-by #(planet-value % world)))))

(defn -main [& args]
  (while true
    (let [planets (read-turn)
          world   (-> {:turns  []
                       :planets (into {} (map #(vector (:id %) %)) planets)}
                    (order-upgrades)
                    (plan-attacks)
                    (move-to-frontier))]
      (move! (:turns world)))))
