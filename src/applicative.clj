;   Copyright (c) Jim Duey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns applicative
  (:refer-clojure :exclude [map])
  (:require [clojure.core.reducers :as r]))

(alias 'clj 'clojure.core)

(defprotocol Applicative
  (map- [fs colls]))

(defn map [fs & colls]
  (map- fs colls))

(defn- cartesian [[coll & colls]]
  (cond
   (empty? colls) (clojure.core/map list coll)
   (empty? coll) (cartesian colls)
   :else (for [x coll
               y (cartesian colls)]
           (cons x y))))

(extend-type clojure.lang.PersistentList
  Applicative
  (map- [fs colls]
    (->> (cons fs colls)
         (cartesian)
         (r/map #(apply (first %) (rest %)))
         (into [])
         (list*))))

(extend-type clojure.lang.PersistentVector
  Applicative
  (map- [fs colls]
    (->> (cons fs colls)
         (cartesian)
         (r/map #(apply (first %) (rest %)))
         (into []))))


(deftype ZipWith [f s]
  clojure.lang.IDeref
  (deref [_]
    (if f
      (repeat f)
      s))

  Applicative
  (map- [zipwith-val colls]
    (->> (apply clojure.core/map vector @zipwith-val colls)
         (r/map #(apply (first %) (rest %)))
         (into [])
         (list*)
         (ZipWith. nil))))

(defn zipwith [v]
  (condp = (class v)
    clojure.lang.PersistentList (ZipWith. nil v)
    clojure.lang.PersistentVector (ZipWith. nil v)
    clojure.lang.PersistentArrayMap (ZipWith. nil v)
    clojure.lang.LazySeq (ZipWith. nil v)
    (ZipWith. v nil)))


(deftype Reader [f]
  clojure.lang.IFn
  (invoke [_ config]
    (f config))

  Applicative
  (map- [_ fns]
    (Reader.
     (fn [config]
       (apply f (clj/map #(% config) fns))))))
