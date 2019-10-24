(ns crux.gen-topology-classes
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [crux.topology-info :as ti]))

(import (com.squareup.javapoet MethodSpec TypeSpec TypeSpec$Builder
                               FieldSpec FieldSpec$Builder JavaFile)
        javax.lang.model.element.Modifier)

(defn format-topology-key [key]
  (-> (name key)
      (string/replace "-" "_")
      (string/upper-case)))


(defn build-key-field[[key value]]
  (let [field (FieldSpec/builder String (format-topology-key key) (into-array ^Modifier [Modifier/PUBLIC]))]
    (.initializer field "$S" (into-array [(str key)]))
    (.build field)))

(defn build-key-default-field[[key value]]
  (let [field (FieldSpec/builder String (str (format-topology-key key) "_DEFAULT")
                                 (into-array ^Modifier [Modifier/PUBLIC]))]
    (.initializer field "$S" (into-array [(str (:default value))]))
    (.build field)))

(defn add-topology-key-code [class [key value]]
  (do
    (.addField class (build-key-field [key value]))
    (when (:default value)
      (.addField class (build-key-default-field [key value])))))

(defn build-java-class [class-name topology-info]
  (let [class (TypeSpec/classBuilder class-name)]
    (.addModifiers class (into-array ^Modifier [Modifier/PUBLIC]))
    (doall (map #(add-topology-key-code class %) (seq topology-info)))
    (.build class)))

(defn build-java-file [class-name topology-info]
  (let [javafile (build-java-class class-name topology-info)
        output (io/file class-name)]
    (-> (JavaFile/builder "crux.api" javafile)
        (.build)
        (.writeTo output))))

(defn gen-topology-file [class-name topology]
  (let [topology-info (ti/get-topology-info topology)]
    (build-java-file class-name topology-info)))

(gen-topology-file "KafkaNode" 'crux.kafka/topology)
