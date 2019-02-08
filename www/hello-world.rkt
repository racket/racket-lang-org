#lang racket/gui

(define my-language 'Italian)

(define translations
  #hash((Chinese . "你好世界")
        (English . "Hello world")
        (French . "Bonjour monde")
        [German . "Hallo Welt"]
        [Greek . "γειά σου κόσμος"]
        [Portuguese . "Olá mundo"]
        [Spanish . "Hola mundo"]))

(define my-hello-world
  (hash-ref translations my-language "hello world"))

;; - - - - - - - - - - - - - - - - - - - - - - - - 

my-hello-world

(printf "~a\n" my-hello-world)

(message-box "" my-hello-world)