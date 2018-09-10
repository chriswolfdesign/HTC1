;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Constant_Definitions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define WIDTH 400)
(define HEIGHT 600)

(* WIDTH HEIGHT)

(define CAT (text "cat image would go here but I'm not searching for one right now" 16 "blue"))

(define RCAT (rotate -10 CAT))

(define LCAT (rotate 10 CAT))