;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Create_Image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(above(rectangle 50 100 "solid" "black")
      (rectangle 100 10 "solid" "black")
      (circle  75 "outline" "black")
      (circle 100 "outline" "black")
      (circle 125 "outline" "black"))