#lang racket/load

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(load "ac.scm")
(require 'ac)
(require "brackets.scm")
(use-bracket-readtable)

(aload "arc.arc")
(aload "libs.arc") 

