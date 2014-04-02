(load "bot.lisp")
(save-lisp-and-die "executable"
           :toplevel #'main
           :executable t)
