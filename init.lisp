(in-package #:svdba)

(defun menu ()
  (list (list :link "/" :title "Главная")
        (list :link "/plan" :title "Планирование и бюджет")
        (list :link "/whywe/" :title "Почему мы?")
        (list :link "/santorini/" :title "Санторини")
        (list :link "/gallery/" :title "Фотогалерея")
        (list :link "/contaces/" :title "Свяжитесь с нами")))


(restas:start '#:svdba :port 7071)

(restas:debug-mode-on)

