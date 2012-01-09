(in-package #:svdba)

(defun menu ()
  (list (list :link "/" :title "Главная")
        (list :link "/plan/" :title "Планирование торжества")
        (list :link "/servis/" :title "Сервис-организатор свадьбы")
        (list :link "/love/" :title "Любовь продолжается")
        (list :link "/gallery/" :title "Фотогалерея")
        (list :link "/about/" :title "Как с нами связаться?")))


(restas:start '#:svdba :port 7071)

(restas:debug-mode-on)

