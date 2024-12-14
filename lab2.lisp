;;; Функція 1: Видалення другого і третього елементів списку
(defun skip-every-second-and-third (lst)
  "Видаляє кожен другий і третій елементи з списку, повертаючи новий список."
  (cond
    ((null lst) nil)  ; Якщо список порожній, повертаємо NIL
    (t (cons (car lst)  ; Беремо перший елемент
             (skip-every-second-and-third (cdddr lst))))))  ; Пропускаємо 2 і 3 елементи

;;; Тести для skip-every-second-and-third
(defun test-skip-every-second-and-third ()
  "Проводить тестування функції skip-every-second-and-third."
  (format t "Тест 1: Видалення другого і третього елементів ~%")
  (format t "~a~%" (equal (skip-every-second-and-third '(A B C D)) '(A D)))
  (format t "Тест 2: Видалення другого і третього елементів ~%")
  (format t "~a~%" (equal (skip-every-second-and-third '(A B C D E F G)) '(A D G)))
  (format t "Тест 3: Короткий список ~%")
  (format t "~a~%" (equal (skip-every-second-and-third '(A B)) '(A))))))

(test-skip-every-second-and-third)

;;; Функція 2: Перетинання двох списків
(defun item-exists-in-list (item lst)
  "Перевіряє, чи міститься елемент ITEM у списку LST."
  (cond
    ((null lst) nil)  ; Якщо список порожній, повертаємо NIL
    ((eql item (car lst)) t)  ; Якщо збігається з головою, повертаємо T
    (t (item-exists-in-list item (cdr lst)))))  ; Перевіряємо залишок списку

(defun intersect-lists (lst1 lst2)
  "Повертає перетин двох списків атомів LST1 та LST2."
  (cond
    ((null lst1) nil)  ; Якщо перший список порожній, перетин пустий
    ((item-exists-in-list (car lst1) lst2)
     (cons (car lst1)  ; Якщо голова першого списку є у другому, додаємо її
           (intersect-lists (cdr lst1) lst2)))  ; Рекурсивно перетинаємо залишки
    (t (intersect-lists (cdr lst1) lst2))))  ; Інакше пропускаємо голову

;;; Тести для intersect-lists
(defun test-intersect-lists ()
  "Проводить тестування функції intersect-lists."
  (format t "Тест 1: Перетинання двох списків ~%")
  (format t "~a~%" (equal (intersect-lists '(1 2 3 4) '(3 4 5 6)) '(3 4)))
  (format t "Тест 2: Перетинання двох списків ~%")
  (format t "~a~%" (equal (intersect-lists '(1 2 4 3) '(3 4 5 6)) '(4 3)))
  (format t "Тест 3: Немає перетину ~%")
  (format t "~a~%" (equal (intersect-lists '(1 2) '(3 4)) nil))))

(test-intersect-lists)
