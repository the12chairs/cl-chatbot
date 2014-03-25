(ql:quickload 'cl-mysql)
(ql:quickload 'split-sequence)

(defun connects ()
  (cl-mysql:connect :host "127.0.0.1"
		    :user "root" 
		    :password "" 
		    :database "botlol"))

(defun disconnects ()
  (cl-mysql:disconnect))



(defun prepare-phrase (phrase)
  ;; Разрезать строку на слова, удалить знаки препинания
  (let* ((splited-phrase (split-sequence:split-sequence #\Space phrase))
	 (prepared-phrase ()))
    (loop for i in splited-phrase do
	 (setq i (remove #\, i))
	 (setq i (remove #\. i))
	 (setq i (remove #\! i))
	 (setq i (remove #\? i))
	 (setq i (string-upcase i))
	 (push i prepared-phrase))
    prepared-phrase))


(defun search-answer (key)
  (let* ((query-to-key (concatenate 'string
				      "SELECT phrase FROM phrases INNER JOIN keywords ON 
                                      (keywords.id_key = phrases.id_key) WHERE keywords.id_key = 
                                      (SELECT id_key FROM keywords WHERE word = \""
				      (princ-to-string key)
				      "\") LIMIT 1;")))
    (caaar (cl-mysql:query query-to-key))))

(defun search-answers (list-keys)
  (loop for i in list-keys do
       (if (not (equal (search-answer i) nil))
	   (print (search-answer i)))))




(defun learn (key)
  (let* ((query-for-key (concatenate 'string
				     "SELECT id_key FROM keywords WHERE word = \""
				     key
				     "\";")))
    (if (equal (caaar (cl-mysql:query query-for-key)) nil)
	(progn
	  (let* ((query-insert (concatenate 'string
					    "INSERT INTO keywords (word) VALUES (\""
					    key
					    "\");")))
	    (cl-mysql:query query-insert))
	  (let* ((query-get-id (concatenate 'string
					    "SELECT id_key FROM keywords WHERE word = \""
					    (princ-to-string key)
					    "\";"))
		 (new-id (caaar (cl-mysql:query query-get-id))))
	    (format t "Что ответить на это?~%")
	    (let* ((my-answer (read-line))
		   (query-answer (concatenate 'string
					      "INSERT INTO phrases (id_key, phrase) VALUES ("
					      (princ-to-string new-id)
					      ", "
					      "\"" 
					      my-answer
					      "\""
					      ");")))
	      (cl-mysql:query query-answer)))))))
	


(defun answer (phrase)
  (search-answers (prepare-phrase phrase)))




(defun 

(defun get-word ()
  (format t "Введите слово: ~%")
  (let* ((word (read))
	 (query-text (concatenate 'string
				  "SELECT id_key FROM keywords WHERE word = \"" 
				  (princ-to-string word)
				  "\";")))
    (if (equal (caar (cl-mysql:query query-text)) nil)
	(let* ((query-insert (concatenate 'string
					 "INSERT INTO keywords (word) VALUES (\""
					 (princ-to-string word)
					 "\");")))
	  (cl-mysql:query query-insert)))
    (let* ((query-get-id (concatenate 'string
				      "SELECT id_key FROM keywords WHERE word = \""
				      (princ-to-string word)
				      "\";"))
	   (new-id (caaar (cl-mysql:query query-get-id))))
      (format t "Что ответить на это?~%")
      (let* ((my-answer (read-line))
	     (query-answer (concatenate 'string
					"INSERT INTO phrases (id_key, phrase) VALUES ("
					(princ-to-string new-id)
					", "
					"\"" 
					my-answer
					"\""
					");")))
	(cl-mysql:query query-answer)))))
	
(defun get-words (i)
  (if (> i 0)
      (progn
	(get-word)
	(get-words (- i 1)))))
