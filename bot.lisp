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


(defun get-prior (key)
  (let ((query-for-word (concatenate 'string
				     "SELECT priority FROM keywords WHERE word = \""
				     (princ-to-string key)
				     "\"")))
   (car (caaar (cl-mysql:query query-for-word)))))

(defun hi-priority-word (prep-phrase)
  (let* ((max-prior-word (car prep-phrase))
	 (max-prior (get-prior (car prep-phrase))))
    (loop for i in prep-phrase do
	 (if (not (equal (get-prior i) nil))
	     (if (> (get-prior i) max-prior)
		 (progn
		   (setq max-prior (get-prior i))
		   (setq max-prior-word i)))))
    max-prior-word))     
	   
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


(defun answer (phrase)
  (search-answers (prepare-phrase phrase)))


(defun up-priority (key)
  (let* ((query-for-up (concatenate 'string
				    "UPDATE keywords SET priority = priority+1 WHERE word = \""
				    (princ-to-string key)
				    "\"")))
    (if (not (equal key "Эйфьятлайокудль"))
	(cl-mysql:query query-for-up))))



(defun get-key-id (key)
  (let ((query-id (concatenate 'string
			       "SELECT id_key FROM keywords WHERE word = \""
			       (princ-to-string key)
			       "\"")))
    (car (caaar (cl-mysql:query query-id)))))

(defun test-word (key)
  (let ((query-for-key (concatenate 'string
				    "SELECT id_key FROM keywords WHERE word = \""
				    (princ-to-string key)
				    "\";")))
    (if (equal (car (caaar (cl-mysql:query query-for-key))) nil)
	nil
	t)))


  (let ((prep-phrase (prepare-phrase phrase)))
    (loop for i in prep-phrase do
	 (if (equal (test-word i) nil)
	     (add-key i)))))


(defun add-key (key)
  (let* ((query-insert (concatenate 'string
				    "INSERT INTO keywords (word) VALUES (\""
				    (princ-to-string key)
				    "\");")))
    (cl-mysql:query query-insert)))

;; Depricated
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


(defun learn (key)
  (if (equal (test-word key) nil)
      (progn
	;; No key in db
	(add-key key)
	(format t "Что отвечать?~%")
	(let* ((phrase (read-line)) 
	       (query-to-add (concatenate 'string 
					  "INSERT INTO phrases (id_key, phrase) VALUES ("
					  (princ-to-string (get-key-id key))
					 ", \""
					  phrase
					  "\");")))
	  (cl-mysql:query query-to-add)))
      (progn 
	;; Key exists
	(up-priority key)
	(if (equal (car (search-answer key)) nil)
	    (progn 
	      ;; No answers
	      (format t "Что отвечать?~%")
	      (let* ((phrase (read-line)) 
		     (query-to-add (concatenate 'string 
						"INSERT INTO phrases (id_key, phrase) VALUES ("
						(princ-to-string (get-key-id key))
						", \""
						phrase
						"\");")))
		(cl-mysql:query query-to-add)))
	    (format t "~A" (car (search-answer key)))))))


(defun chat (phrase)
  (let ((key (hi-priority-word 
	      (prepare-phrase phrase))))
    (learn key))

