(require 'xml-rpc)
(require 'ini)

(defgroup bugmac nil
  "Interfacing with multiple bugzilla instances from emacs"
  :prefix "bugmac-"
  :group 'tools)

(defgroup bugmac-general nil
  "General preferences for bugmac"
  :group 'bugmac)

(defgroup bugmac-faces nil 
  "Customization for bugmac"
  :group 'bugmac)

(defface bugmac-bug-header-face
  '((t :inherit header-line))
  "Face for bug header lines"
  :group 'bugmac-faces)

(defface bugmac-comment-header-face
  '((t :foreground "#CAE1A3"))
  "Comment header face"
  :group 'bugmac-faces)

(defface bugmac-comment-face
  '((t :background "black"
       :bold t))
  "Comment body face"
  :group 'bugmac-faces)

(defface bugmac-comment-quote-face
  '((t :italic t))
  "Comment body face"
  :group 'bugmac-faces)

(defface bug-header-summary
  '((t :color "gray"
       :inherit 'bug-header))
  "Bug summary in header line"
  :group 'bugmac-faces)

(defcustom bugmac-config-filename "~/.emacs.d/bugmac.cf"
  "The location of bugmac configuration file"
  :group 'bugmac-general)

(defvar bugmac-bug-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "m") 
      '(lambda ()
	 (interactive)
	 (bugmac-modify bug)))
    map))

(defvar bugmac-last-buffer 
  ;; the last bugmac buffer to have focus
  nil)
  

(defun bugmac-pkg (alist)
  (let ((tbl (make-hash-table :test 'equal)))
    (loop for (key . value) in alist
	  do

	  (if (xml-rpc-value-datetimep value)
	      (setq value (format-time-string "%Y-%m-%d %H:%M:%S" (cadr value))))

	  (unless (symbolp key)
	    (setq key (intern key)))

	  (puthash key value tbl))
    tbl))



(defun bugmac-call ( method &optional args )
  "Make a method call to the Bugzilla XMLRPC"
  (if bugmac-bugzilla-url
      (xml-rpc-method-call bugmac-bugzilla-url method args)
    (error "No bugzilla url set")))



(defun bugmac-login ( url &optional user password )
  "Login to the Bugzilla server"
  ;; @todo handle incorrect login
  (interactive)
  (let ((buffer-name (concat "* bugmac - " url " *")))
    (get-buffer-create buffer-name)
    (switch-to-buffer buffer-name)
    (setq buffer-read-only t
	  major-mode 'bugmac-mode
	  mode-name "bugmac"
	  mode-line-process ""
	  bugmac-bugzilla-url (concat url "/xmlrpc.cgi")
	  bugmac-bugzilla-user user
	  bugmac-bugzilla-password password)
    (make-local-variable 'bugmac-bugzilla-url)
    (make-local-variable 'bugmac-bugzilla-user)
    (make-local-variable 'bugmac-bugzilla-password)
    (bugmac-call "User.login" `(("login" . ,user)
				("password" . ,password)))
    (bugmac-mybugs)))



(defun bugmac-get-bug (id)
  "Get information about bugs with id in ids"
  (setq bug 
	(let* ((inhibit-read-only t)
	       (ids `(,id))
	       (r (bugmac-call 'Bug.get `(("ids" . ,ids))))
	       (r (cdr (assoc "bugs" r)))
	       (r (car r))
	       (bug (bugmac-pkg r)))

	  (insert "ctime: " (gethash 'creation_time bug) "\n")
	  (if (gethash 'last_change_time bug)
	      (insert "mtime: " (gethash 'last_change_time bug) "\n"))
	  (insert "\n\n")
	  (use-local-map bugmac-bug-map)
	  (bugmac-comments id t)
	  bug)))



(defun bugmac-modify (bug)
  (interactive)
  (get-buffer-create "* bugmac-modify *")
  (switch-to-buffer "* bugmac-modify *")
  (setq mod-args `(("ids" . (,(gethash 'id bug)))))
  (make-local-variable 'mod-args)

  (defun set-arg (key value)
    "Adds a parameter to the API request alist"
    (setq mod-args (assq-delete-all key mod-args))
    (setq mod-args (cons `(,key . ,value) mod-args)))

  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer)
    (widget-create 'menu-choice
		   :tag "Severity"
		   :value (gethash 'severity bug)
		   :notify '(lambda (w &rest args)
			      (interactive)
			      (let ((value (widget-value w)))
				(set-arg "severity" value)))
		   '(choice-item "blocker")
		   '(choice-item "critical")
		   '(choice-item "major")
		   '(choice-item "normal")
		   '(choice-item "minor")
		   '(choice-item "trivial")
		   '(choice-item "enhancement"))
    (widget-create 'menu-choice
		   :tag "Priority"
		   :value (gethash 'priority bug)
		   :notify '(lambda (w &rest args)
			      (interactive)
			      (let ((value (widget-value w)))
				(set-arg "priority" value)))
		   '(choice-item "Highest")
		   '(choice-item "High")
		   '(choice-item "Normal")
		   '(choice-item "Low")
		   '(choice-item "Lowest")
		   '(choice-item "---"))
    (widget-create 'editable-field
		   :size 25
		   :format "Assigned To: %v\n" (gethash 'assigned_to bug)
		   :notify '(lambda (w &rest args)
			      (interative)
			      (let ((value (widget-value w)))
				(set-arg "assigned_to" value))))
    (widget-create 'menu-choice
		   :tag "Status"
		   :value (gethash 'status bug)
		   :notify '(lambda (w &rest args)
			      (interactive)
			      (let ((value (widget-value w)))
				(set-arg "status" value)))
		   '(choice-item "UNCONFIRMED")
		   '(choice-item "CONFIRMED")
		   '(choice-item "IN_PROGRESS")
		   '(choice-item "RESOLVED")
		   '(choice-item "VERIFIED"))
    (widget-create 'menu-choice
		   :tag "Resolution"
		   :deactivate t
		   :notify '(lambda (w &rest args)
			      (let ((value (widget-value w)))
				(set-arg "resolution" value)))
		   '(choice-item :tag "---" :value nil)
		   '(choice-item "FIXED")
		   '(choice-item "INVALID")
		   '(choice-item "WONTFIX")
		   '(choice-item "DUPLICATE")
		   '(choice-item "WORKSFORME"))
    (widget-insert "\n\n\n-- Comment --\n")
    (widget-create 'text 
		   :notify '(lambda (w &rest args)
			      (interactive)
			      (let ((value (widget-value w)))
				;; comment => ([body,comment]=>string [,is_private=>bool])
				(set-arg "comment" `(("comment" . ,value)))))
		   :size (* (window-width) 5))
    (widget-create 'push-button
		   :value "Submit"
		   :action '(lambda (w &optional &rest args)
			      ;; submit update to the server, kill
			      ;; buffer if the update is successful
			      (condition-case err
				  (progn 
				    (bugmac-call 'Bug.update mod-args)
				    (kill-buffer "* bugmac-modify *")
				    (switch-to-buffer "* bugmac *")
				    (bugmac-get-bug (gethash 'id bug)))
				('error err))))
    (use-local-map widget-keymap)
    (widget-setup)))

    

(defun bugmac-comments (id &optional append-mode)
  "Get comments for bugs with id in ids"
  (message "Getting comments for %s" id )
  (let* ((inhibit-read-only t)
	 (ids `(,id))
	 (r (bugmac-call 'Bug.comments `(("ids" . ,ids))))
	 (r (car (cdr (assoc "bugs" r))))
	 (r (cdr (assoc "comments" r)))
	 (i 0))
    (unless append-mode
      (erase-buffer))
    (dolist (c r)
      (let* ((c (bugmac-pkg c))
	     (start (point)))
	(insert (format "%s - %s\n" (gethash 'creator c) (gethash 'time c)))
	(overlay-put 
	 (make-overlay start (point)) 
	 'face 'bugmac-comment-header-face)
	(insert (gethash 'text c) "\n\n")
	(insert "\n")))))



(defun bugmac-mybugs ()
  "Get a list of bugs assigned to/reported by user"
  (interactive)
  (bugmac-search `(("assigned_to" . ,bugmac-bugzilla-user) ("reporter" . ,bugmac-bugzilla-user))))



(defun bugmac-search (args)
  (let* ((inhibit-read-only t)
	 (r (bugmac-call 'Bug.search args))
	 (r (assoc "bugs" r))
	 (r (cdr r)))
    (erase-buffer)
    (dolist (b r)
      (let ((bug (bugmac-pkg b))
	    (start (point))
	    (o nil))

	(insert-button 
	 (concat (number-to-string (gethash 'id bug)) "\t" (gethash 'assigned_to bug) "\n")
	 'id (gethash 'id bug)
	 'action '(lambda (btn)
		    (interactive)
		    (let ((inhibit-read-only t))
		      (if (button-get btn 'open)
			  (progn
			    (delete-region (button-get btn 'start) (button-get btn 'stop))
			    (button-put btn 'open nil))
			(progn 
			  (vertical-motion 1)
			  (save-excursion
			    (button-put btn 'start (point))
			    (bugmac-get-bug (button-get btn 'id))
			    (button-put btn 'stop (point))
			    (button-put btn 'open t)))))))))
    (align (beginning-of-buffer) (end-of-buffer) " ")))


(defun bugmac-add-server (&optional alias url user password)
  (interactive)
  (unless alias 
    (setq alias (read-input "Alias: ")))
  (unless url
    (setq url (read-input "URL: ")))
  (unless user
    (setq user (read-input "Username: ")))
  (unless password
    (setq password (read-passwd "Password: ")))
  ;; write new server to config file
  (with-temp-buffer 
    (insert (format "[%s]\n%s\n%s\n%s\n" alias url user password))
    (append-to-file (beginning-of-buffer) (end-of-buffer) bugmac-config-filename)))


(defun bugmac-load-config ()
  (with-temp-buffer
    (insert-file-contents bugmac-config-filename)
    (ini-decode (buffer-string))))


(defun bugmac-select-server ()
  (interactive)
  (select-window (minibuffer-window))
  (let ((enable-recursive-minibuffers nil)
	(inhibit-read-only t)
	(config (bugmac-load-config)))
    (delete-minibuffer-contents)
    (insert "Select server: ")
    (dolist (x config)
      (let ((name (car x))
	    (alist (cdr x)))
	(message (cdr(assoc 'url x)))
	(insert-button name
		       'follow-link t
		       'server-settings alist
		       'action '(lambda (btn)
				  (let* ((x (button-get btn 'server-settings))
					 (url (cdr (assoc "url" x)))
					 (user (cdr (assoc "user" x)))
					 (pwd (cdr (assoc "password" x)))
					 (inhibit-read-only t))
				    (delete-minibuffer-contents)
				    (bugmac-login url user pwd))))
	(insert " ")))
    (setq buffer-read-only t)))



  
  














