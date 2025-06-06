;;; ob-http.el --- http request in org-mode babel

;; Copyright (C) 2015 Feng Zhou

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-http
;; Version: 0.0.1
;; Package-Requires: ((s "1.9.0") (cl-lib "0.5") (dash "2.20.0") (yaml "1.2.0") (ht "2.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; http request in org-mode babel
;;

;;; Code:
(require 'ob)
(require 's)
(require 'subr-x)
(require 'json)
(require 'ob-http-mode)
(require 'cl-lib)
(require 'dash)
(require 'ht)

(defconst org-babel-header-args:http
  '((pretty . :any)
    (proxy . :any)
    (noproxy . :any)
    (curl . :any)
    (cookie . :any)
    (schema . :any)
    (host . :any)
    (port . :any)
    (user . :any)
    (username . :any)  ;; deprecated, use user instead
    (password . :any)  ;; deprecated
    (follow-redirect . :any)
    (path-prefix . :any)
    (resolve . :any)
    (max-time . :any)
    (data-binary . :any)
    (print-curl . :any)
    (print-time-stats . :any)
    (upload-file . :any))
  "Http header arguments.")

(defgroup ob-http nil
  "Org-mode blocks for http request."
  :group 'org)

(defcustom ob-http:max-time 10
  "Maximum time in seconds that you allow the whole operation to take."
  :group 'ob-http
  :type 'integer)

(defcustom ob-http:remove-cr nil
  "Remove carriage return from header."
  :group 'ob-http
  :type 'boolean)

(defcustom ob-http:curl-custom-arguments nil
  "List of custom headers that shall be added to each curl request."
  :group 'ob-http
  :type '(repeat (string :format "%v")))

(defcustom ob-http:curl-time-stats-format
  "\n\n
time_namelookup:  %{time_namelookup}s\n
 time_connect (time to start connection):  %{time_connect}s\n
   time_appconnect:  %{time_appconnect}s\n
     time_pretransfer:  %{time_pretransfer}s\n
      time_redirect:  %{time_redirect}s\n
        time_starttransfer (time to first byte):  %{time_starttransfer}s\n
                   ----------\n
time_total:  %{time_total}s\n"
  "Format for time stats requested with :print-time-stats header."
  :group 'ob-http
  :type 'string)

(cl-defstruct ob-http-request method url headers body)
(cl-defstruct ob-http-response headers body headers-map)

(defun ob-http-parse-request (input)
  (let* ((headers-body (ob-http-split-header-body input))
         (headers (s-split-up-to "\\(\r\n\\|[\n\r]\\)" (car headers-body) 1))
         (method-url (split-string (car headers) " ")))
    (make-ob-http-request
     :method (car method-url)
     :url (url-encode-url (cadr method-url))
     :headers (if (cadr headers) (s-lines (cadr headers)))
     :body (cadr headers-body))))

(defun ob-http-parse-response (response)
  (let* ((headers-body (ob-http-split-header-body response))
         (headers-map (mapcar 'ob-http-parse-header (s-lines (car headers-body)))))
    (make-ob-http-response
     :headers (car headers-body)
     :body (cadr headers-body)
     :headers-map headers-map)))

(defun ob-http-split-header-body (input)
  (let ((splited (s-split-up-to "\\(\r\n\\|[\n\r]\\)[ \t]*\\1" input 1)))
    (if (and (string-match "^HTTP/\\(1.[0-1]\\|2\\) \\(30\\|100\\|200\\)" (car splited))
             (string-match "^HTTP/\\(1.[0-1]\\|2\\)" (cadr splited)))
        (ob-http-split-header-body (cadr splited))
      splited)))

(defun ob-http-parse-header (line)
  (let ((key-value (s-split-up-to ": " line 1)))
    `(,(s-downcase (car key-value)) . ,(cadr key-value))))

(defun ob-http-parse-content-type (content-type)
  (when content-type
    (cond
     ((string-match "json" content-type) 'json)
     ((string-match "html" content-type) 'html)
     ((string-match "xml" content-type) 'xml))))

(defun ob-http-shell-command-to-string (command input)
  (with-temp-buffer
    (insert input)
    (shell-command-on-region (point-min) (point-max) command nil 't)
    (buffer-string)))

(defun ob-http-pretty-json (str)
  (if (executable-find "jq")
      (ob-http-shell-command-to-string "jq -j ." str)
    (with-temp-buffer
      (insert str)
      (json-pretty-print-buffer)
      (buffer-string))))

(defun ob-http-pretty-xml (str)
  (cond
   ((executable-find "xml_pp") (ob-http-shell-command-to-string "xml_pp" str))
   ((executable-find "xmlstarlet") (ob-http-shell-command-to-string "xmlstarlet fo" str))
   (t str)))

(defun ob-http-pretty-html (str)
  (cond
   ((executable-find "elinks") (ob-http-shell-command-to-string "elinks -dump" str))
   ((executable-find "tidy") (ob-http-shell-command-to-string "tidy -i -raw -q 2> /dev/null" str))
   ((executable-find "pup") (ob-http-shell-command-to-string "pup -p" str))
   (t str)))

(defun ob-http-pretty (body content-type)
  (if (string= "" body)
      body
    (cl-case (ob-http-parse-content-type content-type)
      (json (ob-http-pretty-json body))
      (xml (ob-http-pretty-xml body))
      (html (ob-http-pretty-html body))
      (otherwise body))))

(defun ob-http-pretty-response (response content-type)
  (setf (ob-http-response-body response)
        (ob-http-pretty (ob-http-response-body response)
                        (if (member content-type '("yes" nil))
                            (ob-http-get-response-header response "content-type")
                          content-type))))

(defun ob-http-select (response path)
  (let ((content-type (ob-http-parse-content-type
                       (ob-http-get-response-header response "content-type")))
        (body (ob-http-response-body response)))
    (cond
     ((and (eq 'json content-type) (executable-find "jq"))
      (ob-http-shell-command-to-string (format "jq -r \"%s\"" path) body))
     ((and (eq 'html content-type) (executable-find "pup"))
      (ob-http-shell-command-to-string (format "pup -p \"%s\"" path) body))
     ((and (eq 'xml content-type) (executable-find "xmlstarlet"))
      (ob-http-shell-command-to-string (format "xmlstarlet sel -t -c '%s' | xmlstarlet fo -o" path) body))
     (t body))))

(defun org-babel-expand-body:http (body params)
  (s-format body 'ob-http-aget
            (mapcar (lambda (x) (when (eq (car x) :var) (cdr x))) params)))

(defun ob-http-get-response-header (response header)
  (cdr (assoc (s-downcase header) (ob-http-response-headers-map response))))

(defun ob-http-remove-carriage-return (response)
  (setf (ob-http-response-headers response)
        (s-join "\n" (s-lines (ob-http-response-headers response))))
  response)

(defun ob-http-flatten (l)
  (cond
   ((null l) nil)
   ((atom l) (list l))
   (t
    (append (ob-http-flatten (car l)) (ob-http-flatten (cdr l))))))

(defun ob-http-aget (key alist)
  (assoc-default (intern key) alist))

(defun ob-http-construct-url (path params)
  (if (s-starts-with? "/" path)
      (s-concat
       (format "%s://" (or (assoc-default :schema params) "http"))
       (assoc-default :host params)
       (when (assoc :port params)
         (format ":%s" (assoc-default :port params)))
       (assoc-default :path-prefix params)
       path)
    path))

(defun ob-http-file (response filename)
  (let ((body (ob-http-response-body response)))
    (with-temp-file filename
      (insert body))))

(defun org-babel-execute:http (body params)
  (let* ((request (ob-http-parse-request (org-babel-expand-body:http body params)))
	 (print-curl (assoc :print-curl params))
         (print-time-stats (assoc :print-time-stats params))
         (proxy (cdr (assoc :proxy params)))
         (noproxy (assoc :noproxy params))
         (follow-redirect (and (assoc :follow-redirect params) (not (string= "no" (cdr (assoc :follow-redirect params))))))
         (pretty (assoc :pretty params))
         (prettify (and pretty (not (string= (cdr pretty) "no"))))
         (file (assoc :file params))
         (get-header (cdr (assoc :get-header params)))
         (cookie-jar (cdr (assoc :cookie-jar params)))
         (cookie (cdr (assoc :cookie params)))
         (curl (cdr (assoc :curl params)))
         (select (cdr (assoc :select params)))
         (resolve (cdr (assoc :resolve params)))
         (request-body (ob-http-request-body request))
         (error-output (org-babel-temp-file "curl-error"))
         (binary (assoc :data-binary params))
         (upload-file (cdr (assoc :upload-file params)))
         (args
          (append
           ob-http:curl-custom-arguments
           (list "-i"
                 (when (and proxy (not noproxy)) `("-x" ,proxy))
                 (when noproxy '("--noproxy" "*"))
                 (let ((method (ob-http-request-method request)))
                   (if (string= "HEAD" method) "-I" `("-X" ,method)))
                 (when follow-redirect "-L")
                 (when (and (assoc :username params) (assoc :password params))
                   `("--user" ,(s-format "${:username}:${:password}" 'ob-http-aget params)))
                 (when (assoc :user params) `("--user" ,(cdr (assoc :user params))))
                 (mapcar (lambda (x) `("-H" ,x)) (ob-http-request-headers request))
                 (if (s-present? request-body)
                     (let ((tmp (org-babel-temp-file "http-"))
                           (data-opt (if binary "--data-binary" "--data")))
                       (with-temp-file tmp (insert request-body))
                       (list data-opt (format "@%s" tmp)))
                   (when upload-file (list "--data-binary" (format "@%s" upload-file))))
                 (when cookie-jar `("--cookie-jar" ,cookie-jar))
                 (when cookie `("--cookie" ,cookie))
                 (when print-time-stats `("-w" ,ob-http:curl-time-stats-format))
                 (when resolve (mapcar (lambda (x) `("--resolve" ,x)) (split-string resolve ",")))
                 (when curl (split-string-and-unquote curl))
                 "--max-time"
                 (int-to-string (or (cdr (assoc :max-time params))
                                    ob-http:max-time))
                 "--globoff"
                 (ob-http-construct-url (ob-http-request-url request) params)))))

    (if print-curl (concat "curl "
                           (string-join (mapcar 'shell-quote-argument (ob-http-flatten args)) " ")
                           "\n")

      (with-current-buffer (get-buffer-create "*curl commands history*")
        (goto-char (point-max))
        (insert "curl "
                (string-join (mapcar 'shell-quote-argument (ob-http-flatten args)) " ")
                "\n"))
      (with-current-buffer (get-buffer-create "*curl output*")
        (erase-buffer)
        (if (= 0 (apply 'process-file "curl" nil `(t ,error-output) nil (ob-http-flatten args)))
            (let ((response (ob-http-parse-response (buffer-string))))
              (when prettify (ob-http-pretty-response response (cdr pretty)))
              (when ob-http:remove-cr (ob-http-remove-carriage-return response))
              (cond (get-header (ob-http-get-response-header response get-header))
                    (select (ob-http-select response select))
                    (prettify (ob-http-response-body response))
                    (file (ob-http-file response (cdr file)))
                    (t (s-join "\n\n" (list (ob-http-response-headers response) (ob-http-response-body response))))))
          (with-current-buffer (get-buffer-create "*curl error*")
            (let ((error-output
                   (with-temp-buffer
                     (insert-file-contents-literally error-output)
                     (s-join "\n" (s-lines (buffer-string))))))
              (erase-buffer)
              (insert error-output)
              (string-join
               (seq-remove
                (lambda (s)
                  (or (string-blank-p s)
                      (string-prefix-p "  " s)))
                (string-lines error-output))
               "\n"))
            ))))))

(defun ob-http-export-expand-variables (&optional backend)
  "Scan current buffer for all HTTP source code blocks and expand variables.

Add this function to `org-export-before-processing-hook' to
enable variable expansion before source block is exported."
  (let ((case-fold-search t) elt replacement)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^[ \t]*#\\+begin_src[ \t]+http" nil 'noerror)
        (setq elt (org-element-at-point))
        (when (eq 'src-block (car elt))
          (setq replacement (org-babel-expand-src-block))
          (goto-char (org-element-property :begin elt))
          (delete-region (org-element-property :begin elt) (org-element-property :end elt))
          (insert (org-element-interpret-data (org-element-put-property elt :value replacement))))))))

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("http" . ob-http)))


(defun ob-http-curl-to-ob-http (curl-as-string &optional modify-headers modify-uri)
  "Turn CURL-AS-STRING to a ob-http block. MODIFY-HEADERS is a headers -> headers function."
  (let* ((data (ignore-errors (s-trim (nth 1 (s-match "--data-raw\ +.*'\\({.*\n?}\\)" curl-as-string)))))
         (method (or (nth 1 (s-match "-X '\\([A-z]+\\)' ?" curl-as-string)) (and data "POST") "GET"))
         (uri (nth 1 (s-match "curl '\\(.+\\)' ?" curl-as-string)))
         (headers (-flatten (-map 'cdr (s-match-strings-all "-H '\\(.+: .+\\)' ?" curl-as-string))))
         )
    (format "%s %s\n%s\n\n%s"
            (upcase method)
            (if (functionp modify-uri)
                (funcall modify-uri uri)
              uri)
            (s-join "\n"
                    (if (functionp modify-headers)
                        (funcall modify-headers headers)
                      headers))
            (if data data ""))
    ))
(defun ob-http-curl-to-ob-http-in-kill-ring ()
  "Copy curl into ob-http code in kill ring."
  (interactive)
  (if-let* ((kill (nth 0 kill-ring))
            (_ (s-starts-with-p "curl" (s-trim kill))))
      (kill-new
       (ob-http-curl-to-ob-http kill))
    (error "Not a curl command in kill ring, this will not work!")))

(defun ob-http-to-curl (ob-block-as-string)
  "Turn OB-BLOCK-AS-STRING to a curl command."
  (let* ((block (s-trim ob-block-as-string))
         (lines (s-lines block))
         (headline (s-split " " (car lines)))
         (method (nth 0 headline))
         (uri (nth 1 headline))
         (headers (--keep
                   (and
                    (string-match-p "^[A-z-]+:\ .*$" (s-trim it))
                    (s-trim it))
                   lines))
         (data (when (-contains? '("PUT" "POST") method)
                 (s-trim (nth 1 (s-split (-last-item headers) block))))))
    (concat "curl '"
            uri
            "' \\\ \n"
            "-X "
            method
            " \\\ \n "
            (s-join
             " \\\ \n"
             (--map (concat "-H '" it "'") headers))
            (when data
              (concat " \\\ \n--data-raw '" data "'"))
            )))

(defun ob-http-to-curl-in-kill-ring ()
  "Copy curret ob-http block as curl in kill ring."
  (interactive)
  (if-let ((e (org-element-at-point))
           (_ (and
               (equal (car e) 'src-block)
               (equal (org-element-property :language e) "http"))))
      (kill-new
       (ob-http-to-curl
        (org-element-property
         :value
         e)))
    (error "Not on a ob-http source block, this will not work!")))

(defun ob-http-intepret-parsed-swagger-file (ht-of-file)
  "Take hash table HT-OF-FILE and turns it into a usable plist."
  (cl-labels ((to-json (ht) ;; this translates a $ref schema ht to a json plist that is easy to json-encode
                (-flatten-n
                 1 ;; flatten the structure for properties on the same level so we get the json
                 (ht-map
                  (lambda (key value) ;; for each property/JSON field KEY of the schema (VALUE being the ht with description of the field)
                    (list
                     ;; name of field as a plist key
                     (intern (concat ":" (format "%s" key)))
                     ;; value is ideally example or type or can be another $ref schema
                     (or (ht-get value 'example)
                         (when-let (type (ht-get value 'type))
                           (if (equal type "array")
                               ;; array type defines items with schema (can be $ref)
                               (or
                                (-some--> (ht-get* value 'items 'type)
                                  vector)
                                ;; TODO refactor this
                                (ignore-errors (--> (ht-get* value 'items '$ref)
                                                    (s-split "/" it)
                                                    cdr ;; skip hash
                                                    (mapcar 'intern it)
                                                    (apply 'ht-get* (cons ht-of-file it))
                                                    to-json
                                                    vector)))
                             type))
                         (ignore-errors
                           ;; TODO this is the same as below, so refactor out
                           (--> (ht-get value '$ref)
                                (s-split "/" it)
                                cdr ;; skip hash
                                (mapcar 'intern it)
                                (apply 'ht-get* (cons ht-of-file it))
                                to-json))
                         nil)))
                  (ht-get ht 'properties)))))
    (--> ht-of-file
         (ht-get it 'paths) ;; paths are what is useful for using the api via ob-http
         (ht-map (lambda (path ht)
                   (ht-map
                    (lambda (method ht1) ;; methods are under the path in the specification, so double iteration
                      (list
                       :path (concat
                              ;; add the first server to make a valid http url
                              (ignore-errors (ht-get (first (append (ht-get ht-of-file 'servers) nil)) 'url))
                              ;; apparently the path is a symbol not a string
                              (format "%s" path))
                       :method method
                       :body (-some--> (ignore-errors (ht-get* ht1 'requestBody 'content 'application/json 'schema '$ref)) ;; TODO not sure all OpenAPI docs follow this requestBody path, particularly the application/json...
                               (s-split "/" it)
                               cdr ;; skip hash
                               (mapcar 'intern it)
                               ;; since a path is $ref: '#/components/schemas/Pet', lisp's apply with ht-get* help us here
                               (ignore-errors (apply 'ht-get* (cons ht-of-file it)))
                               (to-json it))))
                    ht))
                 it))
    ))

(defun ob-http-swagger-to-ob-http (file)
  "Get a yaml FILE and select endpoint."
  (interactive "f")
  (let ((swagger-as-plist
         (ob-http-intepret-parsed-swagger-file
          (--> (require 'yaml)
               (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-substring-no-properties
                  (point-min)
                  (point-max)))
               (yaml-parse-string it)
               ))))
    (--> (--map
          (cons (format "%s %s" (plist-get it :method) (plist-get it :path)) it)
          (-flatten-n 1 swagger-as-plist))
         (alist-get
          (completing-read
           "Pick" it)
          it
          nil
          nil
          'equal)
         ;; TODO it would be just super easy to export as curl at this point
         (format "%s %s\n%s\n\n%s" (upcase (symbol-name (plist-get it :method))) (plist-get it :path) "TODO: header"
                 ;; prettify encoded json
                 (with-temp-buffer
                   (insert (json-encode (plist-get it :body)))
                   (json-pretty-print (point-min)
                                      (point-max))
                   (buffer-string)))
         kill-new)
    )
  )

(provide 'ob-http)
;;; ob-http.el ends here
