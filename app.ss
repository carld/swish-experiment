#! /usr/bin/env -S swish --

(library-directories
 (append (library-directories)
	 `(("vendor" . "vendor"))))

(import (mvc))
(import (rest))
(import (data))

(define (admin-db-file)
  (path-combine (data-dir) "admin.db3"))

(db:start&link 'admin-db (make-directory-path (admin-db-file)) 'create)

(setup-data 'admin-db)

(define app-url-handler
  (mvc:url-handler
   (mvc:table
    `(#(GET "^/([^/]+)/?$" ,(rest:action:sql->json 'admin-db "") (table))
      #(GET "^/([^/]+)/([^/]+)$" ,(rest:action:sql->json 'admin-db "") (table id))
      ))))

(http:add-server
 'app-server 9000
 app-url-handler
 (http:options [request-timeout 1000]))

					; Swish out of the box diagnostics pages
					; add to the supervisor specs
(http:add-file-server
 #f 8000
 "web"
 (http:options [file-search-extensions '(".ss" ".html")]
	       [file-transform http:enable-dynamic-pages]))

(app:start)
