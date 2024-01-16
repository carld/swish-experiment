#! /usr/bin/env -S swish --

(library-directories
 (append (library-directories)
	 `(("vendor" . "vendor"))))

(import (middleware))
(import (mvc))
(import (restful-sqlite))
(import (data))

(define (admin-db-file)
  (path-combine (data-dir) "admin.db3"))

(db:start&link 'admin-db (make-directory-path (admin-db-file)) 'create)

(setup-data 'admin-db)

(define app-url-handler
  (mvc:url-handler
   (mvc:table
    `(#(GET ("^/([^/]+)/?$" table)
	    ,(middleware:compose
	      (rest:action:query:many 'admin-db 'table)
	      middleware:log
	      ))
      #(GET ("^/([^/]+)/([^/]+)$" table id)
	    ,(middleware:compose
	      (rest:action:query:one 'admin-db 'table 'id)
	      middleware:log
	      ))
      #(POST ("^/([^/]+)/?$" table)
	     ,(middleware:compose
	       (rest:action:command:insert 'admin-db 'table 'id)
	       middleware:read-json-content
	       middleware:log
	       ))
      #(PATCH ("^/([^/]+)/([^/]+)$" table id)
	      ,(middleware:compose
		(rest:action:command:update 'admin-db 'table 'id)
		middleware:read-json-content
		middleware:log
		))
      #(OPTIONS ("^/([^/]+)/?$" table)
		,(middleware:compose
		  (rest:action:options)
		  middleware:log
		  ))
      #(OPTIONS ("^/([^/]+)/([^/]+)$" table id)
		,(middleware:compose
		  (rest:action:options)
		  middleware:log
		  ))
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
