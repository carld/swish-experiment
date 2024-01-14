#!chezscheme
(library (rest)
  (export rest:controller
	  rest:action:query:sql->json
	  rest:action:command:form->sql
	  rest:action:options
	  )
  (import (chezscheme))
  (import (swish imports))
  (import (mvc))
  (import (json))
  (import (ssql))

  (define content-type "application/hal+json")

  (define (rest:controller)
    (lambda (view params)
      (match view
	[#(ok ,content)
	 `#(200 (("Content-Type" . ,content-type)
		 ("Access-Control-Allow-Origin" . "*"))
		,(string->utf8 content))]

	[#(ok 'allow ,methods)
	 `#(204 (("Allow" . ,methods)
		 ("Access-Control-Allow-Origin" . "*"))
		,(string->utf8 ""))
	 ]
	[#(ok 'redirect ,location)
	 `#(303 (("Location" . ,location)))]
	[#(error 'not-found)
	 `#(404 (("Content-Type" . "text/plain"))
		,(string->utf8 "Not Found"))]
	[#(error ,content)
	 `#(500 (("Content-Type" . "text/plain"))
		,(string->utf8 "Internal Server Error"))])))

  (define (rest:action:options db path-prefix table pk)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (lambda (model params)
		 `#(ok 'allow (OPTIONS GET POST PUT PATCH))))
     (mvc:model (lambda (params form-data) '()))))

  (define (rest:action:query:sql->json db path-prefix table pk)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (rest:view:query:url->sql db path-prefix table pk))
     (mvc:model (rest:model:query:url->sql db path-prefix table pk))))

  (define (rest:action:command:form->sql db path-prefix table pk)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (rest:view:command:form->sql db path-prefix table pk))
     (mvc:model (rest:model:command:form->sql db path-prefix table pk))))

  (define (meta-data db table-name)
    (define sql
      (ssql `(select name (from (pragma_table_info ,table-name)))))
    (db:transaction db (lambda () (execute sql))))

  (define (transpose/vector rows)
    (list->vector (map (lambda (row)
			 (vector-ref row 0))
		       rows)))

  (define (table-columns db table-name)
    (define result (match (meta-data db table-name)
		     [#(ok ,rows) rows]
		     [,err (error 'table-columns "query failed" err)]))
    (vector-map string->symbol (transpose/vector result)))

  (define (rest:model:query:url->sql db path-prefix table pk)
    (define (many-from params cols)
      (define table-name (json:ref params table #f))
      (define sort  (json:ref params 'sort "id"))
      (define dir   (json:ref params 'dir "asc"))
      (define page-size (string->number (json:ref params 'page-size "10")))
      (define page (string->number (json:ref params 'page "0")))
      (define sql
	(ssql `(select
		,cols
		(from ,(string->symbol table-name))
		(order by ,(string->symbol sort) ,(string->symbol dir))
		(limit ?)
		(offset ?))))
      (define bindings (list page-size (* page page-size)))
      (apply execute sql bindings))

    (define (one-from params cols)
      (define table-name (json:ref params table #f))
      (define id (json:ref params pk #f))
      (define sql
	(ssql `(select ,cols (from ,(string->symbol table-name))
		       (where (= id ?)) limit 1)))
      (apply execute sql (list id)))

    (lambda (params form-data)
      (define id (json:ref params 'id #f))
      (define proc (cond [id  one-from]
			 [else many-from]))
      (define cols  (table-columns db (json:ref params table #f)))
      (define result (db:transaction db (lambda () (proc params cols))))
      (match result
	[#(ok ,rows)
	 (cond [(and id (null? rows))
		`#(error 'not-found)]
	       [else `#(ok ,rows ,cols)])]
	[,err
	 (error 'model "error fetching data" err)]))
    )

  (define (rest:view:query:url->sql db path-prefix table pk)
    (define (render-one row cols)
      (define obj (json:make-object))
      (vector-map (lambda (c v) (json:set! obj c v))
		  cols row)
      obj)

    (define (render-many name rows fields)
      (map (lambda (r) (render-one r fields))
	   rows))

    (lambda (model params)
      (define id (json:ref params 'id #f))
      (define table-name (json:ref params table #f))
      (cond [id  (match model
		   [#(ok ,rows ,cols)
		    `#(ok ,(json:object->string
			    (render-one (car rows) cols)))]
		   [,err err]
		   )]
	    [else (match model
		    [#(ok ,rows ,tmd)
		     `#(ok ,(json:object->string
			     (render-many table-name rows tmd)))])])))

  (define (rest:model:command:form->sql db path-prefix table pk)
    (define (insert-one params form-data cols)
      (define table-name (json:ref params table #f))
      (define sql
	(ssql `(insert (into ,(string->symbol table-name) ,cols)
		       (values ,(make-list (vector-length cols) '?)))))
      (define bindings (vector->list
			(vector-map
			 (lambda (c) (json:ref form-data c ""))
			 cols)))
      (apply execute sql bindings))

    (lambda (params form-data)
      (define cols (table-columns db (json:ref params table #f)))
      (define id (uuid->string (osi_make_uuid)))
      (json:set! form-data pk id)
      (match (db:transaction
	      db
	      (lambda ()
		(insert-one params form-data cols)))
	[#(ok ,result) `#(ok ,result)]
	[,err (error 'form-sql "error inserting record" err)])))

  (define (rest:view:command:form->sql db path-prefix table pk)
    (lambda (model params)
      `#(ok ,(json:object->string (json:make-object [result "ok"])))))

  )
