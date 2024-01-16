#!chezscheme
(library (restful-sqlite)
  (export rest:controller
	  rest:action:query:one
	  rest:action:query:many
	  rest:action:command:insert
	  rest:action:options
	  rest:action:command:update
	  )
  (import (chezscheme))
  (import (swish imports))
  (import (mvc))
  (import (json))
  (import (ssql))

  (define content-type "application/json")

  (define (rest:controller)
    (lambda (view params)
      (match view
	[#(ok ,content)
	 `#(200 (("Content-Type" . ,content-type)
		 ("Access-Control-Allow-Origin" . "*"))
		,(string->utf8 content))]
	[#(ok 'created ,content)
	 `#(201 (("Content-Type" . ,content-type)
		 ("Access-Control-Allow-Origin" . "*"))
		,(string->utf8 content))]
	[#(ok 'accepted ,content) ; for background processing
	 `#(202 (("Content-Type" . ,content-type)
		 ("Access-Control-Allow-Origin" . "*"))
		,(string->utf8 content))]

	[#(ok 'allow ,methods)
	 `#(204 (("Allow" . ,methods)
		 ("Access-Control-Allow-Origin" . "*")
		 ("Access-Control-Allow-Methods" . ,methods)
		 ("Access-Control-Allow-Credentials" . "true")
		 ("Access-Control-Allow-Headers" . "Content-Type"))
		,(string->utf8 ""))]
	[#(ok 'redirect ,location)
	 `#(303 (("Location" . ,location)

		 ))]
	[#(error 'not-found)
	 `#(404 (("Content-Type" . "text/plain"))
		,(string->utf8 "Not Found"))]
	[#(error ,content)
	 `#(500 (("Content-Type" . "text/plain"))
		,(string->utf8 "Internal Server Error"))])))

  (define (rest:action:options)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (lambda (model params)
		 `#(ok 'allow
		       ,(join
			 (map symbol->string
			      '(GET HEAD PUT PATCH POST DELETE))
			 ","))))
     (mvc:model (lambda (params) '()))))

  (define (rest:action:query:one db table pk)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (rest:view:query:one))
     (mvc:model (rest:model:query:one db table pk))))

  (define (rest:action:query:many db table)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (rest:view:query:many db table))
     (mvc:model (rest:model:query:many db table))))

  (define (rest:action:command:insert db table pk)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (rest:view:command:insert))
     (mvc:model (rest:model:command:insert db table pk))))

  (define (rest:action:command:update db table pk)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (rest:view:command:update))
     (mvc:model (rest:model:command:update db table pk))))

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

  (define (maybe-convert obj proc)
    (cond [obj (proc obj)]
	  [else  obj]))

  (define (rest:model:query:one db table pk)
    (define (one-from params cols)
      (define table-name (json:ref params table #f))
      (define id (json:ref params pk #f))
      (define sql
	(ssql `(select ,cols (from ,(string->symbol table-name))
			     (where (= ,pk ?)) limit 1)))
      (apply execute sql (list id)))

    (lambda (params)
      (define cols  (table-columns db (json:ref params table #f)))
      (define result (db:transaction db (lambda () (one-from params cols))))
      (match result
	[#(ok ,rows)
	 (cond [(null? rows)
		`#(error 'not-found)]
	       [else `#(ok ,rows ,cols)])]
	[,err
	 (error 'rest:model:query:one "db/sql error" err)]))
    )

  (define (rest:model:query:many db table)
    (define (many-from params cols)
      (define table-name (json:ref params table #f))
					; json-server compatible query string
      (define _start (maybe-convert (json:ref params '_start #f) string->number ))
      (define _end   (maybe-convert (json:ref params '_end   #f) string->number))
      (define _limit (maybe-convert (json:ref params '_limit  #f) string->number))
      (define _page (maybe-convert  (json:ref params '_page "0") string->number))
      (define _per_page (maybe-convert (json:ref params '_per_page "10") string->number))
      (define _sort (json:ref params '_sort "id"))
      (define _order  (json:ref params '_order  "asc"))

      (define sql
	(ssql `(select
		,cols
		(from ,(string->symbol table-name))
		(order by ,(string->symbol _sort) ,(string->symbol _order))
		(limit ?)
		(offset ?))))
      (define bindings
	(cond [(and _start _end) (list (- _end _start) _start)]
	      [(and _start _limit) (list _limit _start)]
	      [(and _page _per_page) (list _per_page (* _per_page _page))]
	      [else (list 10 0)]))
      (apply execute sql bindings))

    (lambda (params)
      (define cols  (table-columns db (json:ref params table #f)))
      (define result (db:transaction db (lambda () (many-from params cols))))
      (match result
	[#(ok ,rows)
	 `#(ok ,rows ,cols)]
	[,err
	 (error 'rest:model:query:many "db/sql error" err
		)])))

  (define (rest:view:one row cols)
    (define obj (json:make-object))
    (vector-map (lambda (c v) (json:set! obj c v))
		cols row)
    obj)

  (define (rest:view:query:one)
    (lambda (model params)
      (match model
	[#(ok ,rows ,cols)
	 `#(ok ,(json:object->string
		 (rest:view:one (car rows) cols)))]
	[,err err])))

  (define (rest:view:query:many db table)
    (define (render-many name rows fields)
      (map (lambda (r) (rest:view:one r fields))
	   rows))
    (lambda (model params)
      (define table-name (json:ref params table #f))
      (match model
	[#(ok ,rows ,cols)
	 `#(ok ,(json:object->string
		 (render-many table-name rows cols)))])))

  (define (rest:model:command:insert db table pk)
    (define (insert-one params cols)
      (define table-name (json:ref params table #f))
      (define form-data (json:ref params 'form-data #f))
      (define sql
	(ssql `(insert (into ,(string->symbol table-name)
			     ,cols)
		       (values ,(make-list (vector-length cols)
					   '?)))))
      (define bindings (vector->list
			(vector-map (lambda (c) (json:ref form-data c ""))
				    cols)))
      (apply execute sql bindings))

    (lambda (params)
      (define cols (table-columns db (json:ref params table #f)))
      (define id (uuid->string (osi_make_uuid)))
      (json:set! params `(form-data ,pk) id)
      (match (db:transaction
	      db
	      (lambda ()
		(insert-one params cols)))
	[#(ok ,result) (json:ref params 'form-data #f)]
	[,err (error 'rest:model:command:insert "db/sql error" err)])))

  (define (rest:view:command:insert)
    (lambda (model params)
      `#(ok 'created ,(json:object->string model))))

  (define (rest:model:command:update db table pk)
    (define (update-one params cols)
      (define table-name (json:ref params table #f))
      (define form-data (json:ref params 'form-data #f))
      (define id (json:ref params pk #f))
      (define cols-no-id
	(filter (lambda (c) (not (equal? pk c)))
		(vector->list cols)))
      (define assignments (map (lambda (c) `(,c ?)) cols-no-id))
      (define sql
	(ssql `(update ,(string->symbol table-name)
		       (set ,@assignments)
		       (where (= ,pk ?)))))
      (define bindings (append
			(map (lambda (c) (json:ref form-data c ""))
			     cols-no-id)
			(list id)))
      (apply execute sql bindings))

    (lambda (params)
      (define cols (table-columns db (json:ref params table #f)))
      (match (db:transaction
	      db
	      (lambda ()
		(update-one params cols)))
	[#(ok ,result)  (json:ref params 'form-data #f)]
	[,err (error 'rest:model:command:update "db/sql error" err)])))

  (define (rest:view:command:update)
    (lambda (model params)
      `#(ok ,(json:object->string model))))

  )
