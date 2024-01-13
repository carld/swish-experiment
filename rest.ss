#!chezscheme
(library (rest)
  (export rest:controller
	  rest:action:sql->json
	  rest:model:url->sql
	  rest:view:url->sql
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
	 `#(200 (("Content-Type" . ,content-type))
		,(string->utf8 content))]
	[#(error 'not-found)
	 `#(404 (("Content-Type" . "text/plain"))
		,(string->utf8 "Not Found"))]
	[#(error ,content)
	 `#(500 (("Content-Type" . "text/plain"))
		,(string->utf8 "Internal Server Error"))])))

  (define (rest:action:sql->json db path-prefix)
    (mvc:action
     (mvc:controller (rest:controller))
     (mvc:view (rest:view:url->sql db path-prefix))
     (mvc:model (rest:model:url->sql db path-prefix))))

					; add PK as an argument?
  (define (rest:model:url->sql db path-prefix)
    (define (many-from params)
      (define table (json:ref params 'table #f))
      (define sort  (json:ref params 'sort "id"))
      (define dir   (json:ref params 'dir "asc"))
      (define page-size (string->number (json:ref params 'page-size "10")))
      (define page (string->number (json:ref params 'page "0")))
      (define sql
	(ssql `(select
		*
		(from ,(string->symbol table))
		(order by ,(string->symbol sort) ,(string->symbol dir))
		(limit ?)
		(offset ?))))
      (define bindings (list page-size (* page page-size)))
      (apply execute sql bindings))

    (define (one-from params)
      (define table (json:ref params 'table #f))
      (define id (json:ref params 'id #f))
      (define sql
	(ssql `(select * (from ,(string->symbol table))
		       (where (= id ?)) limit 1)))
      (define bindings (list id))
      (define result (apply execute sql bindings))
      result
      )

    (define (meta-data params)
      (define table (json:ref params 'table #f))
      (define sql
	(ssql
	 `(select name (from (pragma_table_info ,table)))))
      (db:transaction
       db
       (lambda ()
	 (execute sql))))

    (define (transpose/vector rows)
      (list->vector (map (lambda (row)
			   (vector-ref row 0))
			 rows)))

    (lambda (params)
      (define id (json:ref params 'id #f))
      (define proc (cond [id  one-from]
			 [else many-from]))
      (define md   (match (meta-data params)
		     [#(ok ,rows) rows]
		     [,err  (error 'meta-data "query failed" err)]
		     ))
      (define tmd  (vector-map string->symbol (transpose/vector md)))
      (define result (db:transaction db (lambda () (proc params))))
      (match result
	[#(ok ,rows)
	 (cond [(and id (null? rows))
		`#(error 'not-found)]
	       [else `#(ok ,rows ,tmd)])]
	[,err
	 (error 'model "error fetching data" err)]))
    )

  (define (rest:view:url->sql db path-prefix)
    (define (render-one row fields)
      (define obj (json:make-object))
      (vector-map (lambda (f v) (json:set! obj f v))
		  fields row)
      obj
      )

    (define (render-many name rows fields)
      (define objs (map (lambda (r) (render-one r fields))
			rows))
      (json:make-object [name objs]))

    (lambda (model params)
      (define id (json:ref params 'id #f))
      (define table-name (json:ref params 'name #f))
      (cond [id  (match model
		   [#(ok ,rows ,tmd)
		    `#(ok ,(json:object->string
			    (render-one (car rows) tmd)))]
		   [,err err]
		   )]
	    [else (match model
		    [#(ok ,rows ,tmd)
		     `#(ok ,(json:object->string
			     (render-many table-name rows tmd)))])])))

  )
