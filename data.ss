#!chezscheme
(library (data)
  (export setup-data)
  (import (chezscheme))
  (import (swish imports))
  (import (ssql))

  (define schema-version "2024-01-04_001")
  (define schema-name 'experiment-data)

  (define-syntax (check-db-result stx)
    (syntax-case stx ()
      [(_ dbt ... )
       #`(let ()
	   (match dbt
	     [#(ok ,rows) 'ok]
	     [#(error ,msg) (error db:transaction "db error:" msg)])
	   ...)]))

  (define (setup-data db)
    (check-db-result
     (db:transaction db (lambda () (sql:create-versions)))
     (db:transaction db (lambda () (migrate-db)))
     (db:transaction db (lambda () (init-db)))))

					; taken from log-db.ss
  (define (sql:create-versions)
    (create-table versions
		  (name text primary key)
		  (version text)))

					; taken from log-db.ss
  (define sql:schema-version
    (case-lambda
      [(name)
       (match (execute "select version from versions where name=? limit 1"
		       (symbol->string name))
         [(#(,version)) version]
         [() #f])]
      [(name version)
       (execute "insert or replace into versions (name, version) values (?, ?)"
		(symbol->string name) version)]))

					; create the database tables that don't already exist
  (define (init-db)
    (create-table users
		  (id text) (name text) (email text) (created_at datetime default current_timestamp))

    (execute
     (ssql
      `(insert (into users (name id))
	       (values ("Marty McFly" ,(uuid->string (osi_make_uuid)))))))
    (execute
     (ssql
      `(insert (into users (name id))
	       (values ("Dr Emmett Brown" ,(uuid->string (osi_make_uuid))))))))

					; deal with an pre-existing old schema
  (define (migrate-db)
    (match (sql:schema-version schema-name)
      [,@schema-version 'ok]
      ["2024-01-04"
       (execute "alter table users add column id text")
       (sql:schema-version schema-name "2024-01-04_001")
       (migrate-db)
       ]
      ["2024-01-01"
       (execute (ssql `(alter table users add column email text)))
       (execute "alter table users add column created_at datetime")
       (sql:schema-version schema-name "2024-01-04")
       (migrate-db)
       ]
      [#f
       (sql:schema-version schema-name schema-version)
       (migrate-db)]
      [,version (throw `#(unsupported-db-version ,schema-name version))]))


  )
