(library-directories
 (append (library-directories)
	 `(("vendor" . "vendor")
  )))

(import (ssql))
(import (srfi :78)) ; basic unit testing check function

(define-syntax unit-test
  (syntax-rules ()
    ((_ ssql-exp str)
     (check (ssql ssql-exp) (=> string-ci=? ) str))))

(unit-test `(create (table if not exists test1)
		          ( (col integer primary key auto_increment not null)
		            (col2 (varchar 160))))
	   "CREATE TABLE IF NOT EXISTS test1 (col integer primary key auto_increment not null, col2 varchar(160))")

(unit-test `(select 1) "SELECT 1")

(unit-test `(select (c1 c2 c3 c4) (from t1) (where (and (= c1 1) (= c2 "yo!"))))
	   "SELECT c1, c2, c3, c4 FROM t1 WHERE c1 = 1 AND c2 = 'yo!'"
	   )

(unit-test `(select (c1 c2 c3) (from t1))
	   "SELECT c1, c2, c3 FROM t1")

(unit-test `(select #(c1 c2 c3) (from t1))
	   "SELECT c1, c2, c3 FROM t1")

(unit-test `(select * (from t1))
	   "SELECT * FROM t1")

(unit-test `(select (c1 c2 c3) (from t1))
	   "SELECT c1, c2, c3 FROM t1")

(unit-test `(select (c1 c2 c3) (from t1) (limit 10) (offset 3))
	   "SELECT c1, c2, c3 FROM t1 LIMIT 10 OFFSET 3"
	   )

(unit-test `(select (c1 c2 c3) (from t1 (join t2 on (= t1.id t2.xid))) (limit 10) (offset 3))
	   "SELECT c1, c2, c3 FROM t1 JOIN t2 ON t1.id = t2.xid LIMIT 10 OFFSET 3"
	   )

(unit-test `(select ((max c1) c2) (from t1) (group by c1))
	   "SELECT max(c1), c2 FROM t1 GROUP BY c1")

(unit-test `(insert (into t1 (c1 c2 c3)) (values (1 2 "hello")))
	   "INSERT INTO t1 (c1, c2, c3) VALUES (1, 2, 'hello')"
	   )

(unit-test `(insert (into t1 (c1 c2 c3)) (values (? ? ?)))
	   "INSERT INTO t1 (c1, c2, c3) VALUES (?, ?, ?)"
	   )

(unit-test `(update t1 (set (c1 1) (c2 "hello")))
	   "UPDATE t1 SET c1 = 1, c2 = 'hello'"
	   )

(unit-test `(update t1 (set (c2 "hello")) (where (= c1 1)))
	   "UPDATE t1 SET c2 = 'hello' WHERE c1 = 1"
	   )

(unit-test `(update tab1 (set (c1 "abc") (c2 "xyz")) (where (= c3 70)))
      "UPDATE tab1 SET c1 = 'abc', c2 = 'xyz' WHERE c3 = 70")

(unit-test `(create (table if not exists versions) ( (name text primary key) (version text)))
      "CREATE TABLE IF NOT EXISTS versions (name text primary key, version text)")

(unit-test `(select (id name email created_at) (from users) (order by created_at desc) (limit 10) (offset 0))
      "SELECT id, name, email, created_at FROM users ORDER BY created_at DESC LIMIT 10 OFFSET 0")

(unit-test `(select (id name email created_at) (from users) (order by ? ?) (limit ?) (offset ?))

      "select id, name, email, created_at from users order by ? ? limit ? offset ?")
