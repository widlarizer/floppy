(ql:quickload :com.inuoe.jzon)
(ql:quickload :alexandria)

(defun read-json (x)
  (com.inuoe.jzon:parse (alexandria:read-file-into-string x)))
(defun cpu-json () (read-json "cpu.wide.json"))
(defun firsty (x)
  (let ((keys (alexandria:hash-table-keys x)))
    (gethash (car keys) x)))


(defun first-first (x) (firsty (gethash "cells" (firsty (gethash "modules" x)))))

(defstruct (ir-op
                  (:constructor make-ir-op)
                  (:copier nil))
  "Read-only struct representing an IR operation"
  (name nil :type (or null string) :read-only t)
  (type nil :type (or null string) :read-only t)
  (connections nil :type list :read-only t)
  (parameters nil :type list :read-only t))

(defun create-constant-op (name const value)
  "Create an IR op for a constant value"
  (make-ir-op :name name
              :type "constant"
              :connections (list (list "Y" "output" value))
              :parameters (list (list "VALUE" const))))

(defun extract-connections (cell-hash)
  "Extract port connections from Yosys port hash table"
  (let ((conns-hash (gethash "connections" cell-hash))
        (dirs-hash (gethash "port_directions" cell-hash)))
    (map 'list
         (lambda (port)
           (list port
                 (gethash port dirs-hash)
                 (gethash port conns-hash)))
         (alexandria:hash-table-keys conns-hash))))

(defun extract-parameters (params-hash)
  "Extract parameters from Yosys parameter hash table"
  (let ((parameters '()))
    (when params-hash
      (maphash (lambda (param-name param-value)
                 (push (list param-name param-value) parameters))
               params-hash))
    (nreverse parameters)))

(defun convert-cell (name cell-hash)
  (let* ((connection-mess (extract-connections cell-hash))
         (connections (car connection-mess))
         (consts (cdr connection-mess)))
  (cons (make-ir-op
              :name name
              :type (gethash "type" cell-hash)
              :connections connections
              :parameters (extract-parameters (gethash "parameters" cell-hash)))
        consts)))

(defun convert-module (module)
  (let* ((cells (gethash "cells" module))
         (cell-names (alexandria:hash-table-keys cells)))
    (mapcar
     (lambda (cell-name)
       (convert-cell cell-name
                     (gethash cell-name cells)))
     cell-names)))

(defun convert-all (d)
  (let* ((modules (gethash "modules" d))
         (module-names (alexandria:hash-table-keys modules)))
    (mapcar (lambda (module-name)
              (list module-name
                    (convert-module (gethash module-name modules))))
            module-names)))
