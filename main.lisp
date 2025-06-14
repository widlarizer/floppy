(ql:quickload :com.inuoe.jzon)
(ql:quickload :alexandria)

(defun cpu-json ()
  (alexandria:read-file-into-string "cpu.json"))

(defun cpu-dict ()
  (com.inuoe.jzon:parse (cpu-json)))
