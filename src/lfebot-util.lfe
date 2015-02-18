(defmodule lfebot-util
  (export all))

(defun split (string delimiter)
  (re:split
    string
    delimiter
    (list #(parts 2) #(return binary))))

(defun split-all (string delimiter)
  (re:split
    string
    delimiter
    (list #(return binary))))

(defun timestamp ()
  (timestamp (now)))

(defun timestamp (time)
  (let* (((tuple (tuple year mon day)
                 (tuple hour min sec)) (calendar:now_to_local_time time))
         (stamp (io_lib:format "~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w"
                               (list year mon day hour min sec))))
    (list_to_binary stamp)))

(defun format-irc-line (nick line)
  (binary ((timestamp) binary)
          " <" (nick binary) "> "
          (line binary)))

(defun split-nick (full-nick)
  (split full-nick "!"))

(defun parse-irc
  (((binary ":" (rest binary)))
    (let (((list prefix new-rest)
           (split rest " ")))
      (parse-irc prefix new-rest)))
  ((rest)
    (parse-irc (binary) rest)))

(defun parse-irc (prefix rest)
  (case (re:run rest " :")
    ((tuple match _)
      (let* (((list new-rest remainder) (split rest " :"))
             (arg-parts (split-all new-rest "\s+")))
        (lists:append (list (list prefix)
                            arg-parts
                            (list remainder)))))
    (_
      (lists:append (list (list prefix)
                          (list (split-all rest "\s+")))))))

(defun get-command-args
  (((binary "@" (command-args binary)))
    (case (split command-args " ")
      ((list command args)
        (list command args))
      ((list command)
        (list command (binary)))))
  ((command-args)
    (case (split command-args " ")
      ((list _ good-command-args)
        (get-command-args (binary "@" (good-command-args binary))))
      ((list _)
        (get-command-args (binary "@"))))))

