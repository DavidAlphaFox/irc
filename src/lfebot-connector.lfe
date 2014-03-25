(defmodule lfebot-connector
  (behaviour gen_server)
  ;; API
  (export (connect 2)
          (send 1)
          (start_link 1)
          (stop-bot 0))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defrecord state
  server
  port
  socket)

(defun server-name ()
  (MODULE))

(defun reconnect-time ()
  "30 seconds."
  30000)

;;;===================================================================
;;; API
;;;===================================================================
(defun start_link
  (((list server port))
    (: gen_server start_link
       (tuple 'local (server-name)) (MODULE) `(,server ,port) '())))

(defun connect (host port)
  (let ((tcp-options '(binary #(active true)
                              #(packet line)
                              #(keepalive true))))
    ; XXX replace with lager call
    (: io format '"[~s] Connecting to ~s:~p~n" (list (MODULE) host port))
    (case (: gen_tcp connect host port tcp-options)
      ((tuple 'ok socket)
        (: gen_server cast (server-name) `#(new_sock ,socket))
        (: lfebot-router connected)
      ((tuple 'error reason)
        ; XXX replace with lager call
        (: io format '"[~s] Error connecting to ~s:~p Reason: ~p~n"
                     (list (MODULE) host port reason)))))))

(defun send (line)
  (: gen_server cast
     (server-name) `#(raw_send ,line)))

(defun stop-bot ()
  (: gen_server call (server-name) 'terminate))

;;;===================================================================
;;; gen_server callbacks
;;;===================================================================
(defun init
  (((list server port))
    (process_flag 'trap_exit 'true)
    (connect server port)
    ; XXX use lager ... to give started message
    (: io format '"[~s] Started~n" (MODULE))
    (tuple 'ok (make-state server server port port))))

(defun handle_call
  (('terminate from state)
    `#(stop normal ok ,state)))

(defun handle_cast
  (((tuple 'new_sock socket) state)
    ; XXX replace with lager call
    (: io format '"[~w] New Sock~n" (list socket))
    (tuple 'noreply (set-state-socket state socket)))
  (((tuple 'raw_send message) (= state (tuple 'state _ _ socket)))
    ; XXX replace with lager call
    (: io format '"[~w] Raw send: ~p~n" (list socket message))
    (: gen_tcp send socket message))
  ((data (= state (tuple 'state _ _ socket)))
    ; XXX replace with lager call
    (: io format '"[~w] Unknown cast: ~p~n" (list socket message))
    `#(noreply ,state)))

(defun handle_info (info state)
  (tuple 'noreply state))

(defun terminate (reason state)
  'ok)

(defun code_change (old-version state extra)
  (tuple 'ok state))

;;;===================================================================
;;; Internal functions
;;;===================================================================
(defun flush ()
  "Flush all messages so they dont queue up."
  'noop)

(defun reconnect ()
  ; XXX use lager here to give reconnecting message
  (: io format '"Waiting ~p seconds before reconncting~n"
               (list (reconnect-time)))
  (: erlang send_after (reconnect-time)))
