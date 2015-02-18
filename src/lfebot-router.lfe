(defmodule lfebot-router
  (behaviour gen_server)
  ;; API
  (export (add-subscriber 1)
          (connect 2)
          (remove-subscriber 1)
          (send 1)
          (start_link 1)
          (stop-bot 0)
          ;; Don't call the following unless you really know what you're doing
          (connected 0)
          (disconnected 0)
          (receive-raw 1))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(include-lib "include/records.lfe")

;;;===================================================================
;;; API
;;;===================================================================
(defun add-subscriber (pid)
  (erlang:monitor 'process pid)
  (gen_server:call 'bot-router `#(add-subscriber ,pid)))

(defun remove-subscriber (pid)
  (gen_server:call 'bot-router `#(remove-subscriber ,pid)))

(defun start_link (cmd-word)
  (gen_server:start_link #(local bot-router) (MODULE) (list cmd-word) '()))

;;;===================================================================
;;; gen_server callbacks
;;;===================================================================
(defun init (cmd-word)
  `#(ok ,(make-router-state cmd-word (list_to_binary cmd-word))))

(defun handle_call (_ _ _)
  'noop)

(defun handle_cast (_ _)
  'noop)

(defun handle_info
  (((tuple 'DOWN _ 'process pid _)
    (= (match-router-state subscribers subscribers) state))
    (let ))
  ((message (= (make-router-state) state))
    ;; XXX replace the following format call with a lager call
    (io:format "[~s] Unknown info ~p~n" (list (MODULE) message))
    `#(noreply ,state)))

(defun terminate (_ _)
  'noop)

(defun code_change (_ _ _)
  'noop)

;;;===================================================================
;;; IRC events
;;;===================================================================
(defun bot-command-channel (nick-from channel command args)
  (send-subscriber `#(irc-router
                      command-channel
                      #(,nick-from ,channel ,command ,args)))
  'ok)

(defun bot-command-private (nick-from command args)
  (send-subscriber `#(irc-router
                      command-private
                      #(,nick-from ,command ,args)))
  'ok)

(defun channel-message (nick-from channel line)
  (send-subscriber `#(irc-router
                      channel-message
                      #(,nick-from ,channel ,line)))
  'ok)

(defun connected ()
  (send-subscriber #(irc-router connected))
  'ok)

(defun disconnected ()
  (send-subscriber #(irc-router disconnected))
  'ok)

(defun ready ()
  (send-subscriber #(irc-router ready))
  'ok)

(defun receive-raw (line)
  (let *(((cons line-trimmed _) (re:split line "\r\n"))
         (message `#(irc-router message-received ,(parse-irc line-trimmed))))
    (gen_server:cast 'bot-router ~#(send-subscriber ,message))))

;;;===================================================================
;;; Internal functions
;;;===================================================================
(defun send-subscriber (message)
  (gen_server:cast 'bot-router #(send-subscriber ,message))
  'ok)

(defun send-subscriber
  (('() message)
    'ok)
  ((pids message)
    ;; XXX replace the following format call with a lager call
    (io:format "[~s] send subscribers ~p~n" (list (MODULE) message))
    (lists:map (lambda (pid) (gen_server:cast pid message)) pids)
    'ok))

(defun remove-subscriber (subscribers pid)
  ;; XXX replace the following format call with a lager call
  (io:format "[~s] Remove subscriber ~w~n" (list (MODULE) pid))
  (lists:delete pid subscribers))


(defun parse-irc (line)
  (let (((cons prefix (cons command args)) (lfebot-util:parse-irc line)))
    ;; XXX replace the following format call with a lager call
    (io:format "Prefix '~p'~nCommand '~p'~nArgs '~p'~n"
               (list prefix command args))
    (make-irc-message prefix prefix
                      command command
                      args args)))
