(in-package :redisc)

;; TODO: 仕様把握 & 動作確認
(defcmd psubscribe 1 :list :vary t)
(defcmd publish 2 :integer)
(defcmd punsubscribe 0 :list :vary t) ; TODO(確認): 引数が０の場合にブロックするのはredisの仕様？
(defcmd subscribe 1 :list :vary t)
(defcmd unsubscribe 0 :list :vary t)
