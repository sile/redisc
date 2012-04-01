(in-package :redisc)

;; TODO: 他のコマンドでもmultiを考慮した方が良いかも。(integer型で"QUEUED"が返された場合など)
(defcmd discard 0 :ok)
(defcmd exec 0 :list)
(defcmd multi 0 :ok)
(defcmd unwatch 0 :ok)
(defcmd watch 1 :ok :vary t)

;; TODO: multiとかはラッパーマクロを用意する
