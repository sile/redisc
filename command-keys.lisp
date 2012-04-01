(in-package :redisc)

(defcmd del 1 :integer :vary t)
(defcmd exists 1 :boolean)
(defcmd expire 2 :boolean)
(defcmd expireat 2 :boolean)
(defcmd keys 1 :list)
(defcmd move 2 :boolean)  ; TODO: selectと合わせて動作確認
(defcmd object 1 :object :vary t)
(defcmd persist 1 :boolean)
(defcmd pexpire 2 :boolean)
(defcmd pexpireat 2 :boolean)
(defcmd pttl 1 :integer)  ; NOTE: < ver2.6.0
(defcmd randomkey 0 :string)
(defcmd rename 2 :ok)
(defcmd renamenx 2 :boolean)
(defcmd sort 1 :list :vary t)
(defcmd ttl 1 :integer)
(defcmd type 1 :status)  ; TODO: return keyword instead of string
