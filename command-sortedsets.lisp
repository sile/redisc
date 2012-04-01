(in-package :redisc)

(defcmd zadd 3 :integer :vary t)
(defcmd zcard 1 :integer)
(defcmd zcount 3 :integer)
(defcmd zincrby 3 :string) ; TODO: retruns integer or float
(defcmd zinterstore 3 :integer :vary t)
(defcmd zrange 3 :list :vary t)
(defcmd zrangebyscore 3 :list :vary t)
(defcmd zrank 2 :integer) ; (or integer null)
(defcmd zrem 2 :integer :vary t)
(defcmd zremrangebyrank 3 :integer)
(defcmd zremrangebyscore 3 :integer)
(defcmd zrevrange 3 :list :vary t)
(defcmd zrevrangebyscore 3 :list :vary t)
(defcmd zrevrank 2 :integer) ; (or integer null)
(defcmd zscore 2 :string) ; (or integer float)
(defcmd zunionstore 3 :integer :vary t)
