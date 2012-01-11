(require 'swank)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port 5666)
(push (pathname (format nil "/home/~A/repo/svdba/"
                        (sb-posix:passwd-name
                         (sb-posix:getpwuid
                          (sb-posix:stat-uid
                           (sb-posix:stat (format nil "/proc/~A" (sb-posix:getpid)))))))) asdf:*central-registry*)
(asdf:oos 'asdf:load-op :svdba)

