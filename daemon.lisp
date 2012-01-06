(require 'swank)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port 5666)
(push #p"/home/rigidus/repo/svdba/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :svdba)
