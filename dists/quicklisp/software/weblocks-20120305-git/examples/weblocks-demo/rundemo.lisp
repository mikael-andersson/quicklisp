(push #p"/home/cl-user/quicklisp/dists/quicklisp/software/weblocks-20120305-git/" asdf:*central-registry*)
(push #p"/home/cl-user/quicklisp/dists/quicklisp/software/weblocks-20120305-git/examples/weblocks-demo/" asdf:*central-registry*)
(asdf:operate 'asdf:load-op :weblocks-demo)
(weblocks-demo:start-weblocks-demo)
