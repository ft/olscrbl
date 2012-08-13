(use-modules (olscrbl config))

(set-opt note-default-matcher #f)

(add-account :name 'test
             :user "efftee"
             :password "yearight"
             :uri "localhost"
             :port 8080
             :max-submissions 20)
