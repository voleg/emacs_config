;; This is some templates I am using to start debuging

(dap-register-debug-template "Scive Flow django app"
  (list :type "python"
       :args "runserver --noreload"
       :cwd "/Users/voleg/Projects/SciveFlow/flow/app/"
       :module nil
       :console "integratedTerminal"
       :program "/Users/voleg/Projects/SciveFlow/flow/app/manage.py"
       :request "launch"
       :name "Python: Django"
       :django t)
  
  )
