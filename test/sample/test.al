(ns sample.test
  (:use [agentlang.lang]))

(component :Sample.Test)

(record
 :HelloWorld
 {:Name :String
  :Message :String})

(dataflow
 :SayHello
 {:HelloWorld
  {:Name :SayHello.Name
   :Message '(str "Hello World from " :SayHello.Name)}})

(entity
 :Sample.Test/SalaryInfo
 {:Salary :Int})

(entity
 :Sample.Test/PersonalInfo
 {:Age :Int
  :Salary :Int})

(entity
 :Sample.Test/SalaryInfoOffice
 {:Salaries {:listof :Sample.Test/SalaryInfo}})

(event
 {:Sample.Test/AddPersonalInfo
  {:Age :Int}})

(dataflow
 :Sample.Test/AddPersonalInfo
 {:Sample.Test/SalaryInfoOffice
  {:Salaries [{:Sample.Test/SalaryInfo
               {:Salary 12}}
              {:Sample.Test/SalaryInfo
               {:Salary 15}}]}}
 [:for-each
  :Sample.Test/SalaryInfoOffice.Salaries
  {:Sample.Test/PersonalInfo
   {:Age :Sample.Test/AddPersonalInfo.Age
    :Salary :%.Salary}}])
