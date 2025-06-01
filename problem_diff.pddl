(define (problem greenhouse-problem)
  (:domain greenhouse)
  
  (:objects
    s1 s2 - system
    banana broccoli strawberry - plant
    water food pesticide - resource
    l1 l2 l3 l4 l5 l6 - location
    
  )
  
  (:init
    
    (= (battery-level s1) 0)
    (= (battery-level s2) 10)
    (= (total-cost) 0)
    
    (needs water banana)
    (needs food broccoli)
    (needs pesticide strawberry)
    (needs water strawberry)
    
    (at s1 l1)
    (at s2 l1)
    (recharge-station l6)

    (at-plant broccoli l2)
    (at-plant banana l3)
    (at-plant strawberry l5)

    (resource-available water s1)
    (resource-available food s1)
    (resource-available pesticide s1)

    (adjacent l1 l2) (adjacent l2 l1)
    (adjacent l1 l3) (adjacent l3 l1)
    (adjacent l3 l4) (adjacent l4 l3)
    (adjacent l2 l4) (adjacent l4 l2)
    (adjacent l5 l6) (adjacent l6 l5)
    (adjacent l3 l5) (adjacent l5 l3)
    (adjacent l4 l6) (adjacent l6 l4)
     
    
  )

  (:goal
    (and (healthy banana water) 
         (healthy broccoli food) 
         (healthy strawberry pesticide)
         (healthy strawberry water))
  )

  (:metric minimize (total-cost))
)

