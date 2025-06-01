(define (problem greenhouse-problem)
  (:domain greenhouse)
  
  (:objects
    s1 s2 - system
    banana broccoli strawberry - plant
    water food pesticide - resource
    l1 l2 l3 l4 - location
    
  )
  
  (:init
    
    (= (battery-level s1) 1)
    (= (battery-level s2) 10)
    (= (total-cost) 0)
    
    (needs water banana)
    (needs food broccoli)
    (needs pesticide strawberry)
    
    (at s1 l1)
    (at s2 l2)
    (recharge-station l4)

    (at-plant broccoli l2)
    (at-plant banana l3)
    (at-plant strawberry l4)

    (resource-available water s1)
    (resource-available food s2)
    (resource-available pesticide s2)

    (adjacent l1 l2) (adjacent l2 l1)
    (adjacent l1 l3) (adjacent l3 l1)
    (adjacent l3 l4) (adjacent l4 l3)
    (adjacent l2 l4) (adjacent l4 l2)
    
    
  )

  (:goal
    (and (healthy banana water) 
         (healthy broccoli food) 
         (healthy strawberry pesticide))
  )

  (:metric minimize (total-cost))
)

