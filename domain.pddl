(define (domain greenhouse)
  (:requirements :strips :typing :action-costs)

  (:types plant system location resource)

  (:predicates
    (needs ?r - resource ?p - plant)
    (healthy ?p - plant ?r - resource)
    (resource-available ?r - resource ?s)
    (at ?s - system ?l - location)
    (at-plant ?p - plant ?l - location)
    (adjacent ?l1 - location ?l2 - location)
    (recharge-station ?l - location)
    
  )

  (:functions
    (total-cost)
    (battery-level ?s - system)
  )

  (:action move-system
    :parameters (?s - system ?from - location ?to - location)
    :precondition (and 
      (at ?s ?from) 
      (adjacent ?from ?to)
      (> (battery-level ?s) 0)
    )
    :effect (and 
      (not (at ?s ?from)) 
      (at ?s ?to) 
      (increase (total-cost) 10)
      (decrease (battery-level ?s) 1)
    )
  )

  (:action feed-plant
    :parameters (?p - plant ?s - system ?r - resource ?l - location)
    :precondition (and 
      (needs ?r ?p) 
      (at ?s ?l) 
      (at-plant ?p ?l) 
      (resource-available ?r ?s)
      (> (battery-level ?s) 0)
    )
    :effect (and 
      (not (needs ?r ?p)) 
      (healthy ?p ?r) 
      (not (resource-available ?r ?s)) 
      (increase (total-cost) 1)
      (decrease (battery-level ?s) 1)
    )
  )

  (:action recharge-system
  :parameters (?s - system ?l - location ?r - resource)
  :precondition (and 
    (at ?s ?l)
    (recharge-station ?l)
    (or
      (<= (battery-level ?s) 1)
      (not (resource-available ?r ?s))
    )
    )
    :effect (and 
    (increase (total-cost) 100)
    (when (not (resource-available ?r ?s))
      (resource-available ?r ?s)
    )
    (when (<= (battery-level ?s) 1)
      (increase (battery-level ?s) 10)
    )
    )
    )

  (:action share-resource
    :parameters (?s1 - system ?s2 - system ?l1 - location ?r - resource ?p - plant)
    :precondition (and 
      (needs ?r ?p) 
      (at ?s1 ?l1) 
      (at ?s2 ?l1) 
      (not (resource-available ?r ?s1)) 
      (resource-available ?r ?s2) 
      (> (battery-level ?s1) 0) 
      (> (battery-level ?s2) 0)
    )
    :effect (and 
      (resource-available ?r ?s1) 
      (not (resource-available ?r ?s2)) 
    )
  )

  (:action share-energy
    :parameters (?s1 - system ?s2 - system ?l1 - location)
    :precondition (and 
      (at ?s1 ?l1) 
      (at ?s2 ?l1) 
      (= (battery-level ?s1) 0) 
      (> (battery-level ?s2) 0)
    )
    :effect (and 
      (increase (battery-level ?s1) (battery-level ?s2)) 
      (decrease (battery-level ?s2) (battery-level ?s2))
    )
  )
)

