data Queue a = Queue [a] [a]
     deriving (Show)

     
--new queue
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue q k) = Queue q (a:k)

pop :: Queue a -> Queue a
pop q@(Queue []     []) = q
pop   (Queue []     k ) = Queue (reverse (init k)) [] 
pop   (Queue (q:qs) k ) = Queue qs                 k

top :: Queue a -> a
top (Queue [] k) = last k
top (Queue q _) = head q


empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue q  _ )   = False


instance Eq a => Eq (Queue a) 
    where (Queue q1 k1) == (Queue q2 k2) = q1++(reverse k1) == q2++ (reverse k2)