module Mp3Trees where

import Huffman

tree0 = Node (Node (Node (Leaf (1,1)) (Leaf (0,1))) (Leaf (1,0))) (Leaf (0,0))
tree1 = Node (Node (Node (Node (Node (Node (Leaf (2,2)) (Leaf (0,2)))
        (Leaf (1,2))) (Node (Leaf (2,1)) (Leaf (2,0)))) (Leaf (1,1)))
        (Node (Leaf (0,1)) (Leaf (1,0)))) (Leaf (0,0))
tree2 = Node (Node (Node (Node (Node (Node (Leaf (2,2)) (Leaf (0,2))) 
        (Leaf (1,2))) (Node (Leaf (2,1)) (Leaf (2,0)))) (Leaf (1,0)))
        (Leaf (1,1))) (Node (Leaf (0,1)) (Leaf (0,0)))
tree3 = Node (Node (Node (Node (Node (Node (Node (Node (Leaf (3,3)) 
        (Leaf (2,3))) (Leaf (3,2))) (Leaf (3,1))) (Node (Node (Leaf (1,3))
        (Leaf (0,3))) (Node (Leaf (3,0)) (Leaf (2,2))))) (Node (Node (Leaf 
        (1,2)) (Leaf (2,1))) (Node (Leaf (0,2)) (Leaf (2,0))))) (Leaf (1,1)))
        (Node (Leaf (0,1)) (Leaf (1,0)))) (Leaf (0,0))
tree4 = Node (Node (Node (Node (Node (Node (Node (Leaf (3,3)) (Leaf (0,3))) 
        (Leaf (2,3))) (Node (Leaf (3,2)) (Leaf (3,0)))) (Node (Leaf (1,3)) 
        (Leaf (3,1)))) (Node (Node (Leaf (2,2)) (Leaf (0,2))) (Leaf (1,2)))) 
        (Node (Node (Leaf (2,1)) (Leaf (2,0))) (Leaf (0,1)))) (Node (Leaf (1,1))
        (Node (Leaf (1,0)) (Leaf (0,0))))
tree5 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf (5,5))
        (Leaf (4,5))) (Node (Leaf (5,4)) (Leaf (5,3)))) (Node (Leaf (3,5))
        (Leaf (4,4)))) (Node (Node (Leaf (2,5)) (Leaf (5,2))) (Leaf (1,5))))
        (Node (Node (Leaf (5,1)) (Node (Leaf (0,5)) (Leaf (3,4)))) (Node (Leaf
        (5,0)) (Node (Leaf (4,3)) (Leaf (3,3)))))) (Node (Node (Node (Leaf 
        (2,4)) (Leaf (4,2))) (Leaf (1,4))) (Node (Leaf (4,1)) (Leaf (4,0))))) 
        (Node (Node (Node (Node (Leaf (0,4)) (Leaf (2,3))) (Node (Leaf (3,2)) 
        (Leaf (0,3)))) (Node (Leaf (1,3)) (Leaf (3,1)))) (Node (Node (Leaf 
        (3,0)) (Leaf (2,2))) (Leaf (1,2))))) (Node (Node (Leaf (2,1)) (Node 
        (Leaf (0,2)) (Leaf (2,0)))) (Leaf (1,1)))) (Node (Leaf (0,1)) (Leaf 
        (1,0)))) (Leaf (0,0))
tree6 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf 
        (5,5)) (Leaf (5,4))) (Leaf (4,5))) (Leaf (5,3))) (Node (Node (Leaf 
        (3,5)) (Leaf (4,4))) (Leaf (2,5)))) (Node (Node (Leaf (5,2)) (Leaf 
        (0,5))) (Leaf (1,5)))) (Node (Node (Leaf (5,1)) (Node (Leaf (3,4)) 
        (Leaf (4,3)))) (Node (Node (Leaf (5,0)) (Leaf (3,3))) (Leaf (2,4))))) 
        (Node (Node (Node (Leaf (4,2)) (Leaf (1,4))) (Leaf (4,1))) (Node 
        (Node (Leaf (0,4)) (Leaf (4,0))) (Node (Leaf (2,3)) (Leaf (3,2)))))) 
        (Node (Node (Node (Node (Leaf (1,3)) (Leaf (3,1))) (Node (Leaf (0,3))
        (Leaf (3,0)))) (Leaf (2,2))) (Node (Leaf (0,2)) (Leaf (2,0))))) (Node 
        (Leaf (1,2)) (Leaf (2,1)))) (Leaf (1,1))) (Node (Node (Leaf (0,1)) 
        (Leaf (1,0))) (Leaf (0,0)))
tree7 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf (5,5)) 
        (Leaf (4,5))) (Leaf (3,5))) (Node (Leaf (5,3)) (Node (Leaf (5,4)) 
        (Leaf (0,5))))) (Node (Node (Leaf (4,4)) (Leaf (2,5))) (Node (Leaf 
        (5,2)) (Leaf (1,5))))) (Node (Node (Leaf (5,1)) (Leaf (3,4))) (Node 
        (Leaf (4,3)) (Node (Leaf (5,0)) (Leaf (0,4)))))) (Node (Node (Node 
        (Leaf (2,4)) (Leaf (4,2))) (Node (Leaf (3,3)) (Leaf (4,0)))) (Node 
        (Leaf (1,4)) (Leaf (4,1))))) (Node (Node (Node (Leaf (2,3)) 
        (Leaf (3,2))) (Leaf (1,3))) (Node (Leaf (3,1)) (Node (Leaf (0,3)) 
        (Leaf (3,0)))))) (Node (Node (Node (Leaf (2,2)) (Leaf (0,2))) 
        (Leaf (1,2))) (Node (Leaf (2,1)) (Leaf (2,0))))) (Node (Node 
        (Leaf (1,1)) (Leaf (0,1))) (Node (Leaf (1,0)) (Leaf (0,0))))
tree8 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node 
        (Leaf (7,7)) (Leaf (6,7))) (Node (Leaf (7,6)) (Leaf (5,7)))) (Node 
        (Node (Leaf (7,5)) (Leaf (6,6))) (Leaf (4,7)))) (Node (Node (Leaf (7,4))
        (Leaf (5,6))) (Node (Leaf (6,5)) (Leaf (3,7))))) (Node (Node (Node (Leaf
        (7,3)) (Leaf (4,6))) (Node (Node (Leaf (5,5)) (Leaf (5,4))) (Leaf 
        (6,3)))) (Node (Leaf (2,7)) (Leaf (7,2))))) (Node (Node (Node (Node 
        (Leaf (6,4)) (Leaf (0,7))) (Leaf (7,0))) (Node (Leaf (6,2)) (Node (Leaf
        (4,5)) (Leaf (3,5))))) (Node (Node (Leaf (0,6)) (Node (Leaf (5,3)) (Leaf
        (4,4)))) (Leaf (1,7))))) (Node (Node (Node (Leaf (7,1)) (Node (Leaf 
        (3,6)) (Leaf (2,6)))) (Node (Node (Node (Leaf (2,5)) (Leaf (5,2))) (Leaf
        (1,5))) (Node (Leaf (5,1)) (Node (Leaf (3,4)) (Leaf (4,3)))))) (Node 
        (Node (Leaf (1,6)) (Leaf (6,1))) (Node (Leaf (6,0)) (Node (Leaf (0,5)) 
        (Leaf (5,0))))))) (Node (Node (Node (Node (Node (Leaf (2,4)) (Leaf 
        (4,2))) (Node (Leaf (3,3)) (Leaf (0,4)))) (Node (Leaf (1,4)) (Leaf 
        (4,1)))) (Node (Node (Leaf (4,0)) (Leaf (2,3))) (Node (Leaf (3,2)) 
        (Leaf (0,3))))) (Node (Node (Leaf (1,3)) (Leaf (3,1))) (Node (Leaf 
        (3,0)) (Leaf (2,2)))))) (Node (Node (Node (Leaf (1,2)) (Leaf (2,1)))
        (Node (Leaf (0,2)) (Leaf (2,0)))) (Leaf (1,1)))) (Node (Leaf (0,1))
        (Leaf (1,0)))) (Leaf (0,0))
tree9 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf (7,7))
        (Leaf (6,7))) (Node (Leaf (7,6)) (Leaf (7,5)))) (Node (Node (Leaf (6,6))
        (Leaf (4,7))) (Node (Leaf (7,4)) (Node (Leaf (5,7)) (Leaf (5,5))))))
        (Node (Node (Node (Leaf (5,6)) (Leaf (6,5))) (Leaf (3,7))) (Node (Leaf
        (7,3)) (Leaf (4,6))))) (Node (Node (Node (Node (Leaf (4,5)) (Leaf 
        (5,4))) (Node (Leaf (3,5)) (Leaf (5,3)))) (Leaf (2,7))) (Node (Leaf 
        (7,2)) (Node (Leaf (6,4)) (Leaf (0,7)))))) (Node (Node (Leaf (7,1)) 
        (Node (Leaf (1,7)) (Leaf (7,0)))) (Node (Node (Leaf (3,6)) (Leaf (6,3)))
        (Node (Leaf (6,0)) (Node (Leaf (4,4)) (Leaf (2,5))))))) (Node (Node 
        (Node (Node (Node (Leaf (5,2)) (Leaf (0,5))) (Leaf (1,5))) (Leaf (6,2)))
        (Node (Node (Leaf (2,6)) (Leaf (0,6))) (Leaf (1,6)))) (Node (Node (Leaf
        (6,1)) (Node (Leaf (5,1)) (Leaf (3,4)))) (Node (Node (Leaf (5,0)) (Node
        (Leaf (4,3)) (Leaf (3,3)))) (Node (Leaf (2,4)) (Leaf (4,2))))))) (Node 
        (Node (Node (Node (Node (Leaf (1,4)) (Leaf (4,1))) (Node (Leaf (0,4)) 
        (Leaf (4,0)))) (Node (Leaf (2,3)) (Leaf (3,2)))) (Node (Leaf (1,3)) 
        (Leaf (3,1)))) (Node (Node (Node (Leaf (0,3)) (Leaf (3,0))) (Leaf 
        (2,2))) (Leaf (2,1))))) (Node (Node (Leaf (1,2)) (Node (Leaf (0,2)) 
        (Leaf (2,0)))) (Leaf (1,1)))) (Node (Node (Leaf (0,1)) (Leaf (1,0))) 
        (Leaf (0,0)))
tree10 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf (7,7))
        (Leaf (6,7))) (Leaf (7,6))) (Node (Leaf (5,7)) (Leaf (7,5)))) (Node 
        (Node (Leaf (6,6)) (Leaf (4,7))) (Node (Leaf (7,4)) (Leaf (6,5))))) 
        (Node (Node (Leaf (5,6)) (Leaf (3,7))) (Node (Node (Leaf (7,3)) (Leaf 
        (5,5))) (Leaf (2,7))))) (Node (Node (Node (Leaf (7,2)) (Leaf (4,6)))
        (Node (Leaf (6,4)) (Leaf (1,7)))) (Node (Node (Leaf (7,1)) (Node (Leaf 
        (0,7)) (Leaf (7,0)))) (Node (Leaf (3,6)) (Leaf (6,3)))))) (Node (Node 
        (Node (Node (Leaf (4,5)) (Leaf (5,4))) (Node (Leaf (4,4)) (Node (Leaf
        (0,6)) (Leaf (0,5))))) (Node (Leaf (2,6)) (Leaf (6,2)))) (Node (Node 
        (Leaf (6,1)) (Node (Leaf (1,6)) (Leaf (6,0)))) (Node (Node (Leaf (3,5))
        (Leaf (5,3))) (Node (Leaf (2,5)) (Leaf (5,2))))))) (Node (Node (Node 
        (Node (Leaf (1,5)) (Leaf (5,1))) (Node (Leaf (3,4)) (Leaf (4,3)))) (Node
        (Node (Node (Leaf (5,0)) (Leaf (0,4))) (Leaf (2,4))) (Node (Leaf (4,2))
        (Leaf (1,4))))) (Node (Node (Leaf (3,3)) (Leaf (4,1))) (Node (Leaf 
        (2,3)) (Leaf (3,2)))))) (Node (Node (Node (Node (Node (Leaf (4,0)) (Leaf
        (0,3))) (Leaf (3,0))) (Leaf (1,3))) (Node (Leaf (3,1)) (Leaf (2,2))))
        (Node (Leaf (1,2)) (Leaf (2,1))))) (Node (Node (Node (Node (Leaf (0,2))
        (Leaf (2,0))) (Leaf (0,0))) (Leaf (1,1))) (Node (Leaf (0,1)) (Leaf 
        (1,0))))
tree11 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node
         (Node (Node (Node (Node (Node (Node (Node (Leaf (15,14)) (Leaf 
         (15,12))) (Leaf (15,13))) (Leaf (14,13))) (Leaf (15,15))) (Node (Leaf
         (14,15)) (Leaf (13,15)))) (Node (Node (Leaf (14,14)) (Leaf (12,15)))
         (Node (Leaf (13,14)) (Leaf (11,15))))) (Node (Node (Node (Leaf (15,11))
         (Leaf (12,14))) (Node (Leaf (13,12)) (Node (Leaf (10,15)) (Leaf 
         (14,9))))) (Node (Leaf (14,12)) (Leaf (13,13))))) (Node (Node (Node
         (Node (Leaf (15,10)) (Leaf (12,13))) (Leaf (11,14))) (Node (Leaf 
         (14,11)) (Leaf (9,15)))) (Node (Node (Leaf (15,9)) (Leaf (14,10)))
         (Node (Leaf (11,13)) (Leaf (13,11)))))) (Node (Node (Node (Node (Leaf
         (8,15)) (Leaf (15,8))) (Node (Leaf (12,12)) (Node (Leaf (10,14))
         (Leaf (9,14))))) (Node (Node (Leaf (8,14)) (Node (Leaf (7,15)) (Leaf
         (7,14)))) (Leaf (15,7)))) (Node (Node (Leaf (13,10)) (Node (Leaf 
         (10,13)) (Leaf (11,12)))) (Node (Node (Leaf (12,11)) (Leaf (15,6)))
         (Leaf (6,15)))))) (Node (Node (Node (Node (Leaf (14,8)) (Leaf (5,15)))
         (Node (Leaf (9,13)) (Leaf (13,9)))) (Node (Node (Leaf (15,5)) (Leaf 
         (14,7))) (Node (Leaf (10,12)) (Leaf (11,11))))) (Node (Node (Node (Leaf
         (4,15)) (Leaf (15,4))) (Node (Node (Leaf (12,10)) (Leaf (14,6))) (Leaf
         (15,3)))) (Node (Leaf (3,15)) (Node (Leaf (8,13)) (Leaf (13,8)))))))
         (Node (Node (Node (Node (Leaf (2,15)) (Leaf (15,2))) (Node (Node (Leaf
         (6,14)) (Leaf (9,12))) (Leaf (0,15)))) (Node (Node (Node (Leaf (12,9))
         (Leaf (5,14))) (Leaf (10,11))) (Node (Node (Leaf (7,13)) (Leaf (13,7)))
         (Leaf (4,14))))) (Node (Node (Node (Node (Leaf (12,8)) (Leaf (13,6)))
         (Leaf (3,14))) (Node (Leaf (11,9)) (Node (Leaf (9,11)) (Leaf 
         (10,10))))) (Node (Leaf (1,15)) (Leaf (15,1)))))) (Node (Node (Node 
         (Node (Leaf (15,0)) (Node (Leaf (11,10)) (Leaf (14,5)))) (Node (Node 
         (Leaf (14,4)) (Leaf (8,12))) (Node (Leaf (6,13)) (Leaf (14,3)))))
         (Node (Node (Leaf (14,2)) (Node (Leaf (2,14)) (Leaf (0,14)))) (Node 
         (Leaf (1,14)) (Leaf (14,1))))) (Node (Node (Node (Node (Leaf (14,0))
         (Leaf (5,13))) (Node (Leaf (13,5)) (Leaf (7,12)))) (Node (Node (Leaf 
         (12,7)) (Leaf (4,13))) (Node (Leaf (8,11)) (Leaf (11,8))))) (Node (Node
         (Node (Leaf (13,4)) (Leaf (9,10))) (Node (Leaf (10,9)) (Leaf (6,12))))
         (Node (Leaf (12,6)) (Leaf (3,13))))))) (Node (Node (Node (Node (Node 
         (Node (Leaf (13,3)) (Leaf (7,11))) (Leaf (2,13))) (Node (Leaf (13,2))
         (Leaf (1,13)))) (Node (Node (Leaf (11,7)) (Node (Leaf (5,12)) (Leaf 
         (12,5)))) (Node (Node (Leaf (9,9)) (Leaf (7,10))) (Leaf (12,3)))))
         (Node (Node (Node (Node (Leaf (10,7)) (Leaf (9,7))) (Leaf (4,11)))
         (Leaf (13,1))) (Node (Node (Leaf (0,13)) (Leaf (13,0))) (Node (Leaf
         (8,10)) (Leaf (10,8)))))) (Node (Node (Node (Node (Leaf (4,12)) (Leaf
         (12,4))) (Node (Leaf (6,11)) (Leaf (11,6)))) (Node (Leaf (3,12)) (Leaf
         (2,12)))) (Node (Node (Leaf (12,2)) (Leaf (5,11))) (Node (Node (Leaf
         (11,5)) (Leaf (8,9))) (Leaf (1,12))))))) (Node (Node (Node (Node (Node
         (Leaf (12,1)) (Node (Leaf (9,8)) (Leaf (0,12)))) (Node (Leaf (12,0))
         (Node (Leaf (11,4)) (Leaf (6,10))))) (Node (Node (Node (Leaf (10,6))
         (Leaf (7,9))) (Leaf (3,11))) (Node (Leaf (11,3)) (Node (Leaf (8,8))
         (Leaf (5,10)))))) (Node (Node (Node (Leaf (2,11)) (Node (Leaf (10,5))
         (Leaf (6,9)))) (Node (Leaf (10,4)) (Node (Leaf (7,8)) (Leaf (8,7)))))
         (Node (Node (Leaf (9,4)) (Node (Leaf (7,7)) (Leaf (7,6)))) (Leaf 
         (11,2))))) (Node (Node (Node (Leaf (1,11)) (Leaf (11,1))) (Node (Node
         (Leaf (0,11)) (Leaf (11,0))) (Node (Leaf (9,6)) (Leaf (4,10))))) (Node
         (Node (Node (Leaf (3,10)) (Leaf (10,3))) (Node (Leaf (5,9)) (Leaf
         (9,5)))) (Node (Leaf (2,10)) (Leaf (10,2))))))) (Node (Node (Node (Node
         (Node (Leaf (1,10)) (Leaf (10,1))) (Node (Node (Leaf (0,10)) (Leaf 
         (6,8))) (Leaf (10,0)))) (Node (Node (Node (Leaf (8,6)) (Leaf (4,9)))
         (Leaf (9,3))) (Node (Node (Leaf (3,9)) (Leaf (5,8))) (Node (Leaf (8,5))
         (Leaf (6,7)))))) (Node (Node (Node (Leaf (2,9)) (Leaf (9,2))) (Node
         (Node (Leaf (5,7)) (Leaf (7,5))) (Leaf (3,8)))) (Node (Node (Leaf 
         (8,3)) (Node (Leaf (6,6)) (Leaf (4,7)))) (Node (Node (Leaf (7,4))
         (Leaf (5,6))) (Node (Leaf (6,5)) (Leaf (7,3))))))) (Node (Node (Node
         (Leaf (1,9)) (Leaf (9,1))) (Node (Node (Leaf (0,9)) (Leaf (9,0)))
         (Node (Leaf (4,8)) (Leaf (8,4))))) (Node (Node (Node (Leaf (7,2)) 
         (Node (Leaf (4,6)) (Leaf (6,4)))) (Leaf (2,8))) (Node (Leaf (8,2)) 
         (Leaf (1,8))))))) (Node (Node (Node (Node (Node (Node (Leaf (3,7))
         (Leaf (2,7))) (Leaf (1,7))) (Node (Leaf (7,1)) (Node (Leaf (5,5))
         (Leaf (0,7))))) (Node (Node (Node (Leaf (7,0)) (Leaf (3,6))) (Node
         (Leaf (6,3)) (Leaf (4,5)))) (Node (Node (Leaf (5,4)) (Leaf (2,6)))
         (Node (Leaf (6,2)) (Leaf (3,5)))))) (Node (Node (Leaf (8,1)) (Node
         (Leaf (0,8)) (Leaf (8,0)))) (Node (Node (Leaf (1,6)) (Leaf (6,1)))
         (Node (Leaf (0,6)) (Leaf (6,0)))))) (Node (Node (Node (Node (Node (Leaf
         (5,3)) (Leaf (4,4))) (Leaf (2,5))) (Node (Leaf (5,2)) (Leaf (0,5))))
         (Node (Leaf (1,5)) (Leaf (5,1)))) (Node (Node (Node (Leaf (3,4)) (Leaf
         (4,3))) (Node (Leaf (5,0)) (Leaf (2,4)))) (Node (Node (Leaf (4,2)) 
         (Leaf (3,3))) (Leaf (1,4))))))) (Node (Node (Node (Node (Leaf (4,1))
         (Node (Leaf (0,4)) (Leaf (4,0)))) (Node (Node (Leaf (2,3)) (Leaf 
         (3,2))) (Leaf (1,3)))) (Node (Node (Leaf (3,1)) (Leaf (0,3))) (Node 
         (Leaf (3,0)) (Leaf (2,2))))) (Node (Node (Leaf (1,2)) (Leaf (2,1)))
         (Node (Leaf (0,2)) (Leaf (2,0)))))) (Node (Node (Leaf (1,1)) (Leaf
         (0,1))) (Leaf (1,0)))) (Leaf (0,0))
tree12 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node
        (Node (Leaf (15,15)) (Leaf (14,15))) (Node (Leaf (15,14)) (Leaf (13,15
        )))) (Node (Leaf (14,14)) (Node (Leaf (15,13)) (Leaf (12,15))))) (Node
        (Node (Node (Leaf (15,12)) (Leaf (13,14))) (Node (Leaf (14,13)) (Leaf
        (11,15)))) (Node (Leaf (15,11)) (Node (Leaf (12,14)) (Leaf (14,12))))))
        (Node (Node (Node (Leaf (13,13)) (Leaf (10,15))) (Node (Leaf (15,10))
        (Leaf (11,14)))) (Node (Node (Leaf (14,11)) (Leaf (12,13))) (Node (Leaf
        (13,12)) (Leaf (9,15)))))) (Node (Node (Node (Node (Leaf (15,9)) (Leaf
        (14,10))) (Node (Leaf (11,13)) (Leaf (13,11)))) (Node (Node (Leaf (8,15)
        ) (Leaf (15,8))) (Node (Leaf (12,12)) (Leaf (9,14))))) (Node (Node (Node
        (Leaf (14,9)) (Leaf (7,15))) (Node (Leaf (15,7)) (Leaf (10,13)))) (Node
        (Node (Leaf (13,10)) (Leaf (11,12))) (Node (Leaf (6,15)) (Node (Leaf 
        (10,14)) (Leaf (0,15)))))))) (Node (Node (Node (Node (Leaf (12,11))
        (Leaf (15,6))) (Node (Node (Leaf (8,14)) (Leaf (14,8))) (Node (Leaf 
        (5,15)) (Leaf (9,13))))) (Node (Node (Leaf (15,5)) (Leaf (7,14))) 
        (Node (Leaf (14,7)) (Leaf (10,12))))) (Node (Node (Node (Leaf (12,10))
        (Leaf (11,11))) (Node (Node (Leaf (13,9)) (Leaf (8,13))) (Leaf (4,15))))
        (Node (Node (Leaf (15,4)) (Leaf (3,15))) (Node (Leaf (15,3)) (Leaf (13,8
        ))))))) (Node (Node (Node (Node (Node (Leaf (14,6)) (Leaf (2,15))) (Node
        (Leaf (15,2)) (Node (Leaf (6,14)) (Leaf (15,0))))) (Node (Node (Leaf
        (1,15)) (Leaf (15,1))) (Node (Leaf (9,12)) (Leaf (12,9))))) (Node (Node
        (Node (Leaf (5,14)) (Leaf (10,11))) (Node (Leaf (11,10)) (Leaf (14,5))))
        (Node (Node (Leaf (7,13)) (Leaf (13,7))) (Node (Leaf (4,14)) (Leaf 
        (14,4)))))) (Node (Node (Node (Node (Leaf (8,12)) (Leaf (12,8))) (Node
        (Leaf (3,14)) (Leaf (6,13)))) (Node (Node (Leaf (13,6)) (Leaf (14,3)))
        (Node (Leaf (9,11)) (Leaf (11,9))))) (Node (Node (Node (Leaf (2,14))
        (Leaf (10,10))) (Node (Leaf (14,2)) (Leaf (1,14)))) (Node (Node (Leaf
        (14,1)) (Node (Leaf (0,14)) (Leaf (14,0)))) (Node (Leaf (5,13)) (Leaf
        (13,5)))))))) (Node (Node (Node (Node (Node (Node (Leaf (7,12)) (Leaf
        (12,7))) (Node (Leaf (4,13)) (Leaf (8,11)))) (Node (Leaf (13,4)) (Node
        (Leaf (11,8)) (Leaf (9,10))))) (Node (Node (Node (Leaf (10,9)) (Leaf
        (6,12))) (Node (Leaf (12,6)) (Leaf (3,13)))) (Node (Leaf (13,3)) (Leaf
        (13,2))))) (Node (Node (Node (Node (Leaf (2,13)) (Leaf (0,13))) (Leaf
        (1,13))) (Node (Leaf (7,11)) (Leaf (11,7)))) (Node (Node (Leaf (13,1))
        (Node (Leaf (5,12)) (Leaf (13,0)))) (Node (Leaf (12,5)) (Leaf (8,10)))))
        ) (Node (Node (Node (Node (Leaf (10,8)) (Leaf (4,12))) (Node (Leaf 
        (12,4)) (Leaf (6,11)))) (Node (Node (Leaf (11,6)) (Node (Leaf (9,9))
        (Leaf (0,12)))) (Node (Leaf (3,12)) (Leaf (12,3))))) (Node (Node (Node
        (Leaf (7,10)) (Leaf (10,7))) (Node (Leaf (10,6)) (Node (Leaf (12,0))
        (Leaf (0,11))))) (Node (Leaf (12,2)) (Node (Leaf (2,12)) (Leaf (5,11)))
        ))))) (Node (Node (Node (Node (Node (Node (Leaf (11,5)) (Leaf (1,12)))
        (Node (Leaf (8,9)) (Leaf (9,8)))) (Node (Node (Leaf (12,1)) (Leaf (4,11)
        )) (Node (Leaf (11,4)) (Leaf (6,10))))) (Node (Node (Node (Leaf (3,11))
        (Leaf (7,9))) (Leaf (11,3))) (Node (Node (Leaf (9,7)) (Leaf (8,8))) 
        (Node (Leaf (2,11)) (Leaf (5,10)))))) (Node (Node (Node (Leaf (11,2))
        (Node (Leaf (10,5)) (Leaf (1,11)))) (Node (Leaf (11,1)) (Node (Leaf 
        (11,0)) (Leaf (6,9))))) (Node (Node (Node (Leaf (9,6)) (Leaf (4,10)))
        (Node (Leaf (10,4)) (Leaf (7,8)))) (Node (Node (Leaf (8,7)) (Leaf (3,10)
        )) (Leaf (10,3)))))) (Node (Node (Node (Node (Leaf (5,9)) (Leaf (9,5)))
        (Node (Leaf (2,10)) (Leaf (10,2)))) (Node (Node (Leaf (1,10)) (Leaf 
        (10,1))) (Node (Node (Leaf (0,10)) (Leaf (10,0))) (Leaf (6,8))))) (Node
        (Node (Node (Leaf (8,6)) (Leaf (4,9))) (Node (Leaf (9,4)) (Leaf (3,9))))
        (Node (Node (Leaf (9,3)) (Node (Leaf (7,7)) (Leaf (0,9)))) (Node (Leaf
        (5,8)) (Leaf (8,5)))))))) (Node (Node (Node (Node (Node (Node (Leaf 
        (2,9)) (Leaf (6,7))) (Node (Leaf (7,6)) (Leaf (9,2)))) (Node (Leaf
        (9,1)) (Node (Leaf (1,9)) (Leaf (9,0))))) (Node (Node (Node (Leaf (4,8))
        (Leaf (8,4))) (Node (Leaf (5,7)) (Leaf (7,5)))) (Node (Node (Leaf (3,8))
        (Leaf (8,3))) (Node (Leaf (6,6)) (Leaf (4,7)))))) (Node (Node (Node 
        (Leaf (2,8)) (Leaf (8,2))) (Node (Leaf (1,8)) (Leaf (8,1)))) (Node (Node
        (Node (Leaf (7,4)) (Leaf (0,8))) (Node (Leaf (8,0)) (Leaf (5,6)))) (Node
        (Node (Leaf (6,5)) (Leaf (3,7))) (Node (Leaf (7,3)) (Leaf (4,6)))))))
        (Node (Node (Node (Node (Leaf (2,7)) (Leaf (7,2))) (Node (Leaf (6,4))
        (Leaf (1,7)))) (Node (Node (Leaf (5,5)) (Leaf (7,1))) (Node (Node (Leaf
        (0,7)) (Leaf (7,0))) (Leaf (3,6))))) (Node (Node (Node (Leaf (6,3)) 
        (Leaf (4,5))) (Node (Leaf (5,4)) (Leaf (2,6)))) (Node (Node (Leaf (6,2))
        (Leaf (1,6))) (Node (Node (Leaf (0,6)) (Leaf (6,0))) (Leaf (3,5))))))))
        (Node (Node (Node (Node (Node (Leaf (6,1)) (Node (Leaf (5,3)) (Leaf 
        (4,4)))) (Node (Leaf (2,5)) (Leaf (5,2)))) (Node (Node (Leaf (1,5))
        (Leaf (5,1))) (Node (Node (Leaf (0,5)) (Leaf (5,0))) (Leaf (3,4)))))
        (Node (Node (Node (Leaf (4,3)) (Leaf (2,4))) (Node (Leaf (4,2)) (Leaf 
        (3,3)))) (Node (Leaf (4,1)) (Node (Leaf (1,4)) (Leaf (0,4)))))) (Node 
        (Node (Node (Leaf (2,3)) (Leaf (3,2))) (Node (Node (Leaf (4,0)) (Leaf 
        (0,3))) (Leaf (1,3)))) (Node (Node (Leaf (3,1)) (Leaf (3,0))) (Leaf 
        (2,2)))))) (Node (Node (Node (Node (Leaf (1,2)) (Leaf (2,1))) (Node 
        (Leaf (0,2)) (Leaf (2,0)))) (Leaf (1,1))) (Node (Node (Leaf (0,1)) (Leaf
        (1,0))) (Leaf (0,0))))
tree13 = Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf
        (14,15)) (Leaf (15,14))) (Node (Leaf (13,15)) (Leaf (15,13)))) (Node
        (Node (Leaf (12,15)) (Leaf (15,12))) (Node (Leaf (11,15)) (Leaf (15,11))
        ))) (Node (Node (Leaf (10,15)) (Node (Leaf (15,10)) (Leaf (9,15))))
        (Node (Node (Leaf (15,9)) (Leaf (15,8))) (Leaf (8,15))))) (Node (Node 
        (Node (Leaf (7,15)) (Leaf (15,7))) (Node (Leaf (6,15)) (Leaf (15,6))))
        (Leaf (15,15)))) (Node (Node (Node (Node (Leaf (5,15)) (Leaf (15,5)))
        (Leaf (4,15))) (Node (Leaf (15,4)) (Leaf (15,3)))) (Node (Node (Leaf
        (15,0)) (Node (Leaf (3,15)) (Node (Node (Node (Node (Node (Node (Leaf
        (12,14)) (Node (Leaf (14,12)) (Leaf (13,13)))) (Leaf (13,14))) (Node
        (Leaf (14,9)) (Node (Leaf (14,10)) (Leaf (13,9))))) (Node (Leaf (14,14))
        (Node (Leaf (14,13)) (Leaf (14,11))))) (Node (Node (Leaf (11,14)) (Leaf
        (12,13))) (Node (Node (Leaf (13,12)) (Leaf (13,11))) (Leaf (10,14)))))
        (Node (Node (Node (Leaf (12,12)) (Node (Leaf (10,13)) (Leaf (13,10))))
        (Node (Node (Leaf (7,14)) (Leaf (10,12))) (Leaf (12,10)))) (Node (Node 
        (Node (Leaf (12,9)) (Leaf (7,13))) (Leaf (5,14))) (Leaf (11,13)))))))
        (Leaf (15,2))))) (Node (Node (Node (Node (Leaf (2,15)) (Leaf (0,15)))
        (Leaf (1,15))) (Node (Leaf (15,1)) (Node (Node (Node (Node (Node (Leaf
        (9,14)) (Node (Leaf (11,12)) (Leaf (12,11)))) (Node (Node (Leaf (8,14))
        (Leaf (14,8))) (Node (Leaf (9,13)) (Leaf (14,7))))) (Node (Node (Node
        (Leaf (11,11)) (Leaf (8,13))) (Node (Leaf (13,8)) (Leaf (6,14)))) (Node 
        (Leaf (14,6)) (Leaf (9,12))))) (Node (Node (Node (Node (Leaf (10,11)) 
        (Leaf (11,10))) (Node (Leaf (14,5)) (Leaf (13,7)))) (Node (Leaf (4,14)) 
        (Node (Leaf (14,4)) (Leaf (8,12))))) (Node (Node (Leaf (12,8)) (Leaf 
        (3,14))) (Node (Leaf (6,13)) (Node (Leaf (13,6)) (Leaf (9,11))))))) 
        (Node (Node (Node (Node (Node (Leaf (11,9)) (Leaf (10,10))) (Leaf (14,1)
        )) (Node (Leaf (13,4)) (Node (Leaf (11,8)) (Leaf (10,9))))) (Node (Node 
        (Leaf (7,11)) (Node (Leaf (11,7)) (Leaf (13,0)))) (Leaf (14,3)))) (Node 
        (Node (Node (Leaf (0,14)) (Leaf (14,0))) (Node (Leaf (5,13)) (Leaf 
        (13,5)))) (Node (Node (Leaf (7,12)) (Leaf (12,7))) (Node (Leaf (4,13))
        (Leaf (8,11))))))))) (Node (Node (Node (Node (Node (Node (Node (Leaf 
        (9,10)) (Leaf (6,12))) (Node (Leaf (12,6)) (Leaf (3,13)))) (Node (Node
        (Leaf (5,12)) (Leaf (12,5))) (Leaf (0,13)))) (Node (Node (Node (Leaf
        (8,10)) (Leaf (10,8))) (Node (Leaf (9,9)) (Leaf (4,12)))) (Node (Node
        (Leaf (11,6)) (Leaf (7,10))) (Leaf (3,12))))) (Node (Node (Node (Node
        (Leaf (5,11)) (Leaf (8,9))) (Leaf (1,12))) (Node (Leaf (12,0)) (Node 
        (Leaf (9,8)) (Leaf (7,9))))) (Node (Leaf (14,2)) (Node (Leaf (2,14)) 
        (Leaf (1,14)))))) (Node (Node (Node (Node (Leaf (13,3)) (Leaf (2,13)))
        (Node (Leaf (13,2)) (Leaf (13,1)))) (Node (Node (Leaf (3,11)) (Node 
        (Leaf (9,7)) (Leaf (8,8)))) (Leaf (1,13)))) (Node (Node (Node (Leaf 
        (12,4)) (Leaf (6,11))) (Node (Leaf (12,3)) (Leaf (10,7)))) (Node (Leaf
        (2,12)) (Node (Leaf (12,2)) (Leaf (11,5))))))) (Node (Node (Node (Node
        (Node (Leaf (12,1)) (Leaf (0,12))) (Node (Leaf (4,11)) (Leaf (11,4))))
        (Node (Node (Leaf (6,10)) (Leaf (10,6))) (Leaf (11,3)))) (Node (Node 
        (Node (Leaf (5,10)) (Leaf (10,5))) (Leaf (2,11))) (Node (Leaf (11,2))
        (Leaf (1,11))))) (Node (Node (Node (Leaf (11,1)) (Node (Leaf (0,11)) 
        (Leaf (11,0)))) (Node (Node (Leaf (6,9)) (Leaf (9,6))) (Node (Leaf 
        (4,10)) (Leaf (10,4))))) (Node (Node (Node (Leaf (7,8)) (Leaf (8,7)))
        (Leaf (10,3))) (Node (Node (Leaf (3,10)) (Leaf (5,9))) (Leaf (2,10))))))
        ))) (Node (Node (Node (Node (Node (Node (Node (Node (Leaf (9,5)) (Leaf
        (6,8))) (Leaf (10,1))) (Node (Node (Leaf (8,6)) (Leaf (7,7))) (Leaf
        (9,4)))) (Node (Node (Node (Leaf (4,9)) (Leaf (5,7))) (Leaf (6,7))) 
        (Leaf (10,2)))) (Node (Node (Leaf (1,10)) (Node (Leaf (0,10)) (Leaf 
        (10,0)))) (Node (Node (Leaf (3,9)) (Leaf (9,3))) (Node (Leaf (5,8))
        (Leaf (8,5)))))) (Node (Node (Node (Leaf (2,9)) (Leaf (9,2))) (Node
        (Node (Leaf (7,6)) (Leaf (0,9))) (Leaf (1,9)))) (Node (Node (Leaf (9,1))
        (Node (Leaf (9,0)) (Leaf (4,8)))) (Node (Node (Leaf (8,4)) (Leaf (7,5)))
        (Node (Leaf (3,8)) (Leaf (8,3))))))) (Node (Node (Node (Node (Node (Leaf
        (6,6)) (Leaf (2,8))) (Leaf (8,2))) (Node (Node (Leaf (4,7)) (Leaf (7,4))
        ) (Leaf (1,8)))) (Node (Node (Leaf (8,1)) (Leaf (8,0))) (Node (Node 
        (Leaf (0,8)) (Leaf (5,6))) (Leaf (3,7))))) (Node (Node (Node (Leaf (7,3)
        ) (Node (Leaf (6,5)) (Leaf (4,6)))) (Node (Leaf (2,7)) (Leaf (7,2))))
        (Node (Node (Node (Leaf (6,4)) (Leaf (5,5))) (Leaf (0,7))) (Leaf (1,7)))
        ))) (Node (Node (Node (Node (Leaf (7,1)) (Node (Leaf (7,0)) (Leaf (3,6))
        )) (Node (Node (Leaf (6,3)) (Leaf (4,5))) (Node (Leaf (5,4)) (Leaf (2,6)
        )))) (Node (Node (Leaf (6,2)) (Leaf (1,6))) (Node (Leaf (6,1)) (Node (
        Leaf (0,6)) (Leaf (6,0)))))) (Node (Node (Node (Leaf (5,3)) (Node (Leaf
        (3,5)) (Leaf (4,4)))) (Node (Leaf (2,5)) (Leaf (5,2)))) (Node (Leaf 
        (5,1)) (Node (Leaf (1,5)) (Leaf (0,5)))))))) (Node (Node (Node (Node (
        Node (Node (Leaf (3,4)) (Leaf (4,3))) (Node (Leaf (5,0)) (Leaf (2,4))))
        (Node (Node (Leaf (4,2)) (Leaf (3,3))) (Leaf (1,4)))) (Node (Node (Leaf
        (4,1)) (Node (Leaf (0,4)) (Leaf (4,0)))) (Node (Leaf (2,3)) (Leaf (3,2))
        ))) (Node (Node (Leaf (1,3)) (Leaf (3,1))) (Node (Node (Leaf (0,3)) 
        (Leaf (3,0))) (Leaf (2,2))))) (Node (Node (Leaf (1,2)) (Leaf (2,1))) 
        (Node (Leaf (0,2)) (Leaf (2,0)))))) (Node (Node (Leaf (1,1)) (Leaf 
        (0,1))) (Leaf (1,0)))) (Leaf (0,0))
tree14 = Node (Node (Node (Node (Node (Node (Node (Node (Leaf (14,15)) (Leaf 
        (15,14))) (Node (Leaf (13,15)) (Leaf (15,13)))) (Node (Node (Leaf 
        (12,15)) (Leaf (15,12))) (Node (Leaf (11,15)) (Leaf (15,11))))) (Node 
        (Node (Leaf (15,10)) (Node (Leaf (10,15)) (Leaf (9,15)))) (Node (Leaf 
        (15,9)) (Leaf (15,8))))) (Node (Node (Node (Node (Leaf (8,15)) (Leaf 
        (7,15))) (Leaf (15,7))) (Node (Leaf (6,15)) (Leaf (15,6)))) (Node (Node
        (Leaf (5,15)) (Leaf (15,5))) (Node (Leaf (4,15)) (Leaf (15,4)))))) (Node
        (Node (Node (Node (Leaf (3,15)) (Leaf (15,3))) (Node (Leaf (2,15)) (Leaf
        (15,2)))) (Node (Node (Leaf (15,1)) (Node (Leaf (1,15)) (Leaf (15,0))))
        (Node (Node (Node (Leaf (0,15)) (Node (Node (Leaf (14,14)) (Leaf (13,14)
        )) (Node (Leaf (14,13)) (Leaf (12,14))))) (Node (Node (Node (Leaf 
        (14,12)) (Leaf (13,13))) (Node (Leaf (11,14)) (Leaf (14,11)))) (Node (
        Node (Leaf (12,13)) (Leaf (13,12))) (Node (Leaf (10,14)) (Leaf (14,10)))
        ))) (Node (Node (Node (Node (Leaf (11,13)) (Leaf (13,11))) (Node (Leaf
        (12,12)) (Leaf (9,14)))) (Node (Node (Leaf (14,9)) (Leaf (10,13))) (Node
        (Leaf (13,10)) (Leaf (11,12))))) (Node (Node (Node (Leaf (12,11)) (Leaf
        (8,14))) (Node (Leaf (14,8)) (Leaf (9,13)))) (Node (Node (Leaf (13,9))
        (Leaf (7,14))) (Node (Leaf (14,7)) (Leaf (10,12))))))))) (Leaf (15,15)))
        ) (Node (Node (Node (Node (Node (Node (Node (Node (Node (Leaf (12,10))
        (Leaf (11,11))) (Node (Leaf (8,13)) (Leaf (13,8)))) (Node (Node (Node
        (Leaf (0,14)) (Leaf (14,0))) (Leaf (0,13))) (Leaf (14,6)))) (Node (Node
        (Node (Leaf (6,14)) (Leaf (9,12))) (Leaf (12,9))) (Node (Leaf (5,14))
        (Leaf (11,10))))) (Node (Node (Node (Leaf (14,5)) (Node (Leaf (10,11))
        (Leaf (7,13)))) (Node (Leaf (13,7)) (Leaf (14,4)))) (Node (Node (Leaf
        (8,12)) (Leaf (12,8))) (Node (Node (Leaf (4,14)) (Leaf (2,14))) (Leaf
        (3,14)))))) (Node (Node (Node (Node (Leaf (6,13)) (Leaf (13,6))) (Node
        (Leaf (14,3)) (Leaf (9,11)))) (Node (Node (Leaf (11,9)) (Leaf (10,10)))
        (Node (Leaf (14,2)) (Leaf (1,14))))) (Node (Node (Node (Leaf (14,1)) 
        (Leaf (5,13))) (Node (Leaf (13,5)) (Leaf (7,12)))) (Node (Node (Leaf 
        (12,7)) (Leaf (4,13))) (Node (Leaf (8,11)) (Leaf (11,8))))))) (Node 
        (Node (Node (Node (Node (Leaf (13,4)) (Leaf (9,10))) (Node (Leaf (10,9))
        (Leaf (6,12)))) (Node (Node (Leaf (12,6)) (Leaf (3,13))) (Node (Leaf 
        (13,3)) (Leaf (2,13))))) (Node (Node (Node (Leaf (13,2)) (Leaf (1,13)))
        (Node (Leaf (7,11)) (Leaf (11,7)))) (Node (Node (Leaf (13,1)) (Leaf 
        (5,12))) (Node (Leaf (12,5)) (Leaf (8,10)))))) (Node (Node (Node (Node
        (Leaf (10,8)) (Leaf (9,9))) (Node (Leaf (4,12)) (Leaf (12,4)))) (Node
        (Node (Leaf (6,11)) (Leaf (11,6))) (Node (Node (Leaf (13,0)) (Leaf 
        (0,12))) (Leaf (3,12))))) (Node (Node (Node (Leaf (12,3)) (Leaf (7,10)))
        (Node (Leaf (10,7)) (Leaf (2,12)))) (Node (Node (Leaf (12,2)) 
        (Leaf (5,11))) (Node (Leaf (11,5)) (Leaf (1,12)))))))) (Node (Node (Node
        (Node (Node (Node (Leaf (8,9)) (Leaf (9,8))) (Node (Leaf (12,1)) (Leaf
        (4,11)))) (Node (Node (Node (Leaf (12,0)) (Leaf (0,11))) (Leaf (3,11)))
        (Node (Node (Leaf (11,0)) (Leaf (0,10))) (Leaf (1,10))))) (Node (Node
        (Leaf (11,4)) (Node (Leaf (6,10)) (Leaf (10,6)))) (Node (Node (Leaf
        (7,9)) (Leaf (9,7))) (Node (Node (Leaf (10,0)) (Leaf (0,9))) (Leaf
        (9,0)))))) (Node (Node (Node (Leaf (11,3)) (Leaf (8,8))) (Node (Node 
        (Leaf (2,11)) (Leaf (5,10))) (Leaf (11,2)))) (Node (Node (Node (Leaf 
        (10,5)) (Leaf (1,11))) (Node (Leaf (11,1)) (Leaf (6,9)))) (Node (Leaf 
        (9,6)) (Leaf (10,4)))))) (Node (Node (Node (Node (Node (Leaf (4,10))
        (Leaf (7,8))) (Leaf (8,7))) (Node (Leaf (3,10)) (Leaf (10,3)))) (Node
        (Node (Leaf (5,9)) (Leaf (9,5))) (Node (Leaf (2,10)) (Leaf (10,2)))))
        (Node (Node (Node (Leaf (10,1)) (Leaf (6,8))) (Node (Leaf (8,6)) (Leaf
        (7,7)))) (Node (Node (Leaf (4,9)) (Leaf (9,4))) (Node (Leaf (3,9)) 
        (Leaf (9,3)))))))) (Node (Node (Node (Node (Node (Node (Leaf (5,8)) 
        (Leaf (8,5))) (Node (Leaf (2,9)) (Leaf (6,7)))) (Node (Node (Leaf (7,6))
        (Leaf (9,2))) (Node (Leaf (1,9)) (Leaf (9,1))))) (Node (Node (Node (Leaf
        (4,8)) (Leaf (8,4))) (Node (Leaf (5,7)) (Leaf (7,5)))) (Node (Node (Leaf
        (3,8)) (Leaf (8,3))) (Node (Leaf (6,6)) (Leaf (2,8)))))) (Node (Node 
        (Node (Node (Leaf (8,2)) (Leaf (1,8))) (Node (Leaf (4,7)) (Leaf (7,4))))
        (Node (Node (Leaf (8,1)) (Node (Leaf (0,8)) (Leaf (8,0)))) (Node (Leaf
        (5,6)) (Leaf (6,5))))) (Node (Node (Node (Leaf (1,7)) (Node (Leaf (0,7))
        (Leaf (7,0)))) (Leaf (7,3))) (Node (Node (Leaf (3,7)) (Leaf (2,7))) 
        (Leaf (7,2)))))) (Node (Node (Node (Node (Leaf (4,6)) (Leaf (6,4)))
        (Node (Leaf (5,5)) (Leaf (7,1)))) (Node (Node (Leaf (3,6)) (Leaf (6,3)))
        (Node (Leaf (4,5)) (Leaf (5,4))))) (Node (Node (Node (Leaf (2,6)) (Leaf
        (6,2))) (Node (Leaf (1,6)) (Leaf (6,1)))) (Node (Node (Node (Leaf (0,6))
        (Leaf (6,0))) (Leaf (3,5))) (Node (Leaf (5,3)) (Leaf (4,4))))))))) (Node
        (Node (Node (Node (Node (Node (Node (Leaf (2,5)) (Leaf (5,2))) (Node
        (Leaf (1,5)) (Node (Leaf (0,5)) (Leaf (5,0))))) (Node (Leaf (5,1)) (Node
        (Leaf (3,4)) (Leaf (4,3))))) (Node (Node (Leaf (2,4)) (Leaf (4,2)))
        (Node (Leaf (3,3)) (Leaf (1,4))))) (Node (Node (Node (Leaf (4,1)) (Node 
        (Leaf (0,4)) (Leaf (4,0)))) (Node (Leaf (2,3)) (Leaf (3,2)))) (Node 
        (Leaf (1,3)) (Leaf (3,1))))) (Node (Node (Node (Node (Leaf (0,3)) 
        (Leaf (3,0))) (Leaf (2,2))) (Leaf (1,2))) (Node (Leaf (2,1)) (Node 
        (Leaf (0,2)) (Leaf (2,0)))))) (Node (Node (Leaf (1,1)) (Leaf (0,1))) 
        (Node (Leaf (1,0)) (Leaf (0,0)))))
