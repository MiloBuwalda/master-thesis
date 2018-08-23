module ListZipper where

type ListZipper a = ([a],[a])
type LZ a= ListZipper a

-- (-:) :: (LZ a -> LZ a) -> LZ a
x -: f = f x

makeLZ :: [a] -> ([a],[a])
makeLZ xs = (xs,[])

idFLZ :: ListZipper a -> ListZipper a
idFLZ xs = xs 

gF :: ListZipper a -> ListZipper a
gF (x:xs,bs) = (xs,x:bs)

gB :: ListZipper a -> ListZipper a
gB (xs,b:bs) = (b:xs,bs)

orderLZ :: ListZipper a -> ListZipper a
orderLZ (xs,[]) = (xs,[])
orderLZ (xs,b:bs) = ([b],(reverse bs)++xs)

traverseLZ :: (ListZipper a -> ListZipper a) -> ListZipper a -> [ListZipper a]
traverseLZ f crnt@(xs,_) = go f crnt (length xs) where
  go f crnt 0  = []
  go f crnt@(xs,bs) d = (f nxt) : (go f nxt (d-1)) where
    nxt = (crnt -: gF)


-- del
gF' :: ListZipper a -> ListZipper a
gF' (x,[]) = (x,[])
gF' (bs,x:xs) = (x:bs,xs)

-- put back
gB' :: ListZipper a -> ListZipper a
gB' ([],x) = ([],x)
gB' (b:bs,xs) = (bs,b:xs)

orderLZ' :: ListZipper a -> ListZipper a
orderLZ' (xs,[]) = (xs,[])
orderLZ' (bs,x:xs) = ([x],(reverse bs)++xs)

traverseLZ' :: (ListZipper a -> ListZipper a) -> ListZipper a -> [ListZipper a]
traverseLZ' f crnt@(_,bs) = go f crnt (length bs) where
  go f crnt 0  = []
  go f crnt d = (f crnt) : (go f nxt (d-1)) where
    nxt = (crnt -: gF')