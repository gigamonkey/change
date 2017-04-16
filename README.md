Early on in the classic computer science text, _Structure and
Interpretation of Computer Programs_, in section 1.2.2, “Tree
recursion”, having just discussed the classic recursive Fibonacci
function and how to turn it into an iterative algorithm, they move
onto another problem, that of counting the number of ways you can make
change for a given amount with a set of coin denominations. For
instance, how many ways can you make $1.00 using the pennies, nickles,
dimes, quarters, and half dollars?

They give a recursive solution, but point out that it suffers from the
same liability as the naive recursive Fibonacci function, namely that
many parts of the tree are computed over and over again, so the run
time grows exponentially. They then leave as a challenge for the
reader to figure out an analogue to the iterative Fibonacci function
that can compute the answer in linear time.

I have a particular attachment to this problem because the first time
I tried to read _SICP_, I decided—for whatever muleheaded reason—that
I wouldn’t read any farther until I had figured out the challenge.

The result of that decision was that several years later I was still
stuck in section 1.2.2 of _SICP_ because I hadn’t been able to figure
it out, despite banging of my head against it pretty hard and
returning to the problem again from time to time.

I did eventually figure it out, by which time there was a new edition
of SICP out which I bought and read all the way through but I kind of
stumbled my way to it. I only got what feels like a decent grip on
what was going on later, when I had a chance to dig into dynamic
programming which is the technique that allows you to solve the
problem in linear time and, with a bit of care, constant space.

Lately I’ve been playing around with this problem again, this time in
Haskell which gives us some interesting options not easily available
in strict languages.

To start with let’s review the basic recursive solution. We can start
with the type signature for a function, `ways`, that takes a list of
`Int`s representing the denominations of coins we have available and
another `Int` representing the amount and returns an `Integer`
representing the number of ways to make change. (We use `Integer` for
the return value since the number of ways to make change can get quite
large, more than 2^64. We could use `Integer` for the other two
arguments but in some of the later implementations of this function
that won’t be an option and I want to all the different versions to
have the same type. As an aside, let me say I am not a huge fan of the
numeric stack in Haskell which seems to trip me up over and over.)

As always when using recursion, we need to find a way to break the
problem down into problems that are in some sense “smaller”. Unlike a
simpler problem like computing Fibonacci numbers, there are two
dimensions to the size of the change counting problem: the number of
kinds of coins and the total amount. Thus we can make the problem
smaller on either dimension—we can compute the number of ways of
making change for the same amount with fewer kinds of coins or we can
compute the number of ways of making change for a smaller amount with
the same kinds of coins. With those two ways of shrinking the problem
in mind we can turn to the other crucial part of any recursive
solution, the base cases, i.e. the versions of the problem that are
small enough we don’t need to break them down any further. Given that
we have two dimensions to the problem we will likely have at least two
base cases. If we shrink the problem to the smallest possible number
of coins, zero, then we have the problem of how many ways can we make
change for a given amount with no coins, which is clearly zero. On the
other dimension, the amount, the smallest problem is zero cents. And
there is exactly one way to make change for zero cents, regardless of
how many different kinds of coins we have, i.e. using no coins.
Finally, it turns out there is actually one more base case: since we
know we are going to be recursing on smaller and smaller amounts, it’s
possible that we could try to compute the number of ways of making
change for a negative amount, of which there are zero ways, regardless
of the kinds of coins.

   ways :: [Int] -> Int -> Integer

We can now easily implement the three bases cases:

   ways [] _        = 0
   ways _ 0         = 1
   ways _ n | n < 0 = 0

All that’s left is to implement the recursive case. We can use pattern
matching to split the list of kinds of coins into the first kind and
all the remaining kinds. Then we can reduce this problem to two
smaller problems: the number of ways to make change for the same
amount using all but the first kind of coin and the number of ways to
make change for the original amount minus the denomination of the
first coin using all the kinds of coins. In other words all the ways
to make change without any of the first coin plus all the ways to make
change with at least one of the first kind of coin.

   ways (c:cs) n = ways cs n + ways (c:cs) (n - c)

Unfortunately, as elegant as this solution is, it’s terribly slow. The
problem is, if you look at the tree of calls
