# Chapter 1: All You Need is Lambda

## Lambda Calculus
The lambda calculus is composed of variables, abstractions, and applications.

Examples:
>  x
> 
> \xy.yx
> 
> (\x.x) (\x.x)

Parameters (variables in the head) bind to arguments once the abstraction is applied, replacing all occurrences of the parameters with the bound arguments. Example:

> (\x.x) y
> 
> (\\[x := y].x)
> 
> y

## Alpha Equivalence
Two lambda expressions are alpha-equivalent iff. one can have its variables renamed so as to produce the same expression as the other.

Example:
> (\x.x) == (\y.y) since x can be renamed to y or vice-versa

## Beta Equivalence
The process of symbol shunting involved in application is called beta reduction. Example provided above. Applications are left associative and what that means is:

> Just as 1+2+3 = (1+2)+3
> 
> f g h = (f g) h
> 
> where f, g, h are lambda terms; note also that
> 
> f g h =/= f (g h)
> 
> in general, unlike addition (+)

## Free variables
Variables that are not bound by some abstraction are free. Example:

> \x.xy
> 
> Here, y is a free variable and x is bound.

Important to remember that alpha equivalence cannot exist for two terms with different free variables, but the same "form", such as:

> \x.xy
> 
> \x.xz

Even though the two terms above look similar, they are not alpha equivalent since *y* and *z* can refer to different things.

## Currying
Functions with multiple arguments are essentially multiple nested abstractions like so:
> \x.\y.yxz

Worked example:
> (\xy. xy) (\z.a) 1
> 
> (\\[x:=(\z.a)].\y.xy) 1
> 
> (\y. (\z.a) y) 1
> 
> \\[y:=1]. (\z.a) y
> 
> (\z.a) 1
> 
> \\[z:=1].a
> 
> a

A quick way to see the final output for this particular example:
1. the first abstraction applies its first arg to the second
2. the first arg yields *a* for any input *z*
3. the second arg is *1*
4. therefore, our output is always *a*

Another example:
> (\xyz.xz(yz)) (\mn.m) (\p.p)
> 
> (\\[x:=\mn.m]yz.xz(yz)) (\p.p)
> 
> (\yz.(\mn.m)z(yz)) (\p.p)
> 
> \\[y:=(\p.p)]z.(\mn.m)z(yz)
> 
> \z.(\mn.m)z((\p.p)z)  <-- irreducible top abstraction so go inside
> 
> \z.(\\[m:=z]n.m)((\p.p)z)
> 
> \z.(\n.z)((\p.p)z)
> 
> \z.z

The final step is easy to see as the \mn.m abstraction ignores its second argument.

## Exercise Intermission (pages 19-20)
1. *\xy.xz* is alpha equivalent to *\mn. mz* (since *x* can be renamed to *m*)
2. *\xy.xxy* is alpha equivalent to *\a(\b.aab)* since *a* can be renamed to *x* and *b* can be renamed to *y*
3. *\xyz.zx* is alpha equivalent to *\tos.st* since *t* -> *x*, *o* -> *y*, *s* -> *z*

## Evaluation and Normal Forms
There are many normal forms, but we focus on beta normal form. This basically means an expression that cannot be beta-reduced anymore. This corresponds also to a fully executed program.

Examples:
> \x.x is in beta normal form
> 
> (\x.x)z is **NOT** in beta normal form as there is one step of beta reduction possible

## Combinators
A combinator is a lambda term with no free variables. They basically serve to *combine* their arguments; no more, no less.

Example:
> \xyz.z(yy)

Non-example:
> \x.zx

## Divergence
Some lambda terms, when beta reduced, never reach a normal form. They are said to *diverge*, as opposed to terms that do reduce, or *converge*, to a normal form.

Example:
> (\x.xx) (\x.xx)
> 
> (\\[x:=\x.xx].xx)
> 
> (\x.xx) (\x.xx)

The last step leaves us back where we started. This is a divergent, or non-terminating, computation.