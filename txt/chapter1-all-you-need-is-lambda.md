# Chapter 1: All You Need is Lambda

## Lambda Calculus
The lambda calculus is composed of variables, abstractions, and applications.

Examples:
>  x
> 
> λxy.yx
> 
> (λx.x) (λx.x)

Parameters (variables in the head) bind to arguments once the abstraction is applied, replacing all occurrences of the parameters with the bound arguments. Example:

> (λx.x) y
> 
> (λ[x := y].x)
> 
> y

## Alpha Equivalence
Two lambda expressions are alpha-equivalent iff. one can have its variables renamed so as to produce the same expression as the other.

Example:
> (λx.x) == (λy.y) since x can be renamed to y or vice-versa

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

> λx.xy
> 
> Here, y is a free variable and x is bound.

Important to remember that alpha equivalence cannot exist for two terms with different free variables, but the same "form", such as:

> λx.xy
> 
> λx.xz

Even though the two terms above look similar, they are not alpha equivalent since *y* and *z* can refer to different things.

## Currying
Functions with multiple arguments are essentially multiple nested abstractions like so:
> λx.λy.yxz

Worked example:
> (λxy. xy) (λz.a) 1
> 
> (λ[x:=(λz.a)].λy.xy) 1
> 
> (λy. (λz.a) y) 1
> 
> λ[y:=1]. (λz.a) y
> 
> (λz.a) 1
> 
> λ[z:=1].a
> 
> a

A quick way to see the final output for this particular example:
1. the first abstraction applies its first arg to the second
2. the first arg yields *a* for any input *z*
3. the second arg is *1*
4. therefore, our output is always *a*

Another example:
> (λxyz.xz(yz)) (λmn.m) (λp.p)
> 
> (λ[x:=λmn.m]yz.xz(yz)) (λp.p)
> 
> (λyz.(λmn.m)z(yz)) (λp.p)
> 
> λ[y:=(λp.p)]z.(λmn.m)z(yz)
> 
> λz.(λmn.m)z((λp.p)z)  <-- irreducible top abstraction so go inside
> 
> λz.(λ[m:=z]n.m)((λp.p)z)
> 
> λz.(λn.z)((λp.p)z)
> 
> λz.z

The final step is easy to see as the λmn.m abstraction ignores its second argument.

## Exercise Intermission (pages 19-20)
1. *λxy.xz* is alpha equivalent to *λmn. mz* (since *x* can be renamed to *m*)
2. *λxy.xxy* is alpha equivalent to *λa(λb.aab)* since *a* can be renamed to *x* and *b* can be renamed to *y*
3. *λxyz.zx* is alpha equivalent to *λtos.st* since *t* -> *x*, *o* -> *y*, *s* -> *z*

## Evaluation and Normal Forms
There are many normal forms, but we focus on beta normal form. This basically means an expression that cannot be beta-reduced anymore. This corresponds also to a fully executed program.

Examples:
> λx.x is in beta normal form
> 
> (λx.x)z is **NOT** in beta normal form as there is one step of beta reduction possible

## Combinators
A combinator is a lambda term with no free variables. They basically serve to *combine* their arguments; no more, no less.

Example:
> λxyz.z(yy)

Non-example:
> λx.zx

## Divergence
Some lambda terms, when beta reduced, never reach a normal form. They are said to *diverge*, as opposed to terms that do reduce, or *converge*, to a normal form.

Example:
> (λx.xx) (λx.xx)
> 
> (λ[x:=λx.xx].xx)
> 
> (λx.xx) (λx.xx)

The last step leaves us back where we started. This is a divergent, or non-terminating, computation.