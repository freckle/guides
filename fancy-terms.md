# Fancy Terms
Functional programmers borrow many terms from mathematics. These terms can cause a fair amount of anxiety for those who are not familiar. So why use them?

# Why use them?
The reason we use "fancy" terms is because they are precise. We use the word `Monoid` because it says exactly what we mean. We could say, "this type has a function that takes two arguments and produces the same type, as well that function is associative (uh oh concept), and..." You see where I'm going. Fancy terms allow us to be much more compact, they have high conceptual density and precision. This brevity allows use to communicate very complex concepts precisely and succinctly.

As well as precision and brevity, fancy terms often give us access. There are combined hundreds of years of writing and research built in to these terms. They are the common language of logicians, mathemeticians, computer scientists and other academics who study complex problems. By utilizing these terms we give ourselves access to mountains of solutions to big problems. If you have a problem, it has probably already been solved or atleast researched to death by some lonely academic in a dark dusty corner speaking this foreign tongue of fancy terms.

# What is the cost?
Every choice comes with benefits and costs. The cost of fancy terms is that anyone who uses one must be prepared to explain that term. This document attempts to briefly explain some of these fancy terms and provide some reason why they are beneficial.

# The Terms
* [arrow]
* [morphism]
* [domain]
* [codomain]
* [injective]
* [surjective]
* [bijective]
* [isomorphism]

## arrow
Functional programmers sometimes talk about an arrow from `A` to `B`. There is a typeclass `Arrow`, but often we are just talking about something more abstract. We just want to say `A` is conceptually linked to `B` in a specific direction. An arrow could be a function, a process, a network transfer, a memo I wrote on a napkin; these details don't matter. All that matters is somehow we got from `A` to `B`. This conceptual brevity lets us take a step back and focus on the bigger picture. If I told you I drove a car from point `A` to `B` then you've already inferred a lot of detail that I might not have wanted to convey. Arrows let us focus less on implementation details and more on abstract attributes that we desire.

## morphism
This is a fancy word for arrow used in category theory.

## object
OOP has co-opted the word object, but functional programmers still use it. Objects in OOP are members of a type, that encapsulated state, and have associated methods. In functional programming we throw away the state and methods. An object in FP is just a member of a type. `1` is an object of type `Int`. `"foo"` is an object of type `String`. Often we prefer to use the word value to avoid confusion, but mathematitions still like the word object.

## domain
The left hand side of an arrow. I.E. all the possible arguments to a function. `Int` is the domain of `Int -> String`.

## codomain
The right hand side of an arrow. I.E. All the possible results of a function. `String` is the codomain of `Int -> String`

## injective
An injective arrow is a one to one relationship in a single direction. The arrow `A -> B` is injective if all values in `A` have an arrow to `B` and distinct values in `A` never have an arrow to the same value in `B`. This does not mean that all values in `B` have a counterpart in `A`. The function `toDecimal :: Int -> Decimal` is injective because it will always result in a distinct value for every input, but there are strictly more `Decimal`s than `Int`s so the arrow only goes in one direction. An injective relationship is important because it means that we are not losing precision, but we might lose the ability to return to the original type.

## surjective
A surjection is when there is an arrow from every value in `A` to every value in `B`, but arrows may point to the same value. The function `round :: Decimal -> Int` is surjective. We can transform any `Decimal` to an `Int`, but 5.3 and 5.4 will both become 5. Surjective arrows are total, they do not include any partiality and functional programmers love that.

## bijective
A bijection is a one to one relationship in both directions. It is when an arrow exists between `A` and `B` where all values of `A` have an arrow to a value in `B` and all distinct values in `A` point to a distinct value in `B`. With fancier terms a bijection is an arrow that is injective and surjective. The bijective arrow `A -> B` gives rise to the bijective arrow `B -> A`. This is useful because it means we can change representations without losing information. It may be more convenient to utilize `Map Int Text` because it has a richer API, but more efficient to store in a `HashMap Int Text` and since a bijection exists between them we can freely swap representations for the specific use case.

## isomorphism
Isomorphism is similar to bijection. Nearly all bijections are isomorphisms. The difference is a matter of formulation. An isomorphism has specific laws that it must satisfy. An isomorphism exists between `A` and `B` if the arrows `f :: A -> B` and `g :: B -> A` satisfy the identity law `f . g = id` and `g . f = id`. Isomorphism gives us all the same power as bijection. It is strictly weaker than equality, but when we are transforming data it is often all we care about.
