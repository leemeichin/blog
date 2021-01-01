---
title: Can you crack the code?
date: 2020-12-31
category: programming
status: published
---

What better way to spend the final moments of 2020, locked down in London, than with a logic puzzle and a computer that can, well, do logic? Join me for a few minutes on this auspicious eve, and learn how you can spend an order of magnitude more time computing a solution than what it would take if you used your noggin instead.

I presume you've seen this kind of puzzle before: there is a lock that requires a three or four digit code in order for it to open. You don't know what the code is, of course, but you're given a series of clues that will test your powers of deduction and lead you to the right answer. I've actually got such a puzzle here:

---

<div style="text-align: center">
**CAN YOU CRACK THE CODE?**

🔐 \_ \_ \_ \_

**9** **2** **8** **5**  
One number is correct, but in the wrong place.

**1** **9** **3** **7**  
Two numbers are correct, but in the wrong place.

**5** **2** **0** **1**  
One number is correct, and is in the right place.

**6** **5** **0** **7**  
None of the numbers are correct, in any place.

**8** **5** **0** **4**  
Two numbers are correct, but in the wrong place.

</div>

---

## A brief introduction

If you're unaware of Prolog, it's a _logical progamming_ language that, in its most simplest terms, takes a bunch of facts and rules and then gives you the tools to query them to get the outcome you want. In more complicated terms, a cursory search on the intertubes will lead you to a vast collection of academic papers that explain more. This is not the kind of language that is casually blogged about by the masses, as with more mainstream ones like CSS, HTML, or ColdFusion.

> Programming in Prolog is significantly different from conventional procedural programming and requires a readjustment in the way one things about programming. Logical relationships are asserted, and Prolog is used to determine whether or not certain statements are true, and if true, what variable bindings make them true. This leads to a very declarative style of programming.
>
> Dennis Merritt, _Adventure in Prolog_, 2017[^0].

Mr Merritt is, to put it professionally, **god damn right**. Here's a valid Prolog program:

```prolog
% https://swish.swi-prolog.org/p/KfdGtcJr.swinb

president(trump).
president(obama).
president(brie).

cheese(brie).
cheese(wensleydale).
person(trump).
person(obama).
```

What we have here are some facts, both true and technically true. It's a fact that Obama is a president, as is Trump. It's also a fact that there is a brand of cheese in the UK called President. This is quite ambiguous as a result so some extra facts are supplied, namely that brie is a cheese as much as it is a President-brand cheese, and that Wensleydale is also a cheese. It goes without saying that Trump and Obama are people, so with those facts we should be able to do some querying.

If you're doing this on your own machine, you can save those facts into a file (say, `example.pl`) and then importing it inside a console, like so: `[example].`. Otherwise, you can load up the Swish notebook[^1] and follow along using an online console, no installation needed!

Let's do some querying then, which will show you how Prolog might seem a bit back to front compared to what you're used to.

```prolog
president(trump). % true.
```

So far, so boring. We stated `president(trump)` as a fact in our first prolog file, so this is basically confirming that we did so. Let's spice it up a little.

```prolog
president(X). % trump; obama; brie.
```

<aside>With the online editor you can click 'Next' to see all of the results, and in the console you can type `;`. This can be done repeatedly until the input ends with `.`, which says there are no more facts that fit the query)</aside>

The fuck? What is `X`?

`X` is a variable, or a placeholder if you like. Any word starting with a capital letter is a variable, and when you pass one in a query Prolog will supply the results of the query to those variables. In this case, we're essentially saying `who are all the presidents? I don't know their names so put them all in X for me`.

Let's try one more thing, which should explain enough about Prolog to be dangerous.

```prolog
president(X), cheese(X). % brie.
```

_Now we're cookin' wi' gas!_ as we'd say back up north. A lot of what you do in prolog is chain little sentences like this together (using the comma operator `,`, which means `and`), and in this instance we're asking Prolog to get all the presidents, put them in X, and then show me only the presidents that are also a cheese. The `.` finishes the sentence, or the query. Let's do a similar query to wrap this intro up, and you can see if your guess at the answer is the same as what this produces.

```prolog
president(X), person(X). % trump, obama.
```

This is more or less the essence of Prolog, and your program is essentially a database of facts and rules, and then you use the program by querying those facts and rules. You'll make a query by providing what you _do_ know, and then placing a variable (or a placeholder) in the spots where you don't know the answer. You don't tell Prolog how exactly to compute that answer. And with that explained, I think we can try and crack this code.

## Doing some l33t haxx0ring

Here's the puzzle again, for reference:

---

<div style="text-align: center">
**CAN YOU CRACK THE CODE?**

🔐 \_ \_ \_ \_

**9** **2** **8** **5**  
One number is correct, but in the wrong place.

**1** **9** **3** **7**  
Two numbers are correct, but in the wrong place.

**5** **2** **0** **1**  
One number is correct, and is in the right place.

**6** **5** **0** **7**  
None of the numbers are correct, in any place.

**8** **5** **0** **4**  
Two numbers are correct, but in the wrong place.

</div>

---

According to Leon Sterling and Ehud Shapiro in _The Art of Prolog_[^2], this type of problem falls quite neatly under the umbrella of non-deterministic programming. This is because we're essentially going to build an algorithm that will use what they describe as a `generate and test` solution. We're going to write something that will take our clues and run through all the possible answers until it lands on the only one that fits. We're not aiming for beautiful optimisation here so this good enough, although the code we write will be tightly coupled to the exact puzzle provided.

So, let's begin with our set of rules:

```prolog
:- use_module(library(clpfd)). % we're working with numbers, this makes it easier.

clue_1([9, 2, 8, 5]). % one number correct, but in the wrong place
clue_2([1, 9, 3, 7]). % two numbers are correct, but in the wrong place
clue_3([5, 2, 0, 1]). % one number is correct, and is also in the right place
clue_4([6, 5, 0, 7]). % none of the numbers are correct, anywhere
clue_5([8, 5, 2, 4]). % two numbers are correct, but in the wrong place
```

<aside>If you're curious about the first `use_module` statement, beyond knowing that it makes things easier, check out the docs on <em>Constraint Logic Programming over Finite Domains</em>[^3].</aside>

These clues don't really mean anything by themselves, they're simple facts in Prolog terms, so we need to add a bit more to give these some meaning. All of this will go into the same file, as we're not ready to query yet.

```prolog
% rule: a digit is correct but it is in the wrong place
wrong_place(Digit, Index, Digits) :- nth1(Index1, Digits, Digit), Index \== Index1.

% rule: a digit is correct and it is in the right place
right_place(Digit, Index, Digits) :- nth1(Index, Digits, Digit).

% rule: the digit is wrong.
wrong(_, []).
wrong(Digit, [D|Ds]) :- Digit #\= D, wrong(Digit, Ds).
```

I'll leave the in-depth explanation of these rules to another post for the sake of brevity, and also because I'm not that much of a Prolog expert. These are all used to add meaning to the facts, as with these rules we can now define logic such as _one number is correct but in the wrong position_, and _none of the numbers are correct_. We just have to painstakingly mix and match them.

The next bit is quite long, but this query is where we make the sausage. Commentary will be written inline for ease of copy and paste, until I come back and edit this post with a more digestible version.

```prolog
crack_code(Code) :-
    % A, B, C and D represent the four digits of the code, which are all between 0 and 9.
    A in 0..9,
    B in 0..9,
    C in 0..9,
    D in 0..9,

    % ';' means 'or', whereas ',' means 'and'

    % one digit in D1 is correct, but in the wrong place
    % the other three digits must therefore be incorrect
    % query this for each digit.
    clue_1(D1),
    (
        wrong_place(A, 1, D1), wrong(B, D1), wrong(C, D1), wrong(D, D1);
        wrong_place(B, 2, D1), wrong(A, D1), wrong(C, D1), wrong(D, D1);
        wrong_place(C, 3, D1), wrong(A, D1), wrong(B, D1), wrong(D, D1);
        wrong_place(D, 4, D1), wrong(A, D1), wrong(B, D1), wrong(C, D1)
    ),

    % two digits are correct this time, and they are both in the wrong place
    % exhaustively check every combination where two numbers are correct, and the other two are incorrect.
    clue_2(D2),
    (
        wrong_place(A, 1, D2), wrong_place(B, 2, D2), wrong(C, D2), wrong(D, D2);
        wrong_place(A, 1, D2), wrong_place(C, 3, D2), wrong(B, D2), wrong(D, D2);
        wrong_place(A, 1, D2), wrong_place(D, 4, D2), wrong(B, D2), wrong(C, D2);

        wrong_place(B, 2, D2), wrong_place(A, 1, D2), wrong(C, D2), wrong(D, D2);
        wrong_place(B, 2, D2), wrong_place(C, 3, D2), wrong(A, D2), wrong(D, D2);
        wrong_place(B, 2, D2), wrong_place(D, 4, D2), wrong(A, D2), wrong(C, D2);

        wrong_place(C, 3, D2), wrong_place(A, 1, D2), wrong(B, D2), wrong(D, D2);
        wrong_place(C, 3, D2), wrong_place(B, 2, D2), wrong(A, D2), wrong(D, D2);
        wrong_place(C, 3, D2), wrong_place(D, 4, D2), wrong(A, D2), wrong(B, D2);

        wrong_place(D, 4, D2), wrong_place(A, 1, D2), wrong(B, D2), wrong(C, D2);
        wrong_place(D, 4, D2), wrong_place(B, 2, D2), wrong(A, D2), wrong(C, D2);
        wrong_place(D, 4, D2), wrong_place(C, 3, D2), wrong(A, D2), wrong(B, D2)
    ),

    % one digit is correct, and also in the right place
    % as above, we still don't know which digit that is, so we check each one.
    clue_3(D3),
    (
        right_place(A, 1, D3), wrong(B, D3), wrong(C, D3), wrong(D, D3);
        right_place(B, 2, D3), wrong(A, D3), wrong(C, D3), wrong(D, D3);
        right_place(C, 3, D3), wrong(A, D3), wrong(B, D3), wrong(D, D3);
        right_place(D, 4, D3), wrong(A, D3), wrong(B, D3), wrong(C, D3)
    ),

    % none of the digits are correct, so they can be completely excluded
    % we know for a fact the final result will not contain any of these digits.
    clue_4(D4),
    (
        wrong(A, D4), wrong(B, D4), wrong(C, D4), wrong(D, D4)
    ),

    % again, two digits are correct but not in the right order
    % we do a similar check as before but also need to look
    % back into the previous clue to eliminate wrong candidates;
    % this is why we query D2, as well as D5.
    clue_5(D5),
    (
        wrong_place(A, 1, D5), wrong_place(B, 2, D5), wrong(C, D5), wrong(D, D5);
        wrong_place(A, 1, D5), wrong_place(C, 3, D5), wrong(B, D5), wrong(D, D5);
        wrong_place(A, 1, D5), wrong_place(D, 4, D5), wrong(B, D2), wrong(C, D2);

        wrong_place(B, 2, D5), wrong_place(A, 1, D5), wrong(C, D5), wrong(D, D5);
        wrong_place(B, 2, D5), wrong_place(C, 3, D5), wrong(A, D5), wrong(D, D5);
        wrong_place(B, 2, D5), wrong_place(D, 4, D5), wrong(A, D2), wrong(C, D2);

        wrong_place(C, 3, D5), wrong_place(A, 1, D5), wrong(B, D5), wrong(D, D5);
        wrong_place(C, 3, D5), wrong_place(B, 2, D5), wrong(A, D5), wrong(D, D5);
        wrong_place(C, 3, D5), wrong_place(D, 4, D5), wrong(A, D2), wrong(B, D2);

        wrong_place(D, 4, D5), wrong_place(A, 1, D5), wrong(B, D5), wrong(C, D5);
        wrong_place(D, 4, D5), wrong_place(B, 2, D5), wrong(A, D5), wrong(C, D5);
        wrong_place(D, 4, D5), wrong_place(C, 3, D5), wrong(A, D2), wrong(B, D2)
    ),

    % Take (or cut) the first result, no need for continued backtracking
    % this is probably most similar to an early return or short-circuit.
    !,

    % we've cracked the code! A, B, C, and D each refer to
    % the only answer that makes sense given the previous
    % rules.
    Code = [A, B, C, D].
```

Did you solve the puzzle yourself? Do you remember the answer? If you don't care to copy and paste all of that, you can open up this ready made notebook[^4], and then run the following:

```prolog
crack_code([A, B, C, D]),
write('The first number is: '), write(A), write('\n'),
write('The second number is: '), write(B), write('\n'),
write('The third number is: '), write(C), write('\n'),
write('The fourth number is: '), write(D), write('\n').
```

The exercise of writing that in a less brute-force manner is left to you, my beloved reader.

## The grand finale

So ends 2020, so ends this post. Did your brain-grown answer match the one this Prolog program gave you? What do you think about logic programming in general now you've seen some of it? Why not share it with your friends or whoever, if they're interested, and see what they think?

Mad propz to the Prolog community on Reddit also, whose example solutions helped point me in the right direction [^5].

[^0]: <https://amzi.com/AdventureInProlog/a1start.php> (buy the book, srlsy...)
[^1]: <https://swish.swi-prolog.org/p/KfdGtcJr.swinb>
[^2]: <https://mitpress.mit.edu/books/art-prolog-second-edition>
[^3]: <https://www.swi-prolog.org/man/clpfd.html>
[^4]: <https://swish.swi-prolog.org/p/MgtEUnSv.swinb>
[^5]: <https://www.reddit.com/r/prolog/comments/fzww7m/cracking_this_puzzle_with_prolog/>
