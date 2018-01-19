ndpar_lib
=====

This repository is a collection of algorithms and exercises from the following books

- [Cryptography Engineering](https://www.schneier.com/books/cryptography_engineering/) by Niels Ferguson, Bruce Schneier, and Tadayoshi Kohno
- [Erlang and OTP in Action](https://www.manning.com/books/erlang-and-otp-in-action) by Martin Logan, Eric Merritt, and Richard Carlsson
- [Erlang Programming](http://shop.oreilly.com/product/9780596518189.do) by Francesco Cesarini and Simon Thompson
- [Introduction to Algorithms](https://mitpress.mit.edu/books/introduction-algorithms) by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, and Clifford Stein
- [Programming Erlang](https://pragprog.com/book/jaerlang2/programming-erlang) (1st and 2nd editions) by Joe Armstrong

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit
    $ rebar3 dialyzer

Documentation
-----

    $ rebar3 edoc
