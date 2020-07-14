# Drawing Trees in Haskell

This project is a simple Haskell implementation of the paper
[Functional Pearls: Drawing trees](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/functional-pearls/535113105064F7354260FF55841D529E) by Andrew J. Kennedy.
The original paper uses Machine ML for its implementation.

## Project Structure

The [`DrawingTrees.hs`](/DrawingTrees.hs) file contains the source of the
drawing trees code as a module. The [`example.hs`](/example.hs) file contains a
very short example program that uses the module. You can simple execute it by
running `runhaskell example.hs`.

## Note

This project is meant for academic use only* and is not production ready, as
it has a `O(n^2)` time complexity in the worst case. Take a look at the
paper for more information.

\*You are allowed to use it for non-academic purposes, it's just not recommended.

## License

This project is licensed under the [Apache License 2.0](/LICENSE).
