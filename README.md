# predicateparser

Silly natural language parser into predicates using types and a stack.


## Example

Input: my famous neighbor who lives on a boat

Classic AUG (Applicative Universal Grammar):
  - `my (famous (who neighbor (on (a boat) lives)))`
  - `my (who (famous neighbor) (on (a boat) lives))`
  - `who (my (famous neighbor)) (on (a boat) lives)`

Problem: Multiple correct parses + we want to have predicates instead,
like so:

- "Neighbor" predicates:
  - my
  - famous
  - on(boat)(lives)
- "Boat" predicates:
  - a

Ignoring the "who" connector word for the moment.


## Limitations

Most.


## Building and running

Run `stack install` to install.  See
<https://docs.haskellstack.org/en/stable/install_and_upgrade/>.

```sh
echo 'my famous neighbor lives on a boat' | predicateparser
echo 'my door [lives in a house] on the boat' | predicateparser
```

The raw sentence "my door lives in a house on the boat" technically has
multiple interpretations, at least these two:

  1. my door lives in a [house on the boat]
  2. my door [lives in a house] on the boat

The nuances differ slightly depending on your focus.  We force the
second interpretation by using square brackets, though technically the
system doesn't actually support the first interpretation due to some
current technical limitations. However, it also won't work without
specifying the brackets, due to some other current limitations.


## Misc. notes / TODO

Focus on prepositions: in, from, ...

Adjectives stack.

Relations on noun objects as end result.

Need to build up dictionary.
