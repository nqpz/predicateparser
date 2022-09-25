# predicateparser

Silly natural language parser into predicates using types and a stack.


## Example

Input: my famous neighbor who lives on a boat

Classic AUG (Applicative Universal Grammar):
  - `my (famous (who neighbor (on (a boat) lives)))`
  - `my (who (famous neighbor) (on (a boat) lives))`
  - `who (my (famous neighbor)) (on (a boat) lives)`

Relation parsing result:
- Object: neighbor
  - my(neighbor)
  - famous(neighbor)
  - ((on(boat))(lives))(neighbor)
- Object: boat
  - a(boat)

Problem: Multiple correct parses + we want to have predicates instead,
like so:

- "Neighbor" predicates:
  - my
  - famous
  - on(boat)(lives)
- "Boat" predicates:
  - a


## Limitations

Most.


## Building and running

Run `stack install` to install.  See
<https://docs.haskellstack.org/en/stable/install_and_upgrade/>.


## Misc. notes / TODO

Focus on prepositions: in, from, ...

Adjectives stack.

Relations on noun objects as end result.

Need to build up dictionary.
