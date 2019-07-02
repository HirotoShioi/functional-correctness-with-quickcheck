A trie (sometimes called a prefix tree) is an implementation of a finite map for
structured key types where common prefixes of the keys are grouped together.
Most frequently, tries are used with strings as key type. As hashes are also
strings, such as trie can also be used to map hashes to values.
In Haskell, such a trie can be represented as follows:
data Trie a = Fork (Maybe a) (Map Char (Trie a))
A node in the trie contains an optional value of type a. If the empty string is in
the domain of the trie, the associated value can be stored here. Furthermore, the
trie maps single characters to subtries. If a key starts with one of the chracters
contained in the map, then the rest of the key is looked up in the corresponding
subtrie.
The Map type is from Data.Map in the containers package.
The following is an example trie that maps “f” to 0, “foo” to 1, “bar” to 2 and
“baz” to 3:

```

```

We call a trie valid if all its subtries are non-empty and valid.
Note that even though this definition might sound circular, it is in fact not: For
example, the definition shows that a trie without subtries is valid, because in
this case, the condition is trivially true.
In particular, the trie Fork Nothing empty is valid, and it is the only valid trie
that does not contain any values.
Note also that the definition implies that all subtries of a trie are non-empty.
The implementation should obey the invariant of validity, i.e. all created tries
should be valid.
Implement the following interface:

```
```

Furthermore, make your trie an instance of the Functor and Foldable classes.
Using deriving for these is ok, but please convince yourself that the derived
functions work as expected.
In what way is your trie specific to String? Can you generalize it?