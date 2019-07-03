# Tries

A trie (sometimes called a prefix tree) is an implementation of a finite map for
structured key types where common prefixes of the keys are grouped together.
Most frequently, tries are used with strings as key type. As hashes are also
strings, such as trie can also be used to map hashes to values.
In Haskell, such a trie can be represented as follows:

```haskell
data Trie a = Fork (Maybe a) (Map Char (Trie a))
```

A node in the trie contains an optional value of type a. If the empty string is in
the domain of the trie, the associated value can be stored here. Furthermore, the
trie maps single characters to subtries. If a key starts with one of the chracters
contained in the map, then the rest of the key is looked up in the corresponding
subtrie.
The `Map` type is from `Data.Map` in the [containers](http://hackage.haskell.org/package/containers-0.6.2.1)
package. The following is an example trie that maps “f” to 0, “foo” to 1, “bar” to 2 and
“baz” to 3:

```haskell
Fork Nothing 
    ( fromList 
        [ 
            ( 'b'
            , Fork Nothing 
                ( fromList 
                    [ 
                        ( 'a'
                        , Fork Nothing 
                            ( fromList 
                                [ 
                                    ( 'r'
                                    , Fork ( Just 2 ) ( fromList [] )
                                    ) 
                                , 
                                    ( 'z'
                                    , Fork ( Just 3 ) ( fromList [] )
                                    ) 
                                ] 
                            )
                        ) 
                    ]
                )
            ) 
        , 
            ( 'f'
            , Fork ( Just 0 ) 
                ( fromList 
                    [ 
                        ( 'o'
                        , Fork Nothing 
                            ( fromList 
                                [ 
                                    ( 'o'
                                    , Fork ( Just 1 ) ( fromList [] )
                                    ) 
                                ]
                            )
                        ) 
                    ]
                )
            ) 
        ] 
    )
```

We call a trie *valid* if all its subtries are non-empty and valid.
Note that even though this definition might sound circular, it is in fact not: For
example, the definition shows that a trie *without subtries* is valid, because in
this case, the condition is trivially true.
In particular, the trie `Fork Nothing empty` is valid, and it is the only valid trie
that does not contain any values.
Note also that the definition implies that *all subtries of a trie are non-empty*.
The implementation should obey the invariant of validity, i.e.
*all created tries should be valid*.

## Excercises

1. Implement the following interface:

```haskell
empty  :: Trie a -- produces an empty trie
null   :: Trie a -> Bool -- check if a Trie is empty
valid  :: Trie a -> Bool -- checks if a trie adheres to the invariant
insert :: String -> a -> Trie a -> Trie a -- inserts/overwrites a key-value pair
lookup :: String -> Trie a -> Maybe a -- looks up the value associated with the key
delete :: String -> Trie a -> Trie a -- deletes the key if it exists
```

2. Furthermore, make your trie an instance of the `Functor` and `Foldable` classes.
Using deriving for these is ok, but please convince yourself that the derived
functions work as expected.

3. What can be used as a model of Trie? (Optional: can you explain it?)

4. Implement abstraction function for your Trie

5. After finishing 4, implement following interfaces:

```haskell
member :: String -> Trie a -> Bool -- Checks if given String is member of trie
keys   :: Trie a -> [String] -- Return all String of the trie
elems  :: Trie a -> [a] -- Return all the elements of the trie
union  :: Trie a -> Trie a -> Trie a -- Produces left-biased union of two tries
size   :: Trie a -> Int -- Return size of the trie
```

6. Implement test cases for your Trie using [Quickcheck](http://hackage.haskell.org/package/QuickCheck)

7. Try injecting bugs into your trie to see if your tests can find it.

## Extra challenges

1. In what way is your trie specific to `String`? Can you generalize it?

2. Make your trie an instance of the `Monoid` class

3. Test that your trie follows `Monoid` and `Functor` laws.