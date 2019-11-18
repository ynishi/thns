# thns
Query Condition builder, supports 1000 elements separation as haskell internal DSL.

## Example
### ghci 
```
stack ghci
```
```
*Main Lib> x = G "c1" [1..5]
*Main Lib> y = G "c2" [10..13]
*Main Lib> z = And x y
*Main Lib> z
And (G "c1" [1,2,3,4,5]) (G "c2" [10,11,12,13])
*Main Lib> putStrLn $ pp z
(((c1 in ('1'
,'2'
,'3'
,'4'
,'5'))) and ((c2 in ('10'
,'11'
,'12'
,'13'))))
*Main Lib>
```
* long example 
```
And
  (G "tag" ((map show [1 .. 2000]) :: [String]))
  (G "tag2" (["\"a", "'b"] :: [String]))
```
### from text
* column -> 'column_name
* value -> reteral
* expr
  * column val as entity
  * column [val] as entity
  * entity and entity
```
'column1 [
val1
val2
...
valn
] and 'column2 [
val1'
val2'
...
valn'
] and 'column3 val1''
```
