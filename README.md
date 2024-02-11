# JOHN.rs

A JOHN parser for Rust! The Jane Object Hierarchy Notation is documented [here](https://jane.luemir.xyz/john)

It supports parsing the full standard (i think) and stringifying pre-constructed JohnValue contructs.

If you want to stringify a custom type you can contribute to this library by adding some magic genericism or serialization trait or you can implement a conversion trait for values to be directly packed into the JohnValue enum.
