# Tips

Use ScalaCheck

```
amm
@ import $ivy.`org.scalacheck:scalacheck_2.11:1.11.3`
@ import org.scalacheck.Prop.forAll
@ val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
  l1.size + l2.size == (l1 ::: l2).size }
@ val propSqrt = forAll { (n: Int) => scala.math.sqrt(n*n) == n }
@ propSqrt.check
```
