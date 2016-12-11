trait Foo {

}

trait Bar {
  this:Foo =>
}

class Baz extends Foo with Bar
