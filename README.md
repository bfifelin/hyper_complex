# HyperComplex

This gem provides a `HyperComplex` class highly compatible with other numeric classes.

The [hypercomplex numbers](https://en.wikipedia.org/wiki/Hypercomplex_number) form finite-dimensional algebra over the real numbers. These algebras are produced by the [Cayley–Dickson construction](https://en.wikipedia.org/wiki/Cayley%E2%80%93Dickson_construction). Examples of such algebras are complex numbers, quaternions, octonions, sedenions, etc.

The hypercomplex number can be represented as

$\displaystyle\sum_{i=0}^{2^n-1} a_ie_i$  where  $a_i \in \mathbf{R}$,  $e_0=1$,  $e_1^2= \ldots =e_{2^n-1}^2=-1$,  $n \in \mathbf{N}$

The identity unit ($e_0$) and imaginary units ($e_i, i\gt0$) form the basis for space of dimension $2^n$ over $\mathbf{R}$.

## Requirements
   
   Ruby >= 3.1

## Installation

Add this line to your application's Gemfile:

    gem 'mcalendar'

And then execute:

    $ bundle install

Or install it yourself as:

    $ gem install hyper_complex

## Usage

```ruby
require 'hyper_complex'

# Creation from complex numbers
q1 = HyperComplex.rect((1+2i), (3+4i)) #=> HyperComplex[1, 2, 3, 4]
q2 = HyperComplex.rect((5+6i), (7+8i)) #=> HyperComplex[5, 6, 7, 8]

# Creation from HyperComplex numbers
o1 = HyperComplex.rect(q1, q2) #=> HyperComplex[1, 2, 3, 4, 5, 6, 7, 8]

# Creation from real numbers
o2 = HyperCoplex[9, 10, 11, 12, 13] #=> HyperComplex[9, 10, 11, 12, 13, 0, 0, 0]

# Creation from polar form
q3 = HyperComplex.polar(1, Math::PI/3, Vector[1, 1, 1].normalize) #=> HyperComplex[0.5, 0.5, 0.5, 0.5]

# standard calculations between numeric instances
(q1+q2)*o1 / Complex::I - 24 #=> HyperComplex[(0/1), (88/1), (-40/1), (20/1), (-80/1), (-184/1), (112/1), (-84/1)]
```

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/bfifelin/hyper_complex.

## License

This project is licensed under the terms of the [MIT license](http://opensource.org/licenses/MIT).

## Acknowledgments

This gem is based on the gem [quaternion_c2](https://rubygems.org/gems/quaternion_c2) by Masahiro Nomoto.
