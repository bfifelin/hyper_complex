# frozen_string_literal: true

require_relative 'version'
require 'matrix'

# @private
class Numeric
  def hrect = [real, imag]
  alias hyperrectangular hrect
end

# rubocop:disable Metrics/ClassLength, Lint/MissingCopEnableDirective

## Class HyperComplex
# Uses Cayley-Dickson construction
# * A subclass of +Numeric+
class HyperComplex < Numeric
  private

  @@mt = [[]]

  undef_method(*Comparable.instance_methods)

  def initialize(*arg) = @arv = arg.clone # rubocop:disable Lint/MissingSuper

  public

  class << self
    ##
    # Creates a HyperComplex object arg_a + arg_b*e[[i]]
    #
    # @param arg_a [Numeric]
    # @param arg_b [Numeric]
    # @return [HyperComplex]
    # @raise [ArgumentError]
    #
    # @example
    #  HyperComplex.rect(HyperComplex.rect(1+2i, 3+4i), HyperComplex.rect(5,6))
    #    #=> HyperComplex[1, 2, 3, 4, 5, 6, 0, 0]
    #
    def rect(arg_a, arg_b = 0)
      raise ArgumentError, 'Not all arguments are Numeric' unless arg_a.is_a?(Numeric) && arg_b.is_a?(Numeric)

      a = arg_a.real? ? [arg_a] : arg_a.hrect
      b = arg_b.real? ? [arg_b] : arg_b.hrect
      as = a.length
      bs = b.length
      if as > bs
        b.concat(Array.new(as - bs, 0))
      elsif as < bs
        a.concat(Array.new(bs - as, 0))
      end
      new(*(a + b))
    end
    alias rectangular rect

    ##
    # Creates a HyperComplex object.
    #
    # @param  args [Integer or Rational or Float, ...]
    # @return [HyperComplex]
    # @raise [ArgumentError]
    #
    # @example
    #  HyperComplex[1, 2, 3, 4, 5, 6] #=> HyperComplex[1, 2, 3, 4, 5, 6, 0, 0]
    #
    def hrect(*args)
      begin
        raise ArgumentError, 'Not all components are real' unless args.map(&:real?).all?
      rescue NoMethodError
        raise ArgumentError, 'Not all components are numeric'
      end
      s = args.length
      if s < 2
        if s.zero?
          args = [0, 0]
        else
          args.push 0
        end
        s = 2
      end
      s1 = 1 << (s.bit_length - 1)
      if s == s1
        new(*args)
      else
        new(*args.concat(Array.new((s1 << 1) - s, 0)))
      end
    end
    alias hyperrectangular hrect
    alias [] hrect

    private

    ##
    # Fast variant of rect (only HyperComplex arguments of the same dimension are valid, no any check are performed)
    #
    def _rect(arg_a, arg_b) = new(*(arg_a.hrect + arg_b.hrect))

    ##
    # Fast variant of hrect (no any check are performed for parameters' validity)
    #
    def _hrect(*arg) = new(*arg)

    public

    ##
    # Creates a HyperComplex object.
    #
    # @param radius [Integer or Rational or Float]
    # @param theta [Integer or Rational or Float]
    # @param vector [Vector or *[Integer or Rational or Float]]
    # @return [HyperComplex]
    # @raise [ArgumentError, TypeError]
    #
    # @example
    #  HyperComplex.polar(1, Math::PI / 4 , [1, 2, 3])
    #    #=> HyperComplex[-0.9794859508794878, 0.0538564706483192, 0.1077129412966384, 0.1615694119449576]
    #
    def polar(radius, theta, vector)
      vs1 = vector.size + 1
      raise TypeError, 'Vector size must be at least 3' if vs1 < 4
      raise TypeError, 'Vector size is not equal to 2**n - 1' if vs1 != 1 << (vs1.bit_length - 1)

      begin
        raise ArgumentError, 'Not all components are real' unless [radius, theta, *vector].map(&:real?).all?
      rescue NoMethodError
        raise ArgumentError, 'Not all components are numeric'
      end
      vector = Vector[*vector] unless vector.is_a?(Vector)
      norm = vector.norm
      theta *= norm
      r_cos = radius * Math.cos(theta)
      r_sin = radius * Math.sin(theta)
      r_sin /= norm unless norm.zero?
      hrect(r_cos, *(r_sin * vector))
    end

    ##
    # Creates HyperComplex basis element (identity or imaginary)
    #
    # @param num [Integer]
    # @return [HyperComplex or Integer]
    # @raise [ArgumentError]
    #
    # @example
    #  HyperComplex.e(5) #=> HyperComplex[0, 0, 0, 0, 0, 1, 0, 0]
    #
    def e(num)
      raise ArgumentError, 'Argument must be non-negative integer' if !num.is_a?(Integer) || num.negative?
      return 1 if num.zero?

      hrect(*Array.new(num, 0), 1)
    end

    ##
    # Creates HyperComplex zero
    #
    # @param num [Integer]
    # @return [HyperComplex or Integer]
    # @raise [ArgumentError]
    #
    # @example
    #  HyperComplex.zero(5) #=> HyperComplex[0, 0, 0, 0, 0, 0, 0, 0]
    #
    def zero(num)
      raise ArgumentError, 'Argument must positive integer' unless num.is_a?(Integer) && num.positive?
      return 0 if num == 1

      hrect(*Array.new(num, 0))
    end

    ##
    # Prints the multiplication table to stdout
    #
    # @param dim [Integer]
    # @raise [ArgumentError]
    #
    # @example
    #  HyperComplex.print_mt(8)
    #      |  e0  e1  e2  e3  e4  e5  e6  e7
    #  ----+--------------------------------
    #    e0|  e0  e1  e2  e3  e4  e5  e6  e7
    #    e1|  e1 -e0  e3 -e2  e5 -e4 -e7  e6
    #    e2|  e2 -e3 -e0  e1  e6  e7 -e4 -e5
    #    e3|  e3  e2 -e1 -e0  e7 -e6  e5 -e4
    #    e4|  e4 -e5 -e6 -e7 -e0  e1  e2  e3
    #    e5|  e5  e4 -e7  e6 -e1 -e0 -e3  e2
    #    e6|  e6  e7  e4 -e5 -e2  e3 -e0 -e1
    #    e7|  e7 -e6  e5  e4 -e3 -e2  e1 -e0
    #
    def print_mt(dim) # rubocop:disable Metrics/AbcSize
      table = @@mt = calc_mt(dim) if dim > @@mt.length
      ss = dim
      sf = ss.to_s.length + 3
      out_s = "#{' ' * sf}|"
      (0...ss).each { out_s << format("%#{sf}s", "e#{_1}") }
      puts out_s
      puts "#{'-' * sf}+#{'-' * ss * sf}"
      out_s = format("%#{sf}s|", 'e0')
      (0...ss).each { out_s << format("%#{sf}s", "e#{_1}") }
      puts out_s
      (1...ss).each do |i|
        out_s = format("%#{sf}s|%#{sf}s", "e#{i}", "e#{i}")
        (1...i).each do |j|
          t = table[i][j]
          out_s << format("%#{sf}s", (t.negative? ? '-' : ' ') + "e#{t.abs}")
        end
        out_s << format("%#{sf}s", '-e0')
        ((i + 1)...ss).each do |j|
          t = table[j][i]
          out_s << format("%#{sf}s", (t.negative? ? ' ' : '-') + "e#{t.abs}")
        end
        puts out_s
      end
    end

    private

    def calc_mt(dim) # rubocop:disable Metrics/AbcSize,Metrics/CyclomaticComplexity,Metrics/PerceivedComplexity
      raise ArgumentError, 'Argument must be positive integer' unless dim.is_a?(Integer) && dim.positive?

      raise ArgumentError, 'Argument must be equal to power of 2' unless (dim & (dim - 1)).zero?

      raise "Seems to have infite loop while calculating multiplication table, dim=#{dim}" if dim <= @@mt.length

      mt = Array.new(dim) { [] }
      (1...dim).each do |i|
        ((i + 1)...dim).each do |j|
          c = dim
          r_and = i & j
          res_e = (i ^ j)
          f1 = i
          f2 = j
          while (c >>= 1) > 1
            unless ((i | j) & c).zero? # (Not a & c)
              if !(r_and & c).zero?
                res_e = -res_e if (f2 & ~c).zero?
                f1, f2 = f2, f1 # -d.conj*b
              elsif (f1 & c).zero?
                f1, f2 = f2, f1 # d*a
              else
                res_e = -res_e unless (f2 & ~c).zero? # b*c.conj
              end
            end
            break if (f1 &= ~c).zero? || (f2 &= ~c).zero?
          end
          res_e = -res_e unless (r_and & 1).zero?
          mt[j][i] = -res_e
        end
      end
      mt
    end

    def calc_mt_classic(dim)
      raise ArgumentError, 'Argument must be positive integer' unless dim.is_a?(Integer) && dim.positive?

      raise ArgumentError, 'Argument must be equal to power of 2' unless (dim & (dim - 1)).zero?

      mt = Array.new(dim) { [] }
      t_ar = Array.new(dim, 0)
      t_ar[0] = 1
      ar11 = [1, -1]
      ids = Array.new(dim) { HyperComplex.e(_1) }
      (1...dim).each do |i|
        ((i + 1)...dim).each do |j|
          t_ar = ids[i].mul(ids[j]).hrect
          ind = t_ar.index { ar11.include?(_1) }
          mt[j][i] = -ind * t_ar[ind].to_i
        end
      end
      mt
    end
  end

  ##
  # Accessors
  #

  ##
  # Returns an array of two numbers.
  #
  # @return [[Integer or Rational or Float, Integer or Rational or Float] or [HyperComplex, HyperComplex]]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4].rect #=> [HyperComplex[1, 2], HyperComplex[3, 4]]
  #
  def rect
    ssh = @arv.length >> 1
    return @arv if ssh == 1

    [__new__(*@arv[0...(ssh)]), __new__(*@arv[ssh..])]
  end
  alias rectangular rect

  ##
  # Returns an array of real numbers.
  #
  # @return [[Integer or Rational or Float, ...]]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4].hrect #=> [1, 2, 3, 4]
  #
  def hrect = @arv
  alias hyperrectangular hrect
  alias to_a hrect

  ##
  # Returns the real part.
  #
  # @return [Integer or Rational or Float]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4].real #=> 1
  #
  def real = @arv[0]
  alias scalar real

  ##
  # Returns the imaginary part as a vector.
  #
  # @return [Vector]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4].imag #=> Vector[2, 3, 4]
  #
  def imag = Vector[*hrect.drop(1)]
  alias imaginary imag
  alias vector imag

  ##
  # Returns the angle part of its polar form.
  #
  # @return [Integer or Rational or Float]
  #
  # @example
  #  HyperComplex[3, 2, 2, 1].arg #=> 0.7853981633974483 (Math::PI/4)
  #
  def arg = Math.atan2(imag.norm, real)

  alias angle arg
  alias phase arg
  ##
  # Returns the axis part of its polar form.
  #
  # @return [Vector]
  #
  # @example
  #  HyperComplex[6, 4, 4, 2].axis
  #    #=> Vector[0.6666666666666666, 0.6666666666666666, 0.3333333333333333]
  #
  def axis
    v = imag
    norm = v.norm
    norm.zero? ? Vector[1, *Array.new(@arv.length - 2, 0)] : v / norm
  end

  ##
  # HyperComplex is not real number.
  #
  # @return [false]
  #
  # @example
  #  HyperComplex[6, 4, 4, 2].real? #=> false
  #
  def real? = false

  ##
  # Returns true if all components are zero, otherwise returns false
  #
  # @return [true or false]
  #
  # @example
  #  HyperComplex[6, 4, 4, 2].zero? #=> false
  #
  def zero? = @arv.map(&:zero?).all?

  ##
  # Returns true if any component is NaN, otherwise returns false
  #
  # @return [true or false]
  #
  # @example
  #  HyperComplex[6, 4, Float::NAN, 2].nan? #=> true
  #
  def nan? = @arv.map { |i| i.equal?(Float::NAN) }.any?

  ##
  # Unary operations
  #

  # defined by Numeric:
  # * +@ #=> self

  ##
  # Returns negation of the value.
  #
  # @return [HyperComplex]
  #
  # @example
  #  -HyperComplex[6, 4, 1, 2] #=>  HyperComplex[-6, -4, -1, -2]
  #
  def -@ = __new__(*@arv.map(&:-@))

  ##
  # Returns its conjugate.
  #
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[6, 4, 1, 2].conj #=>  HyperComplex[6, -4, -1, -2]
  #
  def conj = __new__ @arv[0], *@arv[1..].map(&:-@)
  alias conjugate conj

  ##
  # Returns square of the absolute value.
  #
  # @return [Integer or Rational or Float]
  #
  # @example
  #  HyperComplex[2, 2, 2, 2].abs2 #=> 16
  #
  def abs2 = @arv.sum { _1**2 }

  ##
  # Returns the absolute part of its polar form.
  #
  # @return [Integer or Rational or Float]
  #
  # @example
  #  HyperComplex[2, 2, 2, 2].abs #=> 4
  #
  def abs = Math.sqrt(abs2)
  alias magnitude abs

  ##
  # Returns an array; +[abs, arg, axis]+.
  #
  # @return [[Integer or Rational or Float, Integer or Rational or Float, Vector]]
  #
  # @example
  #  HyperComplex[3, 4, 4, 0].polar
  #   #=> [6.4031242374328485, 1.0831800840797905, Vector[0.7071067811865475, 0.7071067811865475, 0.0]]
  #
  def polar = [abs, arg, axis]

  ##
  # Returns true if it equals to the other algebraically.
  #
  # @param other [Object]
  # @return [true or false]
  #
  # @example
  #  HyperComplex[1, 2, 3] == HyperComplex[1, 2, 3, 0] #=> true
  #
  def ==(other)
    sar, oar = homogenize(other)
    sar == oar
  end

  ##
  # Returns true if it have the same dimension and the same elements.
  #
  # @param other [Object]
  # @return [true or false]
  #
  # @example
  #  HyperComplex[1, 2, 3].eql?(HyperComplex[1, 2, 3, 0]) #=> true
  #  HyperComplex[1/1r, 2, 3, 4].eql?(HyperComplex[1, 2, 3, 4]) #=> false
  #
  def eql?(other) = @arv.eql?(other.hrect)

  # defined by Numeric:
  # * nonzero? #=> zero? ? nil : self

  defined_methods = public_instance_methods

  if defined_methods.include?(:finite?)
    ##
    # Returns true if its magnitude is finite, oterwise returns false.
    #
    # @return [true or false]
    #
    # @example
    #  HyperComplex[1, 2, 3, 4].finite? #=> true
    #  HyperComplex[1, 2, Float::INFINITY, 4].finite? #=> false
    #
    def finite? = @arv.map(&:finite?).all?
  end

  if defined_methods.include?(:infinite?)
    ##
    # Returns true if its magnitude is infinite, oterwise returns false.
    #
    # @return [true or false]
    #
    # @example
    #  HyperComplex[1, 2, 3, 4].infinite? #=> false
    #  HyperComplex[1, 2, Float::INFINITY, 4].infinite? #=> true
    #
    def infinite? = @arv.map(&:infinite?).any?
  end

  undef positive? if defined_methods.include?(:positive?)
  undef negative? if defined_methods.include?(:negative?)
  undef step, ceil, floor, round, truncate

  ##
  # Performs addition.
  #
  # @param other [Numeric]
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[1, 1] + HyperComplex[0, 0, 0, 0, 1, 1] #=> HyperComplex[1, 1, 0, 0, 1, 1, 0, 0]
  #
  def +(other)
    sar, oar = homogenize(other)
    __new__(*sar.zip(oar).map(&:sum))
  end

  ##
  # Performs subtraction.
  #
  # @param other [Numeric]
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[1, 1] - HyperComplex[0, 0, 0, 0, 1, 1] #=> HyperComplex[1, 1, 0, 0, -1, -1, 0, 0]
  #
  def -(other)
    sar, oar = homogenize(other)
    __new__(*sar.zip(oar).map { |x| x.reduce(:-) })
  end

  ##
  # Performs multiplication (a, b)*(c, d) = (a*c - d.conj*b, d*a + b*c.conj).
  #
  # @param other [Numeric]
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4] * HyperComplex[4, 3, 2, 1] #=> HyperComplex[-12, 6, 24, 12]
  #
  def *(other) # rubocop:disable Metrics/AbcSize
    sar, oar = homogenize(other)
    ss = sar.length
    begin
      res = Array.new(ss, 0)
      res[0] = sar[0] * oar[0]
      (1...ss).each do |i|
        res[0] -= sar[i] * oar[i]
        res[i] += sar[0] * oar[i] + sar[i] * oar[0]
        ((i + 1)...ss).each do |j|
          ind = @@mt[j][i]
          if ind.positive?
            res[ind] -= sar[i] * oar[j] - sar[j] * oar[i]
          else
            res[-ind] += sar[i] * oar[j] - sar[j] * oar[i]
          end
        end
      end
    rescue NoMethodError, TypeError
      @@mt = HyperComplex.send(:calc_mt, ss)
      retry
    end
    __new__(*res)
  end

  ##
  # Same as *, but 'classic' non-effective algorythm is used.
  #
  # @param other [Numeric]
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4].mul(HyperComplex[4, 3, 2, 1]) #=> HyperComplex[-12, 6, 24, 12]
  #
  def mul(other) # rubocop:disable Metrics/AbcSize
    sar, oar = homogenize(other)
    ss = sar.length
    return __new__(sar[0] * oar[0] - oar[1] * sar[1], oar[1] * sar[0] + sar[1] * oar[0]) if ss == 2

    a = __new__(*sar[0...(ss / 2)])
    b = __new__(*sar[(ss / 2)..])
    c = __new__(*oar[0...(ss / 2)])
    d = __new__(*oar[(ss / 2)..])
    __new__(*(a.mul(c) - d.conj.mul(b)).hrect, *(d.mul(a) + b.mul(c.conj)).hrect)
  end

  ##
  # Inverse element. Rational arithmetic is used as possible.
  #
  # @return [HyperComplex]
  # @raise [ZeroDivisionError] if all componetns are zero and not Float
  #
  # @example
  #  HyperComplex[1/4r, 1/4r, 1/4r, 1/4r] #=> HyperComplex[1, -1, -1, -1]
  #
  def inv
    t = abs2
    __new__(*conj.hrect.map { _1.to_r / t })
  end
  alias inverse inv

  ##
  # Float Inverse element.
  #
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[0.25, 0.25, 0.25, 0.25].inv #=> HyperComplex[1.0, -1.0, -1.0, -1.0]
  #  HyperComplex[0, 0, 0, 0].inv #=> HyperComplex[NaN, NaN, NaN, NaN]
  #
  def finv
    t = abs2
    __new__(*conj.hrect.map { _1.to_f / t })
  end

  ##
  # @!method quo(other)
  #
  # Performs rational as possible division.
  #
  # @param other [Numeric]
  # @return [HyperComplex]
  # @raise [ZeroDivisionError] if +other+ is zero and no conversion to float was performed.
  #
  # @example
  #  HyperComplex[12, -4, -2, 1] / HyperComplex[1, 24, -4, -8]
  #    #=> HyperComplex[-28/219r, -104/219r, 6/73r, 11/219r]
  #
  def quo(other)
    t = other.abs2
    self * __new__(*other.conj.hrect.map { _1.to_r / t })
  end
  alias / quo

  ##
  # @!method fdiv(other)
  #
  # Performs float division.
  #
  # @param other [Numeric]
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[12, -4, -2, 1].fdiv(HyperComplex[1, 24, -4, -8])
  #    #=> HyperComplex[-0.1278538812785388, -0.4748858447488584, 0.0821917808219178, 0.0502283105022831]
  #  HyperComplex[12, -4, -2, 1].fdiv(HyperComplex[0, 0, 0, 0]) #=> HyperComplex[NaN, NaN, NaN, NaN]
  def fdiv(other)
    t = other.abs2
    self * __new__(*other.conj.hrect.map { _1.to_f / t })
  end

  undef div, %, modulo, remainder, divmod

  ##
  # Performs type conversion.
  #
  # @param other [Numeric]
  # @return [[HyperComplex, self]]
  # @raise [TypeError]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4].coerce(1) #=> [HyperComplex[1, 0], HyperComplex[1, 2, 3, 4]]
  #
  def coerce(other) = [__new__(*other.hrect), self]

  ##
  # Performs conversion to Integer.
  #
  # @return [Integer]
  # @raise [RangeError]
  #
  # @example
  #  HyperComplex[3, 0, 0, 0].to_i #=> 3
  #
  def to_i
    raise RangeError, "Can not convert #{inspect} to Integer" unless @arv[1..].map(&:zero?).all?

    @arv[0].to_i
  end

  ##
  # Performs conversion to Float.
  #
  # @return [Float]
  # @raise [RangeError]
  #
  # @example
  #  HyperComplex[3, 0, 0, 0].to_f #=> 3.0
  #
  def to_f
    raise RangeError, "Can not convert #{inspect} to Float" unless @arv[1..].map(&:zero?).all?

    @arv[0].to_f
  end

  ##
  # Performs conversion to Rational.
  #
  # @return [Rational]
  # @raise [RangeError]
  #
  # @example
  #  HyperComplex[3, 0, 0, 0].to_r #=> 3/1r
  #
  def to_r
    raise RangeError, "Can not convert #{inspect} to Rational" unless @arv[1..].map(&:zero?).all?

    @arv[0].to_r
  end

  ##
  # Performs conversion to Complex.
  #
  # @return [Complex]
  # @raise [RangeError]
  #
  # @example
  #  HyperComplex[3, 1, 0, 0].to_f #=> (3+1i)
  #
  def to_c
    return Complex(@arv[0], @arv[1]) if @arv.length <= 2 || @arv[2..].map(&:zero?).all?

    raise RangeError, "Can not convert #{inspect} to Complex"
  end

  ##
  # Performs conversion to String.
  #
  # @return [String]
  #
  # @example
  #  HyperComplex[4, 3, 2, 1].to_f #=> "HyperComplex[4, 3, 2, 1]"
  #
  def to_s = "HyperComplex#{@arv}"
  alias inspect to_s

  ##
  # Returns the dimension of hypercomplex number.
  #
  # @return [Integer]
  #
  # @example
  #  HyperComplex[4, 3, 2, 1].dim #=> 4
  #
  def dim = @arv.length
  alias dimension dim

  ##
  # Returns the index's element of hypercomplex number.
  #
  # @param index [Integer]
  # @return [Integer or Rational or Float or nil]
  #
  # @example
  #  HyperComplex[4, 3, 2, 1].1 #=> 3
  #
  def [](index) = @arv[index]

  ##
  # Performs exponentiation.
  #
  # @param other [Numeric]
  # @return [HyperComplex]
  #
  # @example
  #  HyperComplex[1, 2, 3, 4]**HyperComplex[4, 3, 2, 1]
  #    #=> HyperComplex[9.648225704568818, -5.4921479890865506, -8.477947559523633, -4.2389737797618166]
  #
  def **(other) # rubocop:disable Metrics/AbcSize
    unless other.is_a?(Numeric)
      num1, num2 = other.coerce(self)
      return num1**num2
    end
    if other.zero?
      return __new__(*Array.new(dim, Float::NAN)) if zero?

      return __new__(*Array.new(dim - 1, 0).unshift(1))
    end

    unless other.real?
      begin
        other.to_f
      rescue # rubocop:disable Lint/SuppressedException,Style/RescueStandardError
      else
        other = other.real
      end
    end
    other = other.numerator if other.is_a?(Rational) && other.denominator == 1
    if other.integer?
      x = other >= 0 ? self : 1.to_r / self
      n = other.abs
      z = 1
      loop do
        z *= x if n.odd?
        n >>= 1
        return z if n.zero?

        x *= x
      end
    elsif other.real?
      r, theta, vector = polar
      HyperComplex.polar(r**other, theta * other, vector)
    elsif other.is_a?(HyperComplex)
      r, theta, vector = polar
      q = HyperComplex.hrect(Math.log(r), *(theta * vector))
      q *= other
      HyperComplex.polar(Math.exp(q.real), 1, q.imag)
    else
      num1, num2 = other.coerce(self)
      num1**num2
    end
  end

  private

  def homogenize(other)
    ar = hrect
    as = ar.length
    br = other.hrect
    bs = br.length
    if as > bs
      br.concat(Array.new(as - bs, 0))
    elsif as < bs
      ar.concat(Array.new(bs - as, 0))
    end
    [ar, br]
  end

  def __new__(*arg) = HyperComplex.send(:new, *arg)
end
