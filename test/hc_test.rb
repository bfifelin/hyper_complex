# frozen_string_literal: true

require 'minitest/autorun'
require 'hyper_complex'

class TestHyperComplex < Minitest::Test # rubocop:disable Metrics/ClassLength
  def test_class_extentions
    assert_equal [1, 0], 1.hrect
    assert_equal [1, 1], (1 + 1i).hrect
  end

  def test_class_rect
    assert_instance_of HyperComplex, HyperComplex.rect(0)
    assert_instance_of HyperComplex, HyperComplex.rect(1 + 1i)
    assert_instance_of HyperComplex, HyperComplex.rect(HyperComplex[1, 2], HyperComplex[3, 4, 5, 6])
    assert_equal [0, 0], HyperComplex.rect(0).hrect
    assert_equal [1, 1, 0, 0], HyperComplex.rect(1 + 1i).hrect
    assert_equal [1, 1, 2, 2], HyperComplex.rect(1 + 1i, 2 + 2i).hrect
    assert_equal [1, 2, 0, 0, 3, 4, 5, 6], HyperComplex.rect(HyperComplex[1, 2], HyperComplex[3, 4, 5, 6]).hrect
    assert_equal [1, 2, 3, 4, 0, 0, 0, 0], HyperComplex.rect(HyperComplex[1, 2, 3, 4]).hrect
    assert_raises(ArgumentError) { HyperComplex.rect('1 + 1i') }
  end

  def test_class_hrect
    assert_instance_of HyperComplex, HyperComplex[1]
    assert_equal [0, 0], HyperComplex[0, 0].hrect
    assert_equal [1, 2, 3, 0], HyperComplex[1, 2, 3].hrect
    assert_equal [1, 2, 3, 4, 5, 0, 0, 0], HyperComplex[1, 2, 3, 4, 5].hrect
    assert_raises(ArgumentError) { HyperComplex[1 + 1i] }
  end

  def test_class_polar
    assert_instance_of HyperComplex, HyperComplex.polar(1, Math::PI / 8, [1, 1, 2])
    assert_instance_of HyperComplex, HyperComplex.polar(1, Math::PI / 8, Vector[1, 1, 2])
    assert_equal 4, HyperComplex.polar(1, Math::PI / 8, [1, 1, 2]).dim
    assert_raises(TypeError) { HyperComplex.polar(1, Math::PI / 8, [1, 1, 1, 1]) }
    assert_raises(TypeError) { HyperComplex.polar(1, Math::PI / 8, [1, 1]) }
    assert_raises(ArgumentError) { HyperComplex.polar(1, Math::PI / 8, [1, 1, 's']) }
  end

  def test_class_e
    assert_equal 1, HyperComplex.e(0)
    assert_equal [0, 0, 1, 0], HyperComplex.e(2).hrect
    assert_equal [0, 0, 0, 0, 0, 1, 0, 0], HyperComplex.e(5).hrect
    assert_raises(ArgumentError) { HyperComplex.e(-1) }
    assert_raises(ArgumentError) { HyperComplex.e(1.5) }
  end

  def test_class_zero
    assert_equal 0, HyperComplex.zero(1)
    assert_equal [0, 0], HyperComplex.zero(2).hrect
    assert_equal [0, 0, 0, 0, 0, 0, 0, 0], HyperComplex.zero(5).hrect
    assert_raises(ArgumentError) { HyperComplex.zero(-1) }
    assert_raises(ArgumentError) { HyperComplex.zero(0) }
    assert_raises(ArgumentError) { HyperComplex.zero(1.5) }
  end

  def test_rect
    assert_equal [0, 0], HyperComplex[0, 0].rect
    assert_equal [HyperComplex[1, 2], HyperComplex[3, 4]], HyperComplex[1, 2, 3, 4].rect
  end

  def test_hrect
    assert_equal [0, 0], HyperComplex[0, 0].hrect
    assert_equal [1, 2, 3, 4], HyperComplex[1, 2, 3, 4].hrect
  end

  def test_real
    assert_equal 1, HyperComplex[1, 2, 3, 4].real
  end

  def test_imag
    assert_equal Vector[2, 3, 4], HyperComplex[1, 2, 3, 4].imag
  end

  def test_arg
    assert_in_epsilon(Math::PI / 4, HyperComplex[3, 2, 2, 1].arg, 0.000000001)
  end

  def test_axis
    assert_equal Vector[1, 0, 0], HyperComplex[1, 0, 0, 0].axis
    assert_equal Vector[2/3r, 2/3r, 1/3r], HyperComplex[6, 4, 4, 2].axis
  end

  def test_is_real
    refute HyperComplex[1, 1].real?
  end

  def test_is_zero
    assert HyperComplex[0, 0, 0, 0].zero?
    refute HyperComplex[1, 1, 1, 0].zero?
  end

  def test_is_nan
    assert HyperComplex[0, 0, Float::NAN, 0].nan?
    refute HyperComplex[1, 1, 1, 0].nan?
  end

  def test_unary_minus
    assert_equal HyperComplex[-1, 0, 1, 2], -HyperComplex[1, 0, -1, -2]
  end

  def test_conj
    assert_equal HyperComplex[1, 0, 1, -2], HyperComplex[1, 0, -1, 2].conj
  end

  def test_abs2
    assert_equal 16, HyperComplex[2, 2, 2, 2].abs2
  end

  def test_abs
    assert_equal 4, HyperComplex[2, 2, 2, 2].abs
  end

  def test_polar
    assert_equal [6, Math::PI / 2, Vector[2/3r, 2/3r, 1/3r]], HyperComplex[0, 4, 4, 2].polar
  end

  def test_math_eql
    assert(HyperComplex[1, 2, 0, 0] == HyperComplex[1, 2])
    assert(HyperComplex[1, 0] == 1)
    assert(HyperComplex[1, 1] == 1 + 1i)
  end

  def test_is_eql
    assert(HyperComplex[1, 0].eql?(1))
    assert(HyperComplex[1, 1].eql?(1 + 1i))
    refute(HyperComplex[1, 2, 0, 0].eql?(HyperComplex[1, 2]))
    refute(HyperComplex[1, 2].eql?(HyperComplex[1, 2, 0, 0]))
  end

  def test_is_finite
    if HyperComplex.instance_methods.include?(:finite?)
      assert HyperComplex[1, 2, 3, 4].finite?
      refute HyperComplex[1, 2, 3, Float::INFINITY].finite?
    else
      skip
    end
  end

  def test_is_infinite
    if HyperComplex.instance_methods.include?(:finite?)
      assert HyperComplex[1, 2, 3, Float::INFINITY].infinite?
      refute HyperComplex[1, 2, 3, 4].infinite?
    else
      skip
    end
  end

  def test_addition
    assert_equal HyperComplex[1, 0, 0, 0], 1 + HyperComplex[0, 0, 0, 0]
    assert_equal HyperComplex[1, 1, 0, 0], (1 + 1i) + HyperComplex[0, 0, 0, 0]
    assert_equal HyperComplex[1, 1, 0, 0, 1, 1, 0, 0], HyperComplex[1, 1] + HyperComplex[0, 0, 0, 0, 1, 1]
  end

  def test_substraction
    assert_equal HyperComplex[1, 0, 0, -1], 1 - HyperComplex[0, 0, 0, 1]
    assert_equal HyperComplex[1, 1, -1, 0], (1 + 1i) - HyperComplex[0, 0, 1, 0]
    assert_equal HyperComplex[1, 1, 0, 0, -1, -1, 0, 0], HyperComplex[1, 1] - HyperComplex[0, 0, 0, 0, 1, 1]
  end

  def test_multiplication
    assert_equal HyperComplex[0, 0, 0, 0], 0 * HyperComplex[1, 2, 3, 4]
    assert_equal HyperComplex[1, 2, 3, 4], 1 * HyperComplex[1, 2, 3, 4]
    assert_equal HyperComplex[-2, 6, -2, 14], (2 + 2i) * HyperComplex[1, 2, 3, 4]
    assert_equal HyperComplex[-12, 6, 24, 12], HyperComplex[1, 2, 3, 4] * HyperComplex[4, 3, 2, 1]
    assert_equal HyperComplex[1, 2, 3, 4].mul(HyperComplex[4, 3, 2, 1]),
                 HyperComplex[1, 2, 3, 4] * HyperComplex[4, 3, 2, 1]
    assert_equal HyperComplex[1, 2, 3, 4, 5, 6, 7, 8].mul(HyperComplex[8, 7, 6, 5, 4, 3, 2, 1]),
                 HyperComplex[1, 2, 3, 4, 5, 6, 7, 8] * HyperComplex[8, 7, 6, 5, 4, 3, 2, 1]
  end

  def test_mul
    assert_equal HyperComplex[0, 0, 0, 0], HyperComplex[1, 2, 3, 4].mul(0)
    assert_equal HyperComplex[1, 2, 3, 4], HyperComplex[1, 2, 3, 4].mul(1)
    assert_equal HyperComplex[-2, 6, 14, 2], HyperComplex[1, 2, 3, 4].mul(2 + 2i)
    assert_equal HyperComplex[-12, 6, 24, 12], HyperComplex[1, 2, 3, 4].mul(HyperComplex[4, 3, 2, 1])
  end

  def test_inv
    assert_equal HyperComplex[1, -1, -1, -1], HyperComplex[1/4r, 1/4r, 1/4r, 1/4r].inv
    assert_raises(ZeroDivisionError) { HyperComplex[0, 0, 0, 0].inv }
  end

  def test_finv
    assert_equal HyperComplex[1, -1, -1, -1], HyperComplex[1/4r, 1/4r, 1/4r, 1/4r].finv
  end

  def test_quo
    assert_equal 1, HyperComplex[2, 2, 2, 2] / HyperComplex[2, 2, 2, 2]
    assert_equal HyperComplex[1, 1, 1, 1], HyperComplex[2, 2, 2, 2] / 2
    assert_equal HyperComplex[2, 0, 0, 2], HyperComplex[2, 2, 2, 2] / (1 + 1i)
    assert_equal(HyperComplex[-28/219r, -104/219r, 6/73r, 11/219r],
                 HyperComplex[12, -4, -2, 1] / HyperComplex[1, 24, -4, -8])
    assert_raises(ZeroDivisionError) { HyperComplex[2, 2, 2, 2] / HyperComplex[0, 0, 0, 0] }
  end

  def test_fdiv
    assert_equal HyperComplex[1, 1, 1, 1], HyperComplex[2, 2, 2, 2].fdiv(2)
    assert_equal HyperComplex[2, 0, 0, 2], HyperComplex[2, 2, 2, 2].fdiv(1 + 1i)
    assert_equal 1, HyperComplex[2, 2, 2, 2].fdiv(HyperComplex[2, 2, 2, 2])
  end

  def test_to_i
    assert_instance_of Integer, HyperComplex[1, 0, 0, 0].to_i
    assert_equal 1, HyperComplex[1, 0, 0, 0].to_i
    assert_raises(RangeError) { HyperComplex[1, 1, 0, 0].to_i }
  end

  def test_to_f
    assert_instance_of Float, HyperComplex[1, 0, 0, 0].to_f
    assert_equal 1, HyperComplex[1, 0, 0, 0].to_f
    assert_raises(RangeError) { HyperComplex[1, 1, 0, 0].to_f }
  end

  def test_to_r
    assert_instance_of Rational, HyperComplex[1, 0, 0, 0].to_r
    assert_equal 1, HyperComplex[1, 0, 0, 0].to_r
    assert_raises(RangeError) { HyperComplex[1, 1, 0, 0].to_r }
  end

  def test_to_c
    assert_instance_of Complex, HyperComplex[1, 0, 0, 0].to_c
    assert_equal (1 + 0i), HyperComplex[1, 0, 0, 0].to_c
    assert_equal (1 + 1i), HyperComplex[1, 1, 0, 0].to_c
    assert_raises(RangeError) { HyperComplex[1, 1, 1, 0].to_c }
  end

  def test_to_s
    assert_equal 'HyperComplex[1, 2, 3, 4]', HyperComplex[1, 2, 3, 4].to_s
  end

  def test_inspect
    assert_equal 'HyperComplex[1, 2, 3, 4]', HyperComplex[1, 2, 3, 4].inspect
  end

  def test_dim
    assert_equal 4, HyperComplex[1, 2, 3, 4].dim
  end

  def test_index
    assert_equal 2, HyperComplex[1, 2, 3, 4][1]
    assert_nil HyperComplex[1, 2, 3, 4][5]
  end

  def test_pow
    assert (HyperComplex[0, 0, 0, 0]**0).nan?
    assert_equal HyperComplex[1, 0, 0, 0], HyperComplex[1, 2, 3, 4]**0
    assert_equal HyperComplex[1, 2, 3, 4], HyperComplex[1, 2, 3, 4]**1
    assert_equal HyperComplex[-28, 4, 6, 8], HyperComplex[1, 2, 3, 4]**2
    assert_equal HyperComplex[-28, 4, 6, 8], HyperComplex[1, 2, 3, 4]**(2/1r)
    assert_in_delta(HyperComplex[-28, 4, 6, 8], HyperComplex[1, 2, 3, 4]**2.0, 0.0000001)
    assert_equal HyperComplex[Float::NAN, Float::NAN], 0**HyperComplex[0, 0, 0, 0]
    assert_equal HyperComplex[1, 0], 1**HyperComplex[0, 0, 0, 0]
    assert_equal Complex(1, 0), Complex(1, 1)**HyperComplex[0, 0, 0, 0]
    assert_equal HyperComplex[1.0, 0.0, 0.0, 0.0], HyperComplex[1, 0, 0, 0]**Complex::I  end
end
