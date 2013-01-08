# 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
# Find the sum of all numbers which are equal to the sum of the
# factorial of their digits.

class Integer
  def factorial
    f = 1; for i in 1..self; f *= i; end; f
  end
end

@facts = []
(0..9).to_a.each do |i|
  @facts[i] = i.factorial
end

def factorial_sum(x)
  x.to_s.split('').inject(0) {|sum, char|
    @facts[char.to_i] + sum
  }
end

puts factorial_sum(145)

p @facts
nums = []
3.upto(1000000) do |i|
  nums << i if i == factorial_sum(i)
end

p nums

p nums.inject(0) { |s, i| s + i }
