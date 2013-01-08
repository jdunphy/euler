# The number, 197, is called a circular prime because all rotations of
# the digits: 197, 971, and 719, are themselves prime.

# There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17,
# 31, 37, 71, 73, 79, and 97.

# How many circular primes are there below one million?

require 'prime'

cprimes = []


class Integer

  def circular_prime?
    arr = self.to_s.split('')
    return false if arr.detect {|d| d % 2 == 0 || d % 5 == 0 }
    arr.length.times do |time|
      return false unless arr.join.to_i.prime?
      arr.unshift arr.pop
    end
  end
end

p ARGV

Prime.each(ARGV[0].to_i) do |p|
  cprimes << p if p.circular_prime?
end

p cprimes.length
