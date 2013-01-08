class Integer
  def factorial
    self <= 1 ? 1 : self * (self - 1).factorial
  end
end

chars = (1..9).to_a.map(&:to_s)
used_chars = []
p chars


current = 1_000_000
(1..9).to_a.reverse.each do |i|
  used_chars << chars.delete_at(current / i.factorial)
  current = current % i.factorial
  p current
end

puts used_chars
puts used_chars.join



lex_perms = [0,1,2,3,4,5,6,7,8,9].permutation(10).to_a

puts lex_perms[1_000_000 - 1].join()
