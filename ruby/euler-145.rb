
def rev n
  n.to_s.reverse.to_i
end

def odd_digits? n
  n.to_s =~ /^[13579]+$/
end

def fail_fast n
  (n[0].to_i + n[n.length - 1].to_i).even?
end

def reversible? n
  return false if n % 10 == 0
  n = n.to_s.split("")
  puts "STARRRT!"
  while !n.empty?
    p n
    puts n[0]
    puts n[n.size - 1]
    puts (n[0].to_i + n[n.size - 1].to_i).even?

    return false if (n.delete(0).to_i + n.delete(n.size - 1).to_i).even?
  end
  true
end

puts "for #{ARGV[0]}"
puts (1..ARGV[0].to_i).inject(0) {|s, i| reversible?(i) ? s.next : s}
