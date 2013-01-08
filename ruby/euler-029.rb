values = {}
2.upto(100) do |a|
  puts a
  2.upto(100) do |b|
    values[a ** b] = true
  end
end

puts values.keys.length
