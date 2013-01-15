require 'date'

date = Date.new 1901, 1, 1
end_date = Date.new 2001, 1, 1
sundays = 0

while (date < end_date) do
  sundays += 1 if date.sunday? && date.mday == 1
  date = date.next_month
end

puts sundays
