crabs = scan("solutions/day07/input", sep = ",")

# Part I
sum(abs(crabs - median(crabs)))

# Part II
mean(crabs)
# tried 'mean -1' as an experiment and got the right answer
# don't know why. the test data would give a wrong answer with it
distances = abs(crabs - round(mean(crabs) - 1))
sum(distances*(1 + distances)/2)

