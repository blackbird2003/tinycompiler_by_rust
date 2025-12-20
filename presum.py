def main():
	def presum(x):
		result = None
		result = 0
		while (x) > (0):
			result = (result) + (x)
			x = (x) - (1)
		return result
	print(presum(5), end='\n')
main()
