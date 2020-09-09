format:
	find src/ app/ test/ -name "*.hs" -exec ormolu -i {} +
