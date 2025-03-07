cc_binary(
	name = "owl",
	srcs = glob(["*.cpp", "*.hpp"]),
	# i addeed argparse, llvm-project, fmt, and 
	deps = [
		"@@fmt//:fmt",
		"@@argparse//:argparse",
		"@@llvm-project//:llvm-project",
	]
)
