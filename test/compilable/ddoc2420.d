// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 2420

string genString() {
	return "/// some comment\nvoid foo() {}";
}

mixin(genString());
