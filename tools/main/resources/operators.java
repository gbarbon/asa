/**
 * This library of operators allow to collect the obfuscation value for the basic operators.
 * @author gbarbon
 *
 */


/**
 * Sum operator
 * @param first
 * @param second
 * @return
 */
@@ obf 
@@ implq
int BOPlus(int first, int second) { 
	return first + second;
}

/**
 * Minus operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
int BOMinus(int first, int second) {
	return first - second;
}

/**
 * Multiplier operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
int BOMul(int first, int second) {
	return first * second;
}

/**
 * Divisor operator
 * @param first
 * @param second
 * @return
 * @FIXME: do we need cast??
 */
@@ obf
@@ implq
int BODiv(int first, int second) {
	return  (first / second); 
}

/**
 * Boolean AND operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BOAnd(bool first, bool second) {
	return first && second;
}

/**
 * Boolean OR operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BOOr(bool first, bool second) {
	return first || second;
}

/**
 * Modulo operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
int BOMod(int first, int second) {
	return first % second;
}

/**
 * Less than operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BOLt(int first, int second) {
	return first < second;
}

/**
 * Less than or equal operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BOLeq(int first, int second) {
	return first <= second;
}

/**
 * Equality operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BOEq(int first, int second) {
	return first == second;
}

/**
 * Greather than operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BOGt(int first, int second) {
	return first > second;
}

/**
 * Greather than or equal operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BOGeq(int first, int second) {
	return first >= second;
}

/**
 * Not equal operator
 * @param first
 * @param second
 * @return
 */
@@ obf
@@ implq
bool BONeq(int first, int second) {
	return first != second;
}

/**
 * String concatenation operator
 * @param
 * @param
 * @return 
 */
@@ obf
@@ implq
string BOPlusPlus(string first, string second) {
	return first++second;
}

/**
 * Not operator
 * @param arg
 * @return
 */
@@ obf
@@ implq
bool UNot(bool arg) {
	return !arg;
}

/**
 * Negation operator
 * @param arg
 * @return
 */
@@ obf
@@ implq
int UNeg(int arg) {
	return -arg;
}
