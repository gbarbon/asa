/**
 * This library of operators allow to collect the obfuscation value for the basic operators.
 * @author gbarbon
 *
 */


/**
 * Sum operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf 
@@ implq
int BOPlus(int __first, int __second) { 
	return __first + __second;
}

/**
 * Minus operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
int BOMinus(int ____first, int __second) {
	return __first - __second;
}

/**
 * Multiplier operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
int BOMul(int __first, int __second) {
	return __first * __second;
}

/**
 * Divisor operator
 * @param __first
 * @param __second
 * @return
 * @FIXME: do we need cast??
 */
@@ obf
@@ implq
int BODiv(int __first, int __second) {
	return  (__first / __second); 
}

/**
 * Boolean AND operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BOAnd(bool __first, bool __second) {
	return __first && __second;
}

/**
 * Boolean OR operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BOOr(bool __first, bool __second) {
	return __first || __second;
}

/**
 * Modulo operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
int BOMod(int __first, int __second) {
	return __first % __second;
}

/**
 * Less than operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BOLt(int __first, int __second) {
	return __first < __second;
}

/**
 * Less than or equal operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BOLeq(int __first, int __second) {
	return __first <= __second;
}

/**
 * Equality operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BOEq(int __first, int __second) {
	return __first == __second;
}

/**
 * Greather than operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BOGt(int __first, int __second) {
	return __first > __second;
}

/**
 * Greather than or equal operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BOGeq(int __first, int __second) {
	return __first >= __second;
}

/**
 * Not equal operator
 * @param __first
 * @param __second
 * @return
 */
@@ obf
@@ implq
bool BONeq(int __first, int __second) {
	return __first != __second;
}

/**
 * String concatenation operator
 * @param
 * @param
 * @return 
 */
@@ obf
@@ implq
string BOPlusPlus(string __first, string __second) {
	return __first++__second;
}

/**
 * Not operator
 * @param __arg
 * @return
 */
@@ obf
@@ implq
bool UNot(bool __arg) {
	return !__arg;
}

/**
 * Negation operator
 * @param __arg
 * @return
 */
@@ obf
@@ implq
int UNeg(int __arg) {
	return -__arg;
}
