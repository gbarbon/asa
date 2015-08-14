package it.unive.dais.yaasa.utils;

/**
 * The class operators allow to collect the obfuscation value for the basic operators.
 * @author gbarbon
 *
 */

class operators {
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	int BOPlus(int first, int second) { 
		return first + second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	int BOMinus(int first, int second) {
		return first - second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	int BOMul(int first, int second) {
		return first * second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 * @FIXME: do we need cast??
	 */
	int BODiv(int first, int second) {
		return  (first / second); 
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BOAnd(bool first, bool second) {
		return first && second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BOOr(bool first, bool second) {
		return first || second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	int BOMod(int first, int second) {
		return first % second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BOLt(int first, int second) {
		return first < second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BOLeq(int first, int second) {
		return first <= second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BOEq(int first, int second) {
		return first == second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BOGt(int first, int second) {
		return first > second;
	}
	
	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BOGeq(int first, int second) {
		return first >= second;
	}

	/**
	 * 
	 * @param first
	 * @param second
	 * @return
	 */
	bool BONeq(int first, int second) {
		return first != second;
	}
	
	/**
	 * 
	 * @param arg
	 * @return
	 */
	bool UNot(bool arg) {
		return !arg;
	}
	
	/**
	 * 
	 * @param arg
	 * @return
	 */
	int UNeg(int arg) {
		return -arg;
	}
}