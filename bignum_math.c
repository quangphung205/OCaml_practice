
/**************************
 * bignum_math.c -- an outline for CLab1
 *
 * orginially written by Andy Exley
 * modified by Emery Mizero
 **************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bignum_math.h"


/*
 * Returns true if the given char is a digit from 0 to 9
 */
bool is_digit(char c) {
	return c >= '0' && c <= '9';
}

/*
 * Returns true if lower alphabetic character
 */
bool is_lower_alphabetic(char c) {
	return c >= 'a' && c <= 'z';
}

/*
 * Returns true if upper alphabetic character
 */
bool is_upper_alphabetic(char c) {
	return c >= 'A' && c <= 'Z';
}

/*
 * Convert a string to an integer
 * returns 0 if it cannot be converted.
 */
int string_to_integer(char* input) {
	int result = 0;
	int length = strlen(input);
    int num_digits = length;
	int sign = 1;
	
	int i = 0;
    int factor = 1;

    if (input[0] == '-') {
		num_digits--;
		sign = -1;
    }

	for (i = 0; i < num_digits; i++, length--) {
		if (!is_digit(input[length-1])) {
			return 0;
		}
		if (i > 0) factor*=10;
		result += (input[length-1] - '0') * factor;
	}

    return sign * result;
}

/*
 * Returns true if the given base is valid.
 * that is: integers between 2 and 36
 */
bool valid_base(int base) {
	if(!(base >= 2 && base <= 36)) { 
		return false; 
	}
	return true;
}

/*
 * TODO - Completed
 * Returns true if the given string (char array) is a valid input,
 * that is: digits 0-9, letters A-Z, a-z
 * and it should not violate the given base and should not handle negative numbers
 */
bool valid_input(char* input, int base) {	
	for (int i = 0; i < strlen(input); i++)
	{
		char c = input[i];
		// Check if each character of the input is valid (numbers and characters only)
		if (!is_digit(c) && !is_lower_alphabetic(c) && !is_upper_alphabetic(c)
			|| !(2 <= base && base <= 36))
		{		
			return false;
		}
		
		/* 
		 * Each base has an upper bound (UB) digit/letter
		 * Ex: UB of base 2 is '1' (ASCII = 49) -> 49 - 2 = 47
		 *     UB of base 16 is 'f' (ASCII = 102) -> 102 - 16 = 86
		 *     UB of base 36 is 'Z' (ASCII = 90) -> 90 - 36 = 54
		 * Return false if any digit/letter exceeds this limit
		 */
		if ((is_digit(c) && (c > (base + 47)))
			|| (is_lower_alphabetic(c) && (c > (base + 86)))
			|| (is_upper_alphabetic(c) && (c > (base + 54))))
		{			
			return false;
		}						
	}

	return true;
}

/*
 * converts from an array of characters (string) to an array of integers
 */
int* string_to_integer_array(char* str) {
	int* result;
	int i, str_offset = 0;
		result = malloc((strlen(str) + 1) * sizeof(int));
		result[strlen(str)] = -1;
	for(i = str_offset; str[i] != '\0'; i++) {
		if(is_digit(str[i])) { 
			result[i - str_offset] = str[i] - '0';
		} else if (is_lower_alphabetic(str[i])) {
			result[i - str_offset] = str[i] - 'a' + 10;
		} else if (is_upper_alphabetic(str[i])) {
			result[i - str_offset] = str[i] - 'A' + 10;
		} else {
			printf("I don't know how got to this point!\n");
		}
	}
	return result;
}

/*
 * finds the length of a bignum... 
 * simply traverses the bignum until a negative number is found.
 */
int bignum_length(int* num) {
	int len = 0;
	while(num[len] >= 0) { len++; }
	return len;
}

/*
 * TODO - Completed
 * Prints out a bignum using digits and lower-case characters
 * Current behavior: prints integers
 * Expected behavior: prints characters
 */
void bignum_print(int* num) {
	int i;
	if(num == NULL) { return; }

	/* Handle negative numbers as you want */
	i = bignum_length(num);

	/* Then, print each digit */
	for(i = 0; num[i] >= 0; i++) {
		if (num[i] < 10)		
			printf("%d", num[i]);
		else
			printf("%c", num[i] + 87);
	}
	printf("\n");
}

/*
 *	Helper for reversing the result that we built backward.
 *  see add(...) below
 */
void reverse(int* num) {
	int i, len = bignum_length(num);
	for(i = 0; i < len/2; i++) {
		int temp = num[i];
		num[i] = num[len-i-1];
		num[len-i-1] = temp;
	}
}


/*
 * used to add two numbers with the same sign
 * GIVEN FOR GUIDANCE
 */
int* add(int* input1, int* input2, int base) {
	int len1 = bignum_length(input1);
	int len2 = bignum_length(input2);
	int resultlength = ((len1 > len2)? len1 : len2) + 2;
	int* result = (int*) malloc (sizeof(int) * resultlength);
	int r = 0;
	int carry = 0;
	int sign = input1[len1];
   	int num1, num2;

	len1--;
	len2--;

	while (len1 >= 0 || len2 >= 0) {
		if (len1 >= 0) {
		    num1 = input1[len1];
		} else {
		    num1 = 0;
		}

		if (len2 >= 0) {
		    num2 = input2[len2];
		} else {
		    num2 = 0;
		}

		result[r] = (num1 + num2 + carry) % base;
		carry = (num1 + num2 + carry) / base;
		len1--;
		len2--;
		r++;
	}

	if (carry > 0) {
		result[r] = carry; 
		r++; 
    	}
	result[r] = sign;
	reverse(result);
	return result;
}

/*
 * This function returns true if the first number is less than the second number, false otherwise
 */
bool isLessThan(int* input1, int* input2)
{
  int len1  = bignum_length(input1);
  int len2  = bignum_length(input2);

  // The number which has less length is the smaller
  if (len1 < len2)
    return true;
  else if (len1 > len2)
    {
      printf("len1 %d > len2 %d\n", len1, len2);
      return false;          
    }

  else
    {
      // Both have the same length, need to check each digit/letter manually,
      // starting from the most significant one.
      for (int i = 0; i < len1; i++)
	{	  
	  if (input1[i] > input2[i])
	    {
	      printf("input1 > input2\n");
	      return false;
	    }


	  // return false if they're equal to each other
	  if (i == (len1 - 1) && (input1[i] == input2[i]))
	    {
	      printf("they are equal\n");
	      return false;
	    }
	   

	}
    }

  return true;
}

/*
 * This function returns true if the first number is greater than the second number, false otherwise
 */
bool isGreaterThan(int* input1, int* input2)
{
  int len1  = bignum_length(input1);
  int len2  = bignum_length(input2);

  // The number which has more length is the bigger
  if (len1 > len2)
    return true;
  else if (len1 < len2)
    return false;
  else
    {
      // Both have the same length, need to check each digit/letter manually,
      // starting from the most significant one.
      for (int i = 0; i < len1; i++)
	{	  
	  if (input1[i] < input2[i])
	    return false;

	  // return false if they're equal to each other
	  if (i == (len1 - 1) && (input1[i] == input2[i]))
	    return false;
	}
    }

  return true; 
}

/*
 * This function returns true if both numbers are equal, false otherwise
 */
bool isEqualTo(int* input1, int* input2)
{
  int len1  = bignum_length(input1);
  int len2  = bignum_length(input2);

  // The number which has less length is the smaller
  if (len1 != len2)
    return false;
  else
    {
      // Both have the same length, need to check each digit/letter manually,
      // starting from the most significant one.
      for (int i = 0; i < len1; i++)
	{	  
	  if (input1[i] != input2[i])
	    return false;
	}
    }

  return true;
}


/*
 * TODO
 * This function is where you will write the code that performs the heavy lifting, 
 * actually performing the calculations on input1 and input2.
 * Return your result as an array of integers.
 * HINT: For better code structure, use helper functions.
 */
int* perform_math(int* input1, int* input2, char op, int base) {

	/* 
	 * this code initializes result to be large enough to hold all necessary digits.
	 * if you don't use all of its digits, just put a -1 at the end of the number.
	 * you may omit this result array and create your own.
     */

    	int len1 = bignum_length(input1);
	int len2 = bignum_length(input2);
	int resultlength = ((len1 > len2)? len1 : len2) + 1;
	int* result = (int*) malloc (sizeof(int) * resultlength);
 
	if(op == '+') {
		return add(input1, input2, base);
	}
/* Write your logic for subtraction and comparison here*/
	return result;
}

/*
 * Print to "stderr" and exit program
 */
void print_usage(char* name) {
	fprintf(stderr, "----------------------------------------------------\n");
	fprintf(stderr, "Usage: %s base input1 operation input2\n", name);
	fprintf(stderr, "base must be number between 2 and 36, inclusive\n");
	fprintf(stderr, "input1 and input2 are arbitrary-length integers\n");
	fprintf(stderr, "Two operations are allowed '+' and '-'\n");
	fprintf(stderr, "----------------------------------------------------\n");
	exit(1);
}


/*
 * MAIN: Run the program and tests your functions.
 * sample command: ./bignum 4 12 + 13
 * Result: 31
 */
int main(int argc, char** argv) {

	int input_base;

	int* input1;
    int* input2;
    int* result;
    /*
	if(argc != 5) { 
		print_usage(argv[0]); 
	}

	input_base = string_to_integer(argv[1]);

	if(!valid_base(input_base)) { 
		fprintf(stderr, "Invalid base: %s\n", argv[1]);
		print_usage(argv[0]);
	}
	

	if(!valid_input(argv[2], input_base)) { 
		fprintf(stderr, "Invalid input1: %s\n", argv[2]);
		print_usage(argv[0]);
	}

	if(!valid_input(argv[4], input_base)) { 
		fprintf(stderr, "Invalid input2: %s\n", argv[4]);
		print_usage(argv[0]);
	}

        char op = argv[3][0];
	if(op != '-' && op != '+' && op != '<' && op != '>' && op != '=') {
		fprintf(stderr, "Invalid operation: %s\n", argv[3]);
		print_usage(argv[0]);
	}

	input1 = string_to_integer_array(argv[2]);
    input2 = string_to_integer_array(argv[4]);

    result = perform_math(input1, input2, argv[3][0], input_base);

    printf("Result: ");
    bignum_print(result);
	printf("\n");
    */

    int a[6] = {1,2,3,4,5,-1};
    int b[6] = {1,2,3,4,6,-1};
    printf ("%s\n", isLessThan(a, b) ? "true" : "false");
	exit(0);
}
