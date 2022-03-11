/* Fill in your Name and GNumber in the following two comment fields
 * Name: Kelvin Lu
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fp.h"

#define BIAS 15 //Math.pow(2, exp-1) - 1
#define SIGNVALUE 0x800 //Sign Positioning (12th LSB), Negative Number
#define EXPVALUE 0x7C0 //Exp place all 1s, NAN/Infinity
#define FRACVALUE 0x3F //Frac Value all 1s

/* input: float value to be represented
 * output: fp_gmu representation of the input float
 *
 * Follow the Project Documentation for Instructions
 */

/*
 *Return absolute val of a float |val|
 */
float absVal(float val){
  float output = val; 
  if(val<0){
    output *= -1; 
  }
  return output;
} 
/**
 *Recursive method to take the power of a function. 
 */
float power(float val, int pow){
  if(pow == 0){
     return 1.0; 
  }
  //Inverse the val if negative.
  if(pow<0){
       return (1.0/val) * power(val, pow+1); 
  }
  if(pow>0){ //Normal Power
     return val* power(val, pow-1); 
  }
}

/**
 * Checks if a float val is an overflow value
 */
int checkOverflow(float val){
  float aVal = absVal(val); 

  fp_gmu largestNormalized = 0b1111111<<(BIAS-FRACTION_BITS); //1.frac >> 6
  //float largestNormalized = 1.111111;
  //largestNormalized *= power(2, BIAS); 
  
  //printf("Largest normalized: %d", largestNormalized); 
  if(aVal > largestNormalized){ //Greater than largest positive normalized 
    return 1; 
  } 
  return 0;
}
/*
 *Checks if a float val is an undeflow for our representation. 
 */
int checkUnderflow(fp_gmu val, int exp){
  float M = 1.0; 
  fp_gmu frac = val&FRACVALUE; 

  M+= frac/(power(2,FRACTION_BITS)); 

  float computedValue = M * power(2,exp); 

  float smallestNormalizedValue = 1.0;
  smallestNormalizedValue *= power(2,(1-BIAS)); //1.0 >> 2^(1-Bias) 
 
 // printf("Smallest normalized: %f", smallestNormalizedValue); 

  if(computedValue < smallestNormalizedValue){
    return 1;
  }
  return 0; 
}
/*
 *Adjusts a given M into range [1.0,2.0) 
 */
void adjustM(float *val, fp_gmu *E){

  //Adjust M to range [1.0,2.0)
  while(*val >=2){
     //printf("adjustM f *val: %f\n", *val); 
     *val /= 2; 
     (*E) ++; 
  }
  while(*val<1){ 
     //printf("adjustM f *val: %f\n", *val); 
     *val *= 2; 
     (*E) --; 
  }
  
  //printf("adjustM f *val: %f\n", *val); 
  //printf("adjustM E *E: %d\n", *E); 
}
/**
 * Return 1 if val is NAN
 * 0 otherwise.
 */
int isNAN(fp_gmu val){
 // printf("val %x\n", val); 
  fp_gmu frac = val&FRACVALUE;
  fp_gmu exp = val&EXPVALUE; 
  
//  printf("frac %x\n", frac); 
  if(exp == EXPVALUE && frac != 0){
  //  printf("isNAN activated\n"); 
    return 1; 
  }
  return 0; 
}
/**
 * Return 1 if val is infinity
 * 0 otherwise. 
 */
int isInfinity(fp_gmu val){
  fp_gmu frac = val&FRACVALUE;
  fp_gmu exp = val&EXPVALUE; 

  if(exp == EXPVALUE && frac == 0){
    return 1; 
  }
 return 0;
}
/**
 * Return 1 if val is negative infinity
 * 0 otherwise. 
 */

int isNegativeInfinity(fp_gmu val){
   fp_gmu sign = val&SIGNVALUE; 

   if(sign>0 && isInfinity(val)==1){
      return 1; 
   }
   return 0; 
}

/**
 * Return 1 if val is 0
 * 0 otherwise. 
 */
int isZero(fp_gmu val){
  fp_gmu frac = val&FRACVALUE;
  fp_gmu exp = val&EXPVALUE; 

  if(exp == 0 && frac == 0){
    return 1; 
  }
  return 0;
}

/**
 * Return 1 if val is -0
 * 0 otherwise. 
 */
int isNegativeZero(fp_gmu val){
  fp_gmu sign = val & SIGNVALUE; 
  if(sign && isZero(val)){
    return 1;  
  }
  return 0;
}
/**
 * Rounds Binary to nearest even. 
 */
fp_gmu roundEvenBinary(fp_gmu val, float frac){
  //Frac already represented. 
  if(frac == 1.0){
     return val; 
  }
  //Subtract 1 from frac if applicable 
  if(frac > 1){
     frac -=1; 
  }

  fp_gmu halfwayValue = 0; 

  //Rounding
  if((frac*2)<1){ //Round Down //Lower than halfway point
    //printf("Round Down Binary \n"); 
    return val; 
  }else{
    frac*=2; 
    frac-=1; 
    halfwayValue |= 1; 
  }

  int counter = 0; 

  while((counter < 5)){
    frac*=2; 
    if(frac>=1){//Round up
        val+=1; 
	return val; 
    }
    counter++;
  }
   //Halfway point only [100000]
  //Deciding to round up or down
  if((val&1) == 1){
     val+= 1;
     return val;
  }else{
     return val;
  }
}

fp_gmu compute_fp(float val) {
  //Take Absolute Val of val |val|
  //Convert val into Mantissa Format 
  //Convert frac into binary. 
  //Check overflows and underflows
  //Convert into fp_GMU

  //Return Value
  fp_gmu output = 0; 
 
  if(val ==0){
    return output;
  } 
  //Absolute Value of Val. 
  float aVal = absVal(val);
  //If negative, place a sign bit.
  if(val <0){
	output |= SIGNVALUE; 
  }

  //Binary value of the frac
  fp_gmu decimal = 0; 
  fp_gmu exp = 0; 
  
  adjustM(&aVal, &exp); 
  
  float frac = aVal - 1; //M = 1.x - 1
  //printf("frac: %f\n", frac); 
  int counter = 0; 
 
  //Conversion to Decimal Frac to Binary Frac
  while(counter <6){
    if(frac == 0){ //No Frac
      break; 
    }
    frac*=2; 
    counter++; 
    decimal<<=1; 

    if(frac == 1){ //Last digit of Frac
      decimal|=1; 
      break; 
    }else if(frac>1){ 
       frac-=1; 
       decimal|=1; 
    }else{
      decimal|= 0; 
    }
  }
  //Normalizes so Frac is 6 digits 
  if(counter<FRACTION_BITS){
     decimal<<=(FRACTION_BITS-counter); 
  }

  //printf("Binary Value %x\n", decimal);
  //printf("exp: %d\n", exp);
  decimal = roundEvenBinary(decimal, frac); //Round to Nearest Even
  
  if((decimal&(1<<6))>0){ //Carry overflow from rounding. 
    exp++;
    decimal = 0;
  }
  
  exp += BIAS;
  //printf("exp: %d\n", exp); 
  if(exp > 0b11110){ //Check Overflow
    output |= EXPVALUE; 
    return output; //INFINITY    
  }

  if(exp<= 0){ //Check Undeflow
    if(checkUnderflow(decimal, (exp-BIAS))==1){ 
     // printf("checker activated\n"); 
      return output; //Signed 0 
    }
  }

  //Convert to Normalized value. 
  output |= decimal; //Insert Frac value.
  output |= (exp<<FRACTION_BITS); //Insert Decimal Value
  //Sign already inserted at beginning. 
    
  return output;
}

/* input: fp_gmu representation of our floats
 * output: float value represented by our fp_gmu encoding
 *
 * If your input fp_gmu is Infinity, return the constant INFINITY
 * If your input fp_gmu is -Infinity, return the constant -INFINITY
 * If your input fp_gmu is NaN, return the constant NAN
 *
 * Follow the Project Documentation for Instructions
 */
float get_fp(fp_gmu val) {
  //Check for INFINITY VALUES
  //Extract value from val
  //Check if val is 0
  //Do conversion from fp_gmu to float

  //Checks for Infinity
  if(val == EXPVALUE){
    return INFINITY; 
  } 
  //Checks for Negative Infinity
  if(val == (EXPVALUE | SIGNVALUE)){
    return -INFINITY; 
  } 
  //Checks for NAN
  if(isNAN(val)==1){
    return NAN; 
  }

  //Extract value
  fp_gmu signbit = val&SIGNVALUE; 
  fp_gmu exp = (val&EXPVALUE)>>FRACTION_BITS; 
  fp_gmu frac = (val & FRACVALUE); 

  //Underflow value
  if(frac ==0 && exp ==0){
     if(signbit > 0){
       return -0.0; 
     } 
     return 0;
  }
  float mantissa = 1.0;   
  mantissa += frac/(power(2, (FRACTION_BITS))); //1.0 + frac/2^(6) = 1.0 +frac/64
  mantissa *= power(2, (exp-BIAS)); //M * 2^(E) 

  //M * -1 if negative
  if(signbit > 0){
    mantissa *= -1.0; 
  }

 
  return mantissa;
}

/* input: Two fp_gmu representations of our floats
 * output: The multiplication result in our fp_gmu representation
 *
 * You must implement this using the algorithm described in class:
 *   Xor the signs: S = S1 ^ S2
 *   Add the exponents: E = E1 + E2
 *   Multiply the Frac Values: M = M1 * M2
 *   If M is not in a valid range for normalized, adjust M and E.
 *
 * Follow the Project Documentation for Instructions
 */
fp_gmu mult_vals(fp_gmu source1, fp_gmu source2) { 
  //Extract bit information from source 1 and source 2
  //Check for SPECIAL CASES
  //Do multiplication calculation with floats
  //Convert float back into fp_gmu
  fp_gmu output = 0; 

  //Declare Exp
  fp_gmu S1= source1&SIGNVALUE;
  fp_gmu exp1 = ((source1&EXPVALUE) >> FRACTION_BITS)-BIAS; 
  fp_gmu frac1 = (source1 & FRACVALUE); 
  float M1 = 1 + (frac1/(power(2,FRACTION_BITS))); 
  
  fp_gmu S2= source2&SIGNVALUE;
  fp_gmu exp2 = ((source2&EXPVALUE) >> FRACTION_BITS)-BIAS; 
  fp_gmu frac2 = (source2 & FRACVALUE); 
  float M2 = 1 + (frac2/(power(2,FRACTION_BITS)));

  //CASES: NAN 
  //Check for NAN source
  if(isNAN(source1)==1 || isNAN(source2)==1){
    output |= (EXPVALUE); 
    output |= 1; 
    return output;//NAN 
  }      
  //Infinity * 0, return NAN 
  if(isZero(source1)==1 && isInfinity(source2)){
    output |= EXPVALUE;
    output |= 1; 
    return output; //NAN
  }else if(isInfinity(source1)&& isZero(source2)==1){
    output |= EXPVALUE;
    output |= 1; 
    return output; //NAN
  }

  //CASE: INFINITY
  //One of the sources are infinity.
  if(isInfinity(source1)){
    return source1;
  }else if(isInfinity(source2)){
    return source2;
  }

  //CASE: 0 
  //Sources are 0
  if(isZero(source1)==1){
    return source1; 
  }else if(isZero(source2)==1){
    return source2;
  }
  //CASE: Normalized
  fp_gmu sign = S1 ^ S2;
  fp_gmu exp = exp1 + exp2; 
  float M = M1 * M2; 
  
  if(sign){//If Negative, apply sign.
    output |= SIGNVALUE; 
  }

   adjustM(&M, &exp);
  
  //Overflow check
  if(exp+BIAS > 0b11110){ //Compares E +BIAS to EXP VALUE
     output |= EXPVALUE; 
     return output; 
  }

  if(sign){//If float is negative, *-1; 
    M*= -1; 
  }
  //Calculate final Float value of number 
  M *= power(2,exp); //M * 2^E

  //Reconverts fp_gmu types
  return compute_fp(M);
}

/* input: Two fp_gmu representations of our floats
 * output: The addition result in our fp_gmu representation
 *
 * You must implement this using the algorithm described in class:
 *   If needed, adjust the numbers to get the same exponent E
 *   Add the two adjusted Mantissas: M = M1 + M2
 *   Adjust M and E so that it is in the correct range for Normalized
 *
 * Follow the Project Documentation for Instructions
 */
fp_gmu add_vals(fp_gmu source1, fp_gmu source2) {
  //Check for spcecial CASES
  //Extract values from source1 & source 2
  //	Convert to Float
  //Determine which source needs to be shifted
  //Shift the smaller of the two source
  //M+M
  //Convert outputM to range
  //Use compute_fp(output) to get new fp_gmu value.
  fp_gmu output = 0; 

  //CASES: NAN
  //One of sources is NAN
  if(isNAN(source1)==1 || isNAN(source2)==1){
     output |= EXPVALUE;
     output |=1; 
     return output; //NAN   
  }
  //Infinity + -Infinity
  if(isNegativeInfinity(source1)==1 && isInfinity(source2)==1){
    output|= EXPVALUE; 
    output |=1;
    return output; //NAN
  }else if(isInfinity(source1)==1 && isNegativeInfinity(source2)==1){ 
    output|= EXPVALUE; 
    output |=1; 
    return output; //NAN
  }
 
  //CASES: Infinity
  //One value is negative infinity
  if(isNegativeInfinity(source1)==1 || isNegativeInfinity(source2)==1){
    output |= EXPVALUE | SIGNVALUE; 
    return output; //Negative infinity
  }else if(isInfinity(source1)==1 || isInfinity(source2)==1){ //One source is infinity
    output |= EXPVALUE; 
    return output; //Infinity
  }

  //CASES: ZERO
  if(isNegativeZero(source1) == 1 && isNegativeZero(source2)==1){ //-0 + -0
    output |= SIGNVALUE; 
    return output; //Negative Zero
  }
  if(isZero(source1) == 1 && isNegativeZero(source2)==1){ //0 + -0
    return output; //0 
  }
  if(isNegativeZero(source1) == 1 && isZero(source2)==1){ //-0 + 0
    return output; //0 
  }
  if(isZero(source1)==1 && isZero(source2)==1){ //0 + 0
    return output; //0 
  }

  //Case: Identity Property x+0 = x
  if(isZero(source1)==1){
    return source2; 
  }else if(isZero(source2)==1){
    return source1; 
  }

  //Declare Exp
  fp_gmu S1= source1&SIGNVALUE;
  fp_gmu exp1 = ((source1&EXPVALUE) >> FRACTION_BITS)-BIAS; 
  fp_gmu frac1 = (source1 & FRACVALUE); 
  float M1 = 1 + (frac1/(power(2,FRACTION_BITS))); 
  
  fp_gmu S2= source2&SIGNVALUE;
  fp_gmu exp2 = ((source2&EXPVALUE) >> FRACTION_BITS)-BIAS; 
  fp_gmu frac2 = (source2 & FRACVALUE);
  float M2 = 1 + (frac2/(power(2,FRACTION_BITS))); 

  //Value that'll be shifted 
  float shiftM; 
  fp_gmu shiftE; 
  fp_gmu shiftS; 
  //The larger of the two source
  float largerM; 
  fp_gmu largerE;   
  fp_gmu largerS; 
  if(exp1>exp2){ //Source 1 is larger
    shiftM = M2;
    shiftE = exp2;
    shiftS = S2;  

    largerM = M1; 
    largerE = exp1; 
    largerS = S2; 
  }else{ //Source2 is larger. 
    shiftM = M1;
    shiftE = exp1; 
    shiftS = S1;

    largerM = M2; 
    largerE = exp2;
    largerS = S2;  
  } 

  //Shifts the smaller value
  while(largerE != shiftE){
     shiftM /= 2; 
     shiftE++; 
  }

  //Assign Negative Values for M+M addition
  if(shiftS){
    shiftM*=-1; 
  }  
  if(largerS){
    largerM*=-1; 
  } 

  float outputM = shiftM + largerM;
  fp_gmu outputE = largerE;
  fp_gmu outputS = 0; 
 
  //Check if 0
  if(outputM == 0){
     return output; //0
  }
  
  //If addition negative, take |a+b|
  if(outputM < 0){
     outputS = SIGNVALUE; 
     outputM *=-1; 
  }
  adjustM(&outputM, &outputE); 
  //Computes the Float value for conversion
  float outputFloat = outputM * power(2, outputE); 
  if(outputS){
     outputFloat *= -1; 
  }
  return compute_fp(outputFloat);
}
