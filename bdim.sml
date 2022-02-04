use "smlio.sml";

(*Initializing the mem array with size 20, with all values 0*)
val mem = Array.array(20,0);

(*knot(a) gives binary NOT a *)
fun knot(a) = if(a = 1) then 0 else if(a = 0) then 1 else raise raise Fail("Binary Values Only")

(*oor(a,b) is our function which returns a OR b *)
fun oor(a,b) = if(a = 0 andalso b = 0) then 0 else if(a = 0 andalso b = 1) then 1 else if(a = 1 andalso b = 0) then 1 else if(a = 1 andalso b = 1) then 1 else raise Fail("a & b must be binary");

(*aand(a,b) returns a AND b*)
fun aand(a,b) = if(a = 1 andalso b = 1) then 1 else if(a = 0 andalso b = 0) then 0 else if (a = 0 andalso b = 1) then 0 else if (a = 1 andalso b = 0) then 0 else raise Fail("a & b must be binary");

(*isequal returns 1 if a = b*)
fun isequal(a,b) = if(a = b) then 1 else 0;

(*isgreaterThan returns 1 if a>b*)
fun isgreaterThan(a,b) = if(a>b) then 1 else 0;

fun helper(x,i,j,k) = if (x = 0) then OS.Process.exit(OS.Process.success)
                               else if (k<0) then raise Fail("Index cannot be negative")
                               else if (x = 1) then (let val a = print("Input:");
                                                          val b = readln();
                                                          val c = valOf(Int.fromString(b))
                                                          in Array.update(mem,k,c)
                                                          end)
                                else if (x = 2) then if(i<0) then raise Fail("Index cannot be negative") 
                                                     else(let val a = Array.sub(mem,i)
                                                           in Array.update(mem,k,a)
                                                           end)
                                else if (x = 3) then if(i<0) then raise Fail("Index cannot be negative")
                                                     else (let val a = Array.sub(mem,i);
                                                           val b = knot(a);
                                                           in Array.update(mem,k,b)
                                                           end)
                                else if (x = 4) then if(i<0 orelse j<0) then raise Fail("Index Cannot be Negative")
                                                     else (let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = oor(a,b);
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 5) then if(i<0 orelse j<0) then raise Fail("Index Cannot be negative")
                                                     else (let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = aand(a,b);
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 6) then if(i<0 orelse j<0) then raise Fail("Index Cannot be negative") 
                                                     else (let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = a + b;
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 7) then if(i<0 orelse j<0) then raise Fail("Index Cannot be negative")
                                                     else(let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = a - b; 
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 8) then if(i<0 orelse j<0) then raise Fail("Index Cannot be Negative")
                                                     else(let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = a * b;
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 9) then if(i<0 orelse j<0) then raise Fail("Index Cannot be Negative") 
                                                     else if(Array.sub(mem,j) = 0) then raise Fail("Cannot Divide by Zero")
                                                     else (let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = a div b;
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 10) then if(i<0 orelse j<0) then raise Fail("Index Cannot be Negative")
                                                      else if(Array.sub(mem,j) = 0) then raise Fail("Cannot Divide by Zero")
                                                      else (let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = a mod b;
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 11) then if(i<0 orelse j<0) then raise Fail("Index Cannot be Negative")
                                                      else(let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = isequal(a,b);
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 12) then if(i<0 orelse j<0) then raise Fail("Index Cannot be Negative")
                                                      else(let val a = Array.sub(mem,i);
                                                           val b = Array.sub(mem,j);
                                                           val c = isgreaterThan(a,b);
                                                           in Array.update(mem,k,c)
                                                           end)
                                else if (x = 15) then if(i<0) then raise Fail("Index Cannot be Negative")
                                                      else(let val a = Array.sub(mem,i);
                                                            val b = print(Int.toString(a));
                                                            in helper(0,0,0,0)
                                                            end)
                                else if (x = 16) then Array.update(mem,k,i)
                                else raise Fail("op must be between 0 and 16 inclusive");






(*Reading the .bdim file returns a List of strings stored in s*)





fun firstIndexOfComa (s,i) = if(String.sub(s,i) = #",") then i else firstIndexOfComa(s,i+1);
(*firstIndexOfComa returns the first index after or equal to i in s for which s[index] is equal to a comma (',')*)

fun firstComa (s) = firstIndexOfComa(s,0);
(*firstComa returns the index where first time any character becomes equal to comma in s*)

fun secondComa (s) = firstIndexOfComa(s,firstComa(s)+1);
(*secondComa returns the index where second time any character becomes equal to comma in s*)

fun thirdComa (s) = firstIndexOfComa(s,secondComa(s)+1);
(*thirdComa returns the index where third time any character becomes equal to comma in s*)


fun firstquater(s) = String.substring(s,1,firstComa(s)-1);
fun secondquater(s) = String.substring(s,firstComa(s)+1,secondComa(s)-firstComa(s)-1);
fun thirdquater(s) = String.substring(s,secondComa(s)+1,thirdComa(s)-secondComa(s)-1);
fun fourthquater(s) = String.substring(s,thirdComa(s)+1,String.size(s)-thirdComa(s)-2);
fun toQuadrouple s = let val a = valOf(Int.fromString(firstquater(s))); val b = valOf(Int.fromString(secondquater(s))); val c = valOf(Int.fromString(thirdquater(s))); val d = valOf(Int.fromString(fourthquater(s))); in (a,b,c,d) end;
(*The above 5 functions together convert a given string in the format "(a,b,c,d)" to a quadrouple (a,b,c,d)*)

fun sToReversedTupleList(L,L') = if(null(L)) then L' else sToReversedTupleList(tl(L),toQuadrouple(hd(L))::L');
(*This function takes in a List of Strings and converts them into a list of Quadrouples in reversed order*)

fun sToTupleList(L) = let val x = sToReversedTupleList(L,[]) in rev(x) end;
(*This function takes in a List of Strings and outputs the list of Integer Quadrouples*)



(*interpreter(i) takes in as input an integer which is the index of the code array to be evaluated*)
fun interpreter(i,code) = let val (p,q,r,w) = Array.sub(code,i) in if(p = 13 andalso Array.sub(mem,q) = 1) then interpreter(w,code)
                                                            else if(p = 13 andalso Array.sub(mem,q) = 0) then interpreter(i+1,code)
                                                            else if(p = 14) then interpreter(w,code)
                                                            else (let val temp = helper(p,q,r,w) in interpreter(i+1,code) end) end;
            


fun interpret(filename) =let val s =readLines(filename);val c = sToTupleList(s);val code = Array.fromList(c); in interpreter(0,code) end;
(*We finally call interpreter(0) to call the function interpret from the 0th index*)