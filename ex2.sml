(*Question 1*)
fun valAt l n = if n=0 then hd(l)
else (valAt (tl l) (n-1));
(valAt [13,2,3,4] 0);
fun SumIndices l il = if tl(il)=nil then (valAt l (hd il))
else (valAt l (hd il))+(SumIndices l (tl il));

(SumIndices [13,2,3,4] [0,1]);

(*Question 3*)
(*a*)
exception Undefined;
exception Empty;
fun initEnv ()= fn f:string=>(raise Undefined);


(*c*)
fun emptyNestedEnv () = [initEnv ()];

fun pushEnv env stack=(env::stack);
fun popEnv stack = if tl(stack)=nil then nil
else ((hd stack)::popEnv (tl stack));
fun topEnv stack = if tl(stack)=nil then hd(stack)
else (topEnv (tl stack));


