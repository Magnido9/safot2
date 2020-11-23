(*Question 1*)
fun valAt l n = if n=0 then hd(l)
else (valAt (tl l) (n-1));
(valAt [13,2,3,4] 0);
fun SumIndices l il = if tl(il)=nil then (valAt l (hd il))
else (valAt l (hd il))+(SumIndices l (tl il));

(SumIndices [13,2,3,4] [0,1]);

(*Question 2*)
(*a*)
fun toLower s = 
    let
      val list=String.explode(s)
      
    in
      
     implode( map Char.toLower list)

    end;
(*b*)
fun countOccurrs (str,cha)=
    let

    val s=toLower(str);
    val list= String.explode(s);  
    fun addone x =x+1;
    fun addzero x=x+0; 
    fun plusIf (c,counter) = if (Char.ord(c)=Char.ord(cha)) then addone(counter) else addzero(counter);
    in
   
     foldl plusIf 0 list
    end;
(*c*)
fun getAllOccurrs s=
let
  fun makeTuple c=(c,countOccurrs(s,c));
in
  map makeTuple [#"a",#"b",#"c",#"d",#"e",#"f",#"g",#"h",#"i",#"j",#"k",#"l",#"m",#"n",#"o",#"p",#"q",#"r",#"s",#"t",#"u",#"v",#"w",#"x",#"y",#"z"]
end;
(*d*)
fun areAnagrams(s1,s2)= 
let
fun comptup(x,y)=if(x=y) then true else false;
  fun comList ([],[])=true
  | comList ([],_)=false
  | comList (_,[])=false
  | comList (x::xs , y::ys) =if comptup(x,y) then comList(xs,ys) else false;

in
  comList(getAllOccurrs(s1),getAllOccurrs(s2))
end;


(*Question 3*)
(*a*)
exception Undefined;
exception Empty;
fun initEnv ()= fn f:string=>(raise Undefined);

(*b*)
fun define name old_env value= fn x:string => if(x=name) then value else old_env(x);

(*c*)
fun emptyNestedEnv () = [initEnv ()];

fun pushEnv env stack=(env::stack);
fun popEnv stack = if tl(stack)=nil then nil
else ((hd stack)::popEnv (tl stack));
fun topEnv stack = if tl(stack)=nil then hd(stack)
else (topEnv (tl stack));
(*c*fix -above ^ made some problem while runnig*) 
fun pushEnv env env_list=(env::env_list);
fun popEnv env_list =if List.length(env_list)=1 then raise Empty else tl env_list;    
fun topEnv env_list= if List.length(env_list)=1 then raise Empty else hd env_list;

(*d*)
fun defineNested name env_list value=pushEnv (define name (topEnv(env_list)) value) (popEnv(env_list));

