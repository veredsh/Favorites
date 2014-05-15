(* compiler.sml
 * A compiler for Scheme, written in SML/NJ
 * as part of the compiler construction course
 *
 * Programmer: Mayer Goldberg, 2011
 *)
Control.Print.printDepth := 100000;
Control.Print.printLength := 100000;
Control.Print.stringDepth:= 100000;
Control.polyEqWarn := false;
    
(*Control.Print.printLength:=1000;*)
(*Control.Print.stringDepth:=1000;*)
fun rel5() = use("C:\\users\\uriya\\Desktop\\comp\\compiler.sml");





fun power ((m, n) :(int * int)) = if n <= 0 
								then 1
								else (m * power(m, n-1)) :int;

fun itoa(a) = if (a >= 0 andalso a < 10)
					then str(chr(a+48))
					else if(a < 0)
						then "-"^itoa(a*(~1))
						else let
								val temp = a div 10
								val left = a-(temp*10)
								val leftstr = str(chr(left+48))
							in
								itoa(temp)^leftstr
							end;
local val n = ref 0
in
fun myGensym () =
   ( n := (! n) + 1 ;
	 itoa(! n) ) 
end;							
							
fun fileToString ( filename : string ) =
	let 
		val f = TextIO.openIn filename
			fun loop s =
				( case TextIO.input1 f
				of NONE => s
				|SOME c => loop ( c::s ) )
		val result = String.implode( rev ( loop [ ] ) )
	in
		TextIO.closeIn f;
		result
	end;
fun stringToFile (str : string , filename : string) =
	let 
		val f = TextIO.openOut filename
	in
		( TextIO.output(f, str);
		TextIO.closeOut f )
	end;
fun andmap f nil = true
  | andmap f (a :: s) = (f a) andalso (andmap f s);

fun ormap f nil = false
  | ormap f (a :: s) = (f a) orelse (ormap f s);

fun concatmap f nil = nil
  | concatmap f (a :: s) = (f a) @ (concatmap f s);

exception ErrorMap2UnequalLengths;

fun map2(f, [], [])= []
  | map2(f, a::s, a'::s') = 
		f(a, a')::map2(f, s, s')
  | map2(_, _, _) = 
		raise ErrorMap2UnequalLengths;

fun makeIsChar(string) = 
    let val chars = explode(string)
    in
	fn ch => ormap (fn ch' => ch = ch') chars
    end;

fun makeCharInRange(charFrom, charTo) = 
 fn ch : char => (charFrom <= ch) andalso (ch <= charTo);

fun stringEqual(string1, string2) = 
    String.compare(string1, string2) = EQUAL;

fun stringNotEqual(str, str') = 
    not(stringEqual(str, str'));

fun stringIsMember (str, list) = 
    ormap (fn str' => stringEqual(str, str')) list;

fun stringNotAMember (str, list) = 
    not (stringIsMember (str, list));

fun addStringToSet (str, []) = [str]
  | addStringToSet (str, s as (str' :: rest)) = 
    if (stringEqual(str, str')) then s
    else str' :: (addStringToSet (str, rest));

fun unionStringSets ([], s') = s'
  | unionStringSets ((str :: s), s') = 
    unionStringSets(s, (addStringToSet(str, s')));

datatype SchemeToken = LparenToken
		     | RparenToken
		     | QuoteToken
		     | DotToken
		     | VectorToken
		     | IntToken of int
		     | CharToken of char
		     | StringToken of string
		     | SymbolToken of string
		     | BoolToken of bool;

datatype Sexpr = Void
	       | Nil
	       | Pair of Sexpr * Sexpr
	       | Vector of Sexpr list
	       | Symbol of string
	       | String of string
	       | Number of int
	       | Bool of bool
	       | Char of char;

datatype Expr = Const of Sexpr
	      | Var of string
	      | VarFree of string               (* free variable *)
	      | VarParam of string * int   (* parameter variable *)
	      | VarBound of string * int * int (* bound variable *)
	      | If of Expr * Expr * Expr
	      | Abs of (string list) * Expr
	      | AbsOpt of (string list) * string * Expr
	      | AbsVar of string * Expr
	      | App of Expr * (Expr list)
	      | AppTP of Expr * (Expr list)  (* in tail position *)
	      | Seq of Expr list
	      | Or of Expr list
	      | Set of Expr * Expr
	      | Def of Expr * Expr;

signature SCANNER = 
sig
    val stringToTokens : string -> SchemeToken list;
end;

signature READER = 
sig
    val stringToSexpr : string -> Sexpr;
    val stringToSexprs : string -> Sexpr list;
end;

fun sexprToString'(Void) = "#<void>"
  | sexprToString'(Nil) = "()"
  | sexprToString'(Number(n)) = Int.toString(n)
  | sexprToString'(Char(#" ")) = "#\\space"
  | sexprToString'(Char(#"\t")) = "#\\tab"
  | sexprToString'(Char(#"\f")) = "#\\page"
  | sexprToString'(Char(#"\n")) = "#\\newline"
  | sexprToString'(Char(#"\r")) = "#\\return"
  | sexprToString'(Char(ch)) = 
    if (ch > #" ") then "#\\" ^ Char.toString(ch)
    else let val n = ord(ch)
	     val o3 = n mod 8
	     val tmp = n div 8
	     val o2 = tmp mod 8
	     val o1 = tmp div 8
	 in
	     "#\\" ^
	     Int.toString(o1) ^
	     Int.toString(o2) ^ 
	     Int.toString(o3)
	 end
  | sexprToString'(Bool(true)) = "#t"
  | sexprToString'(Bool(false)) = "#f"
  | sexprToString'(String(str)) = "\"" ^ str ^ "\""
  | sexprToString'(Symbol(name)) = name
  | sexprToString'(Pair(Symbol("quote"),
		   Pair(e, Nil))) = "'" ^ sexprToString'(e)
  | sexprToString'(Pair(car, cdr)) = toStringWithCar(sexprToString'(car), cdr)
  | sexprToString'(Vector(s)) = 
    "#(" ^ (String.concatWith " " (map sexprToString' s)) ^ ")"
and toStringWithCar(car, Nil) = "(" ^ car ^ ")"
  | toStringWithCar(car, Pair(first, second)) = 
    toStringWithCar(car ^ " " ^ sexprToString'(first), second)
  | toStringWithCar(car, e) = "(" ^ car ^ " . " ^ sexprToString'(e) ^ ")"
and sexprToString(Void) = ""
  | sexprToString(e) = sexprToString'(e);

local 
    val bintag = (fn tag => (fn str => "<" ^ tag ^ ">" ^ str ^ "</" ^ tag ^ ">"))
    val ital = bintag "I"
    val bold = bintag "B"
    val sub = bintag "SUB"
    val html = bintag "HTML"
    val head = bintag "HEAD"
    val body = bintag "BODY"
    val bintagAttr = 
	(fn tag => (fn attr => (fn str => "<" ^ tag ^ " " ^ attr ^ ">" ^ str ^ "</" ^ tag ^ ">")))
    val red = bintagAttr "FONT" "COLOR='RED'"
    val blue = bintagAttr "FONT" "COLOR='BLUE'"
    val rec run = 
	(fn (Const(sexpr)) => (sexprToString sexpr)
	  | (Var(v)) => (ital v)
	  | (VarFree(v)) => (ital v) ^
			    (sub "f")
	  | (VarParam(v, mi)) => (ital v) ^
				 (sub ("p" ^ (sub (red (Int.toString mi)))))
	  | (VarBound(v, ma, mi)) => (ital v) ^
				     (sub ("b" ^ (sub ((red (Int.toString ma)) ^ 
						       "," ^ 
						       (red (Int.toString mi))))))
	  | (If(test, dit, (Const(Void)))) => 
	    let val test = run test
		val dit = run dit
	    in
		"(" ^ (bold "if") ^ " " ^ test ^ " " ^ dit ^ ")"
	    end
	  | (If(test, dit, dif)) => 
	    let val test = run test
		val dit = run dit
		val dif = run dif
	    in
		"(" ^ (bold "if") ^ " " ^ test ^ " " ^ dit ^ " " ^ dif ^ ")"
	    end
	  | (Abs(vars, expr)) => 
	    let val vars = String.concatWith " " (map ital vars)
		val expr = run expr
	    in
		"(&lambda; (" ^ vars ^ ") " ^ expr ^ ")"
	    end
	  | (AbsOpt(vars, var, expr)) => 
	    let val vars = String.concatWith " " (map ital vars)
		val var = ital var
		val expr = run expr
	    in
		"(&lambda; (" ^ vars ^ " . " ^ var ^ ") " ^ expr ^ ")"
	    end
	  | (AbsVar(var, expr)) => 
	    let val var = ital var
		val expr = run expr
	    in
		"(&lambda; " ^ var ^ " " ^ expr ^ ")"
	    end
	  | (App(proc, [])) => 
	    let val proc = run proc
	    in
		"(" ^ proc ^ ")"
	    end
	  | (App(proc, args)) => 
	    let val proc = run proc
		val args = String.concatWith " " (map run args)
	    in
		"(" ^ proc ^ " " ^ args ^ ")"
	    end
	  | (AppTP(proc, [])) => 
	    let val proc = run proc
	    in
		(blue "(") ^ proc ^ (blue ")")
	    end
	  | (AppTP(proc, args)) => 
	    let val proc = run proc
		val args = String.concatWith " " (map run args)
	    in
		(blue "(") ^ proc ^ " " ^ args ^ (blue ")")
	    end
	  | (Seq(exprs)) => 
	    let val exprs = String.concatWith " " (map run exprs)
	    in
		"(" ^ exprs ^ ")"
	    end
	  | (Or([])) => "(or)"
	  | (Or(exprs)) => 
	    let val exprs = String.concatWith " " (map run exprs)
	    in
		"(" ^ (bold "or") ^ " " ^ exprs ^ ")"
	    end
	  | (Set(var, expr)) => 
	    let val var = run var
		val expr = run expr
	    in
		"(" ^ (bold "set!") ^ " " ^ var ^ " " ^ expr ^ ")"
	    end
	  | (Def(var, expr)) => 
	    let val var = run var
		val expr = run expr
	    in
		"(" ^ (bold "define") ^ " " ^ var ^ " " ^ expr ^ ")"
	    end
	)
in
fun exprToHTMLString e = 
    html((head "") ^ (body (run e)))
end;

signature TAG_PARSER = 
sig
    val stringToPE : string -> Expr;
    val stringToPEs : string -> Expr list;
end;
	 
signature SEMANTIC_ANALYSIS =
sig
val boxSet : Expr -> Expr;
val annotateTC : Expr -> Expr;
val lexicalAddressing : Expr -> Expr;
val analysis : Expr -> Expr;
end;

exception NotAList of Sexpr;

fun schemeListToML Nil = []
  | schemeListToML (Pair(car, cdr)) = car :: (schemeListToML cdr)
  | schemeListToML e = raise NotAList(e);

fun MLListToScheme [] = Nil
  | MLListToScheme (a :: s) = Pair(a, (MLListToScheme s));

exception ErrorNothingAfterHash;
exception ErrorBadChar of char * string;
exception ErrorStringDoesntEnd of string;
exception ErrorNoMetaChar of string;
exception ErrorNoSuchMetaChar of string;
exception ErrorNoChar;
exception ErrorUnknownNamedChar of string;
exception ErrorHash of string;


structure Scanner : SCANNER = 
struct
val whiteChar = makeIsChar(" \t\r\n");
val delimiterChar = makeIsChar("'()\";, \t\r\n");
val octalChar = makeCharInRange(#"0", #"7");
val upperChar = makeCharInRange(#"A", #"Z");
val lowerChar = makeCharInRange(#"a", #"z");
val digitChar = makeCharInRange(#"0", #"9");
val specialSymbolChar = makeIsChar("!@$%^*-_=+<>/?.&");

fun symbolChar (ch) = 
    lowerChar(ch) orelse 
    upperChar(ch) orelse
    digitChar(ch) orelse
    specialSymbolChar(ch);

local
    fun stInit([]) = []
      | stInit(#";" :: s) = stComment(s)
      | stInit(#"(" :: s) = LparenToken :: stInit(s)
      | stInit(#")" :: s) = RparenToken :: stInit(s)
      | stInit(#"'" :: s) = QuoteToken :: stInit(s)
      | stInit(#"#" :: s) = stHash(s)
      | stInit(#"." :: s) = DotToken :: stInit(s)
      | stInit(#"\"" :: s) = stString(s, [])
      | stInit(ch :: s) = 
	if symbolChar(ch) then stSymbol(s, [ch])
	else if whiteChar(ch) then stInit(s)
	else raise ErrorBadChar(ch, implode(s))
    and stSymbol([], chars) = 
	symbolOrNumberToken(charsToString(chars)) :: stInit([])
      | stSymbol(s as char :: s', chars) = 
	if symbolChar(char) then stSymbol(s', char :: chars)
	else symbolOrNumberToken(charsToString(chars)) :: stInit(s)
    and stString([], chars) = raise ErrorStringDoesntEnd(charsToString(chars))
      | stString(#"\"" :: s, chars) = 
	StringToken(charsToString(chars)) :: stInit(s)
      | stString(#"\\" :: s, chars) = stStringMetaChar(s, chars)
      | stString(ch :: s, chars) = stString(s, ch :: chars)
    and stStringMetaChar([], chars) = 
	raise ErrorNoMetaChar(charsToString(chars) ^ "\\")
      | stStringMetaChar(#"t" :: s, chars) = stString(s, #"\t" :: chars)
      | stStringMetaChar(#"T" :: s, chars) = stString(s, #"\t" :: chars)
      | stStringMetaChar(#"r" :: s, chars) = stString(s, #"\r" :: chars)
      | stStringMetaChar(#"R" :: s, chars) = stString(s, #"\r" :: chars)
      | stStringMetaChar(#"n" :: s, chars) = stString(s, #"\n" :: chars)
      | stStringMetaChar(#"N" :: s, chars) = stString(s, #"\n" :: chars)
      | stStringMetaChar(#"f" :: s, chars) = stString(s, #"\f" :: chars)
      | stStringMetaChar(#"F" :: s, chars) = stString(s, #"\f" :: chars)
      | stStringMetaChar(#"Y" :: s, chars) =
	stString(s, [#"l",#"e",#"a",#"Y"] @ chars)
      | stStringMetaChar(#"\\" :: s, chars) = stString(s, #"\\" :: chars)
      | stStringMetaChar(#"\"" :: s, chars) = stString(s, #"\"" :: chars)
      | stStringMetaChar(ch :: s, chars) = 
	raise ErrorNoSuchMetaChar("\\" ^ Char.toString(ch))
    and stHash([]) = raise ErrorNothingAfterHash
      | stHash(#"t" :: s) = BoolToken(true) :: stInit(s)
      | stHash(#"T" :: s) = BoolToken(true) :: stInit(s)
      | stHash(#"f" :: s) = BoolToken(false) :: stInit(s)
      | stHash(#"F" :: s) = BoolToken(false) :: stInit(s)
      | stHash(#"\\" :: s) = stChar(s)
      | stHash(#"(" :: s) = VectorToken :: stInit(s)
      | stHash(s) = raise ErrorHash("#\\" ^ implode(s))
    and stChar([]) = raise ErrorNoChar
      | stChar(ch :: s) = stChar'(s, [ch])
    and stChar'([], chars) = makeCharToken(chars) :: stInit([])
      | stChar'(s as ch :: s', chars) = 
	if delimiterChar(ch) then makeCharToken(chars) :: stInit(s)
	else stChar'(s', ch :: chars)
    and stComment([]) = stInit([])
      | stComment(#"\n" :: s) = stInit(s)
      | stComment(ch :: s) = stComment(s)
    and charsToString(s) = implode(rev(s))
    and makeCharToken([ch]) = CharToken(ch)
      | makeCharToken(chars as [ch1, ch2, ch3]) = 
	if (andmap octalChar chars) then
	    CharToken(chr(digitToInt(ch1) + 
			  8 * (digitToInt(ch2) + 
			       8 * digitToInt(ch3))))
	else charNameToCharToken(charsToString(chars))
      | makeCharToken(chars) = charNameToCharToken(charsToString(chars))
    and charNameToCharToken(charName) = 
	if stringEqual(charName, "space") then CharToken(#" ")
	else if stringEqual(charName, "return") then CharToken(#"\r")
	else if stringEqual(charName, "newline") then CharToken(#"\n")
	else if stringEqual(charName, "tab") then CharToken(#"\t")
	else if stringEqual(charName, "page") then CharToken(#"\f")
	else raise ErrorUnknownNamedChar(charName)
    and digitToInt(ch) = ord(ch) - ord(#"0")
    and symbolOrNumberToken(string) = 
	case Int.fromString(string) of
	    SOME (n) => IntToken(n)
	  | NONE => SymbolToken(string)
in
fun stringToTokens(string) = stInit(explode(string))
end;
end; (* of structure Scanner *)

exception NoSexprAfterQoute;
exception VectorIsNotGood;
exception NoArgumentAfterDotTokenOrNotRparenAfterTheExpr;
exception BadInputAfterLparenToken;
exception TheListOfTokenThatWeReadNotEmpty;
exception NoRaprenTokenAfterSexprThatAfterDotToken;



fun buildPairVHTW ([], b) =b
	|buildPairVHTW ((head::tail), b) =Pair(head,(buildPairVHTW (tail, b)));
	
fun TokenListToSexpr[]= (NONE,[])
|	TokenListToSexpr(RparenToken::TokenList)= (NONE , (RparenToken::TokenList))
|	TokenListToSexpr(TokenList as DotToken::_) = (NONE , TokenList)
|	TokenListToSexpr(IntToken(n)::TokenList)=(SOME(Number(n)) , TokenList)
|	TokenListToSexpr(BoolToken(n)::TokenList)=(SOME(Bool(n)) , TokenList)
|	TokenListToSexpr(SymbolToken(n)::TokenList)=(SOME(Symbol(n)) , TokenList)
|	TokenListToSexpr(StringToken(n)::TokenList)=(SOME(String(n)) , TokenList)
|	TokenListToSexpr(CharToken(n)::TokenList)=(SOME(Char(n)),TokenList)
|	TokenListToSexpr(QuoteToken::TokenList)=(case (TokenListToSexpr TokenList) of (SOME (sexpr),TokenList2)=> (SOME (Pair (Symbol ("quote"),
																													Pair (sexpr ,Nil))),TokenList2)
																											|_=>raise  NoSexprAfterQoute)
|	TokenListToSexpr(VectorToken::TokenList)=(case (TokenListToSexprs TokenList) of (sexprs,(RparenToken::TokenList2))=>(SOME (Vector(sexprs)),TokenList2)
																												|_=>raise VectorIsNotGood)
|	TokenListToSexpr(LparenToken::TokenList)=(case (TokenListToSexprs TokenList) of (sexprs , (RparenToken::TokenList2))=>(SOME(buildPairVHTW (sexprs,Nil)),TokenList2)
																				| ((sexprs as (_::_)),DotToken::TokenList3)=>(case (TokenListToSexpr TokenList3) of (SOME(sexpr),RparenToken::TokenList4)=>(SOME (buildPairVHTW (sexprs,sexpr)), TokenList4)
																														|_=>raise NoArgumentAfterDotTokenOrNotRparenAfterTheExpr) (*  ^^^ jump over the RparenToken mayer bug :) *)
																				|_=>raise BadInputAfterLparenToken)
and TokenListToSexprs(list1) =(case (TokenListToSexpr (list1)) of  
	(NONE,TokenList)         => ([],TokenList)
	|(SOME(sexpr),TokenList) =>(case (TokenListToSexprs TokenList) of (sexprs,TokenList2) => (sexpr::sexprs,  TokenList2)));
																
									
structure Reader : READER = 
struct
fun stringToSexprs s1 =(case (TokenListToSexprs(Scanner.stringToTokens(s1))) of (sexprlist ,[]) =>sexprlist
																				|_=>raise TheListOfTokenThatWeReadNotEmpty);
fun stringToSexpr  s1 = (case (TokenListToSexprs(Scanner.stringToTokens(s1))) of (sexpr::tail,[]) =>sexpr
																				|_=>raise TheListOfTokenThatWeReadNotEmpty);


end; (* of structure Reader *)

exception ErrorReservedWordUsedImproperly of string;
exception ErrorMalformedLambdaArgList of string;
exception UnrecognizedAST of Sexpr;
exception NotASymbol of Sexpr;
exception UnexpectedArgument of Sexpr;
exception UnexpectedArguments of Sexpr*Sexpr;
exception EmptyLambdaBody;
exception UnrecognizedSexpr of Sexpr;
exception WrongCondSyntax;
exception WrongLetSyntax;
exception NotAPair;
exception IncompatibleLengthLists of Sexpr*Sexpr;
exception NotMITDefine of Sexpr;

fun schemeListAppend(Nil , lst) = lst
  | schemeListAppend(Pair(h1, t1), lst) = Pair(h1, schemeListAppend(t1, lst))
  | schemeListAppend(e1, e2) = raise UnexpectedArguments(e1, e2);

fun schemeMultiListMap(func, Nil, Nil) = Nil
  | schemeMultiListMap(func, Pair(h1, t1), Pair(h2, t2)) = 
    Pair(func(h1, h2), schemeMultiListMap(func, t1, t2))
  | schemeMultiListMap(func, e1, e2) = raise IncompatibleLengthLists(e1, e2);

fun schemeListMap(func, Nil) = Nil
  | schemeListMap(func, Pair(head, tail)) = 
    Pair(func(head), schemeListMap(func, tail))
  | schemeListMap(func,e) = raise NotAList(e);

fun isSchemeList(Nil) = true
  | isSchemeList(Pair(a, b)) = isSchemeList(b)
  | isSchemeList e = false;

fun schemeCar(Pair(a,b)) = a
  | schemeCar(_) = raise NotAPair;

fun schemeCdr(Pair(a,b)) = b
  | schemeCdr(_) = raise NotAPair;


structure TagParser : TAG_PARSER = 
struct
val reservedSymbols = ["and", "begin", "cond", "define", "else", 
		       "if", "lambda", "let", "let*", "letrec", 
		       "or", "quote", "set!"];
local
fun reservedWord(str) = 
    ormap (fn rs => (String.compare(rs, str) = EQUAL)) reservedSymbols

and condRibsToIf(Pair(Pair(Symbol "else", seq), Nil)) = 
    Pair(Symbol("if"),
		      Pair(Bool(true), 
			      Pair(Pair(Symbol "begin", seq), Nil)))
  | condRibsToIf(Pair(Pair(test, seq), Nil)) = 
  (* the case where there is no else rib *)
    Pair(Symbol("if"),
		      Pair(test,
			      Pair(Pair(Symbol "begin", seq), Nil)))	
  | condRibsToIf(Pair(Pair(test, seq), rest)) = 
    Pair(Symbol("if"),
		      Pair(test, 
			      Pair(Pair(Symbol "begin", seq), 
				      Pair(condRibsToIf(rest), Nil))))
  | condRibsToIf e = raise WrongCondSyntax
				      
and andToIf(Nil) = Bool true
  | andToIf(Pair(arg, Nil)) = arg (*one argument*)
  | andToIf(Pair(a,Pair(b,Nil))) =
    Pair(Symbol("if"),Pair(a,Pair(b,Pair(Bool false,Nil))))
  | andToIf(Pair(a, b)) = 
    Pair(Symbol("if"), Pair(a, Pair(andToIf(b), Pair(Bool false, Nil))))
 	
  | andToIf(e) = raise UnrecognizedSexpr e
 

and removeLast(Pair(a, b)) = Pair(a, removeLast(b))
  | removeLast(a) = Nil (*a is not a pair so it is the last and is removed*)

and getLast(Pair(a, b)) = getLast(b)
  | getLast(Symbol(a)) = a (*a is not a pair so it is the last and is returned*)
  | getLast(a) = raise UnexpectedArgument(a) (* because of warnings *)   

and symToString(Symbol(a)) = a
  | symToString(a) = raise UnexpectedArgument(a) (*because of warnings*)

and isDefineMIT(Pair(Symbol "define", Pair(Pair(Symbol name, args), body))) = 
    	true
  | isDefineMIT(e) = false

and mitToSimple(Pair(Symbol "define", Pair(Pair(Symbol name, args), body))) = 
    		Pair(Symbol "define",
    		 Pair(Symbol name,
		      Pair(	   
    		        Pair(Symbol "lambda",	
		             Pair(args, body)),
		      Nil)))
  | mitToSimple(e) = raise NotMITDefine(e)

(* assumption: inner define appears only at the beginning of the sequence *) 
and innerDefineList(e as Pair(Pair(Symbol "define", restdef), restlist)) = 
    if isDefineMIT(schemeCar(e)) then
    	Pair(schemeCdr(mitToSimple(schemeCar(e))), innerDefineList(restlist))
    else 
    	Pair(restdef, innerDefineList(restlist))
    | innerDefineList(e) = Nil

(* eliminate all inner defines from sequence *)
and getNoDefineSeq(Pair(Pair(Symbol "define", restdef), restlist)) = 
    				    getNoDefineSeq(restlist)
  | getNoDefineSeq(e) = e				    

(* implicit sequence in expressions: let, let*, lambda, letrec *)

and parseSeq(e as Pair(a, b)) = 
    let
	val defines = innerDefineList(e)
	val body = getNoDefineSeq(e)
    in 
	if defines = Nil then
	   if b = Nil then parseExpr(a) (* no implicit sequence *)
	   else Seq(map(parseExpr) (schemeListToML(e))) (* implicit sequence *)
	else
	   parseExpr(makeLetrecSexpr(defines, body)) 
	   (* note: body may be empty if seq include only inner defines *)  
    end	
  | parseSeq e = raise EmptyLambdaBody

and makeAppSexpr(operator, operands) = Pair(operator, operands)

and makeLambdaSexpr(argList, seq) = 
    Pair(Symbol "lambda", Pair(argList, seq))

and makeLetSexpr(var_val, seq) = 
    Pair(Symbol "let", Pair(var_val,seq))

and makeLetrecSexpr(val_var, seq) = 
    Pair(Symbol "letrec", Pair(val_var, seq))

and parseExpr(Void) = Const(Void)
  | parseExpr(Nil) = Const(Nil)
  | parseExpr(e as Number(_)) = Const(e)
  | parseExpr(e as Char(_)) = Const(e)
  | parseExpr(e as Bool(_)) = Const(e)
  | parseExpr(e as String(_)) = Const(e)
  | parseExpr(e as Vector(_)) = Const(e)
  | parseExpr(Pair(Symbol("quote"),
		      Pair(e, Nil))) = Const(e)
  | parseExpr(Symbol(e)) =
    if reservedWord(e) then raise ErrorReservedWordUsedImproperly(e)
    else Var(e)

  | parseExpr(Pair(Symbol("if"),
		      Pair(test, 
			   Pair(dit, Nil)))) = 
    If(parseExpr(test), parseExpr(dit), Const(Void))
  | parseExpr(Pair(Symbol("if"),
		      Pair(test, 
			      Pair(dit, 
				      Pair(dif, Nil))))) = 
    If(parseExpr(test), parseExpr(dit), parseExpr(dif))

  | parseExpr(Pair(Symbol("lambda"),
		      Pair(Symbol(arg1), body))) = 
    AbsVar(arg1, parseSeq(body))  
  | parseExpr(Pair(Symbol("lambda"), Pair(Nil, body))) = 
    Abs([], parseSeq(body))
  | parseExpr(Pair(Symbol("lambda"),
		Pair(Pair(a, b), body))) = 
    if isSchemeList(Pair(a,b)) then
       Abs(map(symToString) (schemeListToML(Pair(a,b))), parseSeq(body))
    else
       AbsOpt(map(symToString) (schemeListToML(removeLast(Pair(a,b)))),
                                        getLast(Pair(a,b)),parseSeq(body))

  | parseExpr(Pair(Symbol("or"),Nil)) = parseExpr(Bool false)
  | parseExpr(Pair(Symbol("or"),Pair(a, b))) = 
    Or(map parseExpr(schemeListToML(Pair(a,b))))
  | parseExpr(Pair(Symbol("or"), arg)) = parseExpr(arg)

  | parseExpr(Pair(Symbol("and"),args)) = parseExpr(andToIf(args))
  
  | parseExpr(Pair(Symbol "begin", Nil)) = Const(Void)
  | parseExpr(Pair(Symbol "begin", seq)) = parseSeq(seq)

  | parseExpr(Pair(Symbol "set!", Pair(Symbol var , Pair(exp, Nil)))) = 
    Set(parseExpr(Symbol var), parseExpr(exp))

  | parseExpr(Pair(Symbol "define", Pair(Symbol name, Pair(exp, Nil)))) = 
    Def(parseExpr(Symbol name), parseExpr(exp))
  (* define MIT-style *)
  | parseExpr(Pair(Symbol "define", Pair(Pair(Symbol name, args), body))) = 
    parseExpr(Pair(Symbol "define",
    		 Pair(Symbol name,
		      Pair(	   
    		        Pair(Symbol "lambda",	
		             Pair(args, body)),
		      Nil))))

  (* macro-expand cond *)
  | parseExpr(Pair(Symbol "cond", ribs)) = parseExpr(condRibsToIf(ribs))

  (* macro-expand let *)
  | parseExpr(Pair(Symbol "let", Pair(args, seq))) = 
    let 
    	val vars = schemeListMap(fn(Pair(car, Pair(cdr, Nil))) => car 
	       		      |(_) => raise WrongLetSyntax,
	  		     args)
	val vals = schemeListMap(fn(Pair(car, Pair(cdr, Nil))) => cdr 
	       		      |(_) => raise WrongLetSyntax,
	  		     args)
    in
	parseExpr(makeAppSexpr(makeLambdaSexpr(vars, seq), vals))
    end		  
  | parseExpr(Pair(Symbol "let*", Pair(args, seq))) = 
    let
	fun letStarToApp(Nil, body, depth) = body
	  | letStarToApp(args, body, depth) = 
	    let 
		val res = makeAppSexpr(
				makeLambdaSexpr(Pair(schemeCar(schemeCar(args)), Nil),
						letStarToApp(schemeCdr(args),
								body, depth+1)),				      Pair(schemeCar(schemeCdr(schemeCar(args))),Nil))
	    in 
	     if depth > 0 then Pair(res ,Nil) else res
	    end    
    in
	parseExpr(letStarToApp(args, seq, 0))
    end
    (* macro-expand letrec *)
  | parseExpr(Pair(Symbol "letrec", Pair(args, seq))) = 
    let    
	val vars = schemeListMap(fn(Pair(car, Pair(cdr, Nil))) => car 
	       		      |(_) => raise WrongLetSyntax,
	  		     args)
	val vals = schemeListMap(fn(Pair(car, Pair(cdr, Nil))) => cdr 
	       		      |(_) => raise WrongLetSyntax,
	  		     args)
	val makeSetSexpr = fn(var, value) => Pair(Symbol "set!", 
	    		   	   	         Pair(var, Pair(value, Nil)))
	val initialize = fn(var) => Pair(var, Pair(Void, Nil))
	(* note: seq may be Nil (See parseSeq) *)
	val body = (if seq = Nil then Nil
	    	   else Pair(Pair(makeLambdaSexpr(Nil,seq),Nil),Nil))
		   	
    in
    parseExpr(
	makeLetSexpr(schemeListMap(initialize ,vars), 
		     schemeListAppend(schemeMultiListMap(makeSetSexpr, 
		     					vars, vals), 
			      	      body)))
    end     		 
  | parseExpr (e as Pair(p, q)) = 
    App(parseExpr(p), map parseExpr(schemeListToML(q)))
  (*| parseExpr e = raise UnrecognizedAST(e)*)
in
	fun stringToPE string = parseExpr(Reader.stringToSexpr string);
	fun stringToPEs string = map parseExpr(Reader.stringToSexprs string);
end; (* of local *)
end; (* of structure TagParser *)
(*experiment*)

exception NotYetImplemented;
exception NotVarList;
exception NotAnExpr of Expr;
exception InnerDefine;
exception NotAbs;

fun findStrInList(str : string , nil : string list) = false
  | findStrInList(str1, str2::tail) = 
    	if (str1 = str2) then true
	else findStrInList(str1, tail);

(* 'a list * 'a list-> 'a list *)
fun conjunction(nil, lst2) = []
  | conjunction(lst1, nil) = []
  | conjunction(head::tail, lst2) = 
    if findStrInList(head, lst2) then 
       head::conjunction(tail, lst2)
    else
       conjunction(tail, lst2);
(* string list * string list -> string list *)
(* remove right list elements from left list *)
fun setSubtract(lst, nil) = lst
  | setSubtract(nil, lst) = []
  | setSubtract(head::tail, lst) = 
    if findStrInList(head, lst) then 
       setSubtract(tail, lst)
    else 
       head::setSubtract(tail, lst);



fun unionNoRepeats(lst1,lst2) =
    let
    	  fun iterUnion(nil, res) = res
	    | iterUnion(h::t, res) = 
	      	if findStrInList(h, res) then
	      	   iterUnion(t, res)
		else iterUnion(t, h::res)
    in 
       iterUnion(lst1, lst2)
    end; 
    	   
      

structure SemanticAnalysis : SEMANTIC_ANALYSIS =
struct
local
	
	fun fromLastFrame(str, nil) = false (* free var *)
	  | fromLastFrame(str, head::nil) = findStrInList(str, head)
	  | fromLastFrame(str, head::tail) = 
	    if findStrInList(str, head) then false
	    else fromLastFrame(str, tail)
	
	and findStrInList(str : string , nil : string list) = false
  	  | findStrInList(str1, str2::tail) = 
    	    if (str1 = str2) then true
	    else findStrInList(str1, tail)

	    (* 'a list * 'a list-> 'a list *)
	and conjunction(nil, lst2) = []
  	  | conjunction(lst1, nil) = []
  	  | conjunction(head::tail, lst2) = 
    	    if findStrInList(head, lst2) then 
       	       head::conjunction(tail, lst2)
    	    else
		conjunction(tail, lst2)
	(* string list * string list -> string list
	    remove right list elements from left list *)
	and setSubtract(lst, nil) = lst
  	  | setSubtract(nil, lst) = []
  	  | setSubtract(head::tail, lst) = 
    	    if findStrInList(head, lst) then 
       	       setSubtract(tail, lst)
    	    else 
       	       head::setSubtract(tail, lst)

	and unionNoRepeats(lst1,lst2) =
   	    let
		fun iterUnion(nil, res) = res
	    	  | iterUnion(h::t, res) = 
	      	    if findStrInList(h, res) then
	      	       iterUnion(t, res)
		    else iterUnion(t, h::res)
    	    in 
       	       iterUnion(lst1, lst2)
    	    end


	(* Expr*(string list)*(string list)->string list *)

	  and findSetBoundVars(If(test, dit, dif), env, params, cont) = 
	      	findSetBoundVars(test,env,params, 
		(fn(bt, st)=>findSetBoundVars(dit,env,params,
		(fn(bdit, sdit)=>findSetBoundVars(dif,env,params,
		(fn(bdif, sdif)=>
			 cont(unionNoRepeats(bt, unionNoRepeats(bdit, bdif)),
			      unionNoRepeats(st, unionNoRepeats(sdit, sdif)))))))))
	 
	  | findSetBoundVars(Const(e), env, params, cont) = cont([],[])
	  | findSetBoundVars(Var v, env, params, cont) = 
	    if findStrInList(v, params) then cont([],[])
	    else if fromLastFrame(v, env) then cont([v],[])
	    else cont([],[])

	  | findSetBoundVars(Def(var, exp), env, params, cont) = 
	  (* NOT an inner define *)
	     findSetBoundVars(var,env,params,
	     (fn(bv, sv)=>findSetBoundVars(exp,env,params,
	     (fn(be, se)=>cont(unionNoRepeats(bv,be),
				unionNoRepeats(sv,se))))))
	  
	  | findSetBoundVars(Seq(head::tail), env, params, cont) =
	    	findSetBoundVars(head,env,params,
		(fn(bh,sh)=>
			if (tail=[]) then cont(bh,sh)
			else 
		      	 findSetBoundVars(Seq(tail),env,params,
			(fn(bt,st)=>
				cont(unionNoRepeats(bh,bt),
					unionNoRepeats(sh,st))))))

	  | findSetBoundVars(Or(head::tail), env, params, cont) =
	    	findSetBoundVars(head,env,params,
		(fn(bh,sh)=>
			if (tail=[]) then cont(bh,sh)
			else 
		      	findSetBoundVars(Or(tail),env,params,
			(fn(bt,st)=>
				cont(unionNoRepeats(bh,bt),
					unionNoRepeats(sh,st))))))

	    (* vars is string list *)
	  | findSetBoundVars(Abs(vars, body), env, params, cont) = 
	    	findSetBoundVars(body, params::env, vars, cont)
	  (* vars is string list and opt is string *)
	  | findSetBoundVars(AbsOpt(vars, opt, body), env, params, cont) = 
	    	findSetBoundVars(body, params::env, vars@[opt], cont)
	  (* var is string *)
	  | findSetBoundVars(AbsVar(var, body), env, params, cont) = 
	    	findSetBoundVars(body, params::env, [var], cont)

	  | findSetBoundVars(App(operator, operands), env, params, cont) =
	    	findSetBoundVars(operator,env,params,
		(fn(bop,sop)=>
			if (operands = []) then cont(bop,sop)
			else
			findSetBoundVars(Seq(operands),env,params,
			(fn(bseq,sseq)=>cont(unionNoRepeats(bop,bseq),
						unionNoRepeats(sop,sseq))))))
	
	  | findSetBoundVars(Set(Var(v), y), env, params, cont) = 
	    if findStrInList(v, params) then (* param var *)
	       findSetBoundVars(y, env, params, 
	       	fn(by,sy)=> cont(by, unionNoRepeats([v],sy)))
	    else if fromLastFrame(v, env) then
	       findSetBoundVars(y, env, params, 
	       			fn(by,sy)=>cont(unionNoRepeats([v], by),
						unionNoRepeats([v], sy))) 
	    (* bound from upper frame *)
	    else findSetBoundVars(y, env, params, cont)
	  | findSetBoundVars(e,_,_,_) = raise NotAnExpr(e)

 	(* string list * Expr -> Expr
	   replace every appearence of string from strList in expr 
	   to vector of this string *)

	 and replaceStrByCons(strList, e as Set(Var(v), value)) = 
	    let
		val newValue = replaceStrByCons(strList, value)
	    in
	    	if findStrInList(v, strList) then
		   App(Var "set-car!", [Var(v), newValue])
		else
		   Set(Var(v), newValue)
	    end
	   | replaceStrByCons(strList,e as Var(v)) = 
	     	if findStrInList(v,strList) then 
		   App(Var "car", [Var(v)])
		else e   
	   | replaceStrByCons(strList, If(test,dit, dif)) = 
	     	If(replaceStrByCons(strList, test),
		   replaceStrByCons(strList, dit), 
		   replaceStrByCons(strList, dif))
	
	   | replaceStrByCons(strList, Seq(exps)) = 
	     Seq(map (fn(exp)=>replaceStrByCons(strList,exp))(exps))
	     
	   | replaceStrByCons(strList, Or(exps)) = 
	     Or(map (fn(exp)=>replaceStrByCons(strList,exp))(exps))

	   | replaceStrByCons(strList, e as Abs(vars, body)) = 
	     let 
	     	 val subtract = setSubtract(strList, vars)
	     in
	     	     Abs(vars, replaceStrByCons(subtract, body))
	     end
	   | replaceStrByCons(strList, e as AbsOpt(vars, opt, body)) = 
	     let 
	     	 val subtract = setSubtract(strList, vars@[opt])
	     in
	     	     AbsOpt(vars,opt, replaceStrByCons(subtract, body))
	     end
	   | replaceStrByCons(strList, e as AbsVar(var, body)) = 
	     let
		 val subtract = setSubtract(strList, [var])
	     in
		 AbsVar(var, replaceStrByCons(subtract, body))
	     end 
	   | replaceStrByCons(strList, e as Const(_)) = e
	   | replaceStrByCons(strList, App(operator, operands)) = 
	     let
		val newoperator = replaceStrByCons(strList, operator)
		val newoperands = 
		    map (fn(operand)=>replaceStrByCons(strList, operand))
								(operands)
	     in
		App(newoperator, newoperands)
	     end
	   | replaceStrByCons(strList, e) = raise InnerDefine

	   and makeCons(str : string) = 
	       Set(Var str, App(Var "cons", [Var str, Const Nil]))
	   
	   and changeAbsBody(Abs(args,oldbody), newbody) = 
	       Abs(args, newbody)
	     | changeAbsBody(AbsOpt(args,opt,oldbody), newbody) = 
	       AbsOpt(args,opt,newbody)
	     | changeAbsBody(AbsVar(var, oldbody),newbody) = 
	       AbsVar(var, newbody)
	     | changeAbsBody(_) = raise NotAbs
	     
	   and getAbsBody(Abs(args,body)) = body
	     | getAbsBody(AbsOpt(args,opt,body)) = body 
	     | getAbsBody(AbsVar(var,body)) = body
	     | getAbsBody(_) = raise NotAbs

	   and getArgList(Abs(args, body)) = args
	     | getArgList(AbsOpt(args, opt, body)) = args @ [opt]
	     | getArgList(AbsVar(var, body)) = [var]
             | getArgList(_) = raise NotAbs
		 
	   and runBoxSet(e as (Abs(_,_) | AbsOpt(_,_,_) |
	       		   AbsVar(_,_))) = 
	       if (getArgList(e) = []) then 
	       	  changeAbsBody(e, runBoxSet(getAbsBody(e)))
	       else
	       let
		val strList = findSetBoundVars(getAbsBody(e),[],
						getArgList(e),
						(fn(x,y)=>conjunction(x,y)))
	       in
		if (strList = []) then 
		   changeAbsBody(e, runBoxSet(getAbsBody(e)))
		else let
			val sets = map makeCons(strList)
			val bodyout = (fn(Seq(exps)) => exps
			    	      	|(exp) => [exp])
		     in
			changeAbsBody(e, 
				Seq(sets @ bodyout(
				    runBoxSet(replaceStrByCons(strList, 
						getAbsBody(e))))))
		     end
	       end
	     | runBoxSet(If(test,dit, dif)) = 
	       	If(runBoxSet(test),runBoxSet(dit),runBoxSet(dif))
	     | runBoxSet(Seq(exps)) = 
	       	Seq(map runBoxSet(exps))
	     | runBoxSet(Or(exps)) = 
	       	Or(map runBoxSet(exps))
	     | runBoxSet(Set(var, value)) =  
	        Set(runBoxSet(var), runBoxSet(value))
	     | runBoxSet(App(operator,operands)) = 
	       	App(runBoxSet(operator), map runBoxSet(operands))
	     | runBoxSet(Def(var, value)) =
	       	Def(runBoxSet(var), runBoxSet(value))
	     | runBoxSet(e) = e
	 
		and find(str,[],count)= ~1
			|find(str,h::t,count)=if (str=h) then count else find(str,t,(count+1))
  
		and findInListsList (str,[],countMinor)=[~1,~1]
		  | findInListsList(str,h::t,countMinor)=
			let val major=find(str,h,0)
			in
				if major > ~1 then [countMinor,major] else findInListsList(str,t,countMinor+1)
			end
  
		and lookUp(str,env,params)=
		let  val paramnum=find(str,params,0)
			 val mimj =findInListsList(str,env,0)
		in
			if (paramnum > ~1) then VarParam(str,paramnum) else if (hd(mimj))> ~1 then VarBound(str,hd(mimj),hd(tl(mimj))) else VarFree(str)
		end
  
		and  runLexical (Const(sexpr),env,params)=Const(sexpr)
			|runLexical (Var(str),env,params)=lookUp(str,env,params)
			|runLexical ((Abs(vars,body)),env,params) =Abs(vars,runLexical(body,(params::env),vars))
			|runLexical ((AbsOpt(vars,opt,body)),env,params)=AbsOpt(vars,opt,runLexical(body,(params::env),(vars@[opt])))(*mybe need change Abs to AbsOpt*)
			|runLexical ((AbsVar(var,body)),env,params)=AbsVar(var,runLexical(body,(params::env),[var]))
			|runLexical ((If (test,dit,dif)),env,params)=If(runLexical(test,env,params),runLexical(dit,env,params),runLexical(dif,env,params))
			|runLexical ((Def(v,e)),env,params)=Def(runLexical(v,env,params),runLexical(e,env,params))
			|runLexical ((App(proc,args)),env,params)=App(runLexical(proc,env,params),(map (fn x=> runLexical(x,env,params)) args))
			|runLexical ((AppTP(proc,args)),env,params)=AppTP(runLexical(proc,env,params),(map (fn x=> runLexical(x,env,params)) args))
			|runLexical ((Set(expr1,expr2)),env,params)=Set(runLexical(expr1,env,params),runLexical(expr2,env,params))
			|runLexical ((Seq(exprList)),env,params)=Seq((map (fn x=> runLexical(x,env,params)) exprList))
			|runLexical ((Or(exprList)),env,params)=Or((map (fn x=> runLexical(x,env,params)) exprList))

			|runLexical(x,y,z) = raise NotYetImplemented

		and reverse [] = []
		  | reverse (x::xs) = (reverse xs) @ [x]
  
		and runTP (pe as Const( _ )) inTP = pe
		  | runTP (pe as Var( _ )) inTP = pe 
   	      | runTP (If(test,dit,dif)) inTP= If((runTP test false),(runTP dit inTP),(runTP dif inTP))
    	  | runTP (Seq(some)) inTP=Seq(reverse ((runTP (hd (reverse some)) inTP)::(map (fn x=> runTP x false)  (tl(reverse some)))))
		  | runTP (Or(some)) inTP= Or(reverse ((runTP (hd (reverse some)) inTP)::(map (fn x=> runTP x false)  (tl(reverse some)))))
		  | runTP (Set(v,e)) inTP= Set(v ,(runTP e false))
		  | runTP (Def(v,e)) inTP= Def(v , (runTP e false))
		  | runTP (App(proc,args)) false = App((runTP proc false), (map (fn x=> runTP x false) args))
		  | runTP (App(proc,args)) true = AppTP((runTP proc false), (map (fn x=> runTP x false) args))(*this is the main change*)
		  | runTP (Abs(vars,body)) inTP = Abs(vars, (runTP body true))
		  | runTP (AbsVar(var,body)) inTP = AbsVar(var, (runTP body true))
		  | runTP (AbsOpt(vars,opt,body)) inTP = AbsOpt(vars, opt,(runTP body true))
		  | runTP ( _ ) boolean =raise NotYetImplemented
		 
in
fun boxSet(expr : Expr) = runBoxSet(expr);
fun annotateTC pe= runTP pe true;
fun lexicalAddressing(exp)=runLexical (exp,[[]],[]);
fun analysis expr = lexicalAddressing (annotateTC (boxSet expr));
end; (* of local *) 
end; (* of structure SemanticAnalysis *)

signature CODE_GEN =
sig
(*val tester : string -> unit;*) (***************************debugging*********************)
val cg : Expr list -> string;
val compileSchemeFile : string * string -> unit;
end;

exception NotAConst;
exception ErrorConstTable;
exception illigalConst;
exception illigalExpr;

structure CodeGen : CODE_GEN =
struct
(* Invariant: Mem[linked_list] is the address for the head of the Buckets' linked list *)

(* sizes of scheme types in memory *)	
val S_VOID = 1 ;
val S_NIL = 1 ; 		
val S_BOOL = 2 ; 		
val S_CHAR = 2 ; 		
val S_INTEGER = 2 ;	
(* n is the size of the string *)
val S_STRING = fn(n:int)=>n+2 ;   	
val S_SYMBOL = 2 ; 	
val S_PAIR = 3 ;
(* n is the number of elements in the vector *) 		
val S_VECTOR =  fn(n:int)=>n+2 ;	
val S_CLOSURE = 3 ; 	
(* the size of a bucket *)
val S_BUCKET = 4 ;
(* the offset of the pointer to next in the linked list *)
val NEXT_OFF = 3 ; 

val linked_list = 9;
(* NOTE: index 7 is reserved for gensym's counter *)
val constTable = ref [(Nil, 1), (Void, 2), (Bool(false), 3), (Bool(true), 5), (Number(1), 7)];
(*
val primitives = [Symbol("apply"), Symbol("eq?"), Symbol("gensym"), 
					Symbol("make-string"), Symbol("make-vector"), 
					Symbol("remainder"), Symbol("string->symbol"), 
					Symbol("symbol->string")] ;

val primLabels = ["APPLY", "IS_EQ", "GENSYM", "MAKE_STRING", "MAKE_VECTOR",
					"REMAINDER", "STRING_TO_SYMBOL", "SYMBOL_TO_STRING"];
*)

val primitives = [Symbol("bin+"), Symbol("bin-"), Symbol("bin*"), Symbol("bin/"), Symbol("bin=?"), 
					Symbol("bin<?"), Symbol("bin>?"), Symbol("bin<=?"), Symbol("bin>=?"), Symbol("apply"),
					Symbol("boolean?"), Symbol("car"), Symbol("cdr"), Symbol("char->integer"), Symbol("char?"), 
					Symbol("cons"), Symbol("eq?"), Symbol("gensym"), Symbol("integer?"), Symbol("integer->char"), 
					Symbol("make-string"), Symbol("make-vector"), Symbol("null?"), Symbol("number?"), Symbol("pair?"), 
					Symbol("procedure?"), Symbol("remainder"), Symbol("set-car!"), Symbol("set-cdr!"), Symbol("string->symbol"), 
					Symbol("string-length"), Symbol("string-ref"),Symbol("string-set!"), Symbol("string?"), Symbol("symbol?"), 
					Symbol("symbol->string"), Symbol("vector-length"), Symbol("vector-ref"),Symbol("vector-set!"), Symbol("vector?"), 
					Symbol("zero?"), Symbol("=")]	

val primLabels = ["PLUS", "MINUS", "MULL", "DIVV", "EQNUMBERS", 
					"LOWTHEN", "HIGHTHEN", "LOWTHENEQ", "HIGHTHENEQ", "APPLY", 
					"IS_BOOL", "CAR", "CDR", "CHAR_TO_INTEGER", "IS_CHAR", 
					"CONS", "IS_EQ", "GENSYM", "IS_INT", "INTEGERTOCHAR", 
					"MAKE_STRING", "MAKE_VECTOR", "IS_NULL", "IS_NUM", "IS_PAIR", 
					"IS_PROCEDURE", "REMAINDER", "SETCAR", "SET_CDR", "STRING_TO_SYMBOL", 
					"STRING_LENGTH", "STRING_REF", "STRING_SET", "IS_STRING", "IS_SYM", 
					"SYMBOL_TO_STRING", "VECTOR_LENGTH", "VECTOR_REF", "VECTOR_SET", "IS_VECTOR", 
					"MYISZERO", "EQNUMBERS"]
					

(* the next available place on main memory *)
val count = ref 10;

(* receive Sexpr and constTable and return the index of the 
	Sexpr in the memory if it was found, else return -1 *)		
fun isInConstsTable(a, []) = ~1
  | isInConstsTable(e, (sexpr, index)::tail) = 
	if (e = sexpr) andalso (index <>  7) then index
	else isInConstsTable(e, tail)
(* receive Sexpr list and int list and return int list,
	Sexprs' indices in constTable *) 
and getSubConsts([], adds) = adds
	| getSubConsts(h::t, adds) = 
		let
			val nh = getConst(h)	
		in
			getSubConsts(t, adds @ [isInConstsTable(nh, !constTable)])
		end
		
(* receive Expr and Sexpr*int list and return 
	index and a new Sexpr*int list *)
and getConst(e as Bool(_)) = e (* already in consts *)
	| getConst(e as Void) = e (* already in consts *)	
	| getConst(e as Nil) = e (* already in consts *)
	| getConst(e as Number(a)) = 
		let 
			val i = isInConstsTable(e, !constTable)
		in
			if (i = ~1) then 
				let
					val index = !count
				in
					(count := index+S_INTEGER;
					constTable := !constTable @ [(e, index)];
					e)
				end
			else 
				e
		end
		
	| getConst(e as Char(a)) =
		let
			val i = isInConstsTable(e, !constTable)
		in
			if (i = ~1) then 
				let
					val index = !count
				in
					(count := index+S_CHAR;
					constTable := !constTable @ [(e, index)];
					e)
				end
			else 
				e
		end
	| getConst(e as String(a)) =
		let 
			val i = isInConstsTable(e, !constTable)
		in
			if (i = ~1) then 
				let
					val index = !count
				in
					(count := index+S_STRING(size(a));
					constTable := !constTable @ [(e, index)];
					e)
				end
			else 
				e
		end

	| getConst(e as Symbol(a)) =
		let
			val i = isInConstsTable(e, !constTable)
		in
			if (i = ~1) then 
				let
					val index = !count
				in
					(count := index+S_SYMBOL;
					constTable := !constTable @ [(e, index)];
					e)
				end
			else 
				e	
		end		
		
	| getConst(e as Vector(a)) =
		let 
			val indices = getSubConsts(a,[]) 
			(* if e is in consts then all his sub-consts 
				are in consts and consts = nconsts *)
		in	let 
				val nvec = Vector(map (fn(num)=>Number(num))indices)
			in
				let 
					val i = isInConstsTable(nvec, !constTable)
				in
					if 	(i = ~1) then
						(* e not in nconsts *)
						let
							val index = !count
						in
							(count := index+S_VECTOR(length(indices));
							constTable := !constTable @ [(nvec, index)];
							nvec)
						end
					else 
						nvec
				end
			end
		end
	
	| getConst(e as Pair(car,cdr)) = 
		let 
			val lst = getSubConsts([car, cdr], []) (* length(lst) = 2 *)
		in	let
				val npair = Pair(Number(List.nth(lst,0)), Number(List.nth(lst,1)))
			in
				let
					val ipair = isInConstsTable(npair, !constTable)
				in
					if (ipair = ~1) then 
						(* e not in nconsts *)
						let 
							val index = !count
						in 
							(count := index+S_PAIR;
							constTable := !constTable @ [(npair, index)];
							npair)
						end 
					else
						npair
				end
			end
		end
	
and getConstsSeq([]) = []
	| getConstsSeq(h::t) = getConsts(h)::getConstsSeq(t)
	
	(* receive Expr and return new Expr *)
and getConsts(Const(e)) = 
		Const(getConst(e))
	| getConsts(e as VarFree(str)) = 
		(getConst(Symbol(str)); e)
	| getConsts(If(test, dit, dif)) = 
		let 
			val ntest = getConsts(test)
			val ndit = getConsts(dit)
			val ndif = getConsts(dif)
		in
			If(ntest, ndit, ndif)
		end
	| getConsts(Seq(lst)) = Seq(getConstsSeq(lst))
	| getConsts(Abs(vars, body)) = Abs(vars, getConsts(body))
	| getConsts(AbsOpt(vars,opt,body)) = AbsOpt(vars,opt,getConsts(body))
	| getConsts(AbsVar(var,body)) = AbsVar(var,getConsts(body))
	| getConsts(App(oprt, oprnd)) = App(getConsts(oprt),getConstsSeq(oprnd))
	| getConsts(AppTP(oprt, oprnd)) = AppTP(getConsts(oprt), getConstsSeq(oprnd))
	| getConsts(Or(expr)) = Or(getConstsSeq(expr))
	| getConsts(Set(var, expr)) = Set(getConsts(var), getConsts(expr))
	| getConsts(Def(var, expr)) = Def(getConsts(var), getConsts(expr))
	| getConsts(expr) = expr (* for VarParam and VarBound *)

(* receive Expr list, return new Expr list*)	
and constIter([], exprs) = exprs
	| constIter(h::t, exprs) = constIter(t, exprs@[getConsts(h)]) 

(* receives Expr list, return new Expr list *)
and makeConstTable(exprs) = 
		((map (fn(const)=>getConst(const))primitives); (* adds primitives to constTable *)
		constIter(exprs, []))
																								
(* create initialization code! *)
and initCode([]) = "\n"
	| initCode((Void, index)::rest) =
		initCode(rest) (* already initialyzed *)
		
	| initCode((Nil, index)::rest) = 
		initCode(rest) (* already initialyzed *)
		
	| initCode((Bool false, index)::rest) = 
		initCode(rest) (* already initialyzed *)
		
	| initCode((Bool true, index)::rest) = 
		initCode(rest) (* already initialyzed *)
		
	| initCode((Char c, index)::rest) = 
		(*"printf(\"Create SOB Char In Address "^Int.toString(index)^"\\n\");\n"*)
		"\tPUSH(IMM('"^Char.toString(c)^"'));\n"
		^"\tCALL(MAKE_SOB_CHAR);\n"
		^"\tDROP(1);\n"
		^initCode(rest)
		
	| initCode((Number n, index)::rest) = 
		let
			val str = if (n < 0) then 
						"-"^Int.toString(0-n) 
						else Int.toString(n)
		in 
			(*"printf(\"Create SOB "^sexprToString(Number n)^" In Address "^Int.toString(index)^"\\n\");\n"*)
			"\tPUSH(IMM("^str^"));\n"
			^"\tCALL(MAKE_SOB_INTEGER);\n"
			^"\tDROP(1);\n"
			^initCode(rest)
		end
		
	| initCode((String str, index)::rest) = 
		(*"printf(\"Create SOB "^sexprToString(String str)^" In Address "^Int.toString(index)^"\\n\");\n"*)
		stringInitCode(String.explode(str), size(str))
		^initCode(rest)
		
	| initCode((Vector v, index)::rest) =
		(* v contains pointers to the vector's components *)
		(*"printf(\"Create SOB "^sexprToString(Vector v)^" In Address "^Int.toString(index)^"\\n\");\n"*)
		vectorInitCode(v, length(v)) 
		^initCode(rest)
		
	| initCode((Pair(Number car, Number cdr), index)::rest) = 
		(* car and cdr are pointers to pair's elements *)
		(*"printf(\"Create SOB "^sexprToString(Pair(Number car, Number cdr))^" In Address "^Int.toString(index)^"\\n\");\n"*)
		"\tPUSH(IMM("^Int.toString(cdr)^"));\n"
		^"\tPUSH(IMM("^Int.toString(car)^"));\n"
		^"\tCALL(MAKE_SOB_PAIR);\n"
		^"\tDROP(2);\n"
		^initCode(rest)
		
	| initCode((Symbol sym, index)::rest) = 
		(*"printf(\"Create SOB "^sexprToString(Symbol sym)^" In Address "^Int.toString(index)^"\\n\");\n"*)
		"\t/* pushing pointer to bucket which we don't have yet*/\n"
		^"\tPUSH(IMM(0));\n"
		^"\tCALL(MAKE_SOB_SYMBOL);\n"
		^"\tDROP(1);\n"
		^initCode(rest)
	
	| initCode(_) = raise illigalConst
		
		
and vectorInitCode([], n) = 
		"\tPUSH(IMM("^Int.toString(n)^"));\n"
		^"\tCALL(MAKE_SOB_VECTOR);\n"
		^"\tDROP("^Int.toString(n+1)^");\n"
	| vectorInitCode(Number(h)::t, n) = 
		"\tPUSH(IMM("^Int.toString(h)^"));\n"
		^vectorInitCode(t, n)
	| vectorInitCode(_,_) = raise ErrorConstTable
		
and stringInitCode([], n) = 
		"\tPUSH(IMM("^Int.toString(n)^"));\n"
		^"\tCALL(MAKE_SOB_STRING);\n"
		^"\tDROP("^Int.toString(n+1)^");\n"
	| stringInitCode(#"'"::t, n) = 
		"\tPUSH(IMM('\\''));\n"
		^stringInitCode(t, n)
	| stringInitCode(h::t, n) = 
		"\tPUSH(IMM('"^Char.toString(h)^"'));\n"
		^stringInitCode(t, n)
		
(* now go through all symbols and create buckets *)
(* mem[2] is a pointer to head *)
(* every bucket contain 4 words, the last for the next bucket *)

and makeBuckets([]) = "\n"
	| makeBuckets((Symbol(str), index)::rest) = 
		"\t/* allocate memory for bucket + next pointer */\n"
		^"\t/* bucket for Symbol "^str^" in address "^Int.toString(index)^" */\n"
		^"\tPUSH(IMM("^Int.toString(S_BUCKET)^"));\n"
		^"\tCALL(MALLOC);\n"	(* R0 is the address to the new bucket *)
		^"\tDROP(1);\n"
		^"\tMOV(R1, IND("^Int.toString(linked_list)^"));\n"		(* R1 is the address of head *)
		^"\tMOV(INDD(R0,"^Int.toString(NEXT_OFF)^"), R1);\n"  (* head is turnning to next *)
		^"\t/* make new bucket the head */\n"
		^"\tMOV(IND("^Int.toString(linked_list)^"), R0);\n"
		^"\tMOV(R1, R0);\n"
		^"\t/* now R1 has the address of the bucket */\n"
		^"\t/* allocate memory for symbol's name */\n"
		^ stringInitCode(String.explode(str), size(str)) 
		^"\t/* R0 is a pointer to the string */\n"
		^"\tMOV(IND(R1), R0);\n"
		^"\t/* set bucket to undefined */\n"
		^"\tMOV(INDD(R1, 1), IMM(0));\n"
		^"\tMOV(INDD(R1, 2), IMM(0));\n"
		^"\t/* insert bucket's address to symbol object */\n"
		^"\tMOV(INDD("^Int.toString(index)^", 1), R1);\n"
		^ makeBuckets(rest)
	| makeBuckets(head::rest) = makeBuckets(rest)

and makeInitCode() = 
		"\t/* start initialization */\n"
		(*^"printf(\"Create SOB "^sexprToString(Nil)^" In Address 1\\n\");\n"*)
		^"\tCALL(MAKE_SOB_NIL);\n"
		(*^"printf(\"Create SOB "^sexprToString(Void)^" In Address 2\\n\");\n"*)
		^"\tCALL(MAKE_SOB_VOID);\n"
		(*^"printf(\"Create SOB "^sexprToString(Bool false)^" In Address 3\\n\");\n"*)
		^"\tPUSH(IMM(0));\n"
		^"\tCALL(MAKE_SOB_BOOL);\n"
		^"\tDROP(1);\n"
		(*^"printf(\"Create SOB "^sexprToString(Bool true)^" In Address 5\\n\");\n"*)
		^"\tPUSH(IMM(1));\n"
		^"\tCALL(MAKE_SOB_BOOL);\n"
		^"\tDROP(1);\n"
		(*^"printf(\"Create SOB "^sexprToString(Number 1)^" In Address 7 FOR GENSYM\\n\");\n"*)
		^"\tPUSH(IMM(1));\n"
		^"\tCALL(MAKE_SOB_INTEGER);\n"
		^"\tDROP(1);\n"
		^"\t/* allocate memory for head of bucket list */\n"
		^"\tPUSH(IMM(1));\n"
		^"\tCALL(MALLOC);\n"
		^"\tDROP(1);\n"
		^"\tMOV(IND(R0), IMM(0));\n"
		^"\tPUSH(R1);\n"
		^ initCode(List.drop(!constTable,5))
		^ makeBuckets(!constTable)
		^"\tPOP(R1);\n"
	
		
(* for 	debgging symbol table *)

and sexprToString(Void) = "Void"
	| sexprToString(Nil) = "Nil"
	| sexprToString(Bool(true)) = "Bool true"
	| sexprToString(Bool(false)) = "Bool false"
	| sexprToString(Number(n)) = "Number "^Int.toString(n)
	| sexprToString(Char(c)) = "Char "^Char.toString(c)
	| sexprToString(String(s)) = "String "^s
	| sexprToString(Pair(a,b)) = "Pair("^sexprToString(a)^", "^sexprToString(b)^")"
	| sexprToString(Vector(l)) = "Vector("^String.concat(map (fn(se)=>sexprToString(se)^" ")l)^")"
	| sexprToString(Symbol(s)) = "Symbol "^s
	
(*
Const of Sexpr
	      | Var of string
	      | VarFree of string               (* free variable *)
	      | VarParam of string * int   (* parameter variable *)
	      | VarBound of string * int * int (* bound variable *)
	      | If of Expr * Expr * Expr
	      | Abs of (string list) * Expr
	      | AbsOpt of (string list) * string * Expr
	      | AbsVar of string * Expr
	      | App of Expr * (Expr list)
	      | AppTP of Expr * (Expr list)  (* in tail position *)
	      | Seq of Expr list
	      | Or of Expr list
	      | Set of Expr * Expr
	      | Def of Expr * Expr;
		  *)

and exprToString(Const(sexpr)) = "Const(" ^ sexprToString(sexpr) ^ ")"
	| exprToString(Var(x)) = "Var " ^ x
	| exprToString(VarFree(x)) = "VarFree " ^ x 
	| exprToString(VarParam(x, mi)) = "VarParam(" ^ x ^", " ^ Int.toString(mi) ^ ")"
	| exprToString(VarBound(x, mi, ma)) = "VarBound(" ^ x ^ ", " ^ Int.toString(mi) ^ ", " ^ Int.toString(ma) ^ ")" 
	| exprToString(If(test, dit, dif)) = "If(" ^ exprToString(test) ^ ", " ^ exprToString(dit) ^ ", " ^ exprToString(dif) ^ ")"
	| exprToString(Set(var, expr)) = "Set(" ^ exprToString(var) ^ ", " ^ exprToString(expr) ^ ")" 
	| exprToString(Def(var, expr)) = "Def(" ^ exprToString(var) ^ ", " ^ exprToString(expr) ^ ")"
	| exprToString(Seq(lst)) = "Seq(" ^ String.concat(map exprToString(lst)) ^ ")"
	| exprToString(Or(lst)) = "Or(" ^ String.concat(map exprToString(lst)) ^ ")"
	| exprToString(Abs(vars, body)) = "Abs(" ^ String.concat(map (fn(var)=> var ^", ") vars) ^ exprToString(body) ^ ")"
	| exprToString(AbsOpt(vars, opt, body)) = "AbsOpt(" ^ String.concat(map (fn(var)=> var ^", ") vars) ^ ", " ^ opt ^ "," ^ exprToString(body) ^ ")"
	| exprToString(AbsVar(var, body)) = "AbsVar(" ^ var ^ ", " ^ exprToString(body) ^ ")"
	| exprToString(App(oper, rnds)) = "App(" ^ exprToString(oper) ^ ", " ^ String.concat(map (fn(rnd)=>exprToString(rnd) ^ ",") rnds) ^ ")"
	| exprToString(AppTP(oper, rnds)) = "AppTP(" ^ exprToString(oper) ^ ", " ^ String.concat(map (fn(rnd)=>exprToString(rnd) ^ ",") rnds) ^ ")"
	
and printConsts([]) = "\n"
	| printConsts((sexpr, index)::rest) =
		(*"printf(\"Printing The Constant: "^sexprToString(sexpr)^" In Address "^Int.toString(index)^"\\n\");\n"*)
		"\tPUSH("^Int.toString(index)^");\n"
		^"\tCALL(WRITE_SOB);\n"
		^"\tDROP(1);\n"
		^"\tCALL(NEWLINE);\n"
		^ printConsts(rest)

and printSymbolTable() =
		"\t/* Printing Symbol Table */\n"
		^"\tPUSH(R1);\n"
		^"\tMOV(R1, IND("^Int.toString(linked_list)^"));\n"		(* R1 is pointer to head bucket *)
		^"MY_PRINTER:\n"
		^"\tCMP(R1, IMM(0));\n"
		^"\tJUMP_EQ(MY_PRINTER_END);\n"
		^"\t/* print string */\n"
		^"\tPUSH(IND(R1));\n"
		^"\tCALL(WRITE_SOB);\n"
		^"\tDROP(1);\n"
		^"\tCALL(NEWLINE);\n"
		^"\tMOV(R1, INDD(R1,"^Int.toString(NEXT_OFF)^"));\n"
		^"\tJUMP(MY_PRINTER);\n"
		^"MY_PRINTER_END:\n"
		^"\tPOP(R1);\n"
		^"\t/*Done Printing Symbol Table */\n"
		
	
and runCG (Const(e),env,params,max_major) =
		let 
			val address = isInConstsTable(e, !constTable)
		in
			"\tMOV(R0,IMM("^Int.toString(address)^"));\n"
		end
	
	|runCG (VarFree(str),env,params,max_major)=
		let
			val address = isInConstsTable(Symbol(str), !constTable) (* address <> ~1 *)
			val gen = myGensym()
		in
			"/* GENERATE CODE FOR VARFREE " ^ str ^ "*/\n"
			^"\t/* Get Symbol "^str^" In Symbol Table Through T_SYMBOL Object */\n"
			^"\tMOV(R0, IMM("^Int.toString(address)^"));\n"
			^"\tMOV(R0, INDD(R0, 1));\n" (* R0 is the address of the bucket *)
			^"\t/* Check If Defined */\n"
			^"\tCMP(INDD(R0, 1), IMM(1));\n"
			^"\tJUMP_EQ(LABEL_DEFINED"^gen^");\n"
			^"\tHALT;\n"
			^"LABEL_DEFINED"^gen^":\n"
			^"\tMOV(R0, INDD(R0, 2));\n" (* R0 is address of the value *)
			^"\t/* R0 Is Address Of The Value */\n"
		end
		
	|runCG (e as VarParam(x,mi),env,params,max_major)=
		"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
		^"/* "^x^"=varparam("^itoa(mi)^") */ \n"
		^"  MOV(R0,FPARG(2+"^itoa(mi)^")); \n"
														
	|runCG (e as VarBound(x,ma,mi),env,params,max_major)=		
		"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
		^"/* "^x^"=varbound["^itoa(ma)^"]["^itoa(mi)^"] */ \n"
		^"  MOV(R0,FPARG(0));/*FPARG(0) is the env */\n"
		^"  MOV(R0,INDD(R0,"^itoa(ma)^"));/*R0<-R0[ma]*/\n"
		^"  MOV(R0,INDD(R0,"^itoa(mi)^"));/*R0<-R0[mi] end overall we do R0<-env[ma][mi]*/\n"
								
	| runCG (e as If(test ,dit,dif),env,params,max_major) = 
		let
			val n=myGensym()				
		in
			"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
			^runCG(test,env,params,max_major)^"  CMP(R0,IMM(3));\n"
			^"  JUMP_EQ(Lelseif"^n^");\n"
			^runCG(dit,env,params,max_major)^"\n"
			^"  JUMP(Lexitif"^n^");\n"
			^"Lelseif"^n^":\n"
			^runCG(dif,env,params,max_major)
			^"\nLexitif"^n^":"
		end
   
   |runCG (e as Seq exprList,env,params,max_major)= 
		"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
		^ cgSeq(exprList,env,params,max_major)
		
   |runCG (e as Or(exprList),env,params,max_major) = 
		let
			val n=myGensym()
		in 
			"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
			^ cgOR(exprList,n,env,params,max_major)
		end
															
	|runCG (e as Set(VarParam(x,mi),expr),env,params,max_major) = 	
		"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
		^ "/* GENERATE EXPR CODE BEFORE SET VARPARAM " ^ x ^ " */\n" 
		^ runCG(expr,env,params,max_major)
		^ "/* DONE GENERATE EXPR CODE BEFORE SET VARPARAM " ^ x ^ " */\n" 
		^ "/*put the R0=expr content in x that is the stack*/\n"
		^ "  MOV(FPARG(2+"^itoa(mi)^"),R0);"
		^ "  MOV(R0, IMM(SOB_VOID));\n" (* return void *)
																				   
	|runCG (e as Set(VarBound(x,ma,mi),expr),env,params,max_major)=
		"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
		^ runCG(expr,env,params,max_major)  
		^ "  MOV(R1,FPARG(0));/*FPARG(0) is the env */\n"
		^"  MOV(R1,INDD(R1,"^itoa(ma)^"));/*R1<-R1[ma]*/\n"
		^"  MOV(INDD(R1,"^itoa(mi)^",R0));/*R1[mi]<-R0=[|expr|] end overall we do env[ma][mi]<-[|expr|] and R0=void*/\n"
		^"  MOV(R0, IMM("^Int.toString(isInConstsTable(Void, !constTable))^"));\n" (* return void *)
	   
	|runCG (e as Set(VarFree(str), expr), env, params, max_major) = 
		let
			val gen = myGensym()
		in
			"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" ^
			runCG(expr, env, params, max_major) ^
			"\t/* R0 is value of expr */\n" ^
			"\tMOV(R1, IMM("^Int.toString(isInConstsTable(Symbol str, !constTable))^"));\n" ^
			"\t/* R1 is address of Symbol "^str^" */\n" ^
			"\tMOV(R1, INDD(R1, 1));\n" ^
			"\t/* R1 is address of bucket */\n" ^
			"\t/* check defined */\n" ^
			"\tCMP(INDD(R1, 1), IMM(1));\n" ^
			"\tJUMP_EQ(VARFREE_DEFINED"^gen^");\n" ^
			"\tHALT;\n"^
			"\tVARFREE_DEFINED"^gen^":\n"^
			"\t/* set to expr that in R0 */\n" ^
			"\tMOV(INDD(R1, 2), R0);\n" ^
			"\t/* return void */\n" ^
			"\tMOV(R0, IMM(2));\n" 
		end	
		
	|runCG (e as Def(VarFree(str), expr), env, params, max_major) =
		"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" ^
		runCG(expr, env, params, max_major) ^ 
		"\t/* R0 is value of expr */\n" ^
		"\tMOV(R1, IMM("^Int.toString(isInConstsTable(Symbol str, !constTable))^"));\n" ^
	    "\t/* R1 is address of Symbol "^str^" */\n" ^
		"\tMOV(R1, INDD(R1, 1));\n" ^
		"\t/* R1 is address of bucket */\n" ^
		"\t/* set defined */\n" ^
		"\tMOV(INDD(R1, 1), IMM(1));\n" ^
		"\t/* set address of value */\n" ^
		"\tMOV(INDD(R1, 2), R0);\n" ^
		"\t/* return void - address 2 in mem */\n" ^
		"\tMOV(R0, IMM(2));\n" ^
		(*"\tprintf(\""^str^"\\n\");\n" ^*)
		"\t/* done code for define */\n" 
	|runCG (e as App(proc,args),env,params,max_major) = 
											   "/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
											   ^cgAPP(rev(args),env,params,max_major)
											   ^"  PUSH(IMM("^itoa(length args)^"));\n"
											   ^runCG(proc,env,params,max_major)^"\n"
											   ^"  CMP(INDD(R0,0),IMM(T_CLOSURE));\n"
									(* ^"  SHOW(\"try to print T_CLOS\",INDD(R0,0));\n"*)
											   ^"  JUMP_NE(L_ERRORE_not_procedure);\n"
											   ^"  PUSH(INDD(R0,1));/*PUSH THE ENV OF THE CLOSURE*/\n"
														(*^" SHOW(\"BEFORE CALLA CALLA IS\",INDD(R0,2));\n"*)														
											   ^"  CALLA(INDD(R0,2)); /*call address to label of the lambda body*/ \n"
													(*^"SHOW(\"AFTER CALLA \",INDD(R0,2));\n"*)
											   ^"  DROP(2+STARG(0));\n"	   
											   
	(*|runCG (AppTP(proc,args)::tail,env,params,max_major)= runCG(App(proc,args)::tail,env,params,max_major) (*need to change ofcorse*)*)
	|runCG (e as AppTP(proc,args),env,params,max_major)=  let
																val gen = myGensym()
																val argsLength= itoa(length args)
																val paramLength= itoa(length params)
														  in
															"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n"  
															^ cgAPP(rev(args),env,params,max_major)
															^"  PUSH(IMM("^argsLength^"));\n"
															^"/* GENERATE CODE FOR PROC IN APPTP */\n" 
															^runCG(proc,env,params,max_major)^"\n"
															^"  CMP(INDD(R0,0),IMM(T_CLOSURE));\n"			
											  (* ^"  SHOW(\"try to print T_CLOS\",INDD(R0,0));\n"*)
															
															^"  JUMP_NE(L_ERRORE_not_procedure);\n"
															^"  PUSH(INDD(R0,1));/*PUSH THE ENV OF THE CLOSURE*/\n"
															^"  CMP(FPARG(-1), LABEL(END_LABEL));\n"
															^"  JUMP_EQ(DONT_RUNOVER"^gen^");\n"
															^"  PUSH(FPARG(-1));/*PUSH OLD RET VAL*/\n"
															^"  PUSH(FPARG(-2));/*PUSH OLD FRAME POINTER*/\n"
															^"/* CALLING FOR FROM APPTP */\n"
															^cgFOR("0",itoa ((length args)+4),"1","MOV(FPARG("^paramLength^"+1-R10),FPARG(-3-R10))")	
															^"  MOV(SP,FP);\n"
															^"  SUB(SP,IMM("^paramLength^"));\n"											
															^"  ADD(SP,IMM("^argsLength^"));\n"
															^"  POP(FP);\n"
															^"  JUMPA(INDD(R0,2));\n"
															^"DONT_RUNOVER"^gen^":\n"
															^"  CALLA(INDD(R0, 2));\n"
															^"  DROP(FPARG(1));\n"
															^"  DROP(2);\n" (* drop env and n *)
														end
	
															
								
	|runCG (e as Abs(vars,body),env,params,max_major) = 
											let
												val gen = myGensym()
											in	
												"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
												^"  MOV(R1,FPARG(0));\n"
												^"  PUSH (IMM("^itoa((length env)+1)^"));\n"
											    ^"  CALL(MALLOC);\n"
												^"  DROP(1);\n"
												^"  MOV(R2,R0);\n"
												^"  MOV(R3,R2);\n"
												(*^"  MOV(R8,IMM(1));\n"*)
												^"/* CALLING FOR FROM ABS 1 */\n"
												^cgFOR("0",itoa(length env),"1","MOV(INDD(R3,(R10+1)),INDD(R1,R10))\n  /*INCR(R8)*/;")
												^"  PUSH (IMM("^itoa(length params)^"));\n"													
											    ^"  CALL(MALLOC);\n"
												^"  DROP(1);\n"													
												^"  MOV(INDD(R2,0),R0);\n"
												^"  MOV(R3,INDD(R2,0));\n"
												^"/* CALLING FOR FROM ABS 2 */\n"
												^cgFOR("0",itoa (length params),"1","MOV(INDD(R3,R10),FPARG(2+R10))")(*HERE*)												
												^"  CALL(MAKE_SOB_CLOSURE);\n"												
												^"  MOV(INDD(R0,1),R2);\n"												
												^"  JUMP(CLOSURE_L"^gen^");\n"
												^"CLOSURE_BODY_L"^gen^":\n"
												^"  PUSH(FP);\n"
												^"  MOV(FP,SP);\n"
												^"  CMP(FPARG(1),IMM("^itoa(length vars)^"));\n"
												^"  JUMP_EQ(CLOSURE_ARGS_OK_L"^gen^");\n"
												^"  SHOW(\"args dont fit to this procedure\",R0);\n"												
												^"  HALT;\n"
												^"CLOSURE_ARGS_OK_L"^gen^":\n"
												^"/* GENERATE CODE FOR ABS BODY */\n"
												^runCG(body,params::env,vars,max_major+1)
												^"  MOV(SP,FP);\n"
												^"  POP(FP);\n"
												^"  RETURN;\n"
												^"CLOSURE_L"^gen^":\n"
												^"  MOV(INDD(R0,2),LABEL(CLOSURE_BODY_L"^gen^"));\n"
												^"/* DONE CODE FOR VAR */\n"
											end
						
		|runCG (e as AbsOpt(vars,opt,body),env,params,max_major)=
		let
																val gen = myGensym()
																val varsLength= itoa(length vars)
																val paramLength= itoa(length params)
																val varsPlusOptLength = itoa( length(vars)+1)

															in	
																"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n" 
																^"  MOV(R1,FPARG(0));\n"
																^"  PUSH (IMM("^itoa((length env)+1)^"));\n"
																^"  CALL(MALLOC);\n"
																^"  DROP(1);\n"
																^"  MOV(R2,R0);\n"
																^"  MOV(R3,R2);\n"
																^"/* CALLING FOR FROM ABSOPT 1 */\n"
																^cgFOR("0",itoa(length env),"1","MOV(INDD(R3,(R10+1)),INDD(R1,R10))\n  /*INCR(R8)*/;")
																^"  PUSH (IMM("^itoa(length params)^"));\n"													
																^"  CALL(MALLOC);\n"
																^"  DROP(1);\n"													
																^"  MOV(INDD(R2,0),R0);\n"
																^"  MOV(R3,INDD(R2,0));\n"
																^"/* CALLING FOR FROM ABSOPT 2 */\n"
																^cgFOR("0",itoa (length params),"1","MOV(INDD(R3,R10),FPARG(2+R10))")(*HERE*)												
																^"  CALL(MAKE_SOB_CLOSURE);\n"												
																^"  MOV(INDD(R0,1),R2);\n"												
																^"  JUMP(CLOSURE_L"^gen^");\n"
																^"CLOSURE_BODY_L"^gen^":\n"																
																^"  PUSH(FP);\n"
																^"  MOV(FP, SP);\n "																
																^"/* stack correcting */\n"											
																^"  MOV(R6,SOB_NIL);\n"
																^"/* CALLING FOR FROM ABSOPT 3 */\n"
							(*UNTIL HERE AS ABS SIMPLE*)		^cgFOR("0","FPARG(1)-"^varsLength,"1","PUSH(R6);\n  PUSH(FPARG(FPARG(1)+1-R10));\n  CALL(MAKE_SOB_PAIR);\n  DROP(2);\n  MOV(R6,R0)")														
																^"  MOV(FPARG(2+"^varsLength^"),R6);\n"
																
																^"  PUSH(R6);\n"
																^"  MOV(R3,IMM("^varsPlusOptLength^"));\n\n"
																^"LOOP_L"^gen^":\n"
																^"  CMP(R3,IMM(1));\n"
																^"  JUMP_EQ(END_LOOP_L"^gen^");\n"
				(*BUILDIN "NEW" FRAME AND COPY IT DOWN*)		^"  PUSH(FPARG(R3));\n"
																^"  DECR(R3);\n"
																^"  JUMP(LOOP_L"^gen^");\n\n"
																^"END_LOOP_L"^gen^":\n"
																^"  PUSH(IMM("^varsPlusOptLength^")); \n"
																^"  PUSH(FPARG(0)); // ENV\n"
																^"  PUSH(FPARG(-1)); //RET VAL\n"
																^"  PUSH(FPARG(-2)); //OLD FP\n"
																^"  MOV(R0, FPARG(1));\n"
																^"/* CALLING FOR FROM ABSOPT 4 */\n"
																^cgFOR("0",itoa ((length vars)+4+1),"1","MOV(FPARG(R0+1-R10),FPARG(-3-R10))")																																								
							(*	COPING THE FARME DOWN *)		^" MOV(SP,FP);\n"
																^"  SUB(SP,R0);\n"											
																^"  ADD(SP,IMM("^varsPlusOptLength^"));\n"
																^" MOV(FP,SP);\n"
												
								(*CONTINU AS ABS SIMPLE*)		^"  CMP(FPARG(1),IMM("^varsPlusOptLength^"));\n"
																^"  JUMP_EQ(CLOSURE_ARGS_OK_L"^gen^");\n"
																^"  SHOW(\"args dont fit to this procedure\",R0);\n"
																^"  HALT;\n"
																^"CLOSURE_ARGS_OK_L"^gen^":\n"
																^"/* GENERATE CODE FOR ABSOPT BODY */\n"
																^runCG(body,params::env,vars@[opt],max_major+1)
																^"  MOV(SP,FP);\n"
																^"  POP(FP);\n"
																^"  RETURN;\n"
																^"CLOSURE_L"^gen^":\n"
																^"  MOV(INDD(R0,2),LABEL(CLOSURE_BODY_L"^gen^"));\n"
																^"/* DONE CODE FOR ABSOPT */\n"
															end
|runCG (e as AbsVar(var,body),env,params,max_major)=let
																val gen = myGensym()																
																val paramLength= itoa(length params)

															in	
																"/* GENERATE CODE FOR " ^ exprToString(e) ^ " */\n"  
																^"  MOV(R1,FPARG(0));\n"
																^"  PUSH (IMM("^itoa((length env)+1)^"));\n"
																^"  CALL(MALLOC);\n"
																^"  DROP(1);\n"
																^"  MOV(R2,R0);\n"
																^"  MOV(R3,R2);\n"
																^"/* CALLING FOR FROM ABSVAR 1 */\n"
																^cgFOR("0",itoa(length env),"1","MOV(INDD(R3,(R10+1)),INDD(R1,R10))\n  /*INCR(R8)*/;")
																^"  PUSH (IMM("^itoa(length params)^"));\n"													
																^"  CALL(MALLOC);\n"
																^"  DROP(1);\n"													
																^"  MOV(INDD(R2,0),R0);\n"
																^"  MOV(R3,INDD(R2,0));\n"
																^"/* CALLING FOR FROM ABSVAR 2 */\n"
																^cgFOR("0",itoa (length params),"1","MOV(INDD(R3,R10),FPARG(2+R10))")(*HERE*)												
																^"  CALL(MAKE_SOB_CLOSURE);\n"												
																^"  MOV(INDD(R0,1),R2);\n"												
																^"  JUMP(CLOSURE_L"^gen^");\n"
																^"CLOSURE_BODY_L"^gen^":\n"																
																^"  PUSH(FP);\n"
																^"  MOV(FP, SP);\n "																
																^"/* stack correcting */\n"											
																^"  MOV(R6,SOB_NIL);\n"
																^"/* CALLING FOR FROM ABSVAR 3 */\n"
							(*UNTIL HERE AS ABS SIMPLE*)		^cgFOR("0","FPARG(1)","1","PUSH(R6);\n  PUSH(FPARG(FPARG(1)+1-R10));\n  CALL(MAKE_SOB_PAIR);\n  DROP(2);\n  MOV(R6,R0)")														
																
																^"  PUSH(R6);\n"
																(*^"  MOV(R3,IMM("^varsPlusOptLength^"));\n\n"
																^"LOOP_L"^gen^":\n"
																^"  CMP(R3,IMM(1));\n"
																^"  JUMP_EQ(END_LOOP_L"^gen^");\n"
				(*BUILDIN "NEW" FRAME AND COPY IT DOWN*)		^"  PUSH(FPARG(R3));\n"
																^"  DECR(R3);\n"
																^"  JUMP(LOOP_L"^gen^");\n\n"
																^"END_LOOP_L"^gen^":\n"*)
																^"  PUSH(IMM(1)); \n"
																^"  PUSH(FPARG(0)); // ENV\n"
																^"  PUSH(FPARG(-1)); //RET VAL\n"
																^"  PUSH(FPARG(-2)); //OLD FP\n"
																^"  MOV(R0, FPARG(1));\n"
																^"/* CALLING FOR FROM ABSVAR 4 */\n"
																^cgFOR("0",itoa (5),"1","MOV(FPARG(R0+1-R10),FPARG(-3-R10))")																																								
							(*	COPING THE FARME DOWN *)		^" MOV(SP,FP);\n"
																^"  SUB(SP,R0);\n"											
																^"  ADD(SP,IMM(1));\n"
																^" MOV(FP,SP);\n"
												
								(*CONTINU AS ABS SIMPLE*)		^"  CMP(FPARG(1),IMM(1));\n"
																^"  JUMP_EQ(CLOSURE_ARGS_OK_L"^gen^");\n"
																^"  SHOW(\"args dont fit to this procedure\",R0);\n"
																^"  HALT;\n"
																^"CLOSURE_ARGS_OK_L"^gen^":\n"
																^"/* GENERATE CODE FOR ABSVAR BODY */\n"
																^runCG(body, params::env, [var], max_major+1)
																^"  MOV(SP,FP);\n"
																^"  POP(FP);\n"
																^"  RETURN;\n"
																^"CLOSURE_L"^gen^":\n"
																^"  MOV(INDD(R0,2),LABEL(CLOSURE_BODY_L"^gen^"));\n"	
																^"/* DONE CODE FOR ABSVAR */\n"			
															end	
|runCG (_,_,_,_) = raise illigalExpr
	
			
	
and cgSeq([], env, params, max_major) = ""
	|cgSeq(x::xs, env, params, max_major) = 
		runCG(x, env, params, max_major) ^
		cgSeq(xs, env, params, max_major)
	
	
	
and cgFOR (from,toF,jump,doit)= 
		let
			val gen=myGensym()
		in
			" /*start FOR"^gen^"*/\n"
			^"  MOV(R10,IMM("^from^"));\n"
			^"  MOV(R11,IMM("^toF^"));\n"	
			^"  MOV(R12,IMM("^jump^"));\n"	
			^"FOR_L"^gen^":\n"
			^"  CMP(R10,R11);\n"
			^"  JUMP_GE(END_FOR_L"^gen^");\n"
			^"  "^doit^";\n"
			^"  ADD(R10,R12);\n"
			^"  JUMP(FOR_L"^gen^");\n"
			^"END_FOR_L"^gen^":\n"
		end
and cgOR (head::[],gens_n,env,params,max_major)= runCG(head,env,params,max_major)^"LexitOR"^gens_n^":"
   |cgOR (head::tail,gens_n,env,params,max_major)= let 			    
								val checkif="  CMP(R0,IMM(3));\n  JUMP_NE(LexitOR"^gens_n^");\n"(*mybe need to add ^" CMP(INDD(R0,1),0);\n"^" JUMP_NE(" ^endOr^ ");\n "*)
							in
								runCG(head,env,params,max_major)^checkif^cgOR(tail,gens_n,env,params,max_major)
							end
    |cgOR (_,_,_,_,_) = raise illigalExpr
    
and cgAPP([],env,params,max_major)="\n"
	|cgAPP(head::[],env,params,max_major)= runCG(head,env,params,max_major)^"  PUSH(R0);\n"
	|cgAPP(head::tail,env,params,max_major)= runCG(head,env,params,max_major)^"  PUSH(R0);\n"^	cgAPP(tail,env,params,max_major)

and runCgIter([], env, params, max_major) = ""
	|runCgIter(head::tail, env, params, max_major) = 
		let
			val gen = myGensym()
		in
			runCG(head, env, params, max_major) ^
			"\t/* print the answer in R0 unleas it is void */\n" ^
			"\tCMP(R0, SOB_VOID);\n" ^
			"\tJUMP_EQ(AFTER_WRITE_SOB" ^ gen ^ ");\n" ^
			"\tPUSH(R0);\n" ^
			"\tCALL(WRITE_SOB);\n" ^
			"\tDROP(1);\n" ^
			"\tCALL(NEWLINE);\n" ^
			"AFTER_WRITE_SOB" ^ gen ^ ":\n" ^
			runCgIter(tail, env, params, max_major)
		end

and initPrimitive(sym, label) = 
	"\t/* Initiate "^sexprToString(sym)^" */\n"
	^"\tPUSH(R1);\n"
	^"\tPUSH(LABEL("^label^"));\n"
	^"\t/*Push Fake Env Pointer */\n"
	^"\tPUSH(IMM(30086709));\n"
	^"\tCALL(MAKE_SOB_CLOSURE);\n"
	^"\tDROP(2);\n"
	^"\t/* Put In R1 Address Of T_SYMBOL Object */\n"
	^"\tMOV(R1, IMM("^Int.toString(isInConstsTable(sym, !constTable))^"));\n"
	^"\tMOV(R1, INDD(R1, 1));\n" (* pointer to bucket *)
	^"\tMOV(INDD(R1, 1), IMM(1));\n" (* defined *) 
	^"\tMOV(INDD(R1, 2), R0);\n"
	^"\tPOP(R1);\n"
	
and initPrims() = 
		String.concat(map2((fn(prim, label)=>initPrimitive(prim, label)),
							primitives, 
							primLabels)) 
							
and cg(exprList)=
	"#include <stdio.h>\n"
	^"#include <stdlib.h>\n"
	^"#include \"cisc.h\"\n"
	^"int main()\n"
	^"{\n"
	^"  START_MACHINE;\n"
	^"  JUMP(CONTINUE);\n"
	^"#include "^"\"char.lib\"\n"
	^"#include "^"\"io.lib\"\n"
	^"#include "^"\"math.lib\"\n"
	^"#include "^"\"string.lib\"\n"
	^"#include "^"\"system.lib\"\n"
	^"#include "^"\"scheme.lib\"\n"
	^"CONTINUE:\n"
	^makeInitCode()
	^initPrims()
	^initStack()
	^runCgIter(exprList,[],[],~1)
	^epilog()
	
	

and epilog()= 	"   JUMP(END_LABEL);\n"
				^"L_ERRORE_not_procedure:\n" 
			    ^"  SHOW(\"R0 is not closure %d\",R0);\n" 
				^"  HALT;\n"
				^"END_LABEL:\n"
				^"  STOP_MACHINE;\n"
				^"return 0;\n}\n" 


and pipeline(str)=
	let
		val tag = TagParser.stringToPEs(str)
	in 
		(map SemanticAnalysis.analysis tag)
	end
	
and initStack() =
				"/*INIT_STACK*/\n"
				^"\tPUSH(IMM(0));\n" (*number of args on the stack*)
				^"\tPUSH(IMM(2));\n"
				^"\tCALL(MALLOC);\n" 
				^"\tDROP(1);\n" 
				^"\tMOV(IND(R0),IMM(0));\n"  
				^"\tMOV(INDD(R0,1),IMM(1));\n" 
				^"\tPUSH(R0);\n" (*the empty env*)
				^"\tPUSH(LABEL(END_LABEL));\n" (*return label*)
				^"\tPUSH(34770271);\n" (*fake frame-pointer*)
				^"\tMOV(FP,SP);\n"
				^"\t/*END INIT_STACK*/\n\n" 

					
and compileSchemeFile(inputScm, outputCISC) = 
	let
		val nScmCode = makeConstTable(pipeline(fileToString("C:/Users/Vered/Desktop/project/support-code.scm")^"\n"^fileToString(inputScm)))
	in
		stringToFile(cg(nScmCode), "C:/Users/Vered/Desktop/project/"^outputCISC)
	end;
												
(*TESTS: CodeGen.compileSchemeFile ("C:\\users\\uriya\\Desktop\\comp\\a.scm","makore");*)												
end;(* of structure CODE_GEN *)