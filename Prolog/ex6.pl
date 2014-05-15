
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 1: Relational Logic Programming
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Signature: customer(CustomerID, CompanyName, ContactName, ContactTitle, Address, City, PostalCode,Country)/8
% Purpose: Describe a table in a relational database containing the information of customers.
customer(aLFKI, alfredsFutterkiste, mariaAnders, salesRepresentative, obereStr57, berlin, 12209, germany).
customer(aNATR,	anaTrujilloEmparedadosyHelados, anaTrujillo, owner, avdaDeLaConstitución2222, méxicoDF, 05021, mexico).
customer(aNTON, antonioMorenoTaquería, antonioMoreno, owner, mataderos2312, méxicoDF, 05023, mexico).
customer(aROUT, aroundtheHorn, thomasHardy, salesRepresentative, hanoverSq120, london, wA11DP, uk).
customer(bERGS, berglundsSnabbköp, christinaBerglund, orderAdministrator, berguvsvägen8, luleå, s95822, sweden).

% Signature: product(ProductID, ProductName, UnitPrice)/3
% Purpose: Describe a table in a relational database containing the information of products.
product(1, chai, 18).
product(2, chang, 19).
product(3, aniseedsyrup, 10).
product(4, chefAntonCajunSeasoning, 22).
product(5, chefAntonGumboMix,21.35).
product(6, grandmaBoysenberrySpread, 25).
product(7, uncleBobOrganicDriedPears, 30).
product(8, northwoodsCranberrySauce, 40).
product(9, mishiKobeNiku, 97).
product(10, ikura, 31).

% Signature: order(OrderID, CustomerID)/2
% Purpose: Describe a table in a relational database that contains orders placed by customers.
order(10643, aLFKI).
order(10692, aLFKI).
order(10702, aLFKI).
order(10835, aLFKI).
order(10759, aNATR).
order(10625, aNATR).
order(10572, bERGS).
order(10524, bERGS).

% Signature: order_details(OrderID, ProductID, Quantity, Discount)	/4
% Purpose: Describe a table in a relational database that contains details of customer orders.
order_details(10524, 10, 2, 0).
order_details(10572, 1,	12, 0.1).
order_details(10625, 1,	3, 0).
order_details(10625, 4,	5, 0).
order_details(10625, 6,	10, 0).
order_details(10759, 3,	10, 0).
order_details(10835, 5,	15, 0).
order_details(10835, 7,	2, 0.2).
order_details(10702, 3,	6, 0).
order_details(10702, 7,	15, 0).
order_details(10692, 6,	20, 0).
order_details(10643, 2,	15, 0.25).
order_details(10643, 3,	21, 0.25).
order_details(10643, 4,	2, 0.25).

% Signature: total_price_customer_product(CustomerID, ProductName, Discount)/3
% Purpose: gives for each customer, the name of the products he ordered
% in all orders he placed with the discount he got on that product.
total_price_customer_product(CustomerID, ProductName, Discount):-
 customer(CustomerID, _CompanyName, _ContactName, _ContactTitle, _Address, _City, _PostalCode,_Country),
 order(OrderID, CustomerID),
 order_details(OrderID, ProductID, _Quantity, Discount),
 product(ProductID, ProductName, _UnitPrice).

% Signature: european_customer(CustomerID)/1
% Purpose: finds customers from europe.
 european_customer(CustomerID):-
        customer(CustomerID, _CompanyName, _ContactName, _ContactTitle, _Address, _City, _PostalCode, germany).
 european_customer(CustomerID):-
        customer(CustomerID, _CompanyName, _ContactName, _ContactTitle, _Address, _City, _PostalCode, uk).
 european_customer(CustomerID):-
        customer(CustomerID, _CompanyName, _ContactName, _ContactTitle, _Address, _City, _PostalCode, sweden).



% Signature: europe_ordered_products(ProductName, ProductID)/2
% Purpose: gets the names and ids of products that were ordered by European customers.

 europe_ordered_products(ProductName, ProductID):-
	 product(ProductID, ProductName, _UnitPrice),
	 order_details(OrderID, ProductID, _Quantity, _Discount),
	 order(OrderID, CustomerID),
	 european_customer(CustomerID).

% some products appear more than once because they were invited more
% than once by an european customer(by the same european customer or by
% a diffrent european customers)in two different orders.When the
% intepreter tries to prove the third query in this procedure, he findes
% two posible OrderID for the same productName and ProductID(first he
% finds one and returns an answer. then the user presses ';' and he
% backtrace to this query and finds another answer an returns it too,
% and so on).


% Signature: recommend_product(ProductName, RecommendedProductName)/2
% Purpose: recommends some products according to a given product ProductName.
 recommend_product(ProductName, RecommendedProductName):-
	product(ProductID, ProductName, _UnitPrice),
	order_details(OrderID, ProductID, _Quantity, _Discount),
	order(OrderID, CustomerID),
	order(OrderID_2, CustomerID),
	order_details(OrderID_2, ProductID_2, _Quantity_2, _Discount_2),
	ProductID_2 \= ProductID,
	product(ProductID_2, RecommendedProductName, _UnitPrice_2).


% Signature: recommend(ProductName, RecommendedProductName, Level)/3
% Purpose: gives a recommendation RecommendedProductName, of a different
% product with level of closeness Level, according to given product
% ProductName.
recommend(ProductName, RecommendedProductName, s(0)):- recommend_product(ProductName, RecommendedProductName).
recommend(ProductName, RecommendedProductName, s(N)):-
	recommend_product(ProductName, RecommendedProductName_2),
	recommend(RecommendedProductName_2 ,RecommendedProductName, N),
	RecommendedProductName \= ProductName.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Question 2: Lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Signature: append(List1, List2, ListResult)/3
% Purpose: ListResult is a list starting with the elements of List1
% and ending with the elements of List2, in the original order.
%append([],X,X).
%append([X|Xs],Y,[X|Zs]):- append(Xs,Y,Zs).

% Signature: super_append(SuperList, ListResult)/2
% Purpose: SuperList is a list of lists and ListResult contains all the
% elements of all the lists in SuperList in the order or their
% appearance.
super_append([], []).
super_append([X | Rest], ListResult):-
	super_append(Rest, ListResult_2),
	append(X, ListResult_2, ListResult).


% the query:
% ?- super_append(SuperList, [a,b,c,d]).
% returns a few assignments to SuperList the then an error "out of
% global stack" appears. the intepreter tries to prove the second rule
% in the functor. he tries at first all the permotations of the list
% [a,b,c,d] as an concatenating of up to two lists. when he tries more
% than two lists, it has many options (it can always add the empty list
% as an element in SuperList and get the same result). for a
% concatenation of more then two lists there isn't a concrete binding to
% the variables X and Rest in the rules, so the intepreter gets in to
% infinite loop.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Question 3: Context-Free Grammar parsing and syntax tree
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Signature: boolean(Exp)
% Purpose: recognizes a scheme boolean expression.
boolean(yep).
boolean(na).

% Signature: variable(Exp)
% Purpose: recognizes a scheme language variable.
variable(x).

% Signature: atomic_formula(Exp)
% Purpose: recognizes a scheme atomic expression.
atomic_formula(Exp):- boolean(Exp).
atomic_formula(Exp):- variable(Exp).

s(Expression) :- exp(Expression).

% Signature: exp(Expression)/1
% Purpose: recognizes of expressions of Scheme language.
%
exp(Expression) :- atomic_formula(Expression).
exp(Expression) :- composite(Expression).


% Signature: form(Exp)/1
% Purpose: recognizes a scheme Form expression.
form_eval([Exp|[]]) :- exp(Exp).
form_eval([Exp|Rest]) :- exp(Exp), form_eval(Rest).

% Signature: lambda(Exp)/1
% Purpose: recognizes a scheme Lambda expression.
lambda_eval([lambda | [[x|[]] | Exp]]) :- exp(Exp).

% Signature: if(Exp)/1
% Purpose: recognizes a scheme if expression.
if_eval([if, Exp1, Exp2, Exp3]):-
	exp(Exp1), exp(Exp2), exp(Exp3).


% Signature: symbol(X)/1
% Purpose: recognizes a scheme symbol expression.
symbol_eval([quote, _X]).


% Signature: composite(Exp)/1
% Purpose: recognizes a scheme composite expression.
composite(Exp) :- lambda_eval(Exp).
composite(Exp) :- if_eval(Exp).
composite(Exp) :- symbol_eval(Exp).
composite(Exp) :- form_eval(Exp).

% The quary -?exp(X) returns:
% X = yep ;
% X = na ;
% X = x ;
% X = [lambda, [x]|yep] ;
% X = [lambda, [x]|na] ;
%(only first 5)
% There are infinite answers becase of the sequece form, the interpreter
% can always magnify the size of form expression.

% a parse tree of lambda expression.
lambda(parameter(x),body(_Exp)).

% a parse tree of if expression.
if(condition(_Exp1),consequent(_Exp2),alternative(_Exp3)).

% a parse tree of form expression.
form([_H|_T]).

% a parse tree of lambda's body.
body([_H|_T]).

% a parse tree of lambda's parameter.
parameter(x).

% a parse tree of if's condition.
condition(_X).
% a parse tree of if's consequent.
consequent(_X).
% a parse tree of if's alternative.
alternative(_X).

% a parse tree of quote expression.
quote(_X).



% Signature: parse_seq(Seq, ParseList)/2
% Purpose: a relation between a list of expressions in
% scheme language and a list of parse tree of those expressions.
parse_seq([H|[]],[ParseH|[]]):- parse(H, ParseH).
parse_seq([H|T],[ParseH|ParseT]):- parse(H,ParseH), parse_seq(T, ParseT).

% Signature: parse(Expression, ParseTree)/2
% Purpose: a relation between an expression in scheme language
% and a parse tree of the expression.
parse(yep, boolean(yep)).
parse(na, boolean(na)).
parse(x, variable(x)).
parse([quote | [X|[]]], quote(X)).
parse([lambda | [[x] | Body]], lambda(parameter(x), body(ParsedSeq))):- parse_seq(Body, ParsedSeq).
parse([if | [Exp1 | [Exp2 | [Exp3|[]]]]], if(condition(ParseCond),consequent(ParseCons),alternative(ParseAlter))):-
	parse(Exp1, ParseCond), parse(Exp2, ParseCons), parse(Exp3, ParseAlter).
parse(Expression, form(ParseSeq)):- parse_seq(Expression, ParseSeq).




infinite(X):-infinite(X).

