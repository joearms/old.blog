Nonterminals

seq0 seq elseifs else item.


Terminals
str setToken endSetToken exprToken ifToken forToken endForToken 
elseifToken  elseToken endIfToken.

Rootsymbol seq.

seq -> seq0: {eheSeq, lists:reverse('$1')}.
    
seq0 -> seq0 item : ['$2'|'$1'].
seq0 -> '$empty' : [].

item -> setToken seq endSetToken: {eheSet, unwrap('$1'), '$2'}.
item -> ifToken seq elseifs else endIfToken: 
        {eheIf, [{test,unwrap('$1'),'$2'}|'$3'],'$4'}.
item -> forToken seq endForToken:             {eheFor,unwrap('$1'),'$2'}.
item -> str:                                  {eheStr, unwrap('$1')}.
item -> exprToken:                            {eheExpr, unwrap('$1')}.
    

elseifs -> elseifs elseifToken seq: [{test,unwrap('$2'),'$3'}] ++ '$1'.
elseifs -> '$empty': [].

else -> elseToken seq: {yes,'$2'}.
else -> '$empty':      no.

Erlang code.
-export([simplify/1]).
-uuid("da83550e-aac5-4fab-a31e-7e81c9ae34b0").

-tags([ehe,parser]).

-description("Parses ehe").


unwrap({_,_,V}) -> V.

simplify({_Tag,A,nil}) -> A;
simplify(X) -> X.
