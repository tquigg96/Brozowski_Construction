# Brozowski_Construction
Derivative-based conversion from RegExp' to FSM -- (Brzozowski Construction) (Formal Language - finite automata)

The Brozowski Construction is just one of the many ways to prove that regular expressions are equivalant to FSMs. In the construct of an FSM it utilizes a search pattern in order to accept or reject a given language (sets of string). Let us examine this example of a language:
>>[[i,n,t], [m,a,i,n]]
>>[transition(qs,char) -> ((0,i),(1,n),(2,t),(3,_)(4,m),(5,a),(6,i),(7,n))] essentially d(q,a)

This is a construction of a finite state machine for the simple syntax int main in c++ utilizing d q a. d represents the set of transitions while q represents the state and a represents the char being compared. It searches through each character in each individual string that makes a language and decides whether or not it will accept or reject the language. For instance: if our set of strings contains a syntactial error like, Int main, it would reject it due to the first character being capitalized and not following the rule set outlined in the FSM.



Essentially, what this program is doing is a unique way to do this conversion using derivatives to accomplish the task. 
