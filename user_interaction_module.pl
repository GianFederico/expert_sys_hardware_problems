:- module(user_interaction_module, [askif/1, askifnot/1, start/0, askfor/1]).

:- dynamic(asked/2).

askif(Q) :- ask(Q,A), positive_answer(Q,A).

positive_answer(_,A) :- affirmative(A).
positive_answer(Qcode,A) :- \+(negative(A)), \+(affirmative(A)), write('Please answer yes or no.'), read(A2), retract(asked(Qcode,A)), asserta(asked(Qcode,A2)), affirmative(A2).

askifnot(Q) :- not(askif(Q)).

ask(Qcode,A) :- asked(Qcode,A).
ask(Qcode,A) :- \+(asked(Qcode,A)), questioncode(Qcode,Q), write(Q), write('?'), read(A2), ask2(Q,Qcode,A2,A).

ask2(_,Qcode,'?',A) :- explain(Qcode), ask(Qcode,A).
ask2(_,Qcode,A,A) :- \+(A = '?'), asserta(asked(Qcode,A)).

affirmative(yes).
affirmative(y).
affirmative(ye).
affirmative(right).
affirmative(ok).
affirmative(uhhuh).

negative(no).
negative(n).
negative(not).
negative(never).
negative(impossible).
negative(haha).





reset :-
    retractall(known(_, _, _)). %da rivedere@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

start :-
    reset, nl,
    write('Welcome to FAULT-FINDER!'), nl,
    write('let me ask some question about your pc-related problem:'), nl, nl,
    diagnosis_module:diagnosis(Diagnosis),
    nl,
    format('DIAGNOSIS: ~w~n', [Diagnosis]).


askinfo(Question, Answer) :-
    write(Question),
    read(Answer).

askfor(power_supply) :-
    askinfo('   What is the wattage of the power supply? ', Wattage),
    assertz(diagnosis_module:power_supply(Wattage)).

askfor(cpu) :-
    askinfo('   What is the manifacturer of the CPU (Intel or AMD)? ', Manifacturer),
    askinfo('   What is the TDP of the CPU? ', Tdp),
    assertz(diagnosis_module:cpu(Manifacturer, Tdp)).

askfor(gpu) :-
    askinfo('   What is the TDP of the graphics card? ', Tdp),
    assertz(diagnosis_module:gpu(Tdp)).

askfor(ram) :-
    %askinfo('What is the capacity of RAM (in GB)? ', Capacity),
    % askinfo('What is the speed of RAM? ', Speed),
    askinfo('   What is the type of RAM? (ddr?) ', Type),
    assertz(diagnosis_module:ram(Type)). %ram(Type, Capacity, Speed)

askfor(hdd) :-
    askinfo('   How old is the hard drive?', Age),
    assertz(diagnosis_module:hdd(Age)).

% askfor(ssd) :-
%     askinfo('How old is the solid state drive?', Age),
%     assertz(diagnosis_module:ssd(Age)).

askfor(mobo) :-
    askinfo('   What cpus can the motherboard run (Intel or AMD)', Cpu_comp),
    askinfo('   What type of RAM can the motherboard read (ddr?)', Ram_comp),
    askinfo('   What is the TDP of the motherboard?', Tdp),
    assertz(diagnosis_module:mobo(Cpu_comp, Ram_comp, Tdp)).

