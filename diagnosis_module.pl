:- module(diagnosis_module, [diagnosis/1, questioncode/2]).
:- use_module(user_interaction_module).
%:- dynamic(diagnosis/1).

/* Top-level diagnosis rules */
diagnosis('PSU Turned OFF.') :- power_problem_current, askifnot(psu_on).
diagnosis('PSU not plugged in.') :- power_problem_current, askifnot(psu_on), askifnot(just_plugged).
diagnosis('Insufficient PSU wattage.') :- askif(pc_dead), psu_problem, askif(crashes), askif(more_info), askfor(power_supply), askfor(cpu), askfor(gpu), askfor(mobo),
                                            cpu(_,Cpu_wattage), gpu(Gpu_wattage), power_supply(Psu_wattage), mobo(_,_,Tdp), 
                                            Required_wattage is Cpu_wattage + Gpu_wattage + Tdp + 50,
                                            Psu_wattage < Required_wattage.
diagnosis('Incompatible RAM.') :- askifnot(pc_dead), askif(lights_turn_on), askif(more_info), askfor(mobo), askfor(ram), 
                                    ram(Type), mobo(_,Ram_comp,_), dif(Type, Ram_comp).  %The dif/2 predicate is a constraint that is true if and only if A and B are different terms. If A and B can never unify, dif/2 succeeds deterministically. If A and B are identical, it fails immediately.
diagnosis('Incompatible CPU.') :- askifnot(pc_dead), askifnot(lights_turn_on), askif(more_info), askfor(mobo), askfor(cpu), 
                                    cpu(Manifacturer,_), mobo(Cpu_comp,_,_), dif(Manifacturer, Cpu_comp).
diagnosis('CPU thermal throttle, insufficient cooling.') :- slow_performance, cpu_problem.
diagnosis('Old HDD, You need to replace it.') :- slow_performance, mechanical_problem_hdd, askif(more_info), askfor(hdd), hdd(Hdd_age), Hdd_age>4.
diagnosis('GPU thermal throttle, insufficient cooling.') :- slow_performance, gpu_problem.
diagnosis('Damaged HDD, may require replacement.') :- askif(pc_dead), mechanical_problem_hdd.
diagnosis('Debris or cables between fan blades.') :- askif(pc_dead), mechanical_problem_fans.
diagnosis('Some cables are unplugged, check every internal cable.') :- power_problem_current, askifnot(psu_on), askif(just_plugged).
diagnosis('Out-of-date BIOS.') :- askif(new_component).
diagnosis('PSU may be underspec.') :- askif(pc_dead), psu_problem, askif(crashes), askif(more_info), askfor(power_supply), askfor(cpu), askfor(gpu), askfor(mobo),
                                            cpu(_,Cpu_wattage), gpu(Gpu_wattage), power_supply(Psu_wattage), mobo(_,_,Tdp), 
                                            Required_wattage is Cpu_wattage + Gpu_wattage + Tdp,
                                            Psu_wattage < Required_wattage.
diagnosis('I am sorry. I cannot find the problem with the information you provided.').


/* Definitions of intermediate predicates */                            
power_problem_current :- askifnot(pc_dead).                             

mechanical_problem_hdd :- askif(has('an HDD')), askif(hear('whiny noise from the HDD')).
mechanical_problem_fans :- askif(hear('weird noise from the fans')).

slow_performance :- askif(slow_pc).
cpu_problem :- askif(cpu_termal).
gpu_problem :- askif(gpu_termal).
psu_problem :- askif(crashes).

specific_problem :- askif(more_info), askfor(power_supply), askfor(cpu), askfor(gpu), askfor(ram), askfor(hdd), askfor(ssd), askfor(mobo).

/* Question decoding */
questioncode(more_info, 'Are you able to give me some more specific information').
questioncode(pc_dead, 'Does the pc turn on').
questioncode(cpu_termal, 'Does the CPU run over 85C').
questioncode(gpu_termal, 'Does the GPU run over 85C').
questioncode(slow_pc, 'Does the PC run slow').
questioncode(handyperson, 'Are you confident in fixing things').
questioncode(psu_on, 'Is the ON/OFF switch of the PSU set to ON').
questioncode(just_plugged, 'Did you plug the PC in').
questioncode(has(X), X) :- write('Does the PC have ').
questioncode(hear(X), X) :- write('Do you hear a ').
questioncode(lights_turn_on, 'Do the lights turn on even for a second').
questioncode(crashes, 'Does the PC crashes often while under load').
questioncode(new_component, 'Do you have recently installed a new component').












% % rules

% compatible(X, Y) :- X = psu, power_supply(Y, W), W >= 500.
% compatible(X, Y) :- X = cpu, cpu(Y, intel, _, _, _).
% compatible(X, Y) :- X = graphics_card, graphics_card(Y, _, _, _, _, _).
% compatible(X, Y) :- X = motherboard, motherboard(Y, intel, _, _, _, _, _, _, _).
% compatible(X, Y) :- X = ram, ram(Y, ddr4, _, _).
% compatible(X, Y) :- X = hard_drive, hard_drive(Y, _, _, sata3).
% compatible(X, Y) :- X = solid_state_drive, solid_state_drive(Y, _, sata3, _).
% compatible(X, Y) :- X = cpu_cooler, cpu_cooler(Y, air, _).
% compatible(X, Y) :- X = gpu_cooler, gpu_cooler(Y, fan).

% requires(X, Y) :- X = cpu, Y = psu


