has(dorcus_bucephalus,large_fangs_curved_outside).
has(dorcus_bucephalus,the_front_half_of_the_pliers_is_facing_down_but_the_rounded_end_is_bent_up).
has(dorcus_bucephalus,size_50_to_80mm).
has(dorcus_bucephalus,shadow_wings).

has(alltopus_moellenkanpi,golden_green_skin).
has(alltopus_moellenkanpi,black_spot_color).
has(alltopus_moellenkanpi,the_first_segment_has_many_colored_spots).
has(alltopus_moellenkanpi,size_50_to_70mm).
has(alltopus_moellenkanpi,black_color).

has(hexarthrius_mandibularis,dark_brown).
has(hexarthrius_mandibularis,slender_body).
has(hexarthrius_mandibularis,long-nose_pliers_are_quite_thick_Almost_as_long_as_the_body).
has(hexarthrius_mandibularis,very_small_teeth_arranged_from_middle_to_tip).
has(hexarthrius_mandibularis,size_49_to_108mm).
has(hexarthrius_mandibularis,the_large_tooth_is_in_the_middle).

has(hexarthrius_nigritus,the_teeth_at_the_end_of_the_pliers_are_small_and_tapered).
has(hexarthrius_nigritus,size_43_to_75mm).
has(hexarthrius_nigritus,black_color).

has(phalacrognathus_muelleri,beautiful_color).
has(phalacrognathus_muelleri,rainbow_reflection).
has(phalacrognathus_muelleri,long_pliers_curved_together).
has(phalacrognathus_muelleri,size_37_to_68mm).
has(phalacrognathus_muelleri,the_tip_of_the_horn_is_two-pointed).

has(dorcus_reichei,the_blade_is_less_curved_and_near_the_tip_there_are_many_teeth).
has(dorcus_reichei,size_24_to_45mm).
has(dorcus_reichei,red_black_color).
has(dorcus_reichei,the_tip_of_the_horn_is_two-pointed).
has(dorcus_reichei,smooth_wings).

has(dorcus_tityus,small_teeth_point_forward).
has(dorcus_tityus,size_21_to_67mm).
has(dorcus_tityus,large_curved_fangs).
has(dorcus_tityus,red_black_color).
has(dorcus_tityus,the_large_tooth_is_in_the_middle).
has(dorcus_tityus,smooth_wings).

has(dorcus_curvidens,big_shape_at_the_front).
has(dorcus_curvidens,small_teeth_at_tip).
has(dorcus_curvidens,size_30_to_75mm).
has(dorcus_curvidens,there_are_12_deep_and_horizontal_grooves_on_the_wing).
has(dorcus_curvidens,large_curved_fangs).
has(dorcus_curvidens,the_large_tooth_is_in_the_middle).
has(dorcus_curvidens,black_color).
has(dorcus_curvidens,shadow_wings).

has(hexarthrius_parryi,tip_of_antenna_4_to_5_segments).
has(hexarthrius_parryi,size_43_to_80mm).
has(hexarthrius_parryi,yellow_wings).

has(dorcus_titanus,large_fangs_curved_outward_especially_near_the_tip).
has(dorcus_titanus,the_teeth_are_arranged_like_a_saw).
has(dorcus_titanus,size_40_to_86mm).
has(dorcus_titanus,smooth_wings).
has(dorcus_titanus,large_curved_fangs).

has(dorcus_antaeus,size_40_to_80mm).
has(dorcus_antaeus,shadow_wings).
has(dorcus_antaeus,the_large_tooth_is_in_the_middle).
has(dorcus_antaeus,black_color).
has(dorcus_antaeus,large_curved_fangs).

has(hexarthrius_vitalisii,long-nose_pliers_curve_inward_and_slightly_downward).
has(hexarthrius_vitalisii,tip_of_antenna_5_to_6_segments).
has(hexarthrius_vitalisii,size_45_to_82mm).

% Define the menu options
option(1, 'Input beetle name or input attribute').
option(2, 'confirm beetle breed by attribute check').
option(3, 'Exit').

% Methods corresponding to each option
action_for_option(1) :-
    choice1 , nl.
action_for_option(2) :-
    choice2, nl.

% Display menu options
display_menu :-
    write('Menu:'), nl,
    option(Number, Name),
    format('~w. ~w~n', [Number, Name]),
    fail.
display_menu :- nl.

% Predicate to handle user input
handle_input :-
    repeat,
    write('Enter your choice: '),
    read(Choice),
    process_choice(Choice).

% Process user choice
process_choice(Choice) :-
    Choice == 3, !, % Exit option
    write('Exiting...').
process_choice(Choice) :-
    (Choice == 1 ; Choice == 2), % Check for Option A or Option B
    action_for_option(Choice), % Perform action based on the choice
    handle_input.
process_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    handle_input.

% Start the menu
start_menu :-
    display_menu,
    handle_input.

choice1 :-
write('Type a name beetle or attribute of beetle.'),nl,
    read(A),
    process(A).

process(A) :-
    has(A, B),
    output(B, A),
    fail. % This line ensures that Prolog backtracks to find all solutions

process(A) :-
    has(B, A),
    output(A, B),
    fail.

process(_).

output(X, Y) :-
    write(X), write(' is an attribute of the '), write(Y), write('.'), nl.


choice2:-
breedcheck(Name),
write('the beetle breed is '),
write(Name),
nl,
undo.
breedcheck(dorcus_bucephalus) :- dorcus_bucephalus, !.
breedcheck(alltopus_moellenkanpi) :- alltopus_moellenkanpi, !.
breedcheck(hexarthirus_mandibularis) :- hexarthirus_mandibularis , !.
breedcheck(hexarthirus_nigritus) :- hexarthirus_nigritus, !.
breedcheck(phalacrognathus_muelleri) :- phalacrognathus_muelleri, !.
breedcheck(dorcus_reichei) :- dorcus_reichei, !.
breedcheck(dorcus_tityus) :- dorcus_tityus, !.
breedcheck(dorcus_curvidens) :- dorcus_curvidens, !.
breedcheck(hexarthrius_parryi) :- hexarthrius_parryi, !.
breedcheck(dorcus_antaeus) :- dorcus_antaeus, !.
breedcheck(dorcus_titanus) :- dorcus_titanus, !.
breedcheck(hexarthrius_vitalisii) :- hexarthrius_vitalisii, !.
breedcheck(unknow).

dorcus_bucephalus :-
verify(large_fangs_curved_outside),
verify(the_front_half_of_the_pliers_is_facing_down_but_the_rounded_end_is_bent_up),
verify(size_50_to_80mm),
verify(shadow_wings),
nl.

alltopus_moellenkanpi :-
verify(black_color),
verify(the_first_segment_has_many_colored_spots),
verify(size_50_to_70mm),
verify(back_spot_color),
verify(golden_green_skin),
nl.

hexarthirus_mandibularis :-
verify(dark_brown_color),
verify(slender_body),
verify(long-nose_pillers_are_quite_thick_almost_as_long_as_the_body),
verify(very_small_teeth_arranged_from_middle_to_tip),
verify(size_49_to_1088mm),
verify(the_large_tooth_is_in_the_middle),

nl.

hexarthirus_nigritus :-
verify(black_color),
verify(the_teeth_at_the_end_of_the_pliers_are_small_and_tapered),
verify(size_43_to_75mm),

nl.

phalacrognathus_muelleri :-
verify(beautiful_color),
verify(rainbow_reflection),
verify(long_pliers_curved_together),
verify(size_37_to_68mm),
verify(the_tip_of_the_horn_is_two-pointed),

nl.

dorcus_reichei :-
verify(red_black_color),
verify(the_blade_is_less_curved_and_near_the_tip_there_are_many_teeth),
verify(size_24_to_45mm),
verify(the_tip_of_the_horn_is_two-pointed),
verify(smooth_wings),

nl.

dorcus_tityus :-
verify(red_black_color),
verify(small_teeth_point_forward),
verify(size_21_to_67mm),
verify(large_curved_fangs),
verify(the_large_tooth_is_in_the_middle),
verify(smooth_wings),
nl.

dorcus_curvidens :-
verify(black_color),
verify(big_shape_at_the_front),
verify(small_teeth_at_tip),
verify(size_30_to_75mm),
verify(there_are_12_deep_and_horizontal_grooves_on_the_wing),
verify(large_curved_fangs),
verify(the_large_tooth_is_in_the_middle),
verify(shadow_wings),

nl.

hexarthrius_parryi :-
verify(yellow_wings),
verify(tip_of_antenna_4_to_5_segments),
verify(size_43_to_80mm),

nl.

dorcus_antaeus :-
verify(black_color),
verify(size_40_to_80mm),
verify(shadow_wings),
verify(the_large_tooth_is_in_the_middle),
verify(large_curved_fangs),

nl.

dorcus_titanus :-
verify(large_fangs_curved_outward_especially_near_the_tip),
verify(the_teeth_are_arranged_like_a_saw),
verify(size_40_to_86mm),
verify(smooth_wings),
verify(large_curved_fangs),

nl.

hexarthrius_vitalisii :-
verify(long-nose_pliers_curve_inward_and_slightly_downward),
verify(tip_of_antenna_5_to_6_segments),
verify(size_45_to_82mm),
nl.

/* how to ask questions */
ask(Question) :-
write('Does the beetle have following atribue:'),
write(Question),
write('? '),
read(Response),
nl,
( (Response == yes ; Response == y)
->
assert(yes(Question)) ;
assert(no(Question)), fail).
:- dynamic yes/1,no/1.
/*How to verify something */
verify(S) :-
 (yes(S)
  ->
   true ;
 (no(S)
  ->
   fail ;
 ask(S))).
/* undo all yes/no assertions*/
undo :- retract(yes(_)),fail.
undo :- retract(no(_)),fail.
undo.
